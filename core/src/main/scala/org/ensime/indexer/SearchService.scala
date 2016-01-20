package org.ensime.indexer

import java.sql.SQLException

import akka.actor._
import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2._
import org.ensime.api._
import org.ensime.indexer.DatabaseService._
import org.ensime.util.file._
import scala.util.Failure
import scala.util.Success

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

/**
 * Provides methods to perform ENSIME-specific indexing tasks,
 * receives events that require an index update, and provides
 * searches against the index.
 *
 * We have an H2 database for storing relational information
 * and Lucene for advanced indexing.
 */
class SearchService(
  config: EnsimeConfig,
  resolver: SourceResolver
)(
  implicit
  actorSystem: ActorSystem,
  vfs: EnsimeVFS
) extends ClassfileIndexer
    with FileChangeListener
    with SLF4JLogging {

  private val QUERY_TIMEOUT = 30 seconds
  private val version = "1.0"

  private val index = new IndexService(config.cacheDir / ("index-" + version))
  private val db = new DatabaseService(config.cacheDir / ("sql-" + version))

  implicit val workerEC = actorSystem.dispatchers.lookup("akka.search-service-dispatcher")

  // FIXME: apologies, this is pretty messy. We should move to an
  // actor based system for all of the persisting instead of this
  // hybrid approach.

  private def scan(f: FileObject) = f.findFiles(EnsimeVFS.ClassfileSelector) match {
    case null => Nil
    case res => res.toList
  }

  /**
   * Indexes everything, making best endeavours to avoid scanning what
   * is unnecessary (e.g. we already know that a jar or classfile has
   * been indexed).
   *
   * @return the number of rows (removed, indexed) from the database.
   */
  def refresh(): Future[(Int, Int)] = {
    // it is much faster during startup to obtain the full list of
    // known files from the DB then and check against the disk, than
    // check each file against DatabaseService.outOfDate
    def findStaleFileChecks(checks: Seq[FileCheck]): List[FileCheck] = {
      log.info("findStaleFileChecks")
      val jarUris = config.allJars.map(vfs.vfile).map(_.getName.getURI)
      for {
        check <- checks
        name = check.file.getName.getURI
        if !check.file.exists || check.changed ||
          (!name.endsWith(".class") && !jarUris(name))
      } yield check
    }.toList

    // delete the stale data before adding anything new
    // returns number of rows deleted
    def deleteReferences(checks: List[FileCheck]): Future[Int] = {
      log.info(s"removing ${checks.size} stale files from the index")
      deleteInBatches(checks.map(_.file))
    }

    // a snapshot of everything that we want to index
    def findBases(): Set[FileObject] = {
      log.info("findBases")
      config.modules.flatMap {
        case (name, m) =>
          m.targetDirs.flatMap { d => scan(vfs.vfile(d)) } :::
            m.testTargetDirs.flatMap { d => scan(vfs.vfile(d)) } :::
            m.compileJars.map(vfs.vfile) ::: m.testJars.map(vfs.vfile)
      }
    }.toSet ++ config.javaLibs.map(vfs.vfile)

    def indexBase(base: FileObject, fileCheck: Option[FileCheck]): Future[Option[Int]] = {
      val outOfDate = fileCheck.map(_.changed).getOrElse(true)
      if (!outOfDate) Future.successful(None)
      else {
        val check = FileCheck(base)
        extractSymbolsFromClassOrJar(base).flatMap(persist(check, _))
      }
    }

    // index all the given bases and return number of rows written
    def indexBases(bases: Set[FileObject], checks: Seq[FileCheck]): Future[Int] = {
      log.info("Indexing bases...")
      val checksLookup: Map[String, FileCheck] = checks.map(check => (check.filename -> check)).toMap
      val basesWithChecks: Set[(FileObject, Option[FileCheck])] = bases.map { base =>
        (base, checksLookup.get(base.getName().getURI()))
      }
      Future.sequence(basesWithChecks.map { case (file, check) => indexBase(file, check) }).map(_.flatten.sum)
    }

    def commitIndex(): Future[Unit] = Future {
      blocking {
        log.debug("committing index to disk...")
        index.commit()
        log.debug("...done committing index")
      }
    }

    // chain together all the future tasks
    for {
      checks <- db.knownFiles()
      stale = findStaleFileChecks(checks)
      deletes <- deleteReferences(stale)
      bases = findBases()
      added <- indexBases(bases, checks)
      _ <- commitIndex()
    } yield (deletes, added)
  }

  def refreshResolver(): Unit = resolver.update()

  def persist(check: FileCheck, symbols: List[FqnSymbol]): Future[Option[Int]] = {
    val iwork = Future { blocking { index.persist(check, symbols) } }

    iwork.onComplete {
      case Success(_) => log.info(s"successfully indexed $check")
      case Failure(t) => log.error(s"badness indexing $check", t)
    }

    val dwork = db.persist(check, symbols)
    iwork.flatMap { _ => dwork }
  }

  def extractSymbolsFromClassOrJar(file: FileObject): Future[List[FqnSymbol]] = file match {
    case classfile if classfile.getName.getExtension == "class" =>
      // too noisy to log
      val check = FileCheck(classfile)
      Future {
        blocking {
          extractSymbols(classfile, classfile)
        }
      }
    case jar =>
      log.debug(s"indexing $jar")
      val check = FileCheck(jar)
      Future {
        blocking {
          scan(vfs.vjar(jar)) flatMap (extractSymbols(jar, _))
        }
      }
  }

  private val blacklist = Set("sun/", "sunw/", "com/sun/")
  private val ignore = Set("$$anon$", "$$anonfun$", "$worker$")
  import org.ensime.util.RichFileObject._
  private def extractSymbols(container: FileObject, f: FileObject): List[FqnSymbol] = {
    f.pathWithinArchive match {
      case Some(relative) if blacklist.exists(relative.startsWith) => Nil
      case _ =>
        val name = container.getName.getURI
        val path = f.getName.getURI
        val (clazz, refs) = indexClassfile(f)

        val depickler = new ClassfileDepickler(f)

        val source = resolver.resolve(clazz.name.pack, clazz.source)
        val sourceUri = source.map(_.getName.getURI)

        // TODO: other types of visibility when we get more sophisticated
        if (clazz.access != Public) Nil
        else FqnSymbol(None, name, path, clazz.name.fqnString, None, None, sourceUri, clazz.source.line) ::
          clazz.methods.toList.filter(_.access == Public).map { method =>
            val descriptor = method.descriptor.descriptorString
            FqnSymbol(None, name, path, method.name.fqnString, Some(descriptor), None, sourceUri, method.line)
          } ::: clazz.fields.toList.filter(_.access == Public).map { field =>
            val internal = field.clazz.internalString
            FqnSymbol(None, name, path, field.name.fqnString, None, Some(internal), sourceUri, clazz.source.line)
          } ::: depickler.getTypeAliases.toList.filter(_.access == Public).map { rawType =>
            FqnSymbol(None, name, path, rawType.fqnString, None, None, sourceUri, None)
          }

    }
  }.filterNot(sym => ignore.exists(sym.fqn.contains))

  // TODO: provide context (user's current module and main/test)
  /** free-form search for classes */
  def searchClasses(query: String, max: Int): List[FqnSymbol] = {
    val fqns = index.searchClasses(query, max)
    Await.result(db.find(fqns), QUERY_TIMEOUT) take max
  }

  /** free-form search for classes and methods */
  def searchClassesMethods(terms: List[String], max: Int): List[FqnSymbol] = {
    val fqns = index.searchClassesMethods(terms, max)
    Await.result(db.find(fqns), QUERY_TIMEOUT) take max
  }

  /** only for exact fqns */
  def findUnique(fqn: String): Option[FqnSymbol] = Await.result(db.find(fqn), QUERY_TIMEOUT)

  /* DELETE then INSERT in H2 is ridiculously slow, so we put all modifications
   * into a blocking queue and dedicate a thread to block on draining the queue.
   * This has the effect that we always react to a single change on disc but we
   * will work through backlogs in bulk.
   *
   * We always do a DELETE, even if the entries are new, but only INSERT if
   * the list of symbols is non-empty.
   */

  val backlogActor = actorSystem.actorOf(Props(new IndexingQueueActor(this)), "ClassfileIndexer")

  // deletion in both Lucene and H2 is really slow, batching helps
  def deleteInBatches(
    files: List[FileObject],
    batchSize: Int = 1000
  ): Future[Int] = {
    val removing = files.grouped(batchSize).map(delete)
    Future.sequence(removing).map(_.sum)
  }

  // returns number of rows removed
  def delete(files: List[FileObject]): Future[Int] = {
    // this doesn't speed up Lucene deletes, but it means that we
    // don't wait for Lucene before starting the H2 deletions.
    val iwork = Future { blocking { index.remove(files) } }
    val dwork = db.removeFiles(files)
    iwork.flatMap(_ => dwork)
  }

  def fileAdded(f: FileObject): Unit = fileChanged(f)

  def fileRemoved(f: FileObject): Unit = {
    backlogActor ! FileUpdate(f, Nil)
  }

  // this can be very inefficent because duplicates result in duplicate indexes
  def fileChanged(f: FileObject): Unit =
    extractSymbolsFromClassOrJar(f).onComplete {
      case Success(symbols) => backlogActor ! FileUpdate(f, symbols)
      case Failure(t) => log.error(s"when processing $f", t)
    }

  def shutdown(): Future[Unit] = {
    db.shutdown()
  }
}

case class FileUpdate(
  fileObject: FileObject,
  symbolList: List[FqnSymbol]
)

class IndexingQueueActor(searchService: SearchService) extends Actor with ActorLogging {
  import context.system

  import scala.concurrent.duration._

  case object Process

  // De-dupes files that have been updated since we were last told to
  // index them. No need to aggregate values: the latest wins.
  // Key is the URI because FileObject doesn't implement equals
  var todo = Map.empty[String, (FileObject, List[FqnSymbol])]

  // debounce and give us a chance to batch (which is *much* faster)
  var worker: Cancellable = _

  private def debounce(): Unit = {
    Option(worker).foreach(_.cancel())
    import context.dispatcher
    worker = system.scheduler.scheduleOnce(5 seconds, self, Process)
  }

  override def receive: Receive = {
    case FileUpdate(fo, syms) =>
      todo += ((fo.getName.getURI, (fo, syms)))
      debounce()

    case Process if todo.isEmpty => // nothing to do

    case Process =>
      val (batch, remaining) = todo.splitAt(500)
      todo = remaining

      log.debug(s"Indexing ${batch.size} files")

      import searchService.workerEC
      for {
        _ <- searchService.delete(batch.values.map(_._1)(collection.breakOut))
        _ <- Future.sequence(
          // opportunity to do more batching here
          batch.collect {
            case (uri, (file, syms)) if syms.nonEmpty =>
              log.info(s"persisting $file with $syms")
              searchService.persist(FileCheck(file), syms)
          }
        )
      } yield ()
  }

}
