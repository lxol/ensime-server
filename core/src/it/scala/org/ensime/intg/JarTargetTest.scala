package org.ensime.intg

import akka.event.slf4j.SLF4JLogging
import org.apache.commons.io.FileUtils
import org.ensime.api._
import org.ensime.fixture._
import org.scalatest._
import scala.concurrent.duration._

/**
 * Tries to simulate SBT clean/compile to stress test timing issues.
 *
 * (which also tests the file watchers).
 */
class JarTargetTest extends WordSpec with Matchers with Inside
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture
    with SLF4JLogging {

  val original = EnsimeConfigFixture.SimpleJarTestProject

  "ensime-server" should {
    "index and re-index jar targets" in {
      withEnsimeConfig { implicit config =>
        withTestKit { implicit testkit =>
          withProject { (project, asyncHelper) =>
            import testkit._

            mainTarget(original) should be a 'file

            // we want to support the case where the .jar doesn't
            // exist on startup, and we don't try to create it.
            mainTarget should not be 'exists

            FileUtils.copyFile(mainTarget(original), mainTarget)
            mainTarget should be a 'file

            // means the file addition was detected
            asyncHelper.expectMsg(CompilerRestartedEvent)

            awaitCond(
              {
                // should now be able to search the contents of that jar
                project ! PublicSymbolSearchReq(List("baz", "Foo"), 5)
                val hits = expectMsgType[SymbolSearchResults].syms
                println(hits)
                hits.filter {
                  case TypeSearchResult("baz", "Foo$", DeclaredAs.Class, Some(_)) => true
                  case _ => false
                }.nonEmpty
              },
              interval = 5 seconds
            )
          }
        }
      }
    }
  }

}
