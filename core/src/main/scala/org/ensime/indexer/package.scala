// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime

import scala.util.Properties
package object indexer {
  implicit class RichProperties(val p: Properties.type) extends AnyVal {
    def isJava6: Boolean = p.javaVersion.startsWith("1.6")
  }
}
