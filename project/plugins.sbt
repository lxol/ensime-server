// ensime-sbt is needed for the integration tests
addSbtPlugin("org.ensime" % "ensime-sbt" % "0.3.3")

// BUG https://github.com/sbt/sbt-header/issues/31
//addSbtPlugin("de.heikoseeberger" % "sbt-header" % "1.5.0")

// not working on Windows https://github.com/sbt/sbt/issues/1952
//addMavenResolverPlugin

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.5.1")

// sbt-coveralls needs a new release
// https://github.com/scoverage/sbt-coveralls/issues/52
addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "1.3.3")
//addSbtPlugin("org.scoverage" %% "sbt-coveralls" % "1.0.1")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.1")

scalacOptions in Compile ++= Seq("-feature", "-deprecation")

// sbt, STFU...
ivyLoggingLevel := UpdateLogging.Quiet
