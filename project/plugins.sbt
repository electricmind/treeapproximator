addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")

//addSbtPlugin("de.johoop" % "findbugs4sbt" % "1.2.2")

//addSbtPlugin("com.orrsella" % "sbt-stats" % "1.0.5")

//addSbtPlugin("de.johoop" % "cpd4sbt" % "1.1.4")

//addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.3.2")

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.10.1")

resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.6.2")