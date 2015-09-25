name := """giat"""

version := "1.0"

scalaVersion := "2.11.7"

testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.6.3" cross CrossVersion.binary)
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "com.github.scalaprops" %% "scalaprops" % "0.1.14" % "test"
//libraryDependencies += "com.github.scalaprops" %% "scalaprops-scalazlaws" % "0.1.14" % "test"
libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.4.0"

