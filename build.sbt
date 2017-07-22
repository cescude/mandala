name := "Mandala"

scalaVersion := "2.12.2"

enablePlugins(ScalaJSPlugin)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.3"

libraryDependencies += "com.lihaoyi" %%% "scalarx" % "0.3.2"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.5"

skip in packageJSDependencies := false
