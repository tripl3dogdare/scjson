name := "scjson"

organization := "com.github.tripl3dogdare"

version := "1.0.2"

scalaVersion := "2.12.4"

scalacOptions := Seq("-deprecation", "-feature")

mainClass in (Compile, run) := Some("com.tripl3dogdare.scjson.test.ScJsonTest")