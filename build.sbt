name := "scjson"

organization := "com.tripl3dogdare"

version := "1.0"

scalaVersion := "2.12.4"

scalacOptions := Seq("-deprecation", "-feature")

mainClass in (Compile, run) := Some("com.tripl3dogdare.scjson.test.ScJsonTest")