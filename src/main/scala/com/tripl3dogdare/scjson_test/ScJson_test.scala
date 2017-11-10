package com.tripl3dogdare.scjson.test

import com.tripl3dogdare.scjson._

object ScJsonTest {
  def main(args:Array[String]) {
    println(obj(
      "test" -> "hello",
      "help me" -> 123,
      "array" -> arr(
        "test",
        213,
        arr( 1, 2, 3 )
      ),
      "object" -> obj(
        "test" -> 73
      )
    ).mkString)

    val jwick = obj(
      "name" -> "John Wick",
      "profession" -> "Hitman",
      "age" -> 23,
      "contacts" -> arr(
        obj( "name" -> "Danny Goki", "phone" -> "555-412-1253" ),
        obj( "name" -> "Lord Death", "phone" -> "42-42-564" ),
        obj( "name" -> "Zeus", "phone" -> "888-OLD-GODS" )
      )
    )
    jwick.getArray("contacts").foreach( contact => println(contact.mkString) )
    println(jwick.getArray("contacts").getObject(2).getString("phone"))
    println(jwick("contacts").as[JsonArray].apply(2).as[JsonObject].apply("phone").as[String])
    println(jwick.getArray("skills"))
    println(jwick | "contacts" | 1 $ "phone")

    println(parseJson("""
      {
        "name": "John Wick",
        "profession": "Hitman",
        "age": 23,
        "contacts": [
          { "name": "Danny Goki", "phone": "555-412-1253" },
          { "name": "Lord Death", "phone": "42-42-564" },
          { "name": "Zeus", "phone": "888-OLD-GODS" }
        ]
      }
    """).mkString)
  }
}