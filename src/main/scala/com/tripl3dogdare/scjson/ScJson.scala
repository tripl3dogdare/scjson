package com.tripl3dogdare

import collection.SeqLike
import scala.util.matching.Regex
import scala.util.control.NoStackTrace
import scala.annotation.tailrec
import scala.language.existentials
import scala.language.implicitConversions

/**
 * The main package for ScJson.
 * @author Connor Scialdone
 */
package object scjson {
  /** Convenience definition for the underlying type of JsonObject. */
  type JsonMap = Map[String, JsonValue[_]]

  /** Convenience definition for the underlying type of JsonArray. */
  type JsonList = List[JsonValue[_]]

  /** 
   * The primary superclass for all JSON types.
   * @tparam T The underlying type. Must be viewable as JsonValue.
   */
  sealed abstract class JsonValue[T <% JsonValue[_]] {
    /** The underlying value. */
    val value:T;

    /**
     * Converts this JsonValue to it's JSON string representation.
     * @return The JSON string.
     */
    def mkString:String; 

    /**
     * Casts this JsonValue to the given type.
     * Providese better type safety over asInstanceOf[Type] as it only allows types viewable as JsonValue.
     * @tparam A The desired type.
     */
    def as[A <% JsonValue[_]] = this.asInstanceOf[A]
  }

  /** 
   * A JSON type designed for holding other JSON values, such as an object or array.
   * @tparam K The type to be used as the collection's keys.
   * @tparam T The underlying collection type. Must be viewable as JsonValue. 
   */
  sealed abstract class JsonCollection[K <: JsonValue[_], T <% JsonValue[_]] extends JsonValue[T] {
    /**
     * Gets a value from the collection and casts it to the given type.
     * @tparam A The desired return type. Must be viewable as JsonValue.
     * @param key The key to look up.
     * @return The value at the given key, cast to the given type.
     */
    def getTyped[A <% JsonValue[_]](key:K):A;

    /**
     * Gets a JsonObject from the collection.
     * @param key The key to look up.
     * @return The object at the given key.
     */
    def getObject(key:K) = getTyped[JsonObject](key)

    
    /**
     * Gets a JsonArray from the collection.
     * @param key The key to look up.
     * @return The array at the given key.
     */
    def getArray(key:K) = getTyped[JsonArray](key)
    
    /**
     * Gets a JsonString from the collection.
     * @param key The key to look up.
     * @return The string at the given key.
     */
    def getString(key:K) = getTyped[JsonString](key)

    /**
     * Gets a JsonInt from the collection.
     * @param key The key to look up.
     * @return The integer at the given key.
     */
    def getInt(key:K) = getTyped[JsonInt](key)
    
    /**
     * Gets a JsonFloat from the collection.
     * @param key The key to look up.
     * @return The float at the given key.
     */
    def getFloat(key:K) = getTyped[JsonFloat](key)
    
    /**
     * Gets a JsonBoolean from the collection.
     * @param key The key to look up.
     * @return The boolean at the given key.
     */
    def getBoolean(key:K) = getTyped[JsonBoolean](key)
    
    /**
     * Gets a JsonNull from the collection.
     * @param key The key to look up.
     * @return The null at the given key.
     */
    def getNull(key:K) = getTyped[JsonNull](key)

    /**
     * Gets a JsonObject from the collection.
     * @param key The key to look up.
     * @return The object at the given key.
     */
    def getObj(key:K) = getObject(key)
    
    /**
     * Gets a JsonArray from the collection.
     * @param key The key to look up.
     * @return The array at the given key.
     */
    def getArr(key:K) = getArray(key)

    /**
     * Gets a JsonCollection from the collection.
     * @param key The key to look up.
     * @return The object or array at the given key.
     */
    def |(key:K) = getTyped[JsonCollection[JsonValue[_], JsonCollection[_,_]]](key);

    
    /**
     * Gets a JsonValue from the collection.
     * @param key The key to look up.
     * @return The value at the given key.
     */
    def $(key:K) = getTyped[JsonValue[_]](key)
  }

  /** 
   * Represents a JSON object.
   * Implements JsonCollection over a Map[String, JsonValue[_]].
   * @param value The underlying Map.
   */
  case class JsonObject(val value:JsonMap) extends JsonCollection[JsonString, JsonMap] {
    def mkString = value.map {case (k,v) => quote(k) + ":" + v.mkString}.mkString("{", ",", "}")
    def getTyped[A <% JsonValue[_]](key:JsonString) = value.getOrElse(key, null).asInstanceOf[A]
  }

  /** 
   * Represents a JSON array.
   * Implements JsonCollection over a List[JsonValue[_]].
   * @param value The underlying List.
   */
  case class JsonArray(val value:JsonList) extends JsonCollection[JsonInt, JsonList] {
    def mkString = value.map(_.mkString).mkString("[", ",", "]")
    def getTyped[A <% JsonValue[_]](key:JsonInt) = value.lift(key).getOrElse(null).asInstanceOf[A]
  }

  /**
   * A JSON type designed to hold a single value.
   * @tparam T The underlying type. Must be viewable as JsonValue.
   */
  sealed abstract trait JsonAtom[T] extends JsonValue[T] {
    override def toString = value.toString; 
    def mkString = toString 
  }

  /** Represents a JSON string. 
   *  @param value The underlying value. */
  case class JsonString(val value:String) extends JsonAtom[String] { override def mkString = quote(value) }

  /** Represents a JSON integer. 
   *  @param value The underlying value. */
  case class JsonInt(val value:Int) extends JsonAtom[Int]

  /** Represents a JSON float or fractional. 
   *  @param value The underlying value. */
  case class JsonFloat(val value:Float) extends JsonAtom[Float]
  
  /** Represents a JSON boolean. 
   *  @param value The underlying value. */
  case class JsonBoolean(val value:Boolean) extends JsonAtom[Boolean]

  /** Represents a JSON null. 
   *  @param value The underlying value. */
  case class JsonNull() extends JsonAtom[Null] { 
    val value = null; 
    override def toString = "null"
    override def mkString = "null"
  }

  /** Thrown when an error is encountered while parsing JSON. */
  case class JsonParseError(msg:String) extends Exception(msg) with NoStackTrace
  private final val Whitespace = "\\s".r
  private final val Digit = "\\d".r
  private final type Token = (Symbol, String) 
  private final val Noop = ('Noop, "")

  /**
   * Parses JSON from a string to a usable JsonValue heirarchy.
   * @param from The string to parse.
   * @return The parsed JsonValue.
   */
  def parseJson(from:String):JsonValue[_] = {
    val parsed = parse(lex(from))
    if(parsed._2.filter(_ != Noop).length > 0) 
      throw JsonParseError("Input string contains unexpected tokens before EOF")
    parsed._1
  }

  @tailrec
  private def lex(from:String, tokens:List[Token]=List()):List[Token] = from.headOption match {
    case None => tokens :+ Noop
    case Some(c) => c match {
      case Whitespace() => lex(from.trim, tokens)
      case '{' => lex(from.tail, tokens :+ ('ObjectBegin, "{"))
      case '}' => lex(from.tail, tokens :+ ('ObjectEnd, "}"))
      case '[' => lex(from.tail, tokens :+ ('ArrayBegin, "["))
      case ']' => lex(from.tail, tokens :+ ('ArrayEnd, "]"))
      case ',' => lex(from.tail, tokens :+ ('Comma, ","))
      case ':' => lex(from.tail, tokens :+ ('Colon, ":"))

      case '"' => {
        var last = '\u0000'
        val cont = from.tail.takeWhile(c => { val take = last == '\\' || c != '"'; last = c; take; })
        lex(from.drop(cont.length+2), tokens :+ ('String, cont))
      }

      case Digit() | '-' => {
        var acc = ""
        val tail = from.dropWhile(c => c match {
          case '-' if acc != "" => 
            throw JsonParseError("Unexpected - when parsing number")
          case '.' if acc contains "." => 
            throw JsonParseError("Unexpected . when parsing number")
          case 'e' | 'E' if acc.toLowerCase contains "e" => 
            throw JsonParseError(s"Unexpected $c when parsing number")
          case Digit() | '-' | '.' | 'e' | 'E' => { acc += c; true }
          case _ => false
        })
        lex(from.drop(acc.length), tokens :+ ('Number, acc))
      }

      case _ => 
        if(from startsWith "true") lex(from.drop(4), tokens :+ ('Bool, "true")) else 
        if(from startsWith "false") lex(from.drop(5), tokens :+ ('Bool, "false")) else 
        if(from startsWith "null") lex(from.drop(4), tokens :+ ('Null, "null")) else 
        throw JsonParseError(s"Unexpected $c when parsing JSON")
    }
  }

  private def parse(tokens:List[Token]):(JsonValue[_], List[Token]) = tokens.headOption.getOrElse(Noop) match {
    case ('ObjectBegin, _) => {
      var tail = tokens.tail
      var pairs = List[(String, JsonValue[_])]()
      var cont = tail.headOption.getOrElse(Noop)._1 != 'ObjectEnd

      while(cont) {
        if(tail.length < 4) 
          throw JsonParseError("Unexpected EOF when parsing object")
        val key = tail.head
        val colon = tail.drop(1).head
        val value = tail.drop(2).head
        tail = tail.drop(3)
        if(key._1 != 'String || colon._1 != 'Colon) 
          throw JsonParseError("Objects must follow the structure {\"key1\":value1,\"key2\":value2}")

        val (value1, tail1) = parse(value +: tail)
        pairs = pairs :+ key._2 -> value1
        tail = tail1

        if(tail.head._1 != 'Comma) cont = false
        else tail = tail.tail
      }

      if(tail.head._1 != 'ObjectEnd)
        throw JsonParseError("Objects must follow the structure {\"key1\":value1,\"key2\":value2}")
      (JsonObject(Map(pairs:_*)), tail.tail)
    }

    case ('ArrayBegin, _) => {
      var tail = tokens.tail
      var list:JsonList = List()
      var cont = tail.headOption.getOrElse(Noop)._1 != 'ArrayEnd

      while(cont) {
        if(tail.length < 2) 
          throw JsonParseError("Unexpected EOF when parsing array")
        val value = tail.head
        val next = tail.drop(1).head
        tail = tail.drop(2)

        val (value1, tail1) = parse(List(value, next) ++ tail)
        list = list :+ value1
        tail = tail1

        if(tail.head._1 != 'Comma) cont = false
        else tail = tail.tail
      }

      if(tail.head._1 != 'ArrayEnd)
        throw JsonParseError("Arrays must follow the structure [value1,value2]")
      (JsonArray(list), tail.tail)
    }

    case (typ, value) => typ match {
      case 'String => (JsonString(value), tokens.tail)
      case 'Number => (
        if(value.contains(".") || value.toLowerCase.contains("e")) 
          JsonFloat(value.toFloat) else JsonInt(value.toInt), tokens.tail)
      case 'Bool => (if(value == "true") JsonBoolean(true) else JsonBoolean(false), tokens.tail)
      case 'Null => (JsonNull(), tokens.tail)
      case _ => throw JsonParseError(s"Unexpected $value when parsing JSON")
    }
  }

  /**
   * Shorthand method for creating a JsonObject.
   * @param pairs A list of key/value tuples to convert.
   * @return The created JsonObject.
   */
  def obj(pairs:(String, JsonValue[_])*) = JsonObject(Map(pairs:_*))

  /**
   * Shorthand method for creating a JsonArray.
   * @param pairs A list of values to convert.
   * @return The created JsonArray.
   */
  def arr(elems:JsonValue[_]*) = JsonArray(List(elems:_*))

  // Implicit conversions
  implicit def map2json(from:JsonMap):JsonObject = JsonObject(from)
  implicit def arr2json(from:JsonList):JsonArray = JsonArray(from)
  implicit def str2json(from:String):JsonString = JsonString(from)
  implicit def int2json(from:Int):JsonInt = JsonInt(from)
  implicit def flt2json(from:Float):JsonFloat = JsonFloat(from)
  implicit def bln2json(from:Boolean):JsonBoolean = JsonBoolean(from)
  implicit def nul2json(from:Null):JsonNull = JsonNull()
  
  implicit def json2map(from:JsonObject):JsonMap = from.value
  implicit def json2arr(from:JsonArray):JsonList = from.value
  implicit def json2str(from:JsonString):String = from.value
  implicit def json2int(from:JsonInt):Int = from.value
  implicit def json2flt(from:JsonFloat):Float = from.value
  implicit def json2bln(from:JsonBoolean):Boolean = from.value
  implicit def json2nul(from:JsonNull):Null = null

  // Utility functions
  private def quote(s: String): String = "\"" + escape(s) + "\""
  private def unquote(s: String): String = s.drop(1).dropRight(1)
  private def escape(s: String): String = s.flatMap(escapedChar)
  private def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\\' => "\\\\"
    case _    => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt) 
                else              String.valueOf(ch)
  }
}