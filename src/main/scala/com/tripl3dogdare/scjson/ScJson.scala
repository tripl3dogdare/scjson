package com.tripl3dogdare

import collection.SeqLike
import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.annotation.tailrec

package object scjson {
  // Typedefs
  sealed abstract trait JsonValue {
    def mkString:String; 
    def as[A <% JsonValue] = this.asInstanceOf[A]
  }

  sealed abstract class JsonCollection[K <: JsonValue] extends JsonValue {
    def getTyped[A <% JsonValue](key:K):A;

    def getObject(key:K) = getTyped[JsonObject](key)
    def getArray(key:K) = getTyped[JsonArray](key)
    def getString(key:K) = getTyped[JsonString](key)
    def getInt(key:K) = getTyped[JsonInt](key)
    def getFloat(key:K) = getTyped[JsonFloat](key)
    def getBoolean(key:K) = getTyped[JsonBoolean](key)
    def getNull(key:K) = getTyped[JsonNull](key)

    def getObj(key:K) = getObject(key)
    def getArr(key:K) = getArray(key)

    def |(key:K) = getTyped[JsonCollection[JsonValue]](key);
    def $(key:K) = getTyped[JsonValue](key)
  }

  case class JsonObject(val value:Map[String, JsonValue]) extends JsonCollection[JsonString] {
    def mkString = value.map {case (k,v) => quote(k) + ":" + v.mkString}.mkString("{", ",", "}")
    def getTyped[A <% JsonValue](key:JsonString) = value.getOrElse(key, null).asInstanceOf[A]
  }

  case class JsonArray(val value:List[JsonValue]) extends JsonCollection[JsonInt] {
    def mkString = value.map(_.mkString).mkString("[", ",", "]")
    def getTyped[A <% JsonValue](key:JsonInt) = value.lift(key).getOrElse(null).asInstanceOf[A]
  }

  sealed abstract trait JsonAtom[T] extends JsonValue {
    val value:T; override def toString = value.toString; def mkString = toString }
  case class JsonString(override val value:String) extends JsonAtom[String] { override def mkString = quote(value) }
  case class JsonInt(override val value:Int) extends JsonAtom[Int]
  case class JsonFloat(override val value:Float) extends JsonAtom[Float]
  case class JsonBoolean(override val value:Boolean) extends JsonAtom[Boolean]

  case class JsonNull() extends JsonAtom[Null] { 
    override val value = null; 
    override def toString = "null"
    override def mkString = "null"
  }

  // Parsing
  case object JsonParseError extends Exception("Error while parsing JSON")
  private final val Whitespace = "\\s".r
  private final val Digit = "\\d".r
  private final type Token = (Symbol, String) 
  private final val Noop = ('Noop, "")

  def parseJson(from:String):JsonValue = parse(lex(from))._1

  @tailrec
  private def lex(from:String, tokens:List[Token]=List()):List[Token] = from.headOption match {
    case None => tokens :+ ('EOF, "")
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
          case '-' if acc != "" => throw JsonParseError
          case '.' if acc contains "." => throw JsonParseError
          case 'e' | 'E' if acc.toLowerCase contains "e" => throw JsonParseError
          case Digit() | '-' | '.' | 'e' | 'E' => { acc += c; true }
          case _ => false
        })
        lex(from.drop(acc.length), tokens :+ ('Number, acc))
      }

      case _ => 
        if(from startsWith "true") lex(from.drop(4), tokens :+ ('Bool, "true")) else 
        if(from startsWith "false") lex(from.drop(5), tokens :+ ('Bool, "false")) else 
        if(from startsWith "null") lex(from.drop(4), tokens :+ ('Null, "null")) else 
        throw JsonParseError
    }
  }

  private def parse(tokens:List[Token]):(JsonValue, List[Token]) = tokens.headOption.getOrElse(Noop) match {
    case ('ObjectBegin, _) => {
      var tail = tokens.tail
      var pairs = List[(String, JsonValue)]()
      var cont = tail.headOption.getOrElse(Noop)._1 != 'ObjectEnd

      while(cont) {
        if(tail.length < 4) throw JsonParseError
        val key = tail.head
        val colon = tail.drop(1).head
        val value = tail.drop(2).head
        tail = tail.drop(3)
        if(key._1 != 'String || colon._1 != 'Colon) throw JsonParseError

        val (value1, tail1) = parse(value +: tail)
        pairs = pairs :+ key._2 -> value1
        tail = tail1

        if(tail.head._1 != 'Comma) cont = false
        else tail = tail.tail
      }

      if(tail.head._1 != 'ObjectEnd) throw JsonParseError
      (JsonObject(Map(pairs:_*)), tail.tail)
    }

    case ('ArrayBegin, _) => {
      var tail = tokens.tail
      var list = List[JsonValue]()
      var cont = true

      while(cont) {
        if(tail.length < 2) throw JsonParseError
        val value = tail.head
        val next = tail.drop(1).head
        tail = tail.drop(2)

        val (value1, tail1) = parse(List(value, next) ++ tail)
        list = list :+ value1
        tail = tail1

        if(tail.head._1 != 'Comma) cont = false
        else tail = tail.tail
      }

      if(tail.head._1 != 'ArrayEnd) throw JsonParseError
      (JsonArray(list), tail.tail)
    }

    case (typ, value) => typ match {
      case 'String => (JsonString(value), tokens.tail)
      case 'Number => (
        if(value.contains(".") || value.toLowerCase.contains("e")) 
          JsonFloat(value.toFloat) else JsonInt(value.toInt), tokens.tail)
      case 'Bool => (if(value == "true") JsonBoolean(true) else JsonBoolean(false), tokens.tail)
      case 'Null => (JsonNull(), tokens.tail)
      case _ => throw JsonParseError
    }
  }

  // Implicit conversions and DSL utilities
  def obj(pairs:Tuple2[String, JsonValue]*) = JsonObject(Map(pairs:_*))
  def arr(elems:JsonValue*) = JsonArray(List(elems:_*))

  implicit def map2json(v:Map[String, JsonValue]):JsonObject = JsonObject(v)
  implicit def arr2json(v:List[JsonValue]):JsonArray = JsonArray(v)
  implicit def str2json(v:String):JsonString = JsonString(v)
  implicit def int2json(v:Int):JsonInt = JsonInt(v)
  implicit def flt2json(v:Float):JsonFloat = JsonFloat(v)
  implicit def bln2json(v:Boolean):JsonBoolean = JsonBoolean(v)
  implicit def nul2json(v:Null):JsonNull = JsonNull()
  
  implicit def json2map(v:JsonObject):Map[String, JsonValue] = v.value
  implicit def json2arr(v:JsonArray):List[JsonValue] = v.value
  implicit def json2str(v:JsonString):String = v.value
  implicit def json2int(v:JsonInt):Int = v.value
  implicit def json2flt(v:JsonFloat):Float = v.value
  implicit def json2bln(v:JsonBoolean):Boolean = v.value
  implicit def json2nul(v:JsonNull):Null = null

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