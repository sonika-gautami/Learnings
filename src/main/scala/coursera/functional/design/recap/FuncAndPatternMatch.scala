package coursera.functional.design.recap

import scala.util.Try

object FuncAndPatternMatch extends App {

  abstract class Json

  case object JNull extends Json

  case class JInt(i: Int) extends Json

  case class JString(s: String) extends Json

  case class JObj(m: Map[String, Json]) extends Json

  case class JArr(s: Seq[Json]) extends Json

  case class JBool(b: Boolean) extends Json


  lazy val obj = JObj(Map(
    "bool" -> JBool(true),
    "int" -> JInt(1),
    "jArr" -> JArr(Seq(JInt(11), JInt(22), JInt(33))),
    "jObj" -> JObj(Map("objInt" -> JInt(1)))
  ))
  println(show(obj))


  //Recursive version
  def show(o: Json): String =
    o match {
      case JBool(b) => "true"
      case JString(s) => s"'$s'"
      case JInt(i) => s"$i"
      case JArr(l) => s"[${l.map(show).mkString(", ")}]"
      case JObj(obj) =>
        "{" + obj.mapValues(show).map(t2 => s"'${t2._1}': ${t2._2}").mkString(", ") + "}"
      case any =>
        throw new Exception(s"Not Supported.. ${any.getClass}  ${any}")
    }


  val f1: String => String = {
    case "yes" => "I'm defined"
  }

  //Additional Method:
  // def isDefinedAt(argument: T)
  val f2 : PartialFunction[String, String] = {
    case "yes" => "I'm defined"
  }

  println(f1("yes"))
  println(Try(f1("no")).toOption)

  println(f2.isDefinedAt("yes"))
  println(f2.isDefinedAt("no"))
}
