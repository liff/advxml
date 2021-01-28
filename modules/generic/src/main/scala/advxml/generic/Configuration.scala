package advxml.generic

//sealed trait ManualTypeSelector2
//case class Attr[T](ptype: T) extends ManualTypeSelector2
//case class Node[T](ptype: T) extends ManualTypeSelector2
//case class Text[T](ptype: T) extends ManualTypeSelector2

sealed trait ManualTypeSelector
final class node extends scala.annotation.StaticAnnotation with ManualTypeSelector
final class attr extends scala.annotation.StaticAnnotation with ManualTypeSelector
//final class textOfNode extends scala.annotation.StaticAnnotation with ManualTypeSelector

case class Configuration(
  caseSensitive: Boolean = true,
  trimText: Boolean = true
)

object Configuration {
  lazy val default: Configuration = Configuration()
}
