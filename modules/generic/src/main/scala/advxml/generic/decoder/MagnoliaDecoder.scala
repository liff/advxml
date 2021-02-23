package advxml.generic.decoder

import advxml.generic.{attr, node, Configuration, ManualTypeSelector}
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.Applicative
import magnolia.{CaseClass, Param, SealedTrait}

import scala.reflect.{classTag, ClassTag}
import scala.xml.{NodeSeq, Text}
import advxml.core.=:!=

object MagnoliaDecoder {

  import advxml.core.data._
  import advxml.implicits._
  import cats.syntax.all._

  import reflect.runtime.universe._

  implicit def alwaysPureConverter[F[_]: Applicative, A, B: * =:!= A](implicit
    c: A As B
  ): A As F[B] = c.map(b => Applicative[F].pure(b))

  def combine[T: ClassTag: TypeTag](ctx: CaseClass[XmlDecoder, T])(conf: Configuration): XmlDecoder[T] =
    XmlDecoder.of { ns: NodeSeq =>
      val list = ctx.parameters
        .map((p: Param[XmlDecoder, T]) => {

          val target: ValidatedEx[NodeSeq] = getTargetInfo(ns, ctx, p).andThen {
            case ManualTypeSelector.Attr => $(ns).down(p.label).run[ValidatedEx]
            case ManualTypeSelector.Node =>
              ns
                .attr(p.label)
                .as[ValidatedEx[String]]
                .map(av => if (conf.trimText) av.trim else av)
                .map(Text(_))
          }

          val value: Validated[NonEmptyList[Throwable], p.PType] = target.toValidatedNel
            .andThen(p.typeclass.run(_))

          p match {
            case x if pTypeIs(x, classOf[Option[_]]) => Valid(value.toOption)
            case x if pTypeIs(x, classOf[List[_]])   => Valid(value.toList)
            case _                                   => value
          }
        })
        .toList
        .sequence

      list.map(ctx.rawConstruct)
    }

  def dispatch[T](ctx: SealedTrait[XmlDecoder, T]): XmlDecoder[T] = {
    XmlDecoder.of { ns =>
      ctx.subtypes.head.typeclass.run(ns)
    }
  }

  private def getTargetInfo[T: TypeTag](
    ns: NodeSeq,
    ctx: CaseClass[XmlDecoder, T],
    p: Param[XmlDecoder, T]
  ): ValidatedEx[ManualTypeSelector] =
    (getPAnn[attr, T](p), getPAnn[node, T](p)) match {
      case (Some(_), Some(_)) =>
        fail("Ambiguous annotations, field annotated with both @attr and @node.")
      case (Some(_), None) =>
        ok(ManualTypeSelector.Attr)
      case (None, Some(_)) =>
        ok(ManualTypeSelector.Node)
      case (None, None) =>
        val attrExists: Boolean = ns.exists(hasAttrs(Key(p.label)))
        val nodeExists: Boolean = ns.exists(hasImmediateChild(p.label))
        (attrExists, nodeExists) match {
          case (true, true) =>
            fail(
              s"Ambiguous request, there are both attributes and nodes with the name ${p.label}, " +
              s"please use @attr or @node annotation to specify the expected behavior."
            )
          case (false, false) =>
            fail(s"Missing attribute or node with label '${p.label}' for class ${ctx.typeName.short}")
          case (true, false) =>
            ok(ManualTypeSelector.Attr)
          case (false, true) =>
            ok(ManualTypeSelector.Node)
        }
    }

  private def ok[T <: ManualTypeSelector](mts: T): Valid[T] =
    Valid(mts)

  private def fail[T](message: String): Invalid[RuntimeException] =
    Invalid(new RuntimeException(message))

  private def getPAnn[A: ClassTag, T: TypeTag](p: Param[XmlDecoder, T]): Option[A] = {

    Console.println(
      typeOf[T].typeSymbol.annotations
    )

//    val p1 = classTag[T].runtimeClass
//      .getDeclaredField(p.label)
//      .getDeclaredAnnotations
//      .find((a: Annotation) => a.annotationType.equals(classTag[A].runtimeClass))
//      .map(_.asInstanceOf[A])

//    val p1 = p.typeAnnotations.find(pt => classTag[A].runtimeClass.isInstance(pt)).map(_.asInstanceOf[A])
    Console.println(p.label + " res = " + None)
//    p1

    None
  }

  private def pTypeIs[T: ClassTag](p: Param[XmlDecoder, T], clazz: Class[_]): Boolean =
    clazz.isAssignableFrom(
      classTag[T].runtimeClass
        .getDeclaredField(p.label)
        .getType
    )
}
