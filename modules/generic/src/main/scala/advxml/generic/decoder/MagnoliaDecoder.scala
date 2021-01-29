package advxml.generic.decoder

import advxml.generic.{attr, node, Configuration, ManualTypeSelector}
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import magnolia.{CaseClass, Param, SealedTrait}

import scala.reflect.{classTag, ClassTag}
import scala.xml.{NodeSeq, Text}

object MagnoliaDecoder {

  import advxml.core.data._
  import advxml.implicits._
  import cats.syntax.all._

//  implicit def alwaysPureConverter[F[_]: Applicative, A, B: * =:!= A](implicit
//    c: A As B
//  ): A As F[B] = c.map(b => Applicative[F].pure(b))

  def combine[T](ctx: CaseClass[XmlDecoder, T])(conf: Configuration)(implicit ct: ClassTag[T]): XmlDecoder[T] =
    XmlDecoder.of { ns: NodeSeq =>
      val list = ctx.parameters
        .map((p: Param[XmlDecoder, T]) => {

          val target: ValidatedEx[NodeSeq] = getTargetInfo(ns, ctx, p).andThen {
            case _: node => $(ns).down(p.label).run[ValidatedEx]
            case _: attr =>
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

  private def getTargetInfo[T](
    ns: NodeSeq,
    ctx: CaseClass[XmlDecoder, T],
    p: Param[XmlDecoder, T]
  ): ValidatedEx[ManualTypeSelector] =
    (getPAnn[attr](p), getPAnn[node](p)) match {
      case (None, None) =>
        val attrExists: Boolean = ns.exists(hasAttrs(Key(p.label)))
        val nodeExists: Boolean = ns.exists(hasImmediateChild(p.label))
        (attrExists, nodeExists) match {
          case (true, true) =>
            fail(
              s"Ambiguous request, there are both one attribute and one node with the name ${p.label}, " +
              s"please use @attr or @node annotation to specify the expected behavior."
            )
          case (false, false) =>
            fail(s"Missing attribute or node with label '${p.label}' for class ${ctx.typeName.short}")
          case (true, false) =>
            ok(new attr)
          case (false, true) =>
            ok(new node)
        }

      case (Some(_), Some(_)) =>
        fail("Ambiguous annotations, field annotated with both @attr and @node.")
      case (Some(attrA), None) =>
        ok(attrA)
      case (None, Some(nodeA)) =>
        ok(nodeA)
    }

  private def ok[T <: ManualTypeSelector](mts: T): Valid[T] =
    Valid(mts)

  private def fail[T](message: String): Invalid[RuntimeException] =
    Invalid(new RuntimeException(message))

  private def getPAnn[A: ClassTag](p: Param[XmlDecoder, _]): Option[A] =
    p.typeAnnotations.find(pt => classTag[A].runtimeClass.isInstance(pt)).map(_.asInstanceOf[A])

  private def pTypeIs[T: ClassTag](p: Param[XmlDecoder, T], clazz: Class[_]): Boolean =
    clazz.isAssignableFrom(
      classTag[T].runtimeClass
        .getDeclaredField(p.label)
        .getType
    )
}
