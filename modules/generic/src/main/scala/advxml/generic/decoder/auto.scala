package advxml.generic.decoder
import advxml.core.data.XmlDecoder
import advxml.generic.Configuration
import magnolia.{CaseClass, Magnolia, SealedTrait}

import scala.reflect.ClassTag

object auto {

  type Typeclass[T] = XmlDecoder[T]

  def combine[T](caseClass: CaseClass[Typeclass, T])(implicit ct: ClassTag[T]): Typeclass[T] =
    MagnoliaDecoder.combine(caseClass)(Configuration.default)

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    MagnoliaDecoder.dispatch(sealedTrait)

  implicit def gen[T]: XmlDecoder[T] = macro Magnolia.gen[T]
}
