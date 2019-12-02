package com.github.geirolz.advxml.transform.actions

import com.github.geirolz.advxml.utils.PredicateUtils

import scala.xml.{Node, NodeSeq}

private[advxml] trait FiltersInstances {

  /**
    * Always true predicate.
    */
  lazy val always: XmlPredicate = _ => true

  /**
    * Filter nodes by text property.
    * @param p Text predicate
    * @return
    */
  def text(p: String => Boolean): XmlPredicate = {
    case n: Node => p(n.text)
    case _       => false
  }

  /**
    * Filter nodes by label property.
    * @param p Label predicate
    * @return Predicate for nodes of type `Node`
    */
  def label(p: String => Boolean): XmlPredicate = {
    case n: Node => p(n.label)
    case _       => false
  }

  /**
    * Filter nodes by attributes.
    * @param value Tuple2 where first value represent the attribute key and the second
    *              value represent a predicate function on attribute's value.
    * @param values N predicates for attributes.
    * @return Predicate for nodes of type `Node`
    */
  def attrs(value: (String, String => Boolean), values: (String, String => Boolean)*): XmlPredicate = {

    def attr(key: String, p: String => Boolean): XmlPredicate = ns => p(ns \@ key)

    (Seq(value) ++ values)
      .map(t => attr(t._1, t._2))
      .reduce(PredicateUtils.and[NodeSeq])
  }

  def hasImmediateChild(name: String, predicate: XmlPredicate = always): XmlPredicate = { xml =>
    import com.github.geirolz.advxml.implicits.traverse.try_._
    (xml \? name).toOption.flatten.fold(false)(_.exists(predicate))
  }

  def strictEqualsTo(ns: NodeSeq): XmlPredicate =
    that =>
      (ns, that) match {
        case (e1: Node, e2: Node)         => e1 strict_== e2
        case (ns1: NodeSeq, ns2: NodeSeq) => ns1 strict_== ns2
        case _                            => false
      }
}
