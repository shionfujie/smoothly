package io.s5.smoothly

import scala.language.dynamics
import org.jsoup.nodes.Document
import scala.collection.JavaConverters._

object Smoothly {
  object x {
    class O extends Dynamic {

      private val o = scala.collection.mutable.Map[String, Any]()

      def update(name: String, value: Any) =
        o(name) = value match {
          case f0: Function0[_]                => (_: Unit) => f0()
          case f2: Function2[_, _, _]          => f2.tupled
          case f3: Function3[_, _, _, _]       => f3.tupled
          case f4: Function4[_, _, _, _, _]    => f4.tupled
          case f5: Function5[_, _, _, _, _, _] => f5.tupled
          case f                               => f
        }

      def updateDynamic(name: String)(value: Any) =
        update(name, value)

      def applyDynamic[X, Y](fieldName: String)(arg: X): Y =
        o(fieldName).asInstanceOf[X => Y](arg)

      override def toString: String =
        o.view.toList.map { case (k, v) => k + ": " + v }.mkString("{", ", ", "}")
    }

    object O {
      def apply(props: (String, Any)*): O =
        props.foldLeft(new O) { case (o, (name, value)) =>
          o(name) = value; o;
        }
    }
  }

  object jsoup {
    import java.io.File
    object Jsoup {
      def parseFile(filename: String): Document =
        org.jsoup.Jsoup.parse(new File(filename), "UTF-8")

      def parseURL(url: String): Document =
        org.jsoup.Jsoup.connect(url).get
    }

    implicit def elements2scala(els: org.jsoup.select.Elements) = els.asScala

    import org.jsoup.nodes.Element
    class ElementExts(el: Element) {

      def $(selector: String) = el.selectFirst(selector)

      def $$(selector: String) = el.select(selector)

      def nextElementSiblingsUntil(p: Element => Boolean) = new Iterable[Element] {
        def iterator = Iterator
          .iterate(el)(_.nextElementSibling)
          .drop(1)
          .takeWhile(!p(_))
      }

      def showStructure(depth: Int): String =
        show(el, depth, "")

      private def show(el: Element, depth: Int, indent: String): String =
        if (depth < 1) ""
        else {
          el.children
            .map { ch =>
              indent + ch.tagName + "\n" +
                show(ch, depth - 1, " " + indent)
            }
            .mkString("")
        }

    }
    implicit def element2exts(d: Element) = new ElementExts(d)
  }

  object workRules {
    import x.O
    import jsoup._

    val workRules = new O

    private lazy val chap14 = Jsoup
      .parseFile("/Users/shion.t.fujie/epub/Work Rules Insights from Inside Google That Will Transform How You Live and Lead by Laszlo Bock (z-lib.org)/OEBPS/Text/chapter014.xhtml")
      .$("div#chapter014")
    workRules.chap14 = () => chap14
  }

  object wikiwandPhilanthropy {
    import jsoup._
    lazy val doc = Jsoup.parseURL("https://www.wikiwand.com/en/Philanthropy")

    def fullContent = doc $ ("div#fullContent")
  }
}
import Smoothly.x._
import Smoothly.jsoup._
// import Smoothly.workRules._
import Smoothly.wikiwandPhilanthropy._
