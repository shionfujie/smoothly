package io.s5.smoothly

import scala.language.dynamics
import org.jsoup.nodes.Document
import scala.collection.JavaConverters._
import scala.util.matching.Regex

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
        props.foldLeft(new O) {
          case (o, (name, value)) =>
            o(name) = value; o;
        }
    }

    class RegexExts(regex: Regex) {
      def test(source: CharSequence): Boolean =
        regex.pattern.matcher(source).find
    }
    implicit def regex2exts(r: Regex) = new RegexExts(r)

    class StringExts(s: String) {
      def sentences = s.replaceAll("""([.!?])\s+(?=.)""", "$1|").split("[|]")
    }
    implicit def string2exts(s: String) = new StringExts(s)
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
          .takeWhile(el => el != null && !p(el))
      }

      def headings = $$("h1,h2,h3,h4,h5,h6")

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

  object cats {
    import Smoothly.x._
    import jsoup._

    lazy val doc = Jsoup.parseURL("https://typelevel.org/cats/typeclasses/monad.html")

    def md0 = doc.headings
      .map(h => {
        val title = h.text.trim()
        h.normalName match {
          case "h1" | "h2" => "- " + title
          case "h3"        => "  - " + title
        }
      })
      .mkString("\n")

    def md1 = doc
      .$("div#content")
      .$$("h1,h2,h3,code.language-plaintext.highlighter-rouge,a")
      .view
      .filterNot("""^[A-Z\[_\]]+$""".r test _.text.trim) // Remove type parameters
      .map(el => {
        val title  = el.text.trim()
        val indent = "  "
        el.normalName match {
          case "h1" | "h2" => indent * 0 + "- " + title
          case "h3"        => indent * 1 + "- " + title
          case "code"      => indent * 2 + "- " + title
          case "a"         => indent * 2 + s"- [${title}](${el.absUrl("href")})"
        }
      })
      .mkString("\n")

    def md2 = doc
      .$("div#content")
      .headings
      .view
      .map(el => {
        val title   = el.text.trim()
        val indent  = "  "
        val heading = el.normalName match {
          case "h1" | "h2" => indent * 0 + "- " + title
          case "h3"        => indent * 1 + "- " + title
        }
        // Collect the siblings between headings
        val items   = el
          .nextElementSiblingsUntil(_.normalName match {
            case "h1" | "h2" | "h3" => true
            case _                  => false
          })
          .toStream
          .flatMap(_.$$("a,code.language-plaintext.highlighter-rouge"))
          .filterNot("""^[A-Z\[_\]]+$""".r test _.text.trim)
          .map(el => {
            val title =
              if (el.normalName == "a") s"[${el.text.trim()}](${el.absUrl("href")})"
              else el.text.trim()
            "  " * 2 + "- " + title
          })
          .distinct
          .mkString("\n")
        heading + "\n" + items
      })

    // The refactored equivalent of md2 in terms of for comprehensions
    def md3 =
      for {
        h      <- doc.$("div#content").headings.view
        indent  = "  "
        depth   = h.normalName match {
          case "h1" | "h2" => 0
          case "h3"        => 1
        }
        heading = indent * depth + "- " + h.text.trim
        items   = for {
          p    <- h.nextElementSiblingsUntil(Set("h1", "h2", "h3") contains _.normalName)
          el   <- p.$$("a,code.language-plaintext.highlighter-rouge")
          text  = el.text.trim
          if !"""^[A-Z\[_\]]+$""".r.test(text) // Remove type parameters
          title =
            if (el.normalName == "a") s"[${text}](${el.absUrl("href")})"
            else text
        } yield indent * 2 + "- " + title
      } yield heading + "\n" + items.toStream.distinct.mkString("\n")
  }
}
import Smoothly.x._
import Smoothly.jsoup._
// import Smoothly.workRules._
// import Smoothly.wikiwandPhilanthropy._
import Smoothly.cats._
