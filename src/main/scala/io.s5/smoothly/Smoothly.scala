package io.s5.smoothly

import scala.language.dynamics
import org.jsoup.nodes.Document
import scala.collection.JavaConverters._
import scala.util.matching.Regex
import scala.collection.TraversableLike

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

      def selectDynamic[Y](name: String): Y = {
        val value = o(name) match {
          case f0: Function0[_] => f0()
          case x                => x
        }
        value.asInstanceOf[Y]
      }

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

      def nextElementSiblings: Seq[Element] =
        Stream
          .iterate(el.nextElementSibling)(_.nextElementSibling)
          .takeWhile(el => el != null)

      def nextElementSiblingsUntil(p: Element => Boolean): Seq[Element] =
        nextElementSiblings.takeWhile(!p(_))

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

    import org.jsoup.nodes.Element
    private def depthOf(el: Element) = el.normalName match {
      case "h1" | "h2" => 0
      case "h3"        => 1
      case _           => 2
    }

    private object md {
      def listItem(text: String)           = "- " + text
      // import scala.collection.generic.CanBuildFrom
      // def listItems[CC <: TraversableLike[String, CC]](texts: CC)(implicit cbf: CanBuildFrom[CC, String, CC]): CC = {
      //   val b = cbf(texts); b.sizeHint(texts);
      //   b ++= texts.map(listItem _)
      //   b.result
      // }
      def link(text: String, href: String) = s"[${text}](${href})"
    }

    private def toListItem(el: Element) = {
      val indent   = "  "
      val text     = el.text.trim
      val listItem = el.normalName match {
        case "a" => md.listItem(md.link(text, el.absUrl("href")))
        case _   => md.listItem(text)
      }
      indent * depthOf(el) + listItem
    }

    private def toListItems(els: Traversable[Element]) =
      els.map(toListItem _).mkString("\n")

    // The refactored equivalent of md2 in terms of for comprehensions
    def md3 =
      for {
        h    <- doc.$("div#content").headings.view
        items = for {
          // Collect all the paragraphs between headings
          p  <- h.nextElementSiblings.takeWhile(p => !(Set("h1", "h2", "h3") contains p.normalName))
          el <- p.$$("a,code.language-plaintext.highlighter-rouge")
          if !"""^[A-Z\[_\]]+$""".r.test(el.text.trim) // Remove type parameters
        } yield el
      } yield toListItem(h) + "\n" +
        items.map(toListItem _).distinct.mkString("\n")

    def l4 =
      for {
        h    <- doc.$("div#content").headings.view
        items = for {
          // Collect all the paragraphs between headings
          p  <- h.nextElementSiblings.takeWhile(p => !(Set("h1", "h2", "h3") contains p.normalName))
          el <- p.$$("a,code.language-plaintext.highlighter-rouge")
          if !"""^[A-Z\[_\]]+$""".r.test(el.text.trim) // Remove type parameters
        } yield el
      } yield O(
        "heading" -> h,
        "items" -> items
      )

    def section2MD(section: O) =
      toListItem(section.heading) + "\n" +
        section.items[Seq[Element]].map(toListItem _).distinct.mkString("\n")

    def md4 =
      l4.map(section2MD _)
  }

  object jetBrains {
    import Smoothly.x._
    import jsoup._
    import org.jsoup.nodes.Element

    lazy val doc = Jsoup.parseURL("https://blog.jetbrains.com/idea/2020/12/putting-it-all-together/")

    def toc =
      "Contents\n" + doc.headings.map("- " + _.text.trim).mkString("\n")

    def mainContent =
      doc
        .$("section.main-content div.full-content")
        .headings
        .map(h =>
          "\n" + h.text.trim() + "\n" + h
            .nextElementSiblingsUntil(_.normalName == "h2")
            .flatMap(_.$$("a"))
            .map(el => s"- [${el.text.trim}](${el.absUrl("href")})")
            .mkString("\n") + "\n" + h.nextElementSiblingsUntil(_.normalName == "h2").map(_.text.sentences(0)).filter(_.nonEmpty).map("- " + _).mkString("\n")
        )
        .mkString("\n")

    def relatedPosts =
      "Related Posts" + "\n" +
        doc
          .$$("section.main-content > div.related-posts article")
          .map(article => s"- [${article.$("a").absUrl("href")}](${article.$("h3.post-title").text.trim})")
          .mkString("\n")

    private object md {
      def listItem(text: String) = "- " + text

      def listItems(texts: Traversable[String]) =
        texts.map(listItem _).mkString("\n")

      def link(text: String, href: String) = s"[${text}](${href})"
    }

    private def toListItem(el: Element) = {
      val indent = "  "
      val text   = el.text.trim
      el.normalName match {
        case "a" => md.listItem(md.link(text, el.absUrl("href")))
        case _   => md.listItem(text)
      }
    }

    private def toListItems(els: Traversable[Element]) =
      els.map(toListItem _).mkString("\n")

    def toc1     = doc.headings
    def tocPres1 = "Contents\n" + toListItems(toc1)

    def mainContent1     =
      for {
        h             <- doc.$("section.main-content div.full-content").headings
        paragraphs     = h.nextElementSiblings.takeWhile(_.normalName != "h2")
        anchors        = for {
          el <- paragraphs
          a  <- el.$$("a")
        } yield a
        firstSentences = for {
          el           <- paragraphs
          firstSentence = el.text.sentences(0)
          if firstSentence.nonEmpty
        } yield firstSentence
      } yield O(
        "heading" -> h,
        "anchors"        -> anchors,
        "firstSentences" -> firstSentences
      )
    def mainContentPres1 =
      mainContent1
        .map { section =>
          "\n" + section.heading[Element].text.trim + "\n" +
            toListItems(section.anchors) + "\n" +
            md.listItems(section.firstSentences)
        }
        .mkString("\n")

    def relatedPosts1     =
      for {
        article <- doc.$$("section.main-content > div.related-posts article")
      } yield O(
        "title" -> article.$("h3.post-title").text.trim,
        "href"  -> article.$("a").absUrl("href")
      )
    def relatedPostsPres1 =
      "Related Posts\n" +
        md.listItems(relatedPosts1.map(post => md.link(post.title, post.href)))
  }

  object jsoupDoc {
    import Smoothly.x._
    import jsoup._
    import org.jsoup.nodes.Element

    private object md {
      def listItem(text: String) = "- " + text

      def listItems(texts: Traversable[String]) =
        texts.map(listItem _).mkString("\n")

      def link(text: String, href: String) = s"[${text}](${href})"
    }

    private def toListItem(el: Element) = {
      val indent = "  "
      val text   = el.text.trim
      el.normalName match {
        case "a" => md.listItem(md.link(text, el.absUrl("href")))
        case _   => md.listItem(text)
      }
    }

    private def toListItems(els: Traversable[Element]) =
      els.map(toListItem _).mkString("\n")

    lazy val doc0 = Jsoup.parseURL("https://jsoup.org/cookbook/extracting-data/dom-navigation")

    def methods =
      for {
        h      <- doc0.$$("div.recipe > h3")
        methods = for {
          el           <- h.nextElementSiblings.takeWhile(_.normalName != "h3")
          linkToMethod <- el.$$("li code a")
        } yield O(
          "methodName" -> linkToMethod.text.trim,
          "href"       -> linkToMethod.absUrl("href")
        )
      } yield O(
        "title" -> h.text.trim(),
        "methods" -> methods
      )
    def methodsPres = {
      val overview  = md.listItems(methods.map(_.title[String]))
      val listItems = methods
        .map { section =>
          "\n" + section.title + "\n" +
            md.listItems(section.methods[Seq[O]].map(m => md.link(m.methodName, m.href)))
        }
        .mkString("\n")
      overview + "\n\n" + listItems
    }

    lazy val doc1 = Jsoup.parseURL("https://jsoup.org/cookbook/extracting-data/selector-syntax")

    def syntaxes =
      for {
        h         <- doc1.$$("div.recipe > h3")
        selectorss = for {
          el       <- h.nextElementSiblings.takeWhile(_.normalName != "h3")
          li       <- el.$$("li")
          // Inside an li element, there are code elements that are explaining a 
          // single topic, so groups them to present them in such manner
          selectors = for {
            code    <- li.$$("code")
            selector = code.text.trim
            if selector.length > 1 // These are likely to be "n", "a" or the likes
          } yield O(
            "selector" -> selector
          )
          if selectors.nonEmpty
        } yield selectors
      } yield O(
        "title" -> h.text.trim(),
        "selectorss" -> selectorss
      )
    def syntaxesPres = {
      val overview = md.listItems(syntaxes.map(_.title[String]))
      val listItems = syntaxes
        .map(section => {
          // Joining grouped selectors with new lines
          // Then making those grouped selectors as a single list item
          // Concatenates the headings and list items
          val selectors = section
            .selectorss[Seq[Seq[O]]]
            .map { selectors =>
              selectors.map(_.selector[String]).mkString("\n")
            }
          "\n" + section.title + "\n" +
            md.listItems(selectors)
        })
        .mkString("\n")
      overview + "\n\n" + listItems
    }
  }
}
import Smoothly.x._
import Smoothly.jsoup._
// import Smoothly.workRules._
// import Smoothly.wikiwandPhilanthropy._
// import Smoothly.cats._
// import Smoothly.jetBrains._
import Smoothly.jsoupDoc._
