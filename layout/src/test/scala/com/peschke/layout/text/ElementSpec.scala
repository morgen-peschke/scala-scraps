package com.peschke.layout.text

import org.scalatest.WordSpec
import org.scalatest.ShouldMatchers
import org.scalatest.matchers.{ Matcher, MatchResult }

class ElementSpec extends WordSpec with ShouldMatchers with ElementMatchers {

  "Element" should {
    "render the contents to a block of text" in {
      new Element {
        override def contents: Seq[String] = Seq("This", "is", "a", "test")
      } should renderTo(
        """|This
           |is
           |a
           |test""")
    }

    "report isEmpty and nonEmpty correctly" in {
      EmptyElement.isEmpty should be (true)
      EmptyElement.nonEmpty should be (false)

      Element("").isEmpty should be (true)
      Element("").nonEmpty should be (false)

      Element(Seq()).isEmpty should be (true)
      Element(Seq()).nonEmpty should be (false)

      Element(Seq("")).isEmpty should be (true)
      Element(Seq("")).nonEmpty should be (false)

      Element('x', 0, 0).isEmpty should be (true)
      Element('x', 0, 0).nonEmpty should be (false)

      Element('x', 1, 0).isEmpty should be (true)
      Element('x', 1, 0).nonEmpty should be (false)

      Element('x', 0, 1).isEmpty should be (true)
      Element('x', 0, 1).nonEmpty should be (false)

      Element("x").isEmpty should be (false)
      Element("x").nonEmpty should be (true)

      Element(Seq("x")).isEmpty should be (false)
      Element(Seq("x")).nonEmpty should be (true)

      Element('x', 1, 1).isEmpty should be (false)
      Element('x', 1, 1).nonEmpty should be (true)
    }

    "exist should return true if any of the chars in the Element match" in {
      Element().exists(_ == ' ') should be (false)
      Element(" -").exists(_ == '-') should be (true)
      Element(Seq("abc", "def")).exists(_ == 'e') should be (true)
    }

    "map should transform the contents of the Element" in {
      val border = Element('|', 1, 5)
      val base = Element("""|EEEEE
                            |E
                            |EEEEE
                            |E
                            |EEEEE""".stripMargin.split("\n"))

      base.map(_ => 'x') should renderTo(
        """|xxxxx
           |xxxxx
           |xxxxx
           |xxxxx
           |xxxxx""")

      base.map(c => if (c == 'E') 'x' else '.') should renderTo(
        """|xxxxx
           |x....
           |xxxxx
           |x....
           |xxxxx""")
    }
  }

  "ArrayElement" should {
    "normalize lengths" in {
      val element = ArrayElement("This" :: "is" :: "a" :: "test" :: Nil, '-')
      element should renderTo(
        """|This
           |is--
           |a---
           |test""")
    }
  }

  "LineElement" should {
    "prevent multiple lines" in {
      intercept[IllegalArgumentException](LineElement("xx\nxx"))
        .getMessage should be ("requirement failed: LineElement is a single line only")
    }
  }

  "UniformElement" should {
    "fill the specified rectangle" in {
      UniformElement('x', 2, 2) should renderTo(
        """|xx
           |xx""")
    }

    "allow zero dimensions" in {
      UniformElement('x', 1, 0).render should be ("")
      UniformElement('x', 0, 1).render should be ("")
    }

    "prevent negative dimensions" in {
      intercept[IllegalArgumentException](UniformElement('x', -1, 0))
        .getMessage should be ("requirement failed: negative dimensions are forbidden")
      intercept[IllegalArgumentException](UniformElement('x', 0, -1))
        .getMessage should be ("requirement failed: negative dimensions are forbidden")
      intercept[IllegalArgumentException](UniformElement('x', -1, -1))
        .getMessage should be ("requirement failed: negative dimensions are forbidden")
    }
  }
}

trait ElementMatchers {
  def renderTo(block: String): Matcher[Element] = Matcher { (element: Element) =>
    val rendered = element.render
    val expected = block.stripMargin
    MatchResult(
      rendered == expected,
      s"""|Expected value:
          |
          |${expected}
          |
          |was not equal to:
          |
          |${rendered}
          |
          |""".stripMargin,
      s"""|Expected value:
          |
          |${expected}
          |
          |was equal to:
          |
          |${rendered}
          |
          |""".stripMargin)
  }
}
