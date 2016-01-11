package com.peschke.algorithms.tree

import com.peschke.layout.text.{ Element, ElementComposer, Direction }

object SearchInTree extends App {
  import ElementComposer._
  implicit val settings = Settings(anchor = Option(Direction.NorthWest))

  implicit class SearchableTree[T](val target: Tree[T]) extends AnyVal {
    def prefixMatches(pattern: Tree[T]): Boolean = {
      def valueMatches = !target.isEmpty && target.value == pattern.value
      def leftMatches = target match {
        case NilNode => false
        case ConsNode(_, left, _) => left.prefixMatches(pattern.left)
      }
      def rightMatches = target match {
        case NilNode => false
        case ConsNode(_, _, right) => right.prefixMatches(pattern.right)
      }
      pattern.isEmpty || (valueMatches && leftMatches && rightMatches)
    }

    /**
      * Search the tree for the first instance of `pattern`, giving
      * preference to the left child.
      *
      * `pattern` is a tree, representing a pattern of
      * nodes. `NilNode`s should be considered wildcards.
      *
      * Example patterns
      * ----------------
      * `Tree()`                    Matches any tree.
      *
      * `Tree(3)`                   Matches any tree where the root
      *                             node has a value of `3`.
      *
      * `Tree(3, Tree(2), Tree())`  Matches any tree where the root
      *                             node has a value of `3`, and a
      *                             left sub-tree with a root value
      *                             of `2`, and any (or no) right
      *                             sub-tree.
      */
    def find(pattern: Tree[T]): Option[Tree[T]] = (pattern, target) match {
      case (NilNode, _) => Some(target)
      case (_, NilNode) => None
      case (p, t) if t prefixMatches p => Some(target)
      case (p, ConsNode(_, l, r)) => l.find(p) orElse r.find(p)
    }
  }

  val tree: Tree[Int] =
    Tree(1,
      Tree(2,
        Tree(3,
          Tree(4),
          Tree(3, Tree(4), Tree(5))),
        Tree(5,
          Tree(6,
            Tree(7),
            Tree(8, Tree(9), Tree())),
          Tree(10, Tree(11), Tree()))),
      Tree(12))

  def checkPattern(pattern: Tree[Int], expected: Option[Tree[Int]]): Unit = {
    def elementOrNotFound(value: Option[Tree[Int]]): Element =
       value map(_.toElement) getOrElse Element("NotFound")

    val result = tree find pattern
    val resultElement = elementOrNotFound(result)
    val rendered = (pattern.toElement ++ Element(" => ") ++ resultElement).render
    println("------------------------------")
    println(result)

    def assertionMsg = {
      val expectedElement = Element("Expected") / elementOrNotFound(expected)
      val spacer = Element(" != ")
      val actualElement = Element("Actual") / resultElement
      "\n" + (expectedElement ++ spacer ++ actualElement).render
    }

    assert(result == expected, assertionMsg)
  }

  println(tree)
  checkPattern(Tree(), Option(tree))

  checkPattern(Tree(9), Option(Tree(9)))
  checkPattern(Tree(12), Option(Tree(12)))
  checkPattern(Tree(13), None)
  checkPattern(Tree(1), Option(tree))

  checkPattern(Tree(2, Tree(3), Tree(5)), Option(tree.left))
  checkPattern(Tree(3, Tree(4), Tree(5)), Option(tree.left.left.right))
  checkPattern(Tree(6, NilNode, Tree(8)), Option(tree.left.right.left))
  checkPattern(Tree(6, NilNode, Tree(7)), None)

  checkPattern(
    Tree(5,
      Tree(6,
        Tree(7),
        Tree(8, Tree(9), Tree())),
      Tree(10, Tree(11), Tree())),
    Option(tree.left.right))
}
