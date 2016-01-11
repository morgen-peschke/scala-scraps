package com.peschke.algorithms.interview_questions.search_in_tree

/**
  * Simple immutable Tree data structure
  */
sealed trait Tree[+T] {
  def left: Tree[T]
  def right: Tree[T]
  def value: T
  def isEmpty: Boolean
}

/**
  * Companion object, only constructors
  */
object Tree {
  def apply(): Tree[Nothing] = NilNode
  def apply[T](value: T): Tree[T] = ConsNode(value, NilNode, NilNode)
  def apply[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = ConsNode(value, left, right)
}

/**
  * Node with data and children
  */
final case class ConsNode[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  def isEmpty: Boolean = false
  override def toString: String = (left, right) match {
    case (NilNode, NilNode) => value.toString
    case _ => s"($value $left $right)"
  }
}

/**
  * Terminal Node, contains nothing
  */
case object NilNode extends Tree[Nothing] {
  def left: Nothing = throw new RuntimeException("Accessing left node of NilNode")
  def right: Nothing = throw new RuntimeException("Accessing right node of NilNode")
  def value: Nothing = throw new RuntimeException("Accessing value of NilNode")
  def isEmpty: Boolean = true
  override def toString: String = "_"
}

object Solution extends App {

  implicit class SearchableTree[T](val target: Tree[T]) extends AnyVal {
    def prefixMatches(pattern: Tree[T]): Boolean = {
      def valueMatches = !target.isEmpty && target.value == pattern.value
      def leftMatches = target match {
        case ConsNode(_, left, _) => left.prefixMatches(pattern.left)
        case _ => false
      }
      def rightMatches = target match {
        case ConsNode(_, _, right) => right.prefixMatches(pattern.right)
        case _ => false
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
    val result = tree find pattern
    val mark = if (result == expected) "[✓]" else "[ ]"
    println(s"$mark $pattern")
    assert(result == expected, s"Expected $expected, but got $result")
  }

  println("    Test Pattern")
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
