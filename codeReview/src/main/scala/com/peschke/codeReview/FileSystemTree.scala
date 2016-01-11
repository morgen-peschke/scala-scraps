package com.peschke.codeReview

import java.io.File

case class Node(file: File, parent: Option[Node]) {
  private var filter = (x: Node) => true

  private lazy val _children: Set[Node] =
    if (file.isDirectory) file.listFiles().map(f => Node(f, Some(this))).toSet
    else Set()

  def children: Set[Node] = _children.filter(filter)

  def withFilter(p: Node => Boolean): Node = {
    filter = p
    this
  }

  def foreachDepthFirst(f: (Node, Int) => Unit, depth: Int = 0): Unit = {
    f(this, depth)
    children.foreach(_.foreachDepthFirst(f, depth + 1))
  }
}

object Node extends ((File, Option[Node]) => Node) {
  def getTree(pathToRoot: String): Node = Node(new File(pathToRoot), None)
  def printTree(root: Node): Unit = root.foreachDepthFirst({
    case (Node(f, _), depth) => println(("  " * depth) + f.getName)
  })
}

object FileSystemTree {
  def main(args: Array[String]): Unit = args.foreach(arg => {
    Node.printTree(Node.getTree(arg))
  })
}
