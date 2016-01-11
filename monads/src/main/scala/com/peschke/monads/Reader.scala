package com.peschke.monads

case class Reader[A,B] (fn: A => B) {
  def run(context: A): B = fn(context)

  def flatMap[C](f: B => Reader[A,C]): Reader[A,C] =
    Reader(context => f(this.run(context)).run(context))

  def map[C](f: B => C): Reader[A,C] = Reader(fn andThen f)

  def foreach(f: B => Unit): Reader[A,Unit] = Reader(fn andThen f)
}
