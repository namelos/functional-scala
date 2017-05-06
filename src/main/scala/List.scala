sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]) = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](h: A, l: List[A]) = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case `l` => drop(tail(l), n - 1)
      case _ => Nil
    }

  def map[A, B](l: List[A], f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t, f))
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f)
      else Cons(h, dropWhile(t, f))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def curriedDropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f)
      else Cons(h, dropWhile(t, f))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sumFoldR(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def productFoldR(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def lengthFoldR[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sumFoldL(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def productFoldL(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def lengthFoldL[A](l: List[A]) =
    foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]) =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def foldRWithFoldL[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def appendFoldL[A](a1: List[A], a2: List[A]): List[A] =
    foldRWithFoldL(a1, a2)(Cons.apply)

  def mapIncrement(ns: List[Int]) =
    foldRWithFoldL(ns, List[Int]())((x, acc) => Cons(x + 1, acc))

  def mapDoubleToString(ns: List[Double]) =
    foldRWithFoldL(ns, List[String]())((x, acc) => Cons(x.toString, acc))
}

