package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => cons(h(),t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileFold(p: A => Boolean): Stream[A] = this.foldRight(empty[A])((e,acc) => if(p(e)) cons[A](e,acc) else acc)

  def headOption_1: Option[A] = this.foldRight[Option[A]](None)((e,acc) => Some(e))

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((e,acc) => p(e) && acc)


  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h,t) => h() :: t().toList
  }


  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((e,acc) => cons(f(e),acc))


  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((e,acc) => if(f(e)) cons[A](e,acc) else acc )

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((e,acc) => cons(e,acc) )


  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((e,acc) => f(e).append(acc))


  def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this){
    case Cons(h,t) => Some(f(h()),t())
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold((this,n)) {
    case (Cons(h,t),n) if n >= 0 => Some(h(),(t(), n -1))
    case _ => None
  }

  def takeWhileViaUnfold(f: A => Boolean) = Stream.unfold(this) {
    case Cons(h,t) if f(h()) => Some(h(),t())
    case _ => None
  }

  def zipWith[B,C](s: Stream[B])(f: (A,B) => C):Stream[C] = Stream.unfold((this,s)){
    case (Cons(mh,mt), Cons(oh,ot)) => Some(f(mh(),oh()),(mt(),ot()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold((this,s2)){
    case (Cons(mh,mt), Cons(oh,ot)) => Some((Some(mh()) -> Some(oh()), (mt(),ot())))
    case (Cons(mh,mt), Empty) => Some((Some(mh()) -> None, (mt(),empty)))
    case (Empty, Cons(oh,ot)) => Some((None -> Some(oh()), (empty,ot())))
    case _ => None
  }

  def startsWith[A](s: Stream[A]): Boolean = zipAll(s).takeWhile {
    case (Some(m), Some(sub)) => true
    case _ => false
  }.map {
    case (Some(m), Some(sub)) => (m -> sub)
  }.forAll {
    case (m,sub) => m == sub
  }

  def tails: Stream[Stream[A]] = Stream.unfold(this){
    case Empty => None
    case s => Some( s -> s.drop(1) )
  }.append(Stream(empty))


  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this.foldRight((z,Stream(z)))((e,b) => {
    val (v,acc) = b
    val newv = f(e,v)
    newv -> cons(newv,acc)
  })._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val s:Stream[A] = Stream.cons(a,s)
    s
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs:Stream[Int] = {
    def go(f0:Int,f1:Int):Stream[Int] = {
      cons(f0,go(f1, f0 + f1))
    }
    go(0,1)
  }


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match{
    case None => empty[A]
    case Some((a,ns)) => cons(a,unfold(ns)(f))
  }
}