
import Stream._

object holder {
trait Stream[+A] {
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /* Exercise 5.1 */
  // case Empty => Noneみたいなことを書きたくなったが，
  // 「Cons以外なら空のリストを返す」で十分だった。
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  /* Exercise 5.2 */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 0) => holder.Stream.cons(h(), t().take(n-1))
    case _ => Empty
  }
  // holder.Stream.cons(h(), t())はthisでOK。  
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n == 0) => holder.Stream.cons(h(), t())
    case Cons(h, t) if (n > 0) => t().drop(n-1)
    case _ => Empty
  }

  /* Exercise 5.3 */
  // 解答にはcaseの2行目にあたる部分が無いがミスか?
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => holder.Stream.cons(h(), t().takeWhile(p))
    case Cons(h, t) => t().takeWhile(p)
    case _ => holder.Stream.empty
  }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

  /* Exercise 5.4 */
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a,b) => p(a) && b)

  /* Exercise 5.5 */
  def takeWhile3(p: A => Boolean): Stream[A] = 
    foldRight(holder.Stream.empty[A])((a,b) 
      => if (p(a)) holder.Stream.cons(a, b) else holder.Stream.empty[A])

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList2: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def map[B](f: A => B): Stream[B] =
    foldRight(holder.Stream.empty[B])((h,t) => holder.Stream.cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(holder.Stream.empty[A])((h,t) =>
      if (f(h)) holder.Stream.cons(h, t)
      else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => holder.Stream.cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(holder.Stream.empty[B])((h,t) => f(h) append t)
}

// 空のストリーム
case object Empty extends Stream[Nothing]
// 空でないストリームは先頭と末尾で構成され，どちらも非正格
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  // 空でないストリームを作成するためのスマートコンストラクタ
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    // 評価の繰り返しを避けるためのキャッシュ
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // 特定の型の空のストリームを作成するためのスマートコンストラクタ
  def empty[A]: Stream[A] = Empty

  // 複数要素からStreamを作成するための，可変長引数メソッド
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  // 1が永遠に続くストリーム
  val ones: Stream[Int] = Stream.cons(1, ones)

  /* Exercise 5.8 */
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  /* Exercise 5.9 */
  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  /* Exercise 5.10 */
  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }

  /* Exercise 5.11 */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    }       

  /* Exercise 5.11 */
  val fibsViaUnfold =
    unfold((0,1)){ case (f0,f1) => Some(f0, (f1, f0+f1)) }
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n+1))
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(a => Some(a, a))
  val onesViaUnfold =
    unfold(1)(_ => Some(1,1))
}
}
