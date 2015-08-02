import scala.{Option => _, Either => _, _}

// holderで囲むことでREPLで使えるようにする
object holder {
  sealed trait Option[+A] {
    /* Exercise 4.1 */
    // 値が入っていれば(Noneでなければ)関数fを適用
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }
    // 値が入っていれば値を、入っていなければdefaultを取得
    def getOrElse[B>:A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }
    // map処理してget
    def flatMap[B](f: A => Option[B]): Option[B] =
      map(f).getOrElse(None)
    // 値が入っていればそのままSomeを、入っていなければobを取得
    def orElse[B>:A](ob: => Option[B]): Option[B] = 
      map(Some(_)).getOrElse(ob)    
    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if (f(a)) => this
      case _ => None
    }

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def failingFn(i: Int): Int = {
      val y: Int = throw new Exception("fail!") 
      try {
        val x = 42 + 5
        x + y
      }
      catch { case e: Exception => 43 }
    }
    def failingFn2(i: Int): Int = {
      try {
        val x = 42 + 5
        x + ((throw new Exception("fail!")): Int)
      }
      catch { case e: Exception => 43 }
    }

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = 
      mean(xs.map(x => math.pow(x - xs.sum/xs.length, 2)))

    /* Exercise 4.3 */
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a flatMap (aa => b map (bb => f(aa, bb)))

    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap(hh => sequence(t) map (hh :: _))
    }
  }
}
