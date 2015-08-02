sealed trait List[+A]
// 空のリストを表す実装。
case object Nil extends List[Nothing]
// 空でないリストを表す実装。
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // パターンマッチングを使用し、リスト内の整数を足し合わせる
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  // Aの可変長引数をとり、Listを返す関数
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  // 下2つのケースはCons(_, t)でまとめられる
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => xs
  }

  // Exercise 3.3
  // 解答ではcase Nilでは例外を投げていた。
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 3.4
  // if〜elseでもmatchが使える。
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }

  // Exercise 3.5
  // 解答ではcaseの後，=>の前にif文を書くことで条件をつけていた（パターンガード）。
  // case Cons(x, t) if f(x) => dropWhile(t, f)
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, t) => 
      if(f(x)) dropWhile(t, f)
      else l
  }
    
  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // リストasとNilの場合に返す値z、結合するための関数fを受け取る
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      // リストがNil=空ならzを返す
      case Nil => z
      // 結合するにはf(x, 再帰(xs, z))を呼ぶ
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // foldRightを使ったsum
  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  // foldRightを使ったproduct
  // _ * _は(x,y) => x * yの簡易表記
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  // Exercise 3.7
  // product2は，0.0を検出した時点で直ちに再帰を中止して0.0を返せるか?
  //  -> 無理。foldRightではリストを最後まで走査してからfにより畳み込むため。
  // この値が検出されたらこの値を返すという引数を入れたら...と思ったが無理だった
  def foldRight2[A,B](as: List[A], z: B, c: B, r: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) if (x == c) => r
      case Cons(x, xs) => f(x, foldRight2(xs, z, c, r)(f))
    }
  def product3(ns: List[Double]) =
    foldRight2(ns, 1.0, 0.0, 0.0)(_ * _)

  // Exercise 3.8
  // foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))の結果はどうなるか?
  // 元のList(1,2,3)を返す。

  // Exercise 3.9
  // (x, y)のxは使ってないので，ワイルドカードでOK
  // (_, acc) => acc + 1
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x,y) => 1 + y)

  // Exercise 3.10
  // case Cons(x, xs)における戻り値の呼び出しをfoldLeftにする必要がある。
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
      case Nil => z
      // 結合するには再帰(xs, f(z, x))を呼ぶ
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  // このfoldLeftを使ってsum, product, lengthが書ける。
  def sumLeft(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)
  def productLeft(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)
  def lengthLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((x,y) => x + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, List[A]())((x,y) => Cons(y,x))

  // Exercise 3.14
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x,y) => Cons(x,y))

  // Exercise 3.16
  def addOne(as: List[Int]): List[Int] =
    foldRight(as, List[Int]())((x,y) => Cons(x+1, y))

  // Exercise 3.17
  def doubleListToString(as: List[Double]): List[String] =
    foldRight(as, List[String]())((x,y) => Cons(x.toString, y))

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRight(l, List[B]())((x,y) => Cons(f(x), y))

  // Exercise 3.19
  // foldRightを使いたかったが，条件式の書き方がわからなかったのでmatchで。
  def filter[A](as: List[A])(f: A => Boolean): List[A] = 
    as match {
      case Nil => Nil
      case Cons(h, t) if (f(h)) => Cons(h, filter(t)(f))
      case Cons(h, t) => filter(t)(f)
    }
  // 解答を見ると，どうやら以下のようにif(f(h))を埋め込めるらしい。
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = 
    foldRight(as, Nil:List[A])((h,t) => if(f(h)) Cons(h, t) else t)

  // Exercise 3.20
  // concatとmapを使うことでflatMapが書ける。
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // Exercise 3.21
  def filter3[A](as: List[A])(f: A => Boolean): List[A] = 
    flatMap(as)((x) => if (f(x)) List(x) else Nil)

  // Exercise 3.22
  // ()で括ることで複数の対象をmatch式で扱える。
  def exercise322(a1: List[Int], a2: List[Int]): List[Int] =
    (a1,a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2, exercise322(t1,t2))
    }

  // Exercise 3.23
  // 解答ではa2の型がList[B], 返り値がList[C]と全て異なる型でも使える
  // ようになっていた。(fの型も(A, B) => C)
  def zipWith[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] =
    (a1,a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
    }

  // Exercise 3.24
  // 先頭同士が等しくない場合，もう一度関数に最初に渡したsubが必要になる
  // 最初に渡したsubを保存しておくbkupを使ったが，カッコ悪い...
  def subSequence[A](sup: List[A], sub: List[A], bkup: List[A]): Boolean = 
    (sup,sub) match {
      case (Cons(h1,t1),Cons(h2,t2)) if (h1 == h2 && t2 == Nil) => true
      case (Cons(h1,t1),Cons(h2,t2)) if (h1 == h2) => subSequence(t1, t2, bkup)
      case (Cons(h1,t1),Cons(h2,t2)) => subSequence(t1, bkup, bkup)
      case (Nil, _) => false
      case (_, Nil) => false
    }
  // 解答では，先頭同士が等しかった場合，その後の検索処理を別の関数にやらせている
  @annotation.tailrec
  def startWith[A](l: List[A], search: List[A]): Boolean =
    (l, search) match {
      case (Cons(h1,t1),Cons(h2,t2)) if (h1 == h2 && t2 == Nil) => true
      case (Cons(h1,t1),Cons(h2,t2)) if (h1 == h2) => startWith(t1, t2)
      case _ => false
    }
  @annotation.tailrec
  def subSequence2[A](sup: List[A], sub: List[A]): Boolean = 
    (sup,sub) match {
      case (Cons(h1,t1),Cons(h2,t2)) if (h1 == h2) => startWith(sup, sub)
      case (Cons(h,t), _)  => subSequence2(t, sub)
      case _ => false
    }  
}
