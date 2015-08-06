import java.util.concurrent._OA

object Par {
  type Par[A] = ExecutorService => Future[A]  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // unitはUnitFutureを返す関数として表される。
  // unitはExecutorServiceを使用せず，常に完了し，途中で中止できない。
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) 
  
  // UnitFutureは定数値をラッピングするだけのFutureの簡単な実装である。
  // getメソッドは渡された値を返すだけ。
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  // map2はfの呼び出しを別の論理スレッドで評価しない。
  // 設計上の選択として，並列化を制御する唯一の関数をforkとしているため，
  // fの評価を別スレッドで処理させたい場合はfork(map2(a,b)(f))を実行する。
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      // このmap2の実装はタイムアウトを考慮に入れない。
      // 両方のPar値にExecutorServiceを渡し，afとbfのFutureの結果を待って，
      // それからfを適用してUnitFutureでラッピングするだけである。
      UnitFuture(f(af.get, bf.get))
    }

  // forkの最も単純で自然な実装だが，問題点がいくつかある。
  // 1つは，外側のCollableが内側のタスクが完了するのを待つ間ブロックすること。
  // このブロックにより何らかのリソースが消費されるため並列化の可能性を逃す。
  // 実質的には，スレッド1つで十分なはずの場所でスレッドを2つ使用してしまっている。
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  // 評価されていない引数をParでラッピングし，並列評価の対象としてマークする
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /* Exercise 7.4 */
  // A型の引数aをとり，Par[B]を返す関数を返り値とする。
  // ParBを返すには，lazyUnitを使ってf(a)を引数とする。
  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  // Par[A]の値をPar[B]に変換する。
  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  // Par[List[Int]]をソートする。
  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  /* Exercise 7.5 */
  // 単純な実装。List[Par[A]]をPar[List[A]]に変換する。
  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequence(t)))(_ :: _)
    }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  // 有効な引数ExecutorServiceに対して，
  // それらの結果であるFutureが同じ値なら2つのParは等しい
  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  // 計算が実際に必要になるまでそのインスタンス化を遅らせる。
  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  // フォークする2つの計算のどちらかを関数に選択させる。  
  // condがtrueになる場合はtを，falseになる場合はfを実行する。
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on 
      else f(es)

  /* Exercise 7.11 */
  // nを実行した結果に基いてchoicesから並列計算を選択する。
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val res = run(es)(n).get
      run(es)(choices(res))
    }

  // choiceNを元にchoiceを実装。
  // mapでPar[Boolean]を0か1のPar[Int]に変換し，choicesとしてList(t, f)を渡す
  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(bool => if (bool) 0 else 1))(List(t, f))

  // choiceNのListの代わりにMapを使った実装。
  def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V]
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  /* Exercise 7.13 */
  // choicesをA => Par[B]型の変数に変更している。
  // ※通常，このchooserはflatMapやbind関数と呼ばれる。
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val res = run(es)(pa).get
      run(es)(choices(res))
    }

  // chooserを元にchoiceを実装
  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(bool => if (bool) t else f)

  // chooserを元にchoiceNを実装
  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(i => choices(i))

  // 先ほどのchooserをflatMapとして実装（関数名を変えただけ）
  def flatMap[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val res = run(es)(pa).get
      run(es)(choices(res))
    }

  /* Exercise 7.14 */
  // Par[Par[A]]をPar[A]に変換する。
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(p => p)

  def faltMapViaJoin[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(pa)(choices))
}
