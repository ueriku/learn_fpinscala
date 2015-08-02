trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      // 現在のシードを使って新しいシードを作成
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      // 新しいシードから作成されたRNGインスタンス = 次の状態
      val nextRNG = SimpleRNG(newSeed)
      // 新しい擬似乱数。>>>は0埋め右バイナリシフト
      val n = (newSeed >>> 16).toInt
      // 戻り値は擬似乱数とRNGの次の状態からなるタプル
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /* Exercise 6.1 */
  // RNG.nextIntの絶対値を取れば良い。負の値ならマイナスを付ける。
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    // 返り値は(n, rng2)だが，nだけif/elseで場合分けしている
    (if (n < 0) -(n + 1) else n, rng2)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /* Exercise 6.2 */
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, rng2)
  }

  /* Exercise 6.3 */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng2)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((n, d), rng2) = intDouble(rng)
    ((d, n), rng2)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  /* Exercise 6.4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(cnt: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = {
      if (cnt <= 0) (acc, rng)
      else { 
        val (n, rng2) = rng.nextInt
        go(cnt - 1, n :: acc, rng2)
      }
    }
    go(count, List(), rng)
  }

  /* Exercise 6.5 */
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  /* Exercise 6.6 */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a,b), r2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))
  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  /* Exercise 6.8 */
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  /* Exercise 6.9 */
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))
  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

case class State[S,+A](run: S => (A, S)) {
  /* Exercise 6.10 */
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get 
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val m = Machine(true, 5, 10)
  }
}
