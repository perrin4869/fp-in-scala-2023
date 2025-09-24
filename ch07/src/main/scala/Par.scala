package fpinscala.parallelism

import java.util.concurrent.Callable
import java.util.concurrent.Future
import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit
import scala.annotation.targetName
import java.util.concurrent.TimeoutException

opaque type Par[A] = ExecutorService => Future[A]

object Par:
  def unit[A](a: A): Par[A] = es => UnitFuture(a)
  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false

  def fork[A](a: => Par[A]): Par[A] = es =>
    es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // ex01
  @targetName("map2Static")
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = a.map2(b)(f)
  extension [A](pa: Par[A])
    @targetName("map2Extension")
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = es =>
      val futureA = pa(es)
      val futureB = pb(es)
      UnitFuture(f(futureA.get, futureB.get))

  extension [A](pa: Par[A]) def run(s: ExecutorService): Future[A] = pa(s)

  // ex03
  object Map2:
    private case class Map2Future[A, B, C](
        fa: Future[A],
        fb: Future[B],
        f: (A, B) => C
    ) extends Future[C]:
      def isDone = fa.isDone && fb.isDone
      def get = f(fa.get, fb.get)
      def get(timeout: Long, units: TimeUnit) = {
        val start = System.nanoTime()
        val a = fa.get(timeout, units)
        val elapsedNanos = System.nanoTime() - start
        val b = fb.get(
          TimeUnit.NANOSECONDS.convert(timeout, units) - elapsedNanos,
          TimeUnit.NANOSECONDS
        )
        f(a, b)
      }
      def isCancelled = fa.isCancelled || fb.isCancelled
      def cancel(evenIfRunning: Boolean): Boolean = {
        val ca = fa.cancel(evenIfRunning)
        var cb = fb.cancel(evenIfRunning)
        ca || cb
      }

    extension [A](pa: Par[A])
      def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = es =>
        Map2Future(pa(es), pb(es), f)

    object AnswersKey:
      extension [A](pa: Par[A])
        def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = es =>
          new Future[C]:
            private val futureA = pa(es)
            private val futureB = pb(es)
            @volatile private var cache: Option[C] = None

            def isDone = cache.isDefined

            def get() = get(Long.MaxValue, TimeUnit.NANOSECONDS)

            def get(timeout: Long, units: TimeUnit) =
              val timeoutNs = TimeUnit.NANOSECONDS.convert(timeout, units)
              val started = System.nanoTime
              val a = futureA.get(timeoutNs, TimeUnit.NANOSECONDS)
              val elapsed = System.nanoTime - started
              val b = futureB.get(timeoutNs - elapsed, TimeUnit.NANOSECONDS)
              val c = f(a, b)
              cache = Some(c)
              c

            def isCancelled = futureA.isCancelled || futureB.isCancelled

            def cancel(evenIfRunning: Boolean) =
              futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)

  // ex04
  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  extension [A](pa: Par[A])
    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = parList.map(_.sorted)

  // ex05
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(Nil))((a, acc) => a.map2(acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork:
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)

  // ex06
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    as.foldRight(unit(Nil))((a, acc) =>
      asyncF(f)(a).map2(acc)((b, acc) => if b then a :: acc else acc)
    )

  object AnswersKey:
    // ex05
    def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
      if pas.isEmpty then unit(IndexedSeq.empty)
      else if pas.size == 1 then pas.head.map(a => IndexedSeq(a))
      else
        val (l, r) = pas.splitAt(pas.size / 2)
        sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)

    def sequence[A](pas: List[Par[A]]): Par[List[A]] =
      sequenceBalanced(pas.toIndexedSeq).map(_.toList)

    // ex06
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
      fork:
        val pars: List[Par[List[A]]] =
          as.map(asyncF(a => if f(a) then List(a) else Nil))
        sequence(pars).map(_.flatten)
