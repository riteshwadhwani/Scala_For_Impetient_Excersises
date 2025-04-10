package Excercises.RockWithJVM

object LazilyEvaluatedStream extends  App {


  /* Exercise
  Implement a laizily evaluated , singly linked Stream of elements
   */

  abstract class MyStream[+A] {
    def isEmpty: Boolean;

    def head: A;

    def tail: MyStream[A]

    //Prepends the element
    def #::[B >: A](element: B): MyStream[B]

    def ++[B >: A](anotherStream: MyStream[B]): MyStream[B]

    def foreach(f: A => Unit): Unit

    def map[B](f: A => B): MyStream[B]

    def flatMap[B](f: A => MyStream[B]): MyStream[B]

    def filter(predicate: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A] // takes first n elements

    def takeAsList(n: Int): List[A] //return first n elements as List
  }

  class EmptyStream[A] extends MyStream[A] {

    override def isEmpty: Boolean = true

    override def head: A = ???

    override def tail: MyStream[A] = this

    override def #::[B >: A](element: B): MyStream[B] = new Stream(element, this);

    override def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] = anotherStream

    override def foreach(f: A => Unit): Unit = ()

    override def map[B](f: A => B): MyStream[B] = ???

    override def flatMap[B](f: A => MyStream[B]): MyStream[B] = ???

    override def filter(predicate: A => Boolean): MyStream[A] = ???

    override def take(n: Int): MyStream[A] = ???

    override def takeAsList(n: Int): List[A] = ???
  }

  class Stream[+A](h: A, t: => MyStream[A]) extends MyStream[A] {

    override def isEmpty: Boolean = false

    override def head: A = h

    override def tail: MyStream[A] = {
      lazy val tail = t; tail
    }

    override def #::[B >: A](element: B): MyStream[B] = new Stream(element, this)

    override def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] = if (this.isEmpty) anotherStream else {
      lazy val t = this.tail.++(anotherStream.#::(this.head)); t
    }

    //++ another way new Stream(h,t ++ anotherStream)
    override def foreach(f: A => Unit): Unit = if (this.tail.isEmpty) f(this.h) else {
      f(this.h); this.tail.foreach(f)
    };

    // {f(head); tail.foreach(f);}
    override def map[B](f: A => B): MyStream[B] = if (this.tail.isEmpty) new Stream[B](f(this.head), new EmptyStream[B]) else this.tail.map(f).#::(f(this.head));

    override def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

    override def filter(predicate: A => Boolean): MyStream[A] = if (predicate(this.head)) new Stream(this.head, this.tail.filter(predicate)) else this.filter(predicate);

    override def take(n: Int): MyStream[A] = if (n == 0) new EmptyStream else if (n == 1) new Stream(this.head, new EmptyStream) else this.tail.take(n - 1).#::(this.head)

    override def takeAsList(n: Int): List[A] = if (n == 0) List[A]() else if (n == 1) List(this.head) else {
      this.tail.takeAsList(n - 1).appended(this.head)
    }
  }

  object MyStream {
    def from[A](start: A)(generator: A => A): MyStream[A] = {
      new Stream[A](start, this.from(generator(start))(generator));
    }
  }

  val stream = new Stream[Int](1, new Stream[Int](2, new EmptyStream[Int]));

  val addedStream = 3 #:: stream

  addedStream foreach println

  println("---------------twice---------------");

  addedStream.map(x => x * 2) foreach println

  println("-------------two Added----------------")

  val addedStreams = stream ++ addedStream

  addedStreams foreach println

  println("------------taking As List----------------")
  val list = addedStreams.takeAsList(3);

  list foreach println

  println("--------Crashing------------------")
  lazy val numbers = MyStream.from(1)(x => x + 1)

  println("____100_____")
  numbers.take(100) foreach println

  def getFiboStream[A](func: (A, A) => A, value: A, fib: A): MyStream[A] = {
    new Stream(fib, getFiboStream(func, fib, func(value, fib)))
  }

  val fiboStream = getFiboStream((a:Int,b:Int) => (a + b),1,0);

  println("--------------fibo Series---------------")

  fiboStream.take(10) foreach println

}
