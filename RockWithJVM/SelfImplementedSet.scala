package Excercises.RockWithJVM

import scala.annotation.tailrec


object SelfImplementedSet extends  App {
  trait MySet[A] extends (A=>Boolean) {
    def head : A
    def tail : MySet[A] ;
    def contains(a : A) : Boolean;
    def isEmpty : Boolean;
    def +(element: A) : MySet[A];
    def ++(set : MySet[A]) : MySet[A]
    def map[B](f : A =>B) : MySet[B]
    def flatMap[B](f : A => MySet[B]) : MySet[B];
    def filter(predicate : A => Boolean) : MySet[A];
    def size : Int
    def foreach(func : A => Unit): Unit
  }

  class Empty[A] extends MySet[A] {

    def head : A = ???
    def tail : MySet[A] = ???
    override def contains(a: A): Boolean = ???

    override def isEmpty : Boolean = true;

    override def +(element: A): MySet[A] = new set(element,new Empty);

    override def ++(set: MySet[A]): MySet[A] = set

    override def map[B](f: A => B): MySet[B] = new Empty

    override def flatMap[B](f: A => MySet[B]): MySet[B] = new Empty

    override def filter(predicate: A => Boolean): MySet[A] = new Empty

    override def apply(v1: A): Boolean = false

    override def size: Int = 0

    override def foreach(func: A => Unit): Unit = new Empty
  }


  class set[A](h : A, t : MySet[A]) extends MySet[A]{
    def head : A = h
    def tail : MySet[A] = t

    override def isEmpty : Boolean = false;

    @tailrec
    private def containsHelper(elem : A, isPresent : Boolean, set : MySet[A]): Boolean = {
      if(set.isEmpty) isPresent else containsHelper(elem , isPresent || set.head == elem ,set.tail);
    }
    override def contains(a: A): Boolean = if(this.isEmpty) false else containsHelper(a,false,this);

    override def +(element: A): MySet[A] = if(this.contains(element)) this else new set(element,this);

    def add(A : MySet[A],B : MySet[A],head : A): MySet[A] = {
      if(B.isEmpty) A else {
        if(B.tail.isEmpty){
          add(A + head , new Empty, head);
        }else  if(A.contains(head)) add(A,B.tail,B.tail.head) else add(A + head , B.tail,B.tail.head)
      }

    }
    override def ++(set: MySet[A]): MySet[A] = add(this,set,set.head);


    override def map[B](f: A => B): MySet[B] = if(this.tail.isEmpty) new set(f(this.h),new Empty)
    else new set(f(this.h),this.tail.map(f));

    override def flatMap[B](f: A => MySet[B]): MySet[B] = ???

    override def filter(predicate: A => Boolean): MySet[A] = if(predicate(this.head))
      new set(this.head, this.tail.filter(predicate)) else this.tail.filter(predicate)

    override def apply(v1: A): Boolean = this.contains(v1)

    @tailrec
    private def sizeHelper(set : MySet[A], size : Int): Int = {
      if(set.isEmpty) size else sizeHelper(set.tail,size + 1)
    }
    override def size: Int = sizeHelper(this,0);

    def foreachHelper(func: A => Unit , set : MySet[A]): Unit = {
      if(!set.isEmpty) {
        func (set.head)
        foreachHelper(func,set.tail);
      }
    }
    override def foreach(func: A => Unit): Unit = foreachHelper(func , this);
  }


  //  object MySet {
  //
  //    def apply[A](values : A*) : MySet[A] = {
  //      def buildSet(valSeq : Seq[A],acc : MySet[A]) : MySet[A] = {
  //        if(valSeq.isEmpty) acc else buildSet(valSeq.tail, acc + valSeq.head)
  //      }
  //      buildSet(values.toSeq : Seq[A],new Empty[A]);
  //    }
  //  }

  val set : MySet[Int] = new set[Int](1,new set(2,new set(3,new Empty)));

  println(set.head + " " + set.tail.head + " " + set.tail.tail.head);

  val set2 = set + 5
  println(set2.size);

  val set3 = set2 + 5;

  println(set3.size);

  val set4 = set ++ set3

  println(set4.size)

  println(set2.head + " " + set2.tail.head + " " + set2.tail.tail.head + " " + set2.tail.tail.tail.head);

  val filteredSet = set.filter(x => x%2==0)

  println(filteredSet.size);
  println(filteredSet.head  + " " + (if(filteredSet.tail.isEmpty) 0 else filteredSet.tail.head ) )


  set foreach println
  println("___________")
  set ++ set3 foreach println ;

  set ++ set3 ++ set4 map(x => x*10) filter(x => x %2 == 0) foreach println
}


