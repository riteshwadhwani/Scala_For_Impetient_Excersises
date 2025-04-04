package Excercises.RockWithJVM



package Main

import scala.None
import scala.runtime.Nothing$

trait MyPredicate[-T] {
  def test(condition : T) :Boolean ;
}


trait MyTransformer[-A,B] {
  def transform(input : A) : B;
}


abstract  class MyList[+A] {
  def head : A
  def tail : MyList[A]
  def isEmpty  : Boolean
  def add[B>:A](element : B) : MyList[B]
  def printElements : String
  override def toString: String = "[" + printElements + "]"
  def map[B](input : MyTransformer[A,B]) : MyList[B];
  def flatMap[B](input: MyTransformer[A,MyList[B]]) : MyList[B];
  def filter(predicate : MyPredicate[A]) : MyList[A];
  def ++[B >: A] (list : MyList[B]) : MyList[B]
}

object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException;

  override def tail: MyList[Nothing] = ???

  override def isEmpty: Boolean = true

  override def add[B >: Nothing](element: B): MyList[B] = new Cons(element,Empty);

  override def printElements: String = ""

  override def map[B](input: MyTransformer[Nothing, B]): MyList[B] = Empty;

  override def flatMap[B](input: MyTransformer[Nothing, MyList[B]]): MyList[B] = ???

  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty;

  override def ++[B >: Nothing](list: MyList[B]): MyList[B] = ???
}

class Cons[+A](h:A,t:MyList[A]) extends  MyList[A]{
  override def head: A = h

  override def tail: MyList[A] = t

  override def isEmpty: Boolean = false

  override def add[B >: A](element: B): MyList[B] = new Cons(element,this)

  override def printElements: String = if(t.isEmpty) "" + h else h + " " + t.printElements;

  override def map[B](transformer: MyTransformer[A, B]): MyList[B] = if(t.isEmpty) new Cons[B](transformer.transform(h),Empty)
  else new Cons[B](transformer.transform(h),t.map(transformer));

  override def ++[B >: A](list: MyList[B]): MyList[B] = new Cons(h,t ++ list);

  override def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] =
    transformer.transform(h) ++ t.flatMap(transformer);

  override def filter(predicate: MyPredicate[A]): MyList[A] =
    if(predicate.test(h)) new Cons(h,t.filter(predicate))
    else t.filter(predicate);

}

object MyListImplementations extends App {
  val listOfIntegers : MyList[Int]  = new Cons(1,new Cons(2,new Cons(3,new Cons(4,Empty))));

  println(listOfIntegers.toString);

  println(listOfIntegers.map(new MyTransformer[Int,Int] {
    override def transform(input : Int): Int = input * 2
  }).toString)

  println(listOfIntegers.filter(new MyPredicate[Int] {
    override def test(condition: Int): Boolean = condition %2 == 0
  }).toString)

  println(listOfIntegers.flatMap(new MyTransformer[Int,MyList[Int]] {
    override def transform(input: Int): MyList[Int] = new Cons[Int](input,new Cons[Int](input + 1, Empty));
  }))
}
