package Excercises.Inheritance_Exercise


// I think in subclass if we override range with val keyword it will become variable and on the time of array
//initialization the range is not initialized so the length of array will be zero.

// And if we override it with def the by dynamic method dispathch it will call the range method of subclass
//which will return the value 2 and the length of the array will be two.

class Creature {
  def range : Int = 10;
  val env: Array[Int] = new Array[Int](range);
}

class Ant extends Creature{
  override def range = 2;
}

class Ques9 {
  val a = new Ant();
  print(a.env.length);
}
