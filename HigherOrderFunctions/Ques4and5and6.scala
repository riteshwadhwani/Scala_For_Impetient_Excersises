package Excercises.HigherOrderFunctions

object Ques4and5and6 extends App {

  //Ques 4 Factorial

  println((0 to 5).foldLeft(1)(_ * _));

  //Ques 5 // Getting largest value among the range after putting it on the function.
  println(largest(x => 10 * x - x* x, 1 to 10));
  def largest(fun : Int => Int, inputs : Seq[Int]): Int = {
    inputs.map(x=>fun(x)).reduceLeft((x,y)=> if(x>y) x else y );
  }

  //Ques 6  // same as 5 but now returning which no is giving highest value

  println(largestAt(x => 10 * x - x * x, 1 to 10))

  def largestAt(fun : Int => Int, inputs : Seq[Int]): Int = {
    inputs.map(x=> (x,fun(x))).sortWith(_._2 > _._2).map(x => x._1).head;
  }

}
