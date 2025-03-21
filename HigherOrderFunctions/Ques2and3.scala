package Excercises.HigherOrderFunctions

object Ques2and3 extends App{

  //Ques 2
  val arr = Array(1,2,3,4,5,6,7,8,9);

  val ans = arr.reduceLeft( (x,y)=> if(x>y) x else y);

  println(ans);

  //Ques 3

  print(factorial(5));

  def factorial(value : Int): Int = {
    (1 to value).reduceLeft((x,y)=>x*y);
  }

}
