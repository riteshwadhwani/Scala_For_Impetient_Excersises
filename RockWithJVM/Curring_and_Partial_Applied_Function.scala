package Excercises.RockWithJVM

object Curring_and_Partial_Applied_Function extends App{
  //Excercise

  //Ques 1

  //My Sol
  def curriedFormatter: (String, Double) => String = (_).format(_);

  List(("%8.6f", 2323.232), ("%5.6f", 4.32), ("%6.4f", 12.1)) foreach (x => println(curriedFormatter(x._1, x._2)))

  println("_______ sol2 ________")

  //Sol2

  def curriedFormatter2(x: String)(y: Double): String = x.format(y);
  val simpleFormatter = curriedFormatter2("%4.2f") _;
  List(Math.PI, Math.E, 1.34, 2.45, 5.47) map (x => simpleFormatter(x)) foreach println;
  //OR println(List(Math.PI,Math.E,1.34,2.45,5.47) map(simpleFormatter));


  //Ques2

  def byName(x: => Int) = x + 1;

  def byFunction(f: () => Int) = f() + 1;

  def method: Int = 42;

  def parenMethod(): Int = 42;

  /*calling byName and  byFunction  with int*/

  println(byName(2)) // Ok it can take any thing that returns an int like function variable method
  //  println(byFunction(2)) // NOT OK should take a function which returns an Int value byFunction(()=> 3)

  //  /*calling byName and  byFunction  with method*/
  //
  println(byName(method)) //Ok
  //  println(byFunction(method)) // method is also not ok because it evaluated to its value

  /*calling byName and  byFunction  with parenMethod*/
  //
  println(byName(parenMethod())) // OK
  println(byFunction(parenMethod )) // OK compiler does eta expression (parenMethod _) for us


  /*calling byName and  byFunction  with Lambda*/

  //  println(byName(()=>42)) //NOt OK
  println(byName((() => 45)())) // OK because we are defining and calling which returns the value .
  println(byFunction(() => 42)) // OK


  /*calling byName and  byFunction  with partial Function */

  //  println(byName(partialFunc)) //NOt OK
  byFunction(parenMethod _) //Not OK

  /*
  Here byName only use the value which is returning from the method or we can just supply the function
   */
}
