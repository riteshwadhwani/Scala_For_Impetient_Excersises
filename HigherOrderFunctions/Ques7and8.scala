package Excercises.HigherOrderFunctions

object Ques7and8 extends App {

  // Ques 7

  def abjustToPair = (fun : (Int,Int) => Int) => (p : Any)  =>{

  }

  // Ques 8 checking if the length of string in strings array and the number in the length array on their same index
  //are equal or not for all the string.


  val strings = Array("Ritesh","Rohan","Ashok","Balaji","Dhruv","Pawan","Avish");

  val length = Array(8,5,5,6,5,5,5);

  print(strings.corresponds(length)(_.length == _)); // false

}
