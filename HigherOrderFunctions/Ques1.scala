package Excercises.HigherOrderFunctions

object Ques1 extends  App{



  // Have Doubt on 1st one

  //  def until(condition : Boolean)(block : => Unit): Unit = {
  //    if(!condition){
  //      block;
  //      until(condition)(block);
  //    }
  //  }

  //  def values(func : Int => Int, low : Int , high : Int): scala.collection.mutable.Map[Int,Int] = {
  //    val map = scala.collection.mutable.Map[Int,Int]()
  //    until(low<high){
  //      map += (low -> func(low))
  //    }
  //    map;
  //  }

  def values(func : Int => Int, low : Int , high : Int): IndexedSeq[(Int,Int)] = {
    val map = (low to high).map(x=> (x,func(x)));
    map
  }

  val ans = values(x=> x*x,-5,5);
  print("ans" + ans);

}
