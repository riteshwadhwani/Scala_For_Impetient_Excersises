package Excercises.Inheritance_Exercise

import java.util.Scanner

abstract class Item {
  def price : Double;
  def description : String ;
}

class SimpleItem(initialPrice : Double, initialDescription : String) extends Item{
  override val price: Double = initialPrice;
  override val description: String = initialDescription;
}

class Bundle(noOfItems : Int) {
  private val sc = new Scanner(System.in);
  private var count : Int = 0;
  private val items = new Array[SimpleItem](noOfItems);

  def addItem() : String  = {
    if(count<items.length){
      println("Enter the initial Price of Item");
      val price : Double = sc.nextInt();

      println("Enter the description of Item");
      val description : String = sc.next();
      items(count) = new SimpleItem(price,description);
      count +=1
      s"Item no $count added";
    }
    else{
      s"Bundle is full";
    }
  }
}

class Ques4 {

}
