package Excercises.Inheritance_Exercise


//Ques 5
class Point (var x : Int,var y : Int) {

}

class LabeledPoint(iLabel : String ,x : Int , y : Int) extends  Point(x,y){
  var label : String = iLabel;
}

//Ques 6

abstract class Shape {
  def centerPoint : Any;
}

class Rectangle(p1 : Point, p2 : Point, p3 : Point , p4 : Point) extends Shape {
  var Point1 : Point = p1;
  var Point2 : Point = p2;
  var Point3 : Point = p3;
  var Point4 : Point = p4;

  override def centerPoint: Any = {
    val x : Int = (Point3.x + Point4.x )/ 2;
    val y : Int = (Point1.y + Point3.y) /2;
    (x,y)
  }
}

class Circle(r: Int) extends  Shape {
  var radius : Int = r;
  override def centerPoint: Any = ???
}



class Ques5and6 {

}
