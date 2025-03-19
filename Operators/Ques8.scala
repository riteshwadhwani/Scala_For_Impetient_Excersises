package Excercises.Operators

class Matrix {
  var matrix = Array(Array[Int](2),Array[Int](2));

  def apply(row : Int, col : Int) = {
    matrix(row)(col);
  }

  def +(other : Matrix)  : Matrix = {
    var m = new Matrix;
    m.matrix(0)(0) = this.matrix(0)(0) + other.matrix(0)(0);
    m.matrix(0)(1) = this.matrix(0)(1) + other.matrix(0)(1);
    m.matrix(1)(0) = this.matrix(1)(0) + other.matrix(1)(0);
    m.matrix(1)(1) = this.matrix(1)(1) + other.matrix(1)(1);
    m;
  }

  def *(other : Matrix) : Matrix = {
    var m = new Matrix;
    m.matrix(0)(0) = this.matrix(0)(0) * other.matrix(0)(0);
    m.matrix(0)(1) = this.matrix(0)(1) * other.matrix(0)(1);
    m.matrix(1)(0) = this.matrix(1)(0) * other.matrix(1)(0);
    m.matrix(1)(1) = this.matrix(1)(1) * other.matrix(1)(1);
    m;
  }

  def *(n : Int) = {
    matrix(0)(0) *= n;
    matrix(0)(1) *= n;
    matrix(1)(0) *= n;
    matrix(1)(1) *= n;
  }

}


class Ques8 {

}
