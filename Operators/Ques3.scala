package Excercises.Operators


class Fraction(n:Int , d : Int) {

  var num : Int = if(d == 0) 1 else n * sign(n) / gcd(n,d);
  var deno: Int = if(d==0) 0 else d * sign(d)/gcd(n,d);

  def *(other: Fraction): Fraction = {
    new Fraction(this.num * other.num, this.deno * other.deno);
  }

  def -(other: Fraction) : Fraction = {
    new Fraction(this.num - other.num , this.deno - other.deno);
  }

  def +(other : Fraction) : Fraction = {
    new Fraction(this.num + other.num , this.deno + other.deno);
  }

  def /(other : Fraction) : Fraction = {
    new Fraction(this.num / other.num , this.deno / other.deno);
  }


  def sign(n : Int)  = if(n>0) 1 else if(n<0) -1 else 0;

  def gcd(a : Int, b : Int): Int = if(b == 0) Math.abs(a) else gcd(b,a%b);

}

object Fraction {
  def apply(n : Int, d : Int) : Fraction = new Fraction(n,d);
}

class Ques3 {
  val f1 = Fraction(20,10);
  val f2 = Fraction(30,10);

  val f3 = f1-f2;
  val f4 = f1/f2;
  val f5 = f1*f2;

  println(f3.num + " " + f3.deno); //1 0
  println(f4.num + " " + f4.deno); //0 1
  println(f5.num + " " + f5.deno);// 6 1


}
