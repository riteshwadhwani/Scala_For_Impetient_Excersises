package Excercises.Operators

class Money (d : Int, c : Int)  {
  var dollar : Int = d;
  var cents : Int = if(c <100) c else {val v = c /100 ; dollar += v ; c%100}

  def +(other : Money): Money = {
    new Money(this.dollar + other.dollar , this.cents + other.cents);
  }

  def -(other : Money) : Money = {
    var d = this.dollar - other.dollar;
    var c = this.cents - other.cents;
    var cents = if(c<0) {d - 1; 100 - Math.abs(c)} else c;
    new Money( d, cents);
  }

  def ==(other : Money) : Boolean = {
    this.dollar == other.dollar && this.cents == other.cents;
  }
}

object Money {
  def apply(d : Int, c : Int) = new Money(d,c);
}


object Ques4 extends  App{
  val ans = Money(1,75) + Money(0,50) == Money(2,25);
  print(ans);//true
}
