package Excercises.RockWithJVM

object AdvancePatternMatching extends App{

  class Person (name : String) {
    var personName: String = name;
    var age = 21;

    def name: String = personName;
    def name_=(name : String): Unit = {
      this.personName = name;
    }
  }
  private object Person {
    def unapply(person: Person) : Option[(String,Int)] = {
      Some(person.personName , person.age);
    }
    //    def unapply(person: Person) : Option[(String)]={
    //      Some(person.name);
    //    }
    def unapply(age : Int) : Option[String] = {
      Some(if(age<10) "single digit" else if(age %2 == 0 ) "an even number" else "not both" );
    }
  }

  /*
   Exercise
   */

  //1.
  val n = 45;
  private val isSome = n match {
    case Person(isSome) => s"$isSome"
  }
  println(isSome)
  //2.

  private val n1 = 40;
  private object even {
    def unapply(digit : Int) : Option[Boolean]= {
      if(digit%2 == 0) Some(true) else None
    }
  }
  private object singleDigit {
    def unapply(digit : Int) : Option[Boolean] ={
      if(digit > -10 && digit < 10) Some(true) else None
    }
  }

  private val `is Something` = n1 match {
    case even(_) => "is Even"
    case singleDigit(_) => "sing digit"
    case _ => "none of the above"
  }
  println(`is Something`)

  //3. Or

  private val n2 = 40;

  private object even_ {
    def unapply(digit : Int): Boolean = digit%2 == 0;
  }

  private object singleDigit_ {
    def unapply(digit : Int): Boolean = digit > -10 && digit < 10;
  }
  private val `is Somthing ` = n2 match {
    case even_() => "is Even"
    case singleDigit_() => "sing digit"
    case _ => "none of the above"
  }
  println(`is Somthing `);

  private val person1 = new Person("Prashant");

  private val greeting = person1 match {
    case Person(n,a) => s"I am i $n and my age is $a"
  }
  println(greeting);

}
