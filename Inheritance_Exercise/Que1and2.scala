package Excercises.Inheritance_Exercise


class BankAccount(initialBalace:Double) {
  private var balance = initialBalace;
  def currentBalance = balance;
  def deposit(amount : Double) = {balance += amount ; balance}
  def withdraw(amount : Double) = {balance -= amount ; balance}
}

class CheckingAccount(initialBalance : Double) extends BankAccount(initialBalance){

  override def deposit(amount: Double): Double = super.deposit(amount - 1);

  override def withdraw(amount: Double): Double = super.withdraw(amount - 1);
}

class SavingsAccount(initialBalance : Double) extends BankAccount(initialBalance) {
  private var count = 0;

  override def deposit(amount: Double): Double = { count+= 1 ; super.deposit(amount);}

  override def withdraw(amount: Double): Double = {count +=1 ; super.withdraw(amount)}

  def calculateInterest() : Double = {
    if(count > 3){
      val times : Double = count - 3;
      super.withdraw(times);
      val balance = currentBalance;
      balance;
    }
    else currentBalance;
  }
}

object Que1and2 extends  App {

}
