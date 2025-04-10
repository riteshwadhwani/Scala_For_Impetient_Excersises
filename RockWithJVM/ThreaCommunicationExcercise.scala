package Excercises.RockWithJVM

object ThreaCommunicationExcercise extends  App{
  //Thread Communication Exercises
  /*
  1) think of an example where notifyAll acts in a different way than notify?
  2) create a deadlock
  3) create a livelock
   */

  //1

  def testNotifyAll(): Unit = {
    val bell = new Object
    (0 to 10).foreach(i => new Thread(()=>{
      bell.synchronized{
        println(s"Thread $i is waiting..")
        bell.wait()
        println(s"Thread $i release")
      }
    }).start())
    new Thread(()=>{
      Thread.sleep(2000)
      println("Announcer is announcing...")
      bell.synchronized{
        bell.notifyAll();
      }
    }).start()
  }
  //  testNotifyAll()

  //2
  case class Friend(name : String) {
    def bow(other : Friend) = {
      this.synchronized{
        println(s"$this: I am bowing to my friend $other")
        other.rise(this)
        println(s"$this: my friend $other has risen")
      }
    }
    def rise(other: Friend)={
      this.synchronized{
        println(s"$this: I am rising to my friend $other")
      }
    }
    var side = "right"
    def swithSide() : Unit = {
      if(side == "right") side = "left"
      else side == "right"
    }
    def pass(other : Friend) : Unit = {
      while(this.side == other.side){
        println(s"$this: Oh but please, $other, feel free to pass")
        swithSide()
        Thread.sleep(1000)
      }
    }
  }

  val sam = new Friend("Sam");
  val ritesh = new Friend("Ritesh")

  //  new Thread(()=>sam.bow(ritesh)).start()
  //  new Thread(()=>ritesh.bow(sam)).start()

  //3 - livelock
  new Thread(()=> sam.pass(ritesh)).start()
  new Thread(()=> ritesh.pass(sam)).start()


}
