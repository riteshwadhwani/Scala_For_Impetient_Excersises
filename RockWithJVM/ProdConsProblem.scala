package Excercises.RockWithJVM

import scala.collection.mutable
import scala.util.Random

object ProdConsProblem extends App{

  // The Producer and consumer problem
  class Value {
    var value : Int = 0;
    def isEmpty : Boolean = {
      value == 0;
    }
    def get(): Int = {
      val result = value;
      value = 0
      result;
    }
    def set(setValue : Int) : Unit = {
      value  = setValue;
    }
  }

  def naiveProdCons() = {
    val value = new Value

    val consumer = new Thread(() => {
      println("consumer is waiting...")
      while(value.isEmpty){
        println("cosumer is actively waiting")
      }
      println("Consumer consumed the value "+ value.get())
    })
    val producer = new Thread(() => {
      println("producer is computing")
      Thread.sleep(500)
      val digit = 42;
      println("Producer produces some value "+ digit);
      value.set(digit)
    })
    consumer.start()
    producer.start();
  }

  //   naiveProdCons()

  // By using wait and notify

  def smarterProdCons():Unit = {
    val value  = new Value

    val producer = new Thread(()=>{
      println("Producing the value..")
      Thread.sleep(1000)
      val digit = new Random().nextInt(10)

      value.synchronized{
        println("[producer] producing "+ digit)
        value.set(digit)
        value.notify()
      }
    })

    val consumer = new Thread(()=>{
      println("[consumer] waiting..")
      value.synchronized{
        value.wait()
      }
      println("Consuming the value "+ value.value)
    })

    consumer.start()
    producer.start()
  }

  //   smarterProdCons();

  // Producer Consumer Level 2

  private def smarterProdConsLeveLTwo(): Unit = {
    val capacity = 3
    val buffer = scala.collection.mutable.Queue[Int](3);
    val consumer = new Thread(() =>{
      val random = new Random()
      while(true){
        buffer.synchronized{
          if(buffer.isEmpty){
            println("buffer is empty consumer is waiting...")
            buffer.wait()
          }
          val x = buffer.dequeue()
          println("Consumer consumed " + x);

          buffer.notify();
        }
        Thread.sleep(random.nextInt(500))
      }
    })

    val producer = new Thread(() =>{
      val random = new Random()
      while(true){

        buffer.synchronized{
          if(buffer.size == capacity){
            println("Producer is waiting to  produce... ")
            buffer.wait()
          }
          val value = new Random().nextInt(10);
          buffer.enqueue(value)
          println("Produced the value..." + value)
          buffer.notify();

        }
        Thread.sleep(random.nextInt(250))
      }
    })

    consumer.start();
    producer.start();

  }

  //  smarterProdConsLeveLTwo

  /*
  Level 3 of ProdCons
  producer1 -> [???] -> consumer1
  producer2 -> [???] -> consumer2
   */

  class Consumer(id : Int , buffer : collection.mutable.Queue[Int]) extends  Thread {
    override def run() : Unit ={
      val random  = new Random()

      while (true) {
        buffer.synchronized{
          while(buffer.isEmpty){
            println(s"Consumer with  id $id is waiting")
            buffer.wait()
          }
          val x = buffer.dequeue()
          println(s"[consumber $id] consumed " + x)
          buffer.notify()
        }
      }
      Thread.sleep(random.nextInt(500))
    }
  }

  class Producer(id : Int, buffer : collection.mutable.Queue[Int] , capacity : Int) extends  Thread {
    override def run(): Unit = {
      val random = new Random()
      while(true){
        buffer.synchronized{
          while(buffer.size == capacity){
            println("[producer] buffer is full, waiting...")
            buffer.wait();
          }
          val value = random.nextInt(10);
          buffer.enqueue(value)
          println("Producer produced " + value);
          buffer.notify()
        }
        Thread.sleep(random.nextInt(500));
      }
    }
  }

  def multiProducerConsumer(nConsumers : Int, nProducers : Int) = {
    val buffer : collection.mutable.Queue[Int] = new mutable.Queue[Int](3);
    val capacity : Int = 3;

    (1 to nConsumers).foreach(i => new Consumer(i,buffer).start())
    (1 to nProducers).foreach(i => new Producer(i, buffer , capacity).start())

  }

  multiProducerConsumer(3,3);


}
