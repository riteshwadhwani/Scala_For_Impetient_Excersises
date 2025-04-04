package Excercises.RockWithJVM

import scala.util.Random

object Options extends App{


  val myFristOption : Option[Int] = Some(4);

  val noOption : Option[Int] = None

  // Here it will take care rather to assign the Some(k) or None according to the value coming to it.
  val result = Option(unsafeMethod());

  print(result);

  def unsafeMethod() : String = null;

  def backUpMethod() : String = "Returning the safe value";

  // This is how we worked with the Api like first we call the preferred one and if it fails the the backup one.
  val safedValue = Option(unsafeMethod()) orElse (Option(backUpMethod()));

  // And it's better to make your api's response safe by returning an Option

  def betterUnsafeMethod() : Option[String] = None;
  def betterSafeMethod() : Option[String] = Some("Returning the String");

  val betterSafedValue = betterUnsafeMethod() orElse betterSafeMethod();

  print(betterSafedValue)

  // functions on Options
  println(betterSafedValue.isEmpty) // return Boolean
  println(betterSafedValue.get)  // unsafe can lead to NullPointerException.

  // map flatMap, filter

  println(None.map(n => n));
  println(myFristOption.map(_ * 2)) //Some(8)
  println(myFristOption.filter(x => x >10)) // None
  println(myFristOption.flatMap(x => Option(x *10)));  // Some(40)

  val config : Map[String,String] = Map(
    "host" -> "176.34.55.1" ,
    "port" -> "80"
  )

  class Connection {
    def connect = "Connected"
  }

  object Connection {
    private val random = new Random(System.nanoTime())
    def apply(host : String, port : String ) : Option[Connection] =
      if(random.nextBoolean()) Some(new Connection) else None;
  }

  //One way
  val connection = if(config.contains("host") && config.contains("port")){
    val connect = Connection(config("host"),config("port"));
    if(connect.isEmpty) Some("Unable to connect") else connect.get.connect
  }

  // Other way
  val host = config.get("host");
  val port = config.get("port");
  //Another way here it says if h is not null then if p is not null then do Connection.apply else null
  val connec = host.flatMap(h => port.flatMap(p => Connection.apply(h,p)))
  val connectionStatus = connec.map(c => c.connect); // if c is not null the do c.connect otherwise null

  // More Optimized

  config.get("host")
    .flatMap(host => config.get("port")
      .flatMap(port => Connection(host,port))
      .map(connect => connect.connect))
    .foreach(println);

  println(connection);
  //if connectionStatus is not null then println(None) else print (Some(connectionstatus.get))
  println(connectionStatus)


  // Using for-comprehensions

  val forConnectioStatus = for {
    host <- config.get("host");
    port <- config.get("port");
    connection <- Connection(host,port)
  } yield connection.connect;

  forConnectioStatus.foreach(println);


}
