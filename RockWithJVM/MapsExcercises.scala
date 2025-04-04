package Excercises.RockWithJVM

import scala.annotation.tailrec

object MapsExcercises extends App{

  def add(network : Map[String,Set[String]] , person :String): Map[String,Set[String]] = {
    network + (person -> Set[String]());
  }

  def friend(network : Map[String,Set[String]] , a :String , b : String) = {
    val friendA = network(a);
    val friendB = network(b);
    network + (a -> (friendA + b)) + (b -> (friendB + a));
  }

  def unfriend(network : Map[String,Set[String]] , a :String , b : String) = {
    val friendA = network(a);
    val friendB = network(b);
    network + (a -> (friendA - b)) + (b -> (friendB - a));
  }

  def remove (network : Map[String,Set[String]] , person :String): Map[String,Set[String]] = {
    @tailrec
    def removeAux(friends: Set[String], networkAcc :Map[String,Set[String]], person : String ): Map[String,Set[String]] = {
      if (friends.isEmpty) networkAcc else removeAux(friends.tail,unfriend(networkAcc,person,friends.head), person)
    }
    val unfriended = removeAux(network(person),network,person);
    unfriended;
  }


  val empty : Map[String,Set[String]] = Map();

  val network = add(add(empty,"Bob"),"Mary");
  println(network);
  println(friend(network,"Bob","Mary"));

  val people   = add(add(add(empty,"Bob"),"Jim"),"Mary");

  val jimBob = friend(people,"Bob","Jim");
  println("PeopleJim " + jimBob);
  val testNet = friend(jimBob,"Bob","Mary");
  println("People " + testNet);

  def nFriends (network: Map[String,Set[String]],person : String): Int  = {
    if(!network.contains(person)) 0 else network(person).size
  }

  def mostFriends(network: Map[String,Set[String]],persons : Set[String] , person : String): String = {
    if(!network.contains(person)) mostFriends(network,persons.tail,persons.head) else {
      if(persons.isEmpty) person else if(network(persons.head).size > network(person).size) mostFriends(network,persons.tail,persons.head) else mostFriends(network , persons.tail, person )
    }
  }

  def mostFriends2(network : Map[String,Set[String]]):String = {
    network.maxBy(pair => pair._2.size)._1
  }

  def nPeopleWithNoFriends (network : Map[String,Set[String]]) : Int = {
    network.count(pair => pair._2.isEmpty); // 1
    network.filterKeys(k=>network(k).isEmpty).size; //2 Deprecated
  }

  def socialConnection(network : Map[String,Set[String]] , a : String, b : String ):Boolean = {
    def Bfs(target : String , searchIn : Set[String], isConnect : Boolean): Boolean = {
      if(searchIn.isEmpty) isConnect else Bfs(target,searchIn.tail, searchIn.head == target);
    }
    Bfs(b,network(b),false)
    network(a).contains(b);
  }

  println(testNet);
  println(mostFriends(testNet,testNet.keySet,""));
  println(mostFriends2(testNet));
  println(nPeopleWithNoFriends(testNet));
  println(socialConnection(testNet,"Mary","Jim"));
}
