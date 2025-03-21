package Excercises.Operators

class PathComponent(f : String) {
  var fileName  : String = f;
}

object PathComponent {
  def apply(f: String): PathComponent = {
    new PathComponent(f);
  }
  def unapply(p : PathComponent): Option[(String, String)] = if(p.fileName.isEmpty) None else { var s = p.fileName.split("/"); Some((s(0)+"/" + s(1)+"/"),s(s.length -1))};
}

object Ques9 extends  App {
  val pC = PathComponent("src/myFile/file.txt");

  val PathComponent(uri,fileName) = pC;

  print(uri + " " + fileName);
}
