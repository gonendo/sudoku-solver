package sudoku

class Vertex (val x : Int, val y : Int, var value : Int){
  var connected : List[Vertex] = List.empty //sommets lies
  var degree : Int = 0 //nombre de sommets lies
  var dsat : Int = 0 //nombre de chiffres differents dans les sommets lies
  def isConnectedTo(v : Vertex) : Boolean = {
    return (x == v.x) || (y == v.y) || ((math.ceil(x/3f)==math.ceil(v.x/3f)) && (math.ceil(y/3f)==math.ceil(v.y/3f)))
  }
  override def toString: String = {
    return s"[x:${x},y:${y},value:${value},degree:${degree},dsat:${dsat}]"
  }
}
