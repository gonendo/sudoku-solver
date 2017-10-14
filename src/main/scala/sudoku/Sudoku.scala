package sudoku
import scala.io.Source
import scala.collection.mutable.MutableList
object Sudoku extends App{
  def createGrid() : MutableList[MutableList[Int]] = {
    val it : Iterator[String] = Source.fromResource("sudoku.txt").getLines
    val list : MutableList[MutableList[Int]] = MutableList.empty
    while(it.hasNext){
      val line = it.next
      var list2 : MutableList[Int] = MutableList.empty
      for (i <- 0 to line.size-1) {
        val value = line.charAt(i) match {
          case _ => line.charAt(i).asDigit
        }
        list2 += value
      }
      list += list2
    }
    return list
  }
  def gridToVertices(grid : MutableList[MutableList[Int]]) : List[Vertex] = {
    val vertices : MutableList[Vertex] = MutableList.empty
    for (i <- 0 to grid.size-1){
      val l : MutableList[Int] = grid.get(i).get
      for(j <- 0 to l.size-1){
        vertices += new Vertex(j+1, i+1, l(j))
      }
    }
    return vertices.toList
  }
  def gridToString(grid : MutableList[MutableList[Int]]) : String = {
    var strGrid : String = ""
    val it = grid.iterator
    while(it.hasNext){
      val l : MutableList[Int] = it.next
      for (i <- 0 to l.size - 1){
        strGrid += l(i).toString
      }
    }
    return strGrid
  }
  def updateGridFromVertices(grid : MutableList[MutableList[Int]], vertices : List[Vertex]) = {
    for (i <- 0 to vertices.size-1){
      val v : Vertex = vertices(i)
      grid(v.y-1)(v.x-1) = v.value
    }
  }
}
