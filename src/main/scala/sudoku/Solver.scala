package sudoku
import scala.collection.mutable.MutableList
object Solver {
  var maxDegree : Int = 0
  var maxDsat : Int = 0
  def solve(grid : MutableList[MutableList[Int]]) : String = {
    val vertices : List[Vertex] = Sudoku.gridToVertices(grid)
    // on calcule le degre et le dsat de chaque sommet
    for(i <- 0 to vertices.size-1){
      val v : Vertex = vertices(i)
      val connected : MutableList[Vertex] = MutableList.empty
      // on cherche tous les sommets lies a ce sommet
      for(j <- 0 to vertices.size-1){
        if(v != vertices(j) && v.isConnectedTo(vertices(j))){
          connected += vertices(j)
        }
      }
      v.connected = connected.toList
      v.degree = connected.size
      maxDegree = math.max(maxDegree, v.degree)
      v.dsat = dsat(connected.toList)
      maxDsat = math.max(maxDsat, v.dsat)
    }
    // Ordonner les sommets par ordre décroissant de degrés.
    val sorted : List[Vertex] = vertices.sortWith(_.degree > _.degree)
    // Choisir un sommet avec DSAT maximum. En cas d'égalité, choisir un sommet de degré maximal.
    var next : Option[Vertex] = findNonColoredMaxDsat(sorted)
    while(!next.isEmpty){
      // Colorer ce sommet avec la plus petite couleur possible
      next.get.value = findLowestAvailableColor(next.get.connected)
      Sudoku.updateGridFromVertices(grid, sorted)
      // Si tous les sommets sont colorés alors stop. Sinon on continue
      next = findNonColoredMaxDsat(sorted)
    }
    return Sudoku.gridToString(grid)
  }
  def dsat(vertices : List[Vertex]) : Int = {
    var unique : MutableList[Int] = MutableList.empty
    for(i <-0 to vertices.size-1){
      val v : Vertex = vertices(i)
      if(v.value!=0 && unique.indexOf(v.value) == -1){
        unique += v.value
      }
    }
    return unique.size
  }
  def findNonColoredMaxDegree(list : List[Vertex]) : Option[Vertex] = {
    return list.find(_.degree == maxDegree)
  }
  def findNonColoredMaxDsat(list : List[Vertex]) : Option[Vertex] = {
    var i : Int = 0
    var nonColoredMaxDsat : Option[Vertex] = None
    var nonColoredMaxDsatList : MutableList[Vertex] = MutableList.empty
    while(i < list.size){
      val v : Vertex = list(i)
      if(v.value == 0 && v.dsat == maxDsat){
        nonColoredMaxDsatList += v
      }
      i += 1
    }
    //plusieurs sommets avec un dsat max
    if(nonColoredMaxDsatList.size > 1){
      return findNonColoredMaxDegree(nonColoredMaxDsatList.toList)
    }
    else if(!nonColoredMaxDsatList.isEmpty){
      nonColoredMaxDsat = Some(nonColoredMaxDsatList(0))
    }
    else{
      //on a traité tous les sommets avec un dsat max
      //on verifie s'il reste des sommets non colorés
      if(list.find(_.value == 0).isEmpty){
        return None
      }
      //s'il en reste on met a jour les dsat
      for(i<-0 to list.size-1){
        list(i).dsat = dsat(list(i).connected)
        maxDsat = math.max(maxDsat, list(i).dsat)
      }
      return findNonColoredMaxDsat(list)
    }
    return nonColoredMaxDsat
  }
  def findLowestAvailableColor(list : List[Vertex]) : Int = {
    var availableValues : MutableList[Int] = MutableList.empty
    for (value <- 1 to 9){
      var available : Boolean = true
      var i : Int = 0
      while (i < list.size && available){
        if(list(i).value == value){
          available = false
        }
        i += 1
      }
      if(available){
        availableValues += value
      }
    }
    return availableValues.min
  }
}
