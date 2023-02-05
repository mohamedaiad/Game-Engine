import scala.util.control.Breaks.{break, breakable}

class connect4 {

  // turn : true --> player 1 , false --> player 2
  def checkMove(state : Array[Array[Char]] , input : String) : Boolean = {

    if(input.size != 1) return false

    val x : Int = getIndex(input.charAt(0))

    // x = 7 indicate wrong move (from function getIndex)
    if(x == 7 || state(0)(x) == 'r' || state(0)(x) == 'y') false
    else true
  }

  def doMove(state : Array[Array[Char]] , input : String , turn : Boolean) : Array[Array[Char]] = {

    val x : Int = getIndex(input.charAt(0))
    var i=5
    breakable {
      while (i >= 0) {
        if (state(i)(x) == 'r' || state(i)(x) == 'y') i -= 1
        else {
          break
        }
      }
    }
    if(turn) state(i)(x) = 'r'
    else state(i)(x) = 'y'
    state
  }


  def getIndex(c: Char): Int = {
    val index = c match {
      case 'a' | 'A' => 0
      case 'b' | 'B' => 1
      case 'c' | 'C' => 2
      case 'd' | 'D' => 3
      case 'e' | 'E' => 4
      case 'f' | 'F' => 5
      case 'g' | 'G' => 6
      case _ => 7
    }
    index
  }

}
