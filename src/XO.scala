class XO {

  // turn : true --> player 1 , false --> player 2
  def checkMove(state : Array[Array[Char]] , input : String) : Boolean = {

    if(input.size != 2) return false

    val x : Int = getIndex(input.charAt(1))
    val y : Int = getIndex(input.charAt(0))

    // number 3 in x,y indicate wrong move (from function getIndex)
    if(x == 3 || y==3 || state(x)(y) == 'x' || state(x)(y) == 'o') false
    else true
  }

  def doMove(state : Array[Array[Char]] , input : String , turn : Boolean) : Array[Array[Char]] = {
    val x : Int = getIndex(input.charAt(1))
    val y : Int = getIndex(input.charAt(0))

    if(turn) state(x)(y) = 'x'
    else state(x)(y) = 'o'
    state
  }

  def getIndex(c: Char): Int = {
    val index = c match {
      case 'a' | 'A' | '3' => 0
      case 'b' | 'B' | '2' => 1
      case 'c' | 'C' | '1' => 2
      case _ => 3
    }
    index
  }

}
