class checkersState extends State{
  @Override def getState() = {

    /**
     * initial checkers board state
     */
    val state = Array.ofDim[Char](8, 8)
    var i = 0
    while (i < 3) {
      var j = 1
      if (i == 1) j = 0
      while (j < 8) {
        state(i)(j) = 'b' // black
        j += 2
      }
      i += 1
    }

    i = 7
    while (i > 4) {
      var j = 0
      if (i == 6) j = 1
      while (j < 8) {
        state(i)(j) = 'w' // white
        j += 2
      }
      i -= 1
    }
    i=3
    while(i<5){
      var j=0
      while (j<8){
        state(i)(j)='.'
        j=j+1
      }
      i=i+1
    }
    state
  }

}
