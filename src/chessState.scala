class chessState extends State{
    @Override def getState() = {

        /**
         * initial chess board state
         */
        val state = Array.ofDim[Char](8,8)
        state(0)(0) = 'R'
        state(0)(1) = 'N'
        state(0)(2) = 'B'
        state(0)(3) = 'Q'
        state(0)(4) = 'K'
        state(0)(5) = 'B'
        state(0)(6) = 'N'
        state(0)(7) = 'R'
        state(7)(0) = 'r'
        state(7)(1) = 'n'
        state(7)(2) = 'b'
        state(7)(3) = 'q'
        state(7)(4) = 'k'
        state(7)(5) = 'b'
        state(7)(6) = 'n'
        state(7)(7) = 'r'
        var place = '.'
        var i=2
        while (i<6){
          var j = 0
          while (j < 8){
            state(i)(j) = place
            j+=1
            place = place match {
              case '.' => '-'
              case '-' => '.'
            }
          }
          i+=1
          place = place match {
            case '.' => '-'
            case '-' => '.'
          }
        }

        i=0
        while(i<8){
          state(1)(i) = 'P'
          i+=1
        }
        i=0
        while(i<8){
          state(6)(i) = 'p'
          i+=1
        }
      state
    }
}
