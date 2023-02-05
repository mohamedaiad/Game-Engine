import scala.io.StdIn.readLine
import scala.util.control.Breaks.{break, breakable}

class chess {

  // turn : true --> player 1 , false --> player 2
  def checkMove(state : Array[Array[Char]] , input : String , turn : Boolean) : Boolean = {

    if(input.size < 4 || input.size > 5) return false

    val x : Int = getIndex(input.charAt(1))
    val y : Int = getIndex(input.charAt(0))
    val xNew : Int = getIndex(input.charAt(3))
    val yNew : Int =getIndex(input.charAt(2))

    // number 8 in x,y indicate wrong move (from function getIndex)
    if(x > 7 || y > 7 || xNew > 7 || yNew > 7 || (x==xNew && y==yNew)) return false

    val flag = state(x)(y) match {
      case 'p' | 'P' => pawn(state,input,turn)
      case 'r' | 'R' => rock(state,input,turn,false)
      case 'q' | 'Q' => queen(state,input,turn)
      case 'k' | 'K' => king(state,input,turn)
      case 'b' | 'B' => bishop(state,input,turn,false)
      case 'n' | 'N' => knight(state,input,turn)
      case _ => false
    }
    flag
  }

  def doMove(state : Array[Array[Char]] , input : String) : Array[Array[Char]] = {

    val x : Int = getIndex(input.charAt(1))
    val y : Int = getIndex(input.charAt(0))
    val xNew : Int = getIndex(input.charAt(3))
    val yNew : Int =getIndex(input.charAt(2))

    state(xNew)(yNew) = state(x)(y)
    if((x%2 == 0 && y%2==0) || (x%2==1 && y%2==1)) state(x)(y)='.'
    else state(x)(y) = '-'

    state
  }

  def pawn(state : Array[Array[Char]] , input : String , turn : Boolean) : Boolean ={

    val x : Int = getIndex(input.charAt(1))
    val y : Int = getIndex(input.charAt(0))
    val xNew : Int = getIndex(input.charAt(3))
    val yNew : Int =getIndex(input.charAt(2))
    var promotion : Char = ' '
    if(input.size == 5){
      if(xNew == 0 || xNew == 7) promotion = input.charAt(4)
      else return false
    }

    // check that my king is covered and no piece check on it
    val temp1 = state(x)(y)   // the current place
    val temp2 = state(xNew)(yNew)   // the new place
    state(x)(y) = '.'
    state(xNew)(yNew) = temp1
    val index = getKing(state,turn)
    val flag = isChecked(state,index(0),index(1),turn)
    state(x)(y) = temp1
    state(xNew)(yNew) = temp2
    if(flag) return false

    if(turn){
      if(state(x)(y) != 'p') return false

      // eat
      if(xNew == x -1 && (y== yNew+1 || y==yNew-1) && isPiece2(state(xNew)(yNew))) {
        // check for promotion
        if(xNew == 0){
          state(x)(y) = promotion match {
            case 'q' => 'q'
            case 'r' => 'r'
            case 'b' => 'b'
            case 'n' => 'n'
            case _ => return false
          }
//          if(promotion == 'q') state(x)(y) = 'q'
//          if(promotion == "r") state(x)(y) = 'r'
//          if(promotion == "b") state(x)(y) = 'b'
//          if(promotion == "n") state(x)(y) = 'n'

//          println("There is a promotion.")
//          println("Enter a letter from the following to promote : q(queen) - r(rook) - b(bishop) - n(knight).")
//          val input = readLine()
//          if(input.size > 1) return false
//          if(input == "q") state(x)(y) = 'q'
//          if(input == "r") state(x)(y) = 'r'
//          if(input == "b") state(x)(y) = 'b'
//          if(input == "n") state(x)(y) = 'n'
        }
        return true
      }

      // move 1 step or 2 steps (if it is the first move to this pawn)
      if(((x == 6 && xNew == x-2) || xNew == x-1 ) && y == yNew && isEmpty(state(x-1)(y)) && isEmpty(state(xNew)(y))) {
        // check for promotion
        if(xNew == 0){
          state(x)(y) = promotion match {
            case 'q' => 'q'
            case 'r' => 'r'
            case 'b' => 'b'
            case 'n' => 'n'
            case _ => return false
          }

//          println("There is a promotion.")
//          println("Enter a letter from the following to promote : q(queen) - r(rook) - b(bishop) - n(knight).")
//          val input = readLine()
//          if(input.size > 1) return false
//          if(input == "q") state(x)(y) = 'q'
//          if(input == "r") state(x)(y) = 'r'
//          if(input == "b") state(x)(y) = 'b'
//          if(input == "n") state(x)(y) = 'n'
        }
        return true
      }

      // En passant

    }
    else{
      if(state(x)(y) != 'P') return false

      // eat
      if(xNew == x + 1 && (y== yNew+1 || y==yNew-1) && isPiece1(state(xNew)(yNew))) {
        // check for promotion
        if(xNew == 7){
          state(x)(y) = promotion match {
            case 'q' => 'Q'
            case 'r' => 'R'
            case 'b' => 'B'
            case 'n' => 'N'
            case _ => return false
          }

//          println("There is a promotion.")
//          println("Enter a letter from the following to promote : q(queen) - r(rook) - b(bishop) - n(knight).")
//          val input = readLine()
//          if(input.size > 1) return false
//          if(input == "q") state(x)(y) = 'Q'
//          if(input == "r") state(x)(y) = 'R'
//          if(input == "b") state(x)(y) = 'B'
//          if(input == "n") state(x)(y) = 'N'
        }
        return true
      }


      // move 1 step or 2 steps (if it is the first move to this pawn)
      if(((x == 1 && xNew == x+2) || xNew == x+1)  && y == yNew && isEmpty(state(x+1)(y)) && isEmpty(state(xNew)(y))) {
        // check for promotion
        if(xNew == 7){
          state(x)(y) = promotion match {
            case 'q' => 'Q'
            case 'r' => 'R'
            case 'b' => 'B'
            case 'n' => 'N'
            case _ => return false
          }

//          println("There is a promotion.")
//          println("Enter a letter from the following to promote : q(queen) - r(rook) - b(bishop) - n(knight).")
//          val input = readLine()
//          if(input.size > 1) return false
//          if(input == "q") state(x)(y) = 'Q'
//          if(input == "r") state(x)(y) = 'R'
//          if(input == "b") state(x)(y) = 'B'
//          if(input == "n") state(x)(y) = 'N'
        }
        return true
      }

    }
    false
  }

  def knight(state : Array[Array[Char]] , input : String , turn : Boolean): Boolean = {

    val x : Int = getIndex(input.charAt(1))
    val y : Int = getIndex(input.charAt(0))
    val xNew : Int = getIndex(input.charAt(3))
    val yNew : Int =getIndex(input.charAt(2))

    if((turn && state(x)(y)!= 'n') || (!turn && state(x)(y)!= 'N') || input.size == 5) return false

    // flag1 checks for the move itself
    var flag1 = false
    if( ( xNew == x+1 || xNew==x-1  ) && ( yNew == y+2 || yNew == y-2 )  ) flag1 = true
    if( ( xNew == x+2 || xNew==x-2  ) && ( yNew == y+1 || yNew == y-1 )  ) flag1 = true

    // flag2 checks for the new place is empty or there is piece in this place
    var flag2 = false
    if(turn && (isEmpty(state(xNew)(yNew)) || isPiece2(state(xNew)(yNew)))) flag2 = true
    else if(!turn && (isEmpty(state(xNew)(yNew)) || isPiece1(state(xNew)(yNew)))) flag2 = true

    // do the move and check if the king is checked or not
    val temp1 = state(x)(y)   // the current place
    val temp2 = state(xNew)(yNew)
    state(x)(y) = '.'
    state(xNew)(yNew) = temp1
    val index = getKing(state,turn)
    // flag3 checks that my king is covered and no piece check on it
    val flag3 = isChecked(state,index(0),index(1),turn)

    state(x)(y) = temp1
    state(xNew)(yNew) = temp2
    // 3 flags must be true
    flag1 && flag2 && !flag3
  }

  def king(state : Array[Array[Char]] , input : String , turn : Boolean): Boolean ={
    val x : Int = getIndex(input.charAt(1))
    val y : Int = getIndex(input.charAt(0))
    val xNew : Int = getIndex(input.charAt(3))
    val yNew : Int =getIndex(input.charAt(2))

    if((turn && state(x)(y)!= 'k') || (!turn && state(x)(y)!= 'K') || input.size == 5) return false

    // flag1 checks for the move itself
    var flag1 = false
    if(y==yNew && (xNew == x-1 || xNew == x+1)) flag1 = true
    else if(x == xNew && (yNew == y-1 || yNew == y+1)) flag1 = true
    else if((xNew == x+1 && yNew == y+1) || (xNew == x+1 && yNew == y-1) || (xNew == x-1 && yNew == y+1) || (xNew == x-1 && yNew == y-1)) flag1 = true

    // flag2 checks for the new place is empty or there is piece in this place
    var flag2 = false
    if(turn && (isEmpty(state(xNew)(yNew)) || isPiece2(state(xNew)(yNew)))) flag2 = true
    else if(!turn && (isEmpty(state(xNew)(yNew)) || isPiece1(state(xNew)(yNew)))) flag2 = true

    // flag3 checks for the new position is not checked
    val flag3 = isChecked(state,xNew,yNew,turn)

    // 3 flags must be true
    flag1 && flag2 && !flag3
  }

  def queen(state : Array[Array[Char]] , input : String , turn : Boolean): Boolean = {
    val x : Int = getIndex(input.charAt(1))
    val y : Int = getIndex(input.charAt(0))

    if((turn && state(x)(y)!= 'q') || (!turn && state(x)(y)!= 'Q')) return false

    // queen is mix between rock and bishop
    bishop(state,input,turn,true) || rock(state,input,turn,true)
  }

  def bishop(state : Array[Array[Char]] , input : String , turn : Boolean, isQueen : Boolean): Boolean = {

    val x : Int = getIndex(input.charAt(1))
    val y : Int = getIndex(input.charAt(0))
    val xNew : Int = getIndex(input.charAt(3))
    val yNew : Int =getIndex(input.charAt(2))

    if(input.size == 5) return false
    if( !isQueen && ((turn && state(x)(y)!= 'b') || (!turn && state(x)(y)!= 'B')) ) return false
    if(Math.abs(x-xNew) != Math.abs(y-yNew)) return false

    // flag1 checks for the move itself
    var flag1 = false
    if(yNew > y){
      var j = y+1
      if(xNew > x){
        for(i <- x+1 until xNew){
          if (!isEmpty(state(i)(j))) return false
          j+=1
        }
        flag1 = true
      }
      else {
        var i = x-1
        while (i > xNew){
          if (!isEmpty(state(i)(j))) return false
          j+=1
          i-=1
        }
        flag1 = true
      }
    }
    else if (yNew < y){
      var j = y-1
      if(xNew > x){
        for(i <- x+1 until xNew){
          if (!isEmpty(state(i)(j))) return false
          j-=1
        }
        flag1 = true
      }
      else {
        var i = x-1
        while (i > xNew){
          if (!isEmpty(state(i)(j))) return false
          j-=1
          i-=1
        }
        flag1 = true
      }
    }


    // flag2 checks for the new place is empty or there is piece in this place
    var flag2 = false
    if(turn && (isEmpty(state(xNew)(yNew)) || isPiece2(state(xNew)(yNew)))) flag2 = true
    else if(!turn && (isEmpty(state(xNew)(yNew)) || isPiece1(state(xNew)(yNew)))) flag2 = true

    // do the move and check if the king is checked or not
    val temp1 = state(x)(y)   // the current place
    val temp2 = state(xNew)(yNew)
    state(x)(y) = '.'
    state(xNew)(yNew) = temp1
    val index = getKing(state,turn)
    // flag3 checks that my king is covered and no piece check on it
    val flag3 = isChecked(state,index(0),index(1),turn)

    state(x)(y) = temp1
    state(xNew)(yNew) = temp2
    // 3 flags must be true
    flag1 && flag2 && !flag3
  }

  def rock(state : Array[Array[Char]] , input : String , turn : Boolean, isQueen : Boolean): Boolean = {

    val x : Int = getIndex(input.charAt(1))
    val y : Int = getIndex(input.charAt(0))
    val xNew : Int = getIndex(input.charAt(3))
    val yNew : Int =getIndex(input.charAt(2))

    if(input.size == 5) return false
    if( !isQueen && ((turn && state(x)(y)!= 'r') || (!turn && state(x)(y)!= 'R')) ) return false

    // flag1 checks for the move itself
    var flag1 = false
    if(y==yNew){
      if(xNew > x){
        for(i <- x+1 until xNew){
          if(!isEmpty(state(i)(y))) return false
        }
        flag1 = true
      }
      else {
        for(i <- xNew+1 until x){
          if(!isEmpty(state(i)(y))) return false
        }
        flag1 = true
      }
    }
    else if (x == xNew){
      if(yNew > y){
        for(i <- y+1 until yNew){
          if(!isEmpty(state(x)(i))) return false
        }
        flag1 = true
      }
      else {
        for(i <- yNew+1 until y){
          if(!isEmpty(state(x)(i))) return false
        }
        flag1 = true
      }
    }

    // flag2 checks for the new place is empty or there is piece in this place
    var flag2 = false
    if(turn && (isEmpty(state(xNew)(yNew)) || isPiece2(state(xNew)(yNew)))) flag2 = true
    else if(!turn && (isEmpty(state(xNew)(yNew)) || isPiece1(state(xNew)(yNew)))) flag2 = true

    // do the move and check if the king is checked or not
    val temp1 = state(x)(y)   // the current place
    val temp2 = state(xNew)(yNew)
    state(x)(y) = '.'
    state(xNew)(yNew) = temp1
    val index = getKing(state,turn)
    // flag3 checks that my king is covered and no piece check on it
    val flag3 = isChecked(state,index(0),index(1),turn)

    state(x)(y) = temp1
    state(xNew)(yNew) = temp2
    // 3 flags must be true
    flag1 && flag2 && !flag3
  }

  // x,y are the position of the king
  def isChecked(state : Array[Array[Char]] ,x : Int, y : Int , turn : Boolean) : Boolean = {
    if(turn){

      // if the pawn make check
      if(x-1 >= 0 && (state(x-1)(y-1) == 'P' || state(x-1)(y+1) == 'P')) return true

      // if the knight make check
      try{
        if (state(x + 1)(y + 2) == 'N' || state(x + 1)(y - 2) == 'N' || state(x - 1)(y + 2) == 'N' || state(x - 1)(y - 2) == 'N') return true
        if (state(x + 2)(y + 1) == 'N' || state(x + 2)(y - 1) == 'N' || state(x - 2)(y + 1) == 'N' || state(x - 2)(y - 1) == 'N') return true
      }
      catch { case _ => }

      // if the rock or the queen make check
      breakable {
        for (i <- x+1 to 7){
          if(state(i)(y) == 'R' || state(i)(y) == 'Q') return true
          if(isPiece1(state(i)(y)) || isPiece2(state(i)(y)) || state(i)(y) == 'K') break
        }
      }
      breakable {
        var i = x-1
        while (i >= 0){
          if(state(i)(y) == 'R' || state(i)(y) == 'Q') return true
          if(isPiece1(state(i)(y)) || isPiece2(state(i)(y)) || state(i)(y) == 'K') break
          i-=1
        }
      }
      breakable {
        for (i <- y+1 to 7){
          if(state(x)(i) == 'R' || state(x)(i) == 'Q') return true
          if(isPiece1(state(x)(i)) || isPiece2(state(x)(i)) || state(x)(i) == 'K') break
        }
      }
      breakable {
        var i = y-1
        while (i >= 0){
          if(state(x)(i) == 'R' || state(x)(i) == 'Q') return true
          if(isPiece1(state(x)(i)) || isPiece2(state(x)(i)) || state(x)(i) == 'K') break
          i-=1
        }
      }

      // if the bishop or the queen make check
      breakable {
        var j = y+1
        var i = x+1
        while (i <= 7 && j <= 7){
          if(state(i)(j) == 'B' || state(i)(j) == 'Q') return true
          if(isPiece1(state(i)(j)) || isPiece2(state(i)(j)) || state(i)(j) == 'K') break
          j+=1
          i+=1
        }
      }
      breakable {
        var i = x-1
        var j = y+1
        while (i >= 0 && j <= 7){
          if(state(i)(j) == 'B' || state(i)(j) == 'Q') return true
          if(isPiece1(state(i)(j)) || isPiece2(state(i)(j)) || state(i)(j) == 'K') break
          j+=1
          i-=1
        }
      }
      breakable {
        var i = x+1
        var j = y-1
        while (i <= 7 && j >= 0){
          if(state(i)(j) == 'B' || state(i)(j) == 'Q') return true
          if(isPiece1(state(i)(j)) || isPiece2(state(i)(j)) || state(i)(j) == 'K') break
          j-=1
          i+=1
        }
      }
      breakable {
        var i = x-1
        var j = y-1
        while (i >= 0 && j >= 0){
          if(state(i)(j) == 'B' || state(i)(j) == 'Q') return true
          if(isPiece1(state(i)(j)) || isPiece2(state(i)(j)) || state(i)(j) == 'K') break
          j-=1
          i-=1
        }
      }

      // if the another king is close
      try{
        if(state(x)(y+1) == 'K' || state(x)(y-1) == 'K' || state(x+1)(y) == 'K' || state(x-1)(y) == 'K') return true
        if(state(x+1)(y+1) == 'K' || state(x+1)(y-1) == 'K' || state(x-1)(y+1) == 'K' || state(x-1)(y-1) == 'K') return true
      }
      catch {case _ => }
    }
    else {

      // if the pawn make check
      if(x+1 <= 7 && (state(x+1)(y-1) == 'p' || state(x+1)(y+1) == 'p')) return true

      // if the knight make check
      try{
        if (state(x + 1)(y + 2) == 'n' || state(x + 1)(y - 2) == 'n' || state(x - 1)(y + 2) == 'n' || state(x - 1)(y - 2) == 'n') return true
        if (state(x + 2)(y + 1) == 'n' || state(x + 2)(y - 1) == 'n' || state(x - 2)(y + 1) == 'n' || state(x - 2)(y - 1) == 'n') return true
      }
      catch { case _ => }

      // if the rock or the queen make check
      breakable {
        for (i <- x+1 to 7){
          if(state(i)(y) == 'r' || state(i)(y) == 'q') return true
          if(isPiece1(state(i)(y)) || isPiece2(state(i)(y)) || state(i)(y) == 'k') break
        }
      }
      breakable {
        var i = x-1
        while(i >= 0){
          if(state(i)(y) == 'r' || state(i)(y) == 'q') return true
          if(isPiece1(state(i)(y)) || isPiece2(state(i)(y)) || state(i)(y) == 'k') break
          i-=1
        }
      }
      breakable {
        for (i <- y+1 to 7){
          if(state(x)(i) == 'r' || state(x)(i) == 'q') return true
          if(isPiece1(state(x)(i)) || isPiece2(state(x)(i)) || state(x)(i) == 'k') break
        }
      }
      breakable {
        var i = y-1
        while (i >= 0){
          if(state(x)(i) == 'r' || state(x)(i) == 'q') return true
          if(isPiece1(state(x)(i)) || isPiece2(state(x)(i)) || state(x)(i) == 'k') break
          i-=1
        }
      }

      // if the bishop or the queen make check
      breakable {
        var i = x+1
        var j = y+1
        while (i <= 7 && j <= 7){
          if(state(i)(j) == 'b' || state(i)(j) == 'q') return true
          if(isPiece1(state(i)(j)) || isPiece2(state(i)(j)) || state(i)(j) == 'k') break
          j+=1
          i+=1
        }
      }
      breakable {
        var i = x-1
        var j = y+1
        while (i >= 0 && j <= 7){
          if(state(i)(j) == 'b' || state(i)(j) == 'q') return true
          if(isPiece1(state(i)(j)) || isPiece2(state(i)(j)) || state(i)(j) == 'k') break
          j+=1
          i-=1
        }
      }
      breakable {
        var i = x+1
        var j = y-1
        while (i <= 7 && j >= 0){
          if(state(i)(j) == 'b' || state(i)(j) == 'q') return true
          if(isPiece1(state(i)(j)) || isPiece2(state(i)(j)) || state(i)(j) == 'k') break
          j-=1
          i+=1
        }
      }
      breakable {
        var i = x-1
        var j = y-1
        while (i >= 0 && j >= 0){
          if(state(i)(j) == 'b' || state(i)(j) == 'q') return true
          if(isPiece1(state(i)(j)) || isPiece2(state(i)(j)) || state(i)(j) == 'k') break
          j-=1
          i-=1
        }
      }

      // if the another king is close
      try{
        if(state(x)(y+1) == 'k' || state(x)(y-1) == 'k' || state(x+1)(y) == 'k' || state(x-1)(y) == 'k') return true
        if(state(x+1)(y+1) == 'k' || state(x+1)(y-1) == 'k' || state(x-1)(y+1) == 'k' || state(x-1)(y-1) == 'k') return true
      }
      catch {case _ => }
    }
    false
  }

  def getIndex(c : Char) : Int = {
    val x = c match {
      case 'a' | '8' => 0
      case 'b' | '7' => 1
      case 'c' | '6' => 2
      case 'd' | '5' => 3
      case 'e' | '4' => 4
      case 'f' | '3' => 5
      case 'g' | '2' => 6
      case 'h' | '1' => 7
      case _ => 8
    }
    x
  }

  def getKing(state : Array[Array[Char]] ,turn : Boolean) : Array[Int] ={

    val index = Array.ofDim[Int](2)

    if(turn){
      for (i <- 0 to 7){
        for(j <- 0 to 7){
          if(state(i)(j) == 'k') {
            index(0) = i
            index(1) = j
            return index
          }
        }
      }
    }
    else {
      for (i <- 0 to 7){
        for(j <- 0 to 7){
          if(state(i)(j) == 'K') {
            index(0) = i
            index(1) = j
            return index
          }
        }
      }
    }
    index
  }

  def isPiece1(piece : Char): Boolean ={
    val flag = piece match {
      case 'p' | 'r' | 'n' | 'b' | 'q' => true
      case  _ => false
    }
    flag
  }

  def isPiece2(piece : Char): Boolean ={
    val flag = piece match {
      case 'P' | 'R' | 'N' | 'B' | 'Q' => true
      case  _ => false
    }
    flag
  }

  def isEmpty(place : Char): Boolean ={
    val flag = place match {
      case '.' | '-' => true
      case _ => false
    }
    flag
  }


}
