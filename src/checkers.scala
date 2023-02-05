class checkers {
  // turn : true --> player 1 (black), false --> player 2(white)
  def checkMove(state : Array[Array[Char]] , input : String, turn : Boolean) : Boolean = {

    if(input.size != 4) return false

    val j : Int = getIndex(input.charAt(0))
    val i : Int = getIndex(input.charAt(1))

    val s : Int = getIndex(input.charAt(2))
    val f : Int = getIndex(input.charAt(3))

    // number 8 in i,j,s,f indicate wrong move (from function getIndex)
    if(i == 8 || j==8 || f==8 || s==8 ||state(i)(j) == '.' ) false

    // case of the player does not move his pieces
    else if(turn && (state(i)(j)!='b' && state(i)(j)!='t')) false
    else if(!turn && (state(i)(j)!='w' && state(i)(j)!='f')) false

    // incorrect destination for man piece
    else if(state(i)(j)=='b' &&(f!=i+2 || (s!=j+2 && s!=j-2)) && (f!=i+1 ||(s!=j+1 && s!=j-1))) false
    else if(state(i)(j)=='w' &&(f!=i-2 || (s!=j+2 && s!=j-2)) && (f!=i-1 ||(s!=j+1 && s!=j-1 ))) false

    //case of the player want to put his piece on another one
    else if(state(f)(s)!='.') false

    // incorrect destination for king piece
    else if((state(i)(j)=='t'|| state(i)(j)=='f') && ((f!=i-1 && f!=i+1) ||(s!=j+1 && s!=j-1)) && ((f!=i-2 && f!=i+2)||(s!=j+2 && s!=j-2)))false

    else if(state(i)(j)=='b'|| state(i)(j)=='t'){

      if((f==i-2 && s==j+2)&& i-1!= -1 && j+1!=8){
        if(state(i-1)(j+1)!='w'&& state(i-1)(j+1)!='f'){return false}
      true}

      else if((f==i-2 && s==j-2)&& i-1!= -1 && j-1!= -1){
        if(state(i-1)(j-1)!='w'&& state(i-1)(j-1)!='f'){return false}
      true}

      else if((f==i+2 && s==j+2)&& i+1!=8 && j+1!= 8){
        if(state(i+1)(j+1)!='w'&& state(i+1)(j+1)!='f'){return false}
      true}

      else if((f==i+2 && s==j-2)&& i+1!=8 && j-1!= -1){
        if(state(i+1)(j-1)!='w'&& state(i+1)(j-1)!='f'){return false}
      true}
      else true}

    else if(state(i)(j)=='w'|| state(i)(j)=='f') {
      if((f==i-2 && s==j+2)&& i-1!= -1 && j+1!=8){
        if(state(i-1)(j+1)!='b'&& state(i-1)(j+1)!='t'){return false}
        true}

      else if((f==i-2 && s==j-2)&& i-1!= -1 && j-1!= -1){
        if(state(i-1)(j-1)!='b'&& state(i-1)(j-1)!='t'){return false}
      true}

      else if((f==i+2 && s==j+2)&& i+1!=8 && j+1!= 8){
        if(state(i+1)(j+1)!='b'&& state(i+1)(j+1)!='t'){return false}
      true}

      else if((f==i+2 && s==j-2)&& i+1!=8 && j-1!= -1){
        if(state(i+1)(j-1)!='b'&& state(i+1)(j-1)!='t'){return false}
      true}
      else true
    }
    else true
  }

  def doMove(state : Array[Array[Char]] , input : String , turn : Boolean) : Array[Array[Char]] = {
    val j : Int = getIndex(input.charAt(0))
    val i : Int = getIndex(input.charAt(1))

    val s : Int = getIndex(input.charAt(2))
    val f : Int = getIndex(input.charAt(3))

/** black **/
    if(turn) {
      /** man piece **/
      if(state(i)(j)=='b'){
            //empty square
         if((f==i+1)&&(s==j-1 || s== j+1)){state(i)(j)='.'; state(f)(s)='b'}
            //kill white piece that is not protected
         else if(s>j && (state(i+1)(j+1)=='w'||state(i+1)(j+1)=='f')){state(i)(j)='.';state(f)(s)='b';state(i+1)(j+1)='.'}
         else if(s<j && (state(i+1)(j-1)=='w'||state(i+1)(j-1)=='f')){state(i)(j)='.';state(f)(s)='b';state(i+1)(j-1)='.'}
        //if the piece reach the end it will be king
         if(f==7){state(f)(s)='t'}
      }
       /** king piece **/
      else if(state(i)(j)=='t'){
        if(((f==i+1) || (f==i-1)) && ((s==j+1) ||(s==j-1))){state(f)(s)='t';state(i)(j)='.'}

        else if((f<i && s>j)&& i-1!= -1 && j+1!=8){
           if(state(i-1)(j+1)=='w'||state(i-1)(j+1)=='f'){state(i)(j)='.';state(f)(s)='t';state(i-1)(j+1)='.'}}

         else if((f<i && s<j)&& i-1!= -1 && j-1!= -1){
           if(state(i-1)(j-1)=='w'||state(i-1)(j-1)=='f'){state(i)(j)='.';state(f)(s)='t';state(i-1)(j-1)='.'}}

         else if((f>i && s>j)&& i+1!=8 && j+1!= 8){
           if(state(i+1)(j+1)=='w'||state(i+1)(j+1)=='f'){state(i)(j)='.';state(f)(s)='t';state(i+1)(j+1)='.'}}

        else if((f>i && s<j)&& i+1!=8 && j-1!= -1){
           if(state(i+1)(j-1)=='w'||state(i+1)(j-1)=='f'){state(i)(j)='.';state(f)(s)='t';state(i+1)(j-1)='.'}}
         }
      }

/** white **/
    else{
      /** man piece **/
      if(state(i)(j)=='w'){
        //empty square
        if((f==i-1)&&(s==j-1 || s== j+1)){state(i)(j)='.'; state(f)(s)='w'}
        //kill black piece that is not protected
        else if(s>j && (state(i-1)(j+1)=='b'||state(i-1)(j+1)=='t')) {state(i)(j)='.';state(f)(s)='w';state(i-1)(j+1)='.'}
        else if(s<j && (state(i-1)(j-1)=='b'||state(i-1)(j-1)=='t')) {state(i)(j)='.';state(f)(s)='w';state(i-1)(j-1)='.'}
        //if the piece reach the end it will be king
        if(f==0){state(f)(s)='f'}
      }
      /** king piece **/
      else if(state(i)(j)=='f') {
        if((f==i+1 || f==i-1) && (s==j+1 ||s==j-1)){state(f)(s)='f';state(i)(j)='.'}

        else if((f>i && s<j)&& i+1!= 8 && j-1!= -1){
          if(state(i+1)(j-1)=='b'||state(i+1)(j-1)=='t'){state(i)(j)='.';state(f)(s)='f';state(i+1)(j-1)='.'}}

        else if((f>i && s>j)&& i+1!=8 && j+1!=8){
          if(state(i+1)(j+1)=='b'||state(i+1)(j+1)=='t'){state(i)(j)='.';state(f)(s)='f';state(i+1)(j+1)='.'}}

        else if((f<i && s<j)&& i-1!= -1 && j-1!= -1){
          if(state(i-1)(j-1)=='b'||state(i-1)(j-1)=='t'){state(i)(j)='.';state(f)(s)='f';state(i-1)(j-1)='.'}}

        else if((f<i && s>j)&& i-1!= -1 && j+1!=8){
          if(state(i-1)(j+1)=='b'||state(i-1)(j+1)=='t'){state(i)(j)='.';state(f)(s)='f';state(i-1)(j+1)='.'}}
        }
      }
    state
  }

  def getIndex(c: Char): Int = {
    val index = c match {
      case 'a' |'8' => 0
      case 'b' |'7' => 1
      case 'c' |'6' => 2
      case 'd' |'5' => 3
      case 'e' |'4' => 4
      case 'f' |'3' => 5
      case 'g' |'2' => 6
      case 'h' |'1' => 7
      case _ => 8
    }
    index
  }
}
