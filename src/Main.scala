import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{BorderFactory, ImageIcon, JButton, JFrame, JLabel, JPanel, JTextField, Timer}
import java.awt.{Color, Font, Graphics, Image}
import javax.imageio.ImageIO
import java.io.File
import scala.io.StdIn.readLine

object Main{

  // frames of the 4 games
  val chessBoardFrame = new JFrame()
  val checkersBoardFrame = new JFrame()
  val XOBoardFrame = new JFrame()
  val cconnect4BoardFrame = new JFrame()

  def actionPerformed (frame: JFrame,x : Int) = {

    frame.setVisible(false)
    if (x == 1) {
      val state = new chessState().getState()
      gameEngine(chessDrawer, chessController, state)
    }
    if (x == 2) {
      val state = new checkersState().getState()
      gameEngine(checkersDrawer, checkersController, state)
    }
    if (x == 3) {
      val state = new XOState().getState()
      gameEngine(XODrawer, XOController, state)
    }
    if (x == 4) {
      val state = new connect4State().getState()
      gameEngine(connect4Drawer, connect4Controller, state)
    }
  }

  def welcome() = {

    val frame = new JFrame()

    /**
     * chess
     */
    // chess icon
    var chess = new ImageIcon("chess.jpeg")
    val modifiedChess = chess.getImage().getScaledInstance(225, 225, java.awt.Image.SCALE_SMOOTH)
    chess = new ImageIcon(modifiedChess)

    // chess button
    val chessButton = new JButton()
    chessButton.setBounds(150, 400, 150, 40)
    chessButton.setText("Chess Game")
    chessButton.setFocusable(false)
    chessButton.addActionListener(_ => actionPerformed(frame,1))
    chessButton.setForeground(Color.white)
    chessButton.setBackground(new Color(0, 102, 153))
    chessButton.setBorder(BorderFactory.createRaisedBevelBorder())

    // chess text
    val chessText = new JLabel()
    chessText.setIcon(chess)
    chessText.setBounds(110, 130, 20, 50)
    chessText.setSize(500, 300)


    /**
     * checkers
     */
    // checkers icon
    var checkers = new ImageIcon("checkers.jpeg")
    val modifiedCheckers = checkers.getImage().getScaledInstance(230, 230, java.awt.Image.SCALE_SMOOTH)
    checkers = new ImageIcon(modifiedCheckers)

    // checkers button
    val checkersButton = new JButton()
    checkersButton.setBounds(590, 400, 150, 40)
    checkersButton.setText("Checkers Game")
    checkersButton.setFocusable(false)
    checkersButton.addActionListener(e => actionPerformed(frame,2))
    checkersButton.setForeground(Color.white)
    checkersButton.setBackground(new Color(0, 102, 153))
    checkersButton.setBorder(BorderFactory.createRaisedBevelBorder())

    // checkers text
    val CheckersText = new JLabel()
    CheckersText.setIcon(checkers)
    CheckersText.setBounds(550, 130, 20, 50)
    CheckersText.setSize(500, 300)


    /**
     * XO
     */
    // xo icon
    var xo = new ImageIcon("XO.jpeg")
    val modifiedXO = xo.getImage().getScaledInstance(230, 210, java.awt.Image.SCALE_SMOOTH)
    xo = new ImageIcon(modifiedXO)

    // xo button
    val xoButton = new JButton()
    xoButton.setBounds(150, 700, 150, 40)
    xoButton.setText("XO Game")
    xoButton.setFocusable(false)
    xoButton.addActionListener(_ => actionPerformed(frame,3))
    xoButton.setForeground(Color.white)
    xoButton.setBackground(new Color(0, 102, 153))
    xoButton.setBorder(BorderFactory.createRaisedBevelBorder())

    // XO text
    val XOText = new JLabel()
    XOText.setIcon(xo)
    XOText.setBounds(110, 440, 20, 50)
    XOText.setSize(500, 300)


    /**
     * connect 4
     */
    // connect4 icon
    var connect4 = new ImageIcon("connect4.jpeg")
    val modifiedConnect4 = connect4.getImage().getScaledInstance(230, 210, java.awt.Image.SCALE_SMOOTH)
    connect4 = new ImageIcon(modifiedConnect4)

    // connect 4 button
    val connect4Button = new JButton()
    connect4Button.setBounds(590, 700, 150, 40)
    connect4Button.setText("Connect 4 Game")
    connect4Button.setFocusable(false)
    connect4Button.addActionListener(_ => actionPerformed(frame,4))
    connect4Button.setForeground(Color.white)
    connect4Button.setBackground(new Color(0, 102, 153))
    connect4Button.setBorder(BorderFactory.createRaisedBevelBorder())

    // connect 4 text
    val connect4Text = new JLabel()
    connect4Text.setIcon(connect4)
    connect4Text.setBounds(550, 440, 20, 50)
    connect4Text.setSize(500, 300)


    /**
     * welcome screen
     */
    val welocmeScreen = new JLabel("WELCOME!")
    welocmeScreen.setBounds(330, 0, 20, 50)
    welocmeScreen.setSize(500, 100)
    welocmeScreen.setForeground(Color.BLACK)
    welocmeScreen.setFont(new Font("Times New Roman", Font.BOLD, 50))

    /**
     * instruction
     */
    val instruction = new JLabel("Choose the game you want.")
    instruction.setBounds(295, 50, 20, 50)
    instruction.setSize(500, 100)
    instruction.setForeground(Color.BLACK)
    instruction.setFont(new Font("Times New Roman", Font.TYPE1_FONT, 30))


    /**
     * frame
     */
    frame.getContentPane.setBackground(new Color(0xADD8E6))
    frame.setSize(1000, 800)
    frame.setResizable(false)
    frame.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    frame.setTitle("Game Engine")
    frame.add(welocmeScreen)
    frame.add(instruction)
    frame.add(chessButton)
    frame.add(chessText)
    frame.add(checkersButton)
    frame.add(CheckersText)
    frame.add(xoButton)
    frame.add(XOText)
    frame.add(connect4Button)
    frame.add(connect4Text)
    frame.add(new JLabel("")) // temp
    val image = new ImageIcon("game_engine.jpg")
    frame.setIconImage(image.getImage)
    frame.setVisible(true)
  }

  def chessDrawer(state : Array[Array[Char]]) ={

    chessBoardFrame.setSize(730,760)
    chessBoardFrame.setResizable(false)
    chessBoardFrame.setTitle("Chess game")
    chessBoardFrame.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)

    /**
     * pieces
     */
    val allpieces = ImageIO.read(new File("pieces.png"))
    var imgs : List[Image] = List()

    var y = 0
    while (y < 400){
      var x = 0
      while (x < 1200){
        imgs = imgs:+(allpieces.getSubimage(x, y, 200, 200).getScaledInstance(90, 90, java.awt.Image.SCALE_SMOOTH))
        x+=200
      }
      y+=200
    }

    /**
     * draw the chess board
     */
    val board = new JPanel(){

      @Override
      override def paint(g : Graphics){
        var white = true
        var i=0

        while (i< 8){
          var j=0
          while (j < 8){
            if(white) g.setColor(new Color(235,235,208))
            else g.setColor(new Color(118,150,86))
            g.fillRect(j*90 , i*90 , 90 , 90)
            white = !white
            j+=1
          }
          white = !white
          i+=1
        }
        // place every piece
        i=0
        while (i<8){
          var j=0
          while(j < 8){
            var index = -1
            if(state(i)(j) == 'k' || state(i)(j) == 'K') index=0
            if(state(i)(j) == 'q' || state(i)(j) == 'Q') index=1
            if(state(i)(j) == 'b' || state(i)(j) == 'B') index=2
            if(state(i)(j) == 'n' || state(i)(j) == 'N') index=3
            if(state(i)(j) == 'r' || state(i)(j) == 'R') index=4
            if(state(i)(j) == 'p' || state(i)(j) == 'P') index=5
            if(state(i)(j) >= 'A' && state(i)(j) <= 'Z') index+=6
            if(index >= 0) g.drawImage(imgs(index) , j*90 , i*90 , this)
            j += 1
          }
          i += 1
        }

      }
    }

    chessBoardFrame.add(board)
    chessBoardFrame.setVisible(true)
  }

  def chessController(state : Array[Array[Char]], input : String, turn : Boolean) ={

    val chess = new chess()
    var board = state
    val flag = chess.checkMove(state,input,turn)
    if(flag) {
      board = chess.doMove(state,input)
    }
    else {
      println("ERROR! Invalid Move")
      board = null
    }
    board
  }

  def checkersDrawer(state : Array[Array[Char]]) = {

    /**
     * pieces
     */
    val getwhite = ImageIO.read(new File("white_checker.png"))
    val whitePiece = getwhite.getScaledInstance(90,90,java.awt.Image.SCALE_SMOOTH)

    val getBlack = ImageIO.read(new File("black_checker.png"))
    val blackPiece = getBlack.getScaledInstance(90,90,java.awt.Image.SCALE_SMOOTH)

    val getBlackKing = ImageIO.read(new File("black_checkerP.png"))
    val blackKing = getBlackKing.getScaledInstance(90,90,java.awt.Image.SCALE_SMOOTH)

    val getWhiteKing = ImageIO.read(new File("white_checkerP.png"))
    val whiteKing = getWhiteKing.getScaledInstance(90,90,java.awt.Image.SCALE_SMOOTH)


    checkersBoardFrame.setSize(730,760)
    checkersBoardFrame.setResizable(false)
    checkersBoardFrame.setTitle("Checkers game")
    checkersBoardFrame.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)

    val board = new JPanel(){

      @Override
      override def paint(g : Graphics){
        var white = true
        var i=0

        while (i< 8){
          var j=0
          while (j < 8){
            if(white) g.setColor(Color.white)
            else g.setColor(Color.BLACK)
            g.fillRect(j*90 , i*90 , 90 , 90)
            white = !white
            j+=1
          }
          white = !white
          i+=1
        }

        // place every piece
        i=0
        while(i < 8){
          var j =0
          while(j < 8) {
            if(state(i)(j) == 'b') g.drawImage(blackPiece, j*90 , i*90 , this)
            if(state(i)(j) == 'w') g.drawImage(whitePiece , j*90 , i*90 , this)
            if(state(i)(j) == 't') g.drawImage(blackKing , j*90 , i*90 , this)
            if(state(i)(j) == 'f') g.drawImage(whiteKing , j*90 , i*90 , this)
            j+=1
          }
          i+=1
        }
      }
    }

    checkersBoardFrame.add(board)
    checkersBoardFrame.setVisible(true)

  }

  def checkersController(state : Array[Array[Char]], input : String, turn : Boolean) ={
    val checkers = new checkers()
    var board = state
    val flag = checkers.checkMove(state,input,turn)
    if(flag) {
      board = checkers.doMove(state,input,turn)
    }
    else {
      println("ERROR! Invalid Move")
      board = null
    }
    board
  }

  def XODrawer(state : Array[Array[Char]]) = {

    /**
     * pieces
     */
    val getX = ImageIO.read(new File("xxx.png"))
    val XPiece = getX.getScaledInstance(110,110,java.awt.Image.SCALE_SMOOTH)

    val getO = ImageIO.read(new File("o.png"))
    val OPiece = getO.getScaledInstance(110,110,java.awt.Image.SCALE_SMOOTH)


    XOBoardFrame.setSize(380,405)
    XOBoardFrame.setResizable(false)
    XOBoardFrame.setTitle("XO game")
    XOBoardFrame.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)

    val board = new JPanel(){

      override def paintComponent(g : Graphics){

        g.setColor(Color.BLACK)
        g.fillRect(0,0,360,360)

        g.setColor(new Color(255, 255, 77))
        g.fillRect(120,0,7,360)
        g.fillRect(240,0,7,360)
        g.fillRect(0,120,360,7)
        g.fillRect(0,240,360,7)

        // place every piece
        var i=0
        while(i < 3){
          var j =0
          while(j < 3) {
            var y = j*120
            var x = i*120
            if(j!=0) y+=10
            if(i!=0) x+=10
            if(state(i)(j) == 'x') g.drawImage(XPiece, y , x , this)
            if(state(i)(j) == 'o') g.drawImage(OPiece , y , x , this)
            j+=1
          }
          i+=1
        }
      }
    }

    //    var input = ""
    //    height = 405
    //    val textField = new JTextField()
    //    textField.setPreferredSize(new Dimension(250,40))
    //    textField.setBounds(20,400,230,30)

    //    val enterButton = new JButton("Enter")
    //    enterButton.setBounds(250,400,100,30)
    //    enterButton.addActionListener(new ActionListener() {
    //      override def actionPerformed(e: ActionEvent): Unit = {
    //        input = textField.getText()
    //        println(input)
    //      }
    //    })
    //
    //
    //    XOBoardFrame.add(textField)
    //    XOBoardFrame.add(enterButton)
    XOBoardFrame.add(board)
    XOBoardFrame.setVisible(true)
  }

  def XOController(state : Array[Array[Char]], input : String, turn : Boolean) = {

    val xo = new XO()
    var board = state
    val flag = xo.checkMove(state,input)
    if(flag) {
      board = xo.doMove(state,input,turn)
    }
    else {
      println("ERROR! Invalid Move")
      board = null
    }
    board
  }

  def connect4Drawer(state : Array[Array[Char]]) = {

    /**
     * pieces
     */

    val origPiece = ImageIO.read(new File("PLOT.png"))
    val whitePiece = origPiece.getScaledInstance(90, 90, java.awt.Image.SCALE_SMOOTH)

    val p1Piece = ImageIO.read(new File("red.png"))
    val redPiece = p1Piece.getScaledInstance(90, 90, java.awt.Image.SCALE_SMOOTH)

    val p2Piece = ImageIO.read(new File("yellow.png"))
    val yellowPiece = p2Piece.getScaledInstance(90, 90, java.awt.Image.SCALE_SMOOTH)


    cconnect4BoardFrame.setSize(650, 610)
    cconnect4BoardFrame.setResizable(false)
    cconnect4BoardFrame.setTitle("connect 4 game")
    cconnect4BoardFrame.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    cconnect4BoardFrame.setVisible(true)

    val board = new JPanel(){
      @Override
      override def paint(g: Graphics) {
        for(i<-0 to 5){
          for(j<-0 to 6){
            g.drawImage(whitePiece, j * 90, i * 90, this)
          }
        }
        // place every piece
        var i = 0
        while (i <= 5) {
          var j = 0
          while (j <= 6 ) {
            if (state(i)(j) == 'r') g.drawImage(redPiece, j * 90, i * 90, this)
            if (state(i)(j) == 'y') g.drawImage(yellowPiece, j * 90, i * 90, this)
            j += 1
          }
          i += 1
        }
      }}
    cconnect4BoardFrame.add(board)
    cconnect4BoardFrame.setVisible(true)
  }

  def connect4Controller(state : Array[Array[Char]], input : String, turn : Boolean) = {
    val connect4 = new connect4()
    var board = state
    val flag = connect4.checkMove(state,input)
    if(flag) {
      board = connect4.doMove(state,input,turn)
    }
    else {
      println("ERROR! Invalid Move")
      board = null
    }
    board
  }

  def gameEngine(drawer : (Array[Array[Char]]) => Unit, controller : (Array[Array[Char]] , String , Boolean) => Array[Array[Char]] , initState : Array[Array[Char]]) = {


    var turn = true
    var state = initState

    //game loop
    drawer(state)
    val timer = new Timer(0, new ActionListener() {
      def actionPerformed(evt:ActionEvent): Unit = {
        drawer(state)
        val t = turn match {
          case true => 1
          case false => 2
        }
        print(s"Turn $t: ")
        val input = readLine()
        val checkValid = controller(state,input,turn)
        if(checkValid != null) {
          state = checkValid
          turn = !turn
        }
      }
    })
    timer.start()
  }

  def main(args: Array[String]): Unit = {
    welcome()
  }
}
