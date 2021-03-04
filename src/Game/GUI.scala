package Game
import Game._
import scala.swing._
import swing.event._
import GridBagPanel._
import scala.swing.BorderPanel.Position._
import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage
import javax.swing.ImageIcon
import scala.collection.mutable.Buffer
import java.awt.Color
import com.sun.org.apache.xalan.internal.xsltc.compiler.Output
import Game.Deck.Card
import java.util.Calendar
import Game.Deck.Deck


object GUI extends SimpleSwingApplication {

  //Creating an example placeholder game 
  var gameTable = new table
  gameTable.players = scala.collection.mutable.Buffer(new player("example player 1"), new player("example player 2"))
  gameTable.bots = scala.collection.mutable.Buffer(new bot("example bot 1"), new bot("example bot 2"))
  gameTable.start

  //This is the gridBagPanel that holds
  //all the buttons for the cards
  var bagui = new GridBagPanel {
    background = Color.GREEN
    val constraints = new Constraints


    //takeCards button
    //for taking cards
    val takeCards = new Button("Take cards"){
      name = "takeCards"
      constraints.gridx = 0;
      constraints.gridy = 7;
    }
    layout(takeCards) = constraints
    listenTo(takeCards)

    
    //placeCard button
    //for placing a card
    val placeCard = new Button("Place card"){
      name = "placeCard"
      constraints.gridx = 0;
      constraints.gridy = 6;
    }
    layout(placeCard) = constraints
    listenTo(placeCard)



    //create CheckBoxes for tableCards    
    var listOfTableCardButtons = Buffer[CheckBox]()

    val TableCards = {
      val pos = Buffer((1,1),(2,1),(3,1),(4,1),(1,2),(2,2),(3,2),(4,2),(1,3),(2,3),(3,3),(4,3))
      for(i <- 0 until pos.length){
        new CheckBox{
          visible = true
          background = Color.YELLOW
          borderPainted = true
          name = "table" + (i+1)
          constraints.gridx = pos(i)._1
          constraints.gridy = pos(i)._2
          layout(this)= constraints
          listOfTableCardButtons += this
        }
      }
      listOfTableCardButtons.foreach(button => listenTo(button))
    }


    //unselect cards after a move
    def unselectTableCards = {
      listOfTableCardButtons.foreach(card => card.selected = false)
    }
    
    
    //update the Buttons for tablecards
    def updateTableCardButtons = {
      listOfTableCardButtons.foreach(_.icon = new ImageIcon("./pictures/green_back_blank.resized.png"))
      listOfTableCardButtons.foreach(card => card.enabled = false)
      for(i <- 0 until gameTable.cards.length){
        listOfTableCardButtons(i).icon = gameTable.cards(i).pic.get
        listOfTableCardButtons(i).enabled = true
      }
    }
    

    //list of selected tableCards
    def updateSelectedTableCards = {
      val selectedTableCardIndexes = listOfTableCardButtons.filter(checkBox => checkBox.selected).map(card => card.name.takeRight(1).toInt -1)
      val selected = selectedTableCardIndexes.map(i => gameTable.cards(i)).toBuffer
      selected
    }


    //current user name
    val currentName = new Label{
      text = current.name
      font = new Font("Arial",0,40)
      background = Color.WHITE
      opaque = true
      constraints.gridx = 5
      constraints.gridy = 6
    }
    layout(currentName) = constraints
    
    
    //These are the cards for the user that is on turn
    //They will be displayed on the bottom of the window
    //with the face up
    def current = gameTable.current
    def currentCards = current.handCards
    var listOfCurrentCardButtons = Buffer[RadioButton]()    
    val pos = Buffer((1,5), (2,5), (3,5), (4,5))
    val currentCardButtons = new ButtonGroup{
      for(i <- 0 until currentCards.length){
        buttons += new RadioButton{
          visible = true
          background = Color.YELLOW
          borderPainted = true
          name = "card" + (i+1)
          preferredSize = (new Dimension(104,158))
          constraints.gridx = pos(i)._1
          constraints.gridy = pos(i)._2
          layout(this) = constraints
          listOfCurrentCardButtons += this
          }
        }
        buttons.foreach(button => listenTo(button))
      }
    
    
    
    //updates buttons and displayed name for current user
    def updateCurrentCardButtons = {
      currentName.text = current.name
      listOfCurrentCardButtons.foreach(_.visible = false)
      for(i <- 0 until currentCards.length){
        listOfCurrentCardButtons(i).visible = true
        listOfCurrentCardButtons(i).icon = currentCards(i).pic.get
      }
    }
    
    
    //Labels for other players cards
    //which are displayed on the sides
    val otherPos = Buffer((0,1),(0,2),(0,3),(0,4),(5,1),(5,2),(5,3),(5,4),(1,0),(2,0),(3,0),(4,0))
    val listOfLabels = Buffer[Label]()
    val otherCards = {
      for(pos <- 0 until otherPos.length){
        new Label{
          name = "label" + (pos+1)
          visible = true
          listOfLabels += this
          if(pos < 8){
            listOfLabels(pos).icon = new ImageIcon("./pictures/blue_back.rotated.png")
          } else {
            listOfLabels(pos).icon = new ImageIcon("./pictures/blue_back.png")
          }
          constraints.weightx = 0.5
          constraints.gridx = otherPos(pos)._1
          constraints.gridy = otherPos(pos)._2
          layout(this) = constraints
        }
      }
    }
    
    
    //update otherCardPictures
    def updateOtherCards = {
      val nextCards = gameTable.users(gameTable.nextUserIndex).handCards
      val previousCards = gameTable.users(gameTable.previousUserIndex).handCards
      val otherCards = gameTable.users(gameTable.otherUserIndex).handCards
      listOfLabels.foreach(_.enabled = false)
      for(i <- 0 until nextCards.length){
        listOfLabels(i).icon = new ImageIcon("./pictures/blue_back.rotated.png")
        listOfLabels(i).enabled = true
      }
      for(i <- 0 until previousCards.length){
        listOfLabels(i+4).icon = new ImageIcon("./pictures/blue_back.rotated.png")
        listOfLabels(i+4).enabled = true
      }
      for(i <- 0 until otherCards.length){
        listOfLabels(i+8).icon = new ImageIcon("./pictures/blue_back.png")
        listOfLabels(i+8).enabled = true
      }  
    }
    
    
    //helper function for updating GUI
    def updateGUI = {
      updateTableCardButtons
      updateCurrentCardButtons
      updateOtherCards
    }
    
    updateGUI



    //textbox for error messages
    val textArea = new TextPane {
      text = "Deck size: " + gameTable.deck.cards.length.toString
      font = new Font("Arial", 0, 20)
      background = Color.white
      constraints.gridx = 5
      constraints.gridy = 5
    }
    layout(textArea) = constraints
    
    
    //display points after game has ended
    def END = {
      if(gameTable.endOfGame){
        currentName.visible = false
        val userOrder = gameTable.users.sortBy(_.points).reverse
        textArea.text = "Winner: " + userOrder.head.name + " with " + userOrder.head.points + " points!\nSecond place: " +
        userOrder(1).name + " with " + userOrder(1).points + " points!"
      } else if(gameTable.endOfRound){
        textArea.text = "New round!"
        gameTable.start
        updateGUI
      }      
    }


    //helper variables for matching
    var handIndex = 4
    val tableMatch = "(table.*)".r
    val handMatch = "(card.*)".r
    
    //button reactions
    reactions += {
      case ButtonClicked(b) => b.name match {
        case tableMatch(_) => updateSelectedTableCards
        case handMatch(_) => handIndex = b.name.takeRight(1).toInt - 1
        case "takeCards" => handIndex match {
          case 4 => textArea.text = "No Card selected!"
          case _ => {
            if(updateSelectedTableCards.length != 0){
              if(gameTable.checkTake(currentCards(handIndex), updateSelectedTableCards)){
                gameTable.current.takeCards(currentCards(handIndex),updateSelectedTableCards,gameTable)
                updateGUI
                textArea.text = "Deck size: " + gameTable.deck.cards.length.toString
                END
              } else {
                textArea.text = "Invalid move!"
              }
              handIndex = 4
              unselectTableCards
            } else {
              textArea.text = "No tablecards selected!"
            }
          }
        }
        case "placeCard" => handIndex match {
          case 4 => {
            unselectTableCards
            textArea.text = "No Card selected!"
          }
          case _ => {
            unselectTableCards
            gameTable.current.placeCard(currentCards(handIndex), gameTable)
            updateGUI
            handIndex = 4
            textArea.text = "Deck size: " + gameTable.deck.cards.length.toString
            END
          }
        }
        case _ => textArea.text = "Unknown command"
      }
    }
  }


  //mainframe of the GUI
  //with MenuBar buttons
    def top = new MainFrame {
    title = "Cassino"
    menuBar = new MenuBar {
      contents += new Menu("File") {
        
        //Load saved game
        contents += new MenuItem(Action("Open") {
          val chooser = new FileChooser
          if(chooser.showOpenDialog(null)==FileChooser.Result.Approve){
            Game.load.loadGame(chooser.selectedFile.toString(), gameTable)
            bagui.updateGUI
          }
        })
        
        //Save current game
        contents += new MenuItem(Action("Save") {
          val chooser = new FileChooser
          if(chooser.showSaveDialog(null)==FileChooser.Result.Approve){
            Game.save.saveGame(chooser.selectedFile.toString(), gameTable)
          } 
        })
        contents += new Separator
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
      
      //Menu for creating a new game
      //with sliders for players and bots
      contents += new Menu("Game") {
        contents += new Label("choose number of players")
        val playerNum = new Label{
          text = 4.toString
        }
        contents += playerNum
        val players = new Slider{
          name = "players"
          min = 1
          max = 4
        }
        listenTo(players)
        contents += players
        contents += new Label("choose number of bots")
        val botNum = new Label{
          text = 4.toString
        }
        contents += botNum
        val bots = new Slider{
          name = "bots"
          min = 0
          max = 3
        }
        listenTo(bots)
        contents += bots
        contents += new Label("Total number may not exceed 4")
        val newGame = new Button("New Game"){
         name = "New Game"
       }
        contents += newGame

        reactions += {
          case ValueChanged(s) => s.name match {
            case "players" => {
              playerNum.text = players.value.toString()
              bots.max = 4 - players.value
              bots.value = 4 - players.value
            }
            case "bots" => botNum.text = bots.value.toString()
          }
          case ButtonClicked(b) => b.name match {
            case "New Game" => {
              gameTable = new table
              for(i <- 1 to players.value){
                gameTable.players += new player("Player" + i)
              }
              for(i <- 0 to bots.value){
                gameTable.bots += new bot("Bot" + i)
              }
              gameTable.start
              bagui.updateGUI
            }
          }
        }
        listenTo(newGame)
      }
    }

    contents = bagui
    background = Color.GREEN
    size = new Dimension(1500,1100)
  }
}
