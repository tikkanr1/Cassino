package Game
import scala.io.Source
import scala.io.BufferedSource
import java.io.File
import Game.Deck.Card
import javax.swing.ImageIcon
import scala.collection.mutable.Buffer

object load {
  def loadGame(file: String, table: table) = {
    table.zero
    val lines = Source.fromFile(file).getLines()
    var userIndex = 0
    

    //read information from next line
    def readUserInfo(next: String) = {
        next match {
          case a if next.startsWith("name") => {
            val mainInfo = next.split(":")
            val name = mainInfo(1)
            val isBot = mainInfo(2).toBoolean
            isBot match {
              case false => table.users += new player(name)
              case true => table.users += new bot(name)
            } 
            var nextLine = lines.next()
            table.users(userIndex).points = nextLine.split(":")(1).toInt
            if(nextLine.split(":").length > 1){
              readCards(nextLine)
            }
            nextLine = lines.next()
            if(nextLine.split(":").length > 1){
              readCards(nextLine)
            }
            nextLine = lines.next
            table.users(userIndex).sweeps = nextLine.split(":")(1).toInt
            userIndex += 1
          }
          case b if next.startsWith("tableCards") => {
            readCards(next)
          }
          case c if next.startsWith("deckCards") => {
            readCards(next)
          }
        }
      }
    
    //read card info from the next line
    //create a card Class accordingly
    //and add the card to the correct user/table/deck
    def readCards(next: String) = {
      val nextLine = next.split(":")
      val pos = nextLine.head
      val cards = nextLine.tail(0).split(",")
      for(card <- cards){
        val special = {
          card match {
            case a if card.tail.toInt == 1 => Some(14)
            case "D10" => Some(16)
            case "S2" => Some(15)
            case _ => None
          }
        }
        val suite = card.head.toString()
        val rank = card.tail.toInt
        val pic = Some(new ImageIcon("./pictures/" + rank + suite + ".png"))
        pos match {
          case "handCards" => table.users(userIndex).handCards += new Card(rank, suite, pic, special)
          case "pickedCards" => table.users(userIndex).pickedCards += new Card(rank, suite, pic, special)
          case "tableCards" => table.cards += new Card(rank, suite, pic, special)
          case "deckCards" => table.deck.cards += new Card(rank, suite, pic, special)
        }
      }
    }
    
    
    //continue reading until the end of file
    def read = {
      var next = lines.next()
      while(!next.startsWith("END")){
        readUserInfo(next)
        next = lines.next()
      }
    }
    
    read
  }
}