package Game
import java.io._

object save {
  def saveGame(filename: String, table: table) = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    var info = ""
    for(user <- table.users){
      info += "name:" + user.name + ":" + user.isBot + "\n"
      info += "points:" + user.points + "\n"
      info += "handCards:"
      for(card <- user.handCards){
        info += card.suite + card.rank + ","
      }
      info += "\npickedCards:"
      for(card <- user.pickedCards){
        info += card.suite + card.rank + ","
      }
      info += "\nsweeps:" + user.sweeps + "\n"
    }
    info += "tableCards:"
    for(card <- table.cards){
      info += card.suite + card.rank + ","
    }
    info += "\ndeckCards:"
    for(card <- table.deck.cards){
      info += card.suite + card.rank + ","
    }
    info += "\nEND\n"
    bw.write(info)
    bw.close()
  }
}