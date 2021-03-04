//deck class
package Game
import scala.util.Random
import scala.collection.mutable.Buffer
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import java.io.File
import java.awt.image.BufferedImage

object Deck {

    //define lists of suites and ranks
    val suites = List("S","H","C","D")
    val ranks = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
    
    class Card(val rank: Int, val suite: String ,var pic: Option[ImageIcon], val special: Option[Int])

    //create a new deck with values for special cards
    class Deck() {
        var cards = Buffer[Card]()
        def isEmpty: Boolean = {
          this.cards.size == 0
        }
        for(rank <- ranks; suite <- suites){
          if(rank == 1){
            cards += new Card(rank, suite, None, Some(14))
          } else if(rank == 2 && suite == "S"){
            cards += new Card(rank, suite, None, Some(15))
          } else if(rank == 10 && suite == "D"){
            cards += new Card(rank, suite, None, Some(16))
          } else {
            cards += new Card(rank, suite, None, None)
          }
        }
        
        //read in the images for each card
        for(card <- cards){
         card.pic = Some(new ImageIcon("./pictures/" + card.rank + card.suite.toString().head + ".png"))
        }
        
        //shuffle the deck
        def shuffle() = {
          this.cards = Random.shuffle(this.cards)//shuffle cards
        }
        
        //deal cards to a user
        def deal(user: Game.user) = {
          user.handCards += cards.head
          cards = cards.drop(1)
        }
        //deal cards to the table
        def deal(table: table) = {
          table.cards += cards.head
          cards = cards.drop(1)
        }
        
    }

}
