//user class
package Game
import scala.collection.mutable.Buffer
import scala.collection.mutable.Map
import Deck._

//create a master class for users and bots
abstract class user {
  def name: String
  def isBot: Boolean
  def selectMove(table: table): Unit
  def placeCard(handCard: Card, table: table)
  def takeCards(handCard: Card, tableCards: Buffer[Card], table: table)
  var handCards = Buffer[Card]()
  var pickedCards = Buffer[Card]()
  var total: Int = pickedCards.size
  var spades: Int = 0
  var sweeps: Int = 0
  var points: Int = 0
  
}
  
//player class
case class player(name: String) extends user{
    def isBot = false
    def selectMove(table:table) = None
    
    //take cards from the table
    def takeCards(handCard: Card, tableCards: Buffer[Card], table: table){
      this.handCards -= handCard
      this.pickedCards += handCard
      for(card <- tableCards){
        table.cards -= card
        this.pickedCards += card
      }
      if(table.cards.isEmpty){
        this.sweeps += 1
      }
      table.next
    }
  
    //place the selected card on the table
    def placeCard(handCard: Card, table: table){
      this.handCards -= handCard
      table.cards += handCard
      table.next
    }
  }


//bot class
case class bot(name: String) extends user{
    val isBot = true
    
    //create a map for each handCard as the key and all possible combinations for that card
    def handTableCombinations(table: table) = {
      val tableCardCombinations: List[List[Card]] = table.cards.toSet.subsets.map(_.toList).toList.tail
      var cardCombinations = Map[Card,List[List[Card]]]()
      for(card <- this.handCards){
        val possibleCombinations = tableCardCombinations.filter(list => list.foldLeft(0)(_ + _.rank) == card.rank)
        cardCombinations(card) = possibleCombinations 
      }
      cardCombinations
    }
    
    //find the combination which returns the most spades
    //returns a Map with the best card and the best combination
    def mostSpades(table:table) = {
      var most = Map[Card,List[Card]]()
      var mostCount = 0
      
      for((card,combination) <- this.handTableCombinations(table)){
        for(comb <- combination){
          var count = 0
          if(card.suite == "S"){
            count += 1
          }
          for(singleCard <-comb){
            if(singleCard.suite == "S"){
              count += 1
            }
          }
          if(count > mostCount){
            mostCount = count
            most = most.empty
            most(card) = comb
          }
        }
      }
      most
    }
    
    //find the combination which returns the most points
    //returns a Map with the best card and the best combination
    def mostPoints(table:table) = {
      var most = Map[Card,List[Card]]()
      var mostCount = 0
      
      for((card,combination) <- this.handTableCombinations(table)){
        for(comb <- combination){
          var count = 0
          if(card.rank == 1){
            count += 1
          }
          for(singleCard <- comb){
            if(singleCard.rank == 1){
              count += 1
            } else if(singleCard.special == Some(16)){
              count += 1
            } else if(singleCard.special == Some(15)){
              count += 1
            }
          }
          if(count > mostCount){
            mostCount = count
            most = most.empty
            most(card) = comb
          }
        }
      }
      most
    }
    
    //find combination which returns the most cards
    //returns a Map with the best card and the best combination
    def mostCards(table:table) = {
      var most = Map[Card,List[Card]]()
      var mostCount = 0
      
      for((card,combination) <- this.handTableCombinations(table)){
        for(comb <- combination){
          var count = comb.size
          if(count > mostCount){
            mostCount = count
            most = most.empty
            most(card) = comb
          }
        }
      }
      most
    }
    
    //try to take a sweep
    //returns true if successful, else false
    def sweep(table:table) = {
      if(!this.mostCards(table).isEmpty){
        val mostCards = this.mostCards(table)
        val hand = mostCards.keySet.head
        val tableCards = mostCards(hand)
        if(tableCards.length == table.cards.length){
          this.takeCards(hand, tableCards.toBuffer, table)
          true
        } else {
          false
        }
      } else {
        false
      }
    }
    
    //takes cards from the table
    def takeCards(handCard: Card, tableCards: Buffer[Card], table: table) = {
      this.handCards -= handCard
      this.pickedCards += handCard
      for(card <- tableCards){
        table.cards -= card
        this.pickedCards += card
      }
      if(table.cards.isEmpty){
        this.sweeps += 1
      }
      table.next
    }
    
    //select the next move in order:
    //sweep -> mostPoints -> mostSpades -> mostCards -> placeCard
    def selectMove(table:table) = {
      if(!this.sweep(table)){
        if(!this.mostPoints(table).isEmpty){
          val mostPoints = this.mostPoints(table)
          val card = mostPoints.keySet.head
          val tableCards = mostPoints(card).toBuffer
          this.takeCards(card, tableCards, table)
        } else if(!this.mostSpades(table).isEmpty){
          val mostSpades = this.mostSpades(table)
          val card = mostSpades.keySet.head
          val tableCards = mostSpades(card).toBuffer
          this.takeCards(card, tableCards, table)
        } else if(!this.mostCards(table).isEmpty){
          val mostCards = this.mostCards(table)
          val card = mostCards.keySet.head
          val tableCards = mostCards(card).toBuffer
          this.takeCards(card, tableCards, table)
        } else {
          this.placeCard(this.handCards(0), table)
        }
      }
    }
    
    //place a Card 
    def placeCard(handCard: Card, table: table){
      table.cards += handCard
      this.handCards -= handCard
      table.next
    }
  }
  

