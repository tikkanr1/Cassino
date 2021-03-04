//table class
package Game
import scala.collection.mutable.Buffer
import scala.io.StdIn._
import Deck._

//table class consists of players, cards and a deck
class table {
  var players = Buffer[player]()
  var bots = Buffer[bot]()
  var users = this.players ++ this.bots
  var cards = Buffer[Card]()
  var deck = new Deck
  var userIndex = 0
  
  //helper function to zero all information
  def zero: Unit = {
    this.players = Buffer[player]()
    this.bots = Buffer[bot]()
    this.cards = Buffer[Card]()
    this.deck = new Deck
    this.userIndex = 0
    this.users = this.players ++ this.bots
  }
  
  //helper function to check if the given card is special
  def isSpecial(card: Card): Boolean = {
  card.special != None
  }
  
  
  //Checks whether the attempted take is valid
  //returns true if valid
  def checkTake(handCard: Card, tableCards: Buffer[Card]): Boolean = {
    var selectedCards = tableCards.clone                                        //copy the selected cards for validation
    var handValue = handCard.rank                                               //check if user has the selected card in his handCards
    if(isSpecial(handCard)){
      handValue = handCard.special.get                                          //change value if special card
    }
    var combinations = selectedCards.toSet.subsets.map(_.toList).toList.tail    //create all unique combinations of the selected cards
    var i = 0
    while(i < combinations.length){                                             //if the sum of a combination
      val sumOfCards = combinations(i).foldLeft(0)(_ + _.rank)                  //equals to the sum of the handCard
      if(sumOfCards == handValue){                                              //the cards in the combination are
        combinations(i).foreach(x => selectedCards -= x)                        //removed from the selectedCards
	      combinations = selectedCards.toSet.subsets.map(_.toList).toList.tail    //and the list of combinations is updated
	      i = 0
      } else {
	      i += 1		
	    }
    }
    if(selectedCards.length == 0){                                              //if no cards are left in the 
      true                                                                      //selectedCards the move is valid
    } else {
      false                                                                     //and the user can proceed to take the cards
    }
  }
  
  
  //count the points for each individual player/user
  def countPoints: Unit = {
    for(user <- users){
      for(card <- user.pickedCards){
        if(isSpecial(card)){
          if(card.rank == 1 || card.rank == 2){
            user.points += 1
          } else {
            user.points += 2
          }
        }
        if(card.suite == "S"){
          user.spades += 1
        }
      }
      user.points += user.sweeps
    }
    users.maxBy(_.spades).points += 1                                         //the player with the most spades and most  
    users.maxBy(_.total).points += 1                                          //cards gets an extra point                  
  }
  
  
 //check if end of round
  def endOfRound: Boolean = {
    var ret = true
    for(user <- this.users){
      if(user.handCards.length > 0){
        ret = false
      }
    }
    if(ret){
      this.countPoints
    }
    ret
  }
  
  //check if game has ended
  def endOfGame: Boolean = {
    var ret = false
    if(endOfRound){
      for(user <- this.users){
        if(user.points >= 16){
          ret = true
        }
      }
    }
    ret 
  }

  
  //starts a new game by 
  //shuffling the deck and dealing initial cards
  def start: Unit = {
    this.cards = Buffer[Card]()
    this.deck = new Deck
    this.deck.shuffle()
    this.users = this.players ++ this.bots
    for(user <- this.users){
      while(user.handCards.length != 4){
        this.deck.deal(user)
      }
      user.spades = 0
      user.sweeps = 0
      user.pickedCards = Buffer[Card]()
      user.total = 0
    }
    while(this.cards.length != 4){
      this.deck.deal(this)
    }
  }
  
  
  //nextUserIndex
  def nextUserIndex = {
    if(userIndex == this.users.length-1){
      0
    } else {
      userIndex + 1
    }
  }
  
  //previousUserIndex
  def previousUserIndex = {
    if(userIndex == 0){
      this.users.length -1
    } else {
      userIndex - 1
    }
  }
  
  //otherUserindex 
  def otherUserIndex = {
    userIndex match {
      case 0 => 2
      case 1 => 3
      case 2 => 0
      case 3 => 1
    }
  }
  
  //returns the user who is on turn
  def current = {
    this.users(userIndex)
  }
  
  
  //shifts the turn to the next user
  //if the next user is a bot
  //the bot makes a move
  def next: Unit = {
    if(!this.deck.isEmpty){
         deck.deal(this.users(userIndex))
       }
    userIndex = nextUserIndex 
    if(this.current.handCards.length == 0){
      if(!endOfRound){
        this.next
      } 
    } else {
      if(this.current.isBot){
        this.current.selectMove(this)
      } 
    }
  }  
}

