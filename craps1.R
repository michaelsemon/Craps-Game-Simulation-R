# Global Simulations // No odds bets/other bets
dice1 <- sample(1:6,1)
dice2 <- sample(1:6,1)
dice.roll <- dice1 + dice2
passline.win <- c(7,11)
passline.craps <- c(2,3,11)
passline.pass <- c(4,5,6,8,9,10,12)

pass.bet <- 5 #Pass line bet
stack <- 50 #starting cash

#Shoot for the Point 
point.roll <- function(point) {
  roll <- dice.roll
    if (roll == point) {
        stack <- stack + pass.bet
        print("hit the point")
    } else if (roll %in% point.loss) {
        stack <- stack - pass.bet
    } else {
        point.roll(point)
    }
}


#Simulate Craps 
craps <- function() {
  roll <- dice.roll  #pass roll
  if(roll %in% passline.win) {  
    stack <- stack + pass.bet
  } else if (roll %in% passline.craps) {
      stack <- stack - pass.bet
  } else {
      point <- roll
      point.roll(point)
  }
}



