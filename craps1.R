# Global Simulations // No odds bets/other bets
dice1 <- sample(1:6,1)
dice2 <- sample(1:6,1)
dice.roll <- dice1 + dice2
passline.win <- c(7,11)
passline.craps <- c(2,3,11)
passline.pass <- c(4,5,6,8,9,10,12)

pass.bet <- 5
stack <- 50 #starting cash

#Simulate Pass Line Round 
initial.roll <- function() {
  if(dice.roll %in% passline.win) {
    stack <- stack + pass.bet
      print(stack)
  } else {
    print('hi')
  }
}


