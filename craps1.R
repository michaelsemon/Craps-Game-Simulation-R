# Global Simulations // No odds bets/other bets
rm(list = ls())
passline.win <- c(7,11)
passline.craps <- c(2,3,12)
passline.pass <- c(4,5,6,8,9,10,12)

#Simulate Craps 
craps <- function() {
  pass.bet <- 5 #Pass line bet
  stack <- 50 #starting cash
  initial.roll <- sample(1:6,1) + sample(1:6,1)
  
  if(initial.roll %in% passline.win) {  
    stack <- stack + pass.bet
    
  } else if (initial.roll %in% passline.craps) {
      stack <- stack - pass.bet
      
  } else {
    point <- initial.roll 
    roll <- sample(1:6,1) + sample(1:6,1)
      if (roll == 7) {
        stack <- stack - pass.bet
        print("7 out")
      } else if (roll == point) {
        stack <- stack-pass.bet
        print("hit the point initially")
      }
    
      while ( !(roll %in% c(7,point)) ) {
        roll <- sample(1:6,1) + sample(1:6,1)
       
          if (roll == point) {
            stack <- stack + pass.bet
            print("hit the point in while loop")
            break
          
        } else if (roll == 7){
            stack <- stack-pass.bet
            print('7 out in while loop')
            break
      }
    } #end of while loop
  }
  print(stack)
}


craps()
