# Global Simulations // No odds bets/other bets
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
rm(list = ls())
passline.win <- c(7,11)
passline.craps <- c(2,3,12)
passline.pass <- c(4,5,6,8,9,10,12)
n <- 100 #number of games
outcomes <- c() #results
starting.budget <- 100

#Simulate Craps 
craps <- function() {
  
  #Loop games
  for (i in 1:n) {
    pass.bet <- 5 #Pass line bet
    result <- 50 #starting cash
    
    #Dice Roll
    initial.roll <- sample(1:6,1) + sample(1:6,1)
    
    #7 or 11 wins
    if(initial.roll %in% passline.win) {  
      result <- 5
      
    #Craps Out  
    } else if (initial.roll %in% passline.craps) {
      result <- -5
    
    #Roll any point number (4,5,6,8,9,10)    
      } else {
        point <- initial.roll 
        roll <- sample(1:6,1) + sample(1:6,1)
        
          #7 out
          if (roll == 7) {
            result <- -5
            #print("7 out")
          }
          
          # Hit the point
          else if (roll == point) {
            result <- 5
            #print("hit the point initially")
          }
        
          #Keep rolling if point or 7 was not rolled
            while ( !(roll %in% c(7,point)) ) {
              roll <- sample(1:6,1) + sample(1:6,1)
              
          #Hit the point    
              if (roll == point) {
                result <- 5
                #print("hit the point in while loop")
                break
          # 7 Out      
              } else if (roll == 7){
                result <- -5
                #print('7 out in while loop')
                break
              }
            } #end of while loop
      }
    outcomes <- append(outcomes,result,after=length(outcomes)) 
    
  }
  games <- c(1:n)  #Number of Games
  outcomes <- data.frame(games,outcomes) %>% mutate(total.winnings=starting.budget-cumsum(outcomes))
  
  
  print(outcomes) #print results
  
  #Plot Results
  pl <- ggplot(outcomes,aes(x=games,y=total.winnings))
  
  pl2 <- pl + geom_line() 
  print(pl2)
  #
  
}

craps()
