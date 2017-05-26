# Global Simulations // Passline + Odds Bet
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggthemes)

rm(list = ls())
passline.win <- c(7,11)
passline.craps <- c(2,3,12)
passline.pass <- c(4,5,6,8,9,10,12)
n <- 25 #number of games
outcomes <- c() #results
starting.budget <- 100
pass.odds.bet <- c()

# Odds Bet on Pass Line - function
odds.bet <- function(point) {
  if (point == 4 | point == 10){
    pass.odds.bet <- 5
    return(pass.odds.bet*2*(2/1))
  } else if (point == 5 | point== 9){
    pass.odds.bet <- 6
    return(pass.odds.bet*2*(3/2))
  } else if (point == 6 | point == 8){
    pass.odds.bet <- 5
    return(pass.odds.bet*2*(6/5))
  }
}

#Simulate Craps 
craps <- function() {
  
  #Loop games
  for (i in 1:n) {
    pass.bet <- 5 #Pass line bet
    
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
        result <- -5 - (odds.bet(point))
        #print("7 out")
      }
      
      # Hit the point
      else if (roll == point) {
        result <- 5 + (odds.bet(point))
        #print("hit the point initially")
      }
      
      #Keep rolling if point or 7 was not rolled
      while ( !(roll %in% c(7,point)) ) {
        roll <- sample(1:6,1) + sample(1:6,1)
        
        #Hit the point    
        if (roll == point) {
          result <- 5 + (odds.bet(point))
          #print("hit the point in while loop")
          break
          # 7 Out      
        } else if (roll == 7){
          result <- -5 - (odds.bet(point))
          #print('7 out in while loop')
          break
        }
      } #end of while loop
    }
    outcomes <- append(outcomes,result,after=length(outcomes)) 
    
  }
  games <- c(1:n)  #Number of Games
  outcomes <- data.frame(games,outcomes) %>% mutate(total.winnings=starting.budget+cumsum(outcomes))
  
  
  print(outcomes) #print results
  
  #Plot Results
  pl <- ggplot(outcomes,aes(x=games,y=total.winnings))
  pl2 <- pl + geom_line() + theme_classic()
  print(pl2)
  
  #multiple linear regression
  #fit <- lm(total.winnings ~ games + outcomes, data=outcomes)
  #print(fit)
  
}

#Run Program
craps()

