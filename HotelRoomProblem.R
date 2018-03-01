
#In this simulation we sample demand for every sample s and every month t. 
#Using this simulated demand and the given transition function, 
#we calculate the remaining rooms at the beginning of each month 
#as well as the revenue generated for the complete year. 
#We repeat the process S number of times (S = 100,1000,10000) and 
#save the results in data frames.

library(Rmisc)

# utility function for import from csv file
import.csv <- function(filename) {
  return(read.csv(filename, sep = ",", header = TRUE))
}

#this csv of optimal booking was generated through an optimization problem generated for
#different scenarios in GAMS/CPLEX
u <- import.csv('Pb3_optimal_booking_limits.csv')
n.rooms <- 100
n.rooms.beginning.s <- data.frame()

rooms.sold.s <- data.frame()
lambda.demand <- 9
price.t <- c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210)
# S <- 100
# S <- 1000
S <- 10000
revenue.s <- c()

for(s in 1:S)
{
  revenue.t <- 0
  n.rooms.beginning.t <- c()
  rooms.sold.t <- c()
  x.t <- n.rooms
  
  for (t in 1:12)
  {
    # t = 1
    # x.t <-100
    #the demand has poisson distribution 
    demand <- rpois(1,lambda.demand)
    rooms.sold <- min(min(demand,u[x.t+1,t+1]),x.t)
    rooms.sold.t <- c(rooms.sold.t, rooms.sold )
    n.rooms.beginning.t <- c(n.rooms.beginning.t, x.t)
    x.t <- x.t - rooms.sold
    revenue.t <- price.t[t]*rooms.sold + revenue.t
    
  }
  
  revenue.s <- c(revenue.s,revenue.t)
  rooms.sold.s <- rbind(rooms.sold.s, rooms.sold.t)
  n.rooms.beginning.s <- rbind(n.rooms.beginning.s, n.rooms.beginning.t)
}

colnames(n.rooms.beginning.s) = seq(1:12)
colnames(rooms.sold.s) = seq(1:12)

hist(revenue.s)
