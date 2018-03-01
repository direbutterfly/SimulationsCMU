
#The simulation approach is that we sample the number of people infected 
#in each day for a whole month. We change the variables accordingly to 
#find the number of people immune, susceptible, and affected every day 
#(including total number infected on that day).
#we used samplw wstimates and 95% CI to make predictions for probabilities

infected=data.frame()
immune = data.frame()
susceptible = data.frame()
t.infected = data.frame()


for(i in 1:1000)
{
  Ni = 5 #number of people infected at the start of the month
  Nimm = 0 #number of people immune at the start of the month
  N = 10000 #total number of people
  Ns = N - Ni - Nimm #number of people susceptible
  
  #parameters for binomial distribution of # of people infected
  p=0.1 
  lambda=3
  
  immune.v <- Nimm
  susceptible.v <- Ns
  t.infected.v <- Ni
  temp.infected <- Ni
  n.infected = 1
  for (t in 1:31)
  {
    ps=1-exp(-lambda*p*Ni/N)
    binom_sample <- rbinom(1,Ns,ps) #random sampling from binomial distribution
    if(t == 1)
    {
      infect.p <- c(binom_sample)
      Ns <- Ns - infect.p[t]
    }else{
      infect.p <- c(infect.p,binom_sample)
      Ns <- Ns - infect.p[t]
    }
    if(length(infect.p)>5)
    {
      Nimm <- Nimm+infect.p[t-5]
      temp.infected <- temp.infected - infect.p[t-5]
    }
    if(length(infect.p)==5)
    {
      Nimm <- Nimm+5
      temp.infected <- temp.infected - 5
    }
    Ni <- N-Ns-Nimm
    
    immune.v <- cbind(immune.v,Nimm)
    susceptible.v <- cbind(susceptible.v,Ns)
    temp.infected <- temp.infected + binom_sample
    t.infected.v <- cbind(t.infected.v,temp.infected)
  }
  
  infected <- rbind(infected, infect.p)
  immune <- rbind(immune,immune.v)
  susceptible <- rbind(susceptible,susceptible.v)
  t.infected <-rbind(t.infected,t.infected.v) 
  
}

colnames(infected)=seq(1,31,by=1)
colnames(immune)=seq(0,31,by=1)
colnames(susceptible)=seq(0,31,by=1)
colnames(t.infected)=seq(0,31,by=1)
