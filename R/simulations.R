SIR <- function (beta=0.15, gamma=1/10, init.states=list(S=1000, I=1, R=0), 
                 time.step=0.25, num.steps=400, agg.time.steps=4) {
  # Default parameters: beta=0.15 and gamma=1/10 -> R0 = beta/gamma = 1.5, duration of infection = 10 days
  # Population size of 1000 with a single infected individual
  # Simulation time step=0.25, total time = num.steps*time step = 100 days
  # In the resulting output, aggregate incidence every agg.time.steps steps
  states <- init.states
  N <- sapply(init.states, sum)
  states$inc <- 0
  epi.mat <- matrix(0, nrow=ceiling(num.steps / agg.time.steps), ncol=length(init.states)+1)
  epi.mat[1, ] <- c(unlist(init.states), 0)
  tx <- 2
  while (tx <= num.steps) {
    new.infections <- rbinom(1, states$S, prob=beta*states$I/N*time.step)
    removals <- rbinom(1, states$I, prob=gamma*time.step)
    states$S <- states$S - new.infections
    states$I <- states$I + new.infections - removals
    states$R <- states$R + removals
    states$inc <- states$inc + removals
    if (((tx-1) %% agg.time.steps)==0) {
      epi.mat[(tx %/% agg.time.steps)+1, ] <- unlist(states)
      states$inc <- 0
    }
    tx <- tx + 1
  }
  epi.df <- data.frame(Time=seq(0, by=time.step*agg.time.steps, length.out=nrow(epi.mat)), epi.mat)
  names(epi.df)[-1] <- c("S", "I", "R", "Incidence")
  return (epi.df)
}