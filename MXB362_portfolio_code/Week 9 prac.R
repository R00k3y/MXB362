# define parameters for a harvested fishery model 
r <- 0.25 # intrinsic growth rate 
h <- 0.1 # per-capita harvest rate 
st <- 0.25 # standard deviation of demographic stochasticity 
K <- 1 # carrying capacity, assuming a scaled population and rates 
 # first order dynamics
TMax <- 50 # maximum time 

# initialize the population, perform a single simulation
N <- rep(0.5, TMax)
for (t in 1:(TMax -1)){
  rt <- r *(1 +rnorm(1)*st)
  ht <- h *(1+rnorm(1)*st)
  N[t+1] <- N[t] + rt*N[t] * (1-(N[t]/K)^z) - ht * N[t]
}


# how many times are we going to evaluate demographic stochasity 
NumReps <- 2500

# scenario 1: run the model with first order equations 
z <- 1 
# initialize the population and simulate for 50 years 

N<- matrix(0.5, nrow = NumReps, ncol = TMax)
for (i in 1:NumReps){
  for (t in 1:(TMax -1)){
    rt <- r *(1 +rnorm(1)*st)
    ht <- h *(1+rnorm(1)*st)
    N[i,t+1] <- N[i,t] + rt * N[i,t] * (1-(N[i,t]/K)^z) - ht * N[i,t]
  }
}

Q <- apply(N,2,quantile,probs = c(0.025,0.975))
plot.new()
plot(NA,
     ylim = c(0,1.25),
     xlim = c(1,t),
     xlab = expression("Time"),
     ylab = expression("Abundance"),
     title(main = "Figure 1", font.main = 1, cex.main = 1.5, line = -1),
     polygon(x =c(1:TMax, TMax:1), y=c(Q[1,], rev(Q[2,])), col = rgb(1,0,0,0.25), border = NA))

# Structural/Parametric uncertainty: z = 1 and z = 2
z_vals <- c(1, 2)

# Scenario uncertainty: h = 0.1 and h = 0.2
h_vals <- c(0.1, 0.2)

# Initialize storage for results
results_structural <- list()
results_scenario <- list()

# Run simulation for structural/parametric uncertainty
for (z in z_vals) {
  N <- matrix(0.5, nrow = NumReps, ncol = TMax)
  for (i in 1:NumReps) {
    for (t in 1:(TMax - 1)) {
      rt <- r * (1 + rnorm(1) * st)
      ht <- 0.1 * (1 + rnorm(1) * st) # Keep harvest rate fixed at 0.1
      N[i, t + 1] <- N[i, t] + rt * N[i, t] * (1 - (N[i, t] / K)^z) - ht * N[i, t]
    }
  }
  results_structural[[as.character(z)]] <- apply(N, 2, quantile, probs = c(0.025, 0.975))
}

# Run simulation for scenario uncertainty with z = 1
for (h in h_vals) {
  N <- matrix(0.5, nrow = NumReps, ncol = TMax)
  for (i in 1:NumReps) {
    for (t in 1:(TMax - 1)) {
      rt <- r * (1 + rnorm(1) * st)
      ht <- h * (1 + rnorm(1) * st)
      N[i, t + 1] <- N[i, t] + rt * N[i, t] * (1 - (N[i, t] / K)^1) - ht * N[i, t] # Assume z = 1 for scenario uncertainty
    }
  }
  results_scenario[[as.character(h)]] <- apply(N, 2, quantile, probs = c(0.025, 0.975))
}

# Plot all results on a single plot
plot(NA,
     ylim = c(0,1.25),
     xlim = c(1,t),
     xlab = expression("Time"),
     ylab = expression("Abundance"),
     title(main = "Figure 1", font.main = 1, cex.main = 1.5, line = -1))

# Plot structural uncertainty 
polygon(x = c(1:TMax, TMax:1), y = c(results_structural[["1"]][1, ], rev(results_structural[["1"]][2, ])), col = rgb(1, 0, 0, 0.25), border = NA)
polygon(x = c(1:TMax, TMax:1), y = c(results_structural[["2"]][1, ], rev(results_structural[["2"]][2, ])), col = rgb(0, 0, 1, 0.25), border = NA)

# Plot scenario uncertainty 
polygon(x = c(1:TMax, TMax:1), y = c(results_scenario[["0.2"]][1, ], rev(results_scenario[["0.2"]][2, ])), col = rgb(0, 1, 0, 0.25), border = NA)

# Add legend
legend("topright", legend = c("Demographic uncertainty", "Structural/Parametric uncertainty", "Scenario uncertainty"), fill = c(rgb(1, 0, 0, 0.25), rgb(0, 1, 0, 0.25), rgb(0.5, 0, 0.5, 0.25)), border = NA)

