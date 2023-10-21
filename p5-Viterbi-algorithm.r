# define HMM parameters
S <- c("sunny", "cloudy", "rainy")
O <- c("hot", "warm", "cool")
A <- matrix(c(0.7, 0.2, 0.1, 0.3, 0.5, 0.2, 0.2, 0.4, 0.4), nrow = 3, ncol = 3, byrow = TRUE)
B <- matrix(c(0.4, 0.4, 0.2, 0.2, 0.6, 0.2, 0.1, 0.3, 0.6), nrow = 3, ncol = 3, byrow = TRUE,
            dimnames = list(S, O)) # add row and column names to B matrix
pi <- c(1/3, 1/3, 1/3)

# define viterbi function
viterbi <- function(obs, S, O, A, B, pi) {
  # initialize variables
  T <- length(obs)
  delta <- matrix(0, nrow = length(S), ncol = T)
  psi <- matrix(0, nrow = length(S), ncol = T)
  # set delta and psi for t = 1
  delta[,1] <- pi * B[,obs[1]]
  # compute delta and psi for t > 1
  for (t in 2:T) {
    for (j in 1:length(S)) {
      # compute delta[j,t]
      tmp <- delta[,t-1] * A[,j] * B[j,obs[t]]
      delta[j,t] <- max(tmp)
      # compute psi[j,t]
      psi[j,t] <- which.max(tmp)
    }
  }
  # backtrack to find most likely state sequence
  states <- numeric(T)
  states[T] <- which.max(delta[,T])
  for (t in (T-1):1) {
    states[t] <- psi[states[t+1], t+1]
  }
  # return most likely state sequence
  return(S[states])
}

# call viterbi function with example observation sequence
obs <- c("hot", "warm", "cool", "hot", "warm")
result <- viterbi(obs, S, O, A, B, pi)

# write output to a file
write(paste(result, collapse = ","), file = "output-HMM-Viterbi-algorithm.txt")