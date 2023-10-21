# define HMM parameters
S <- c("sunny", "cloudy", "rainy")
O <- c("hot", "warm", "cool")
A <- matrix(c(0.7, 0.2, 0.1, 0.3, 0.5, 0.2, 0.2, 0.4, 0.4), nrow = 3, ncol = 3, byrow = TRUE)
B <- matrix(c(0.4, 0.4, 0.2, 0.2, 0.6, 0.2, 0.1, 0.3, 0.6), nrow = 3, ncol = 3, byrow = TRUE,
            dimnames = list(S, O))
pi <- c(1/3, 1/3, 1/3)

# define forward function
forward <- function(obs, S, O, A, B, pi) {
  # initialize variables
  T <- length(obs)
  alpha <- matrix(0, nrow = length(S), ncol = T)
  # set alpha for t = 1
  alpha[,1] <- pi * B[,obs[1]]
  # compute alpha for t > 1
  for (t in 2:T) {
    for (j in 1:length(S)) {
      # compute alpha[j,t]
      alpha[j,t] <- sum(alpha[,t-1] * A[,j]) * B[j,obs[t]]
    }
  }
  # return alpha
  return(alpha)
}

# call forward function with example observation sequence
obs <- c("hot", "warm", "cool")
alpha <- forward(obs, S, O, A, B, pi)

# save alpha matrix to file
write.table(alpha, file = "alpha_matrix.txt", sep = "\t", quote = FALSE)

# print success message
cat("Alpha matrix saved to file 'alpha_matrix.txt'.\n")