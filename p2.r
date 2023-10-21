# Define the input sequences and the scoring parameters
seq1 <- "AGCTAGCTAGCTA"
seq2 <- "AGTACGATCGAT"
match_reward <- 1
mismatch_penalty <- -1
gap_penalty <- -2

# Define the global_alignment function
global_alignment <- function(seq1, seq2, match_reward, mismatch_penalty, gap_penalty) {
  # Initialize the score matrix
  nrow <- nchar(seq1) + 1
  ncol <- nchar(seq2) + 1
  score_matrix <- matrix(0, nrow=nrow, ncol=ncol)
  
  # Initialize the first row and column of the score matrix
  for (i in 1:nrow) {
    score_matrix[i,1] <- gap_penalty * i
  }
  for (j in 1:ncol) {
    score_matrix[1,j] <- gap_penalty * j
  }
  
  # Fill in the rest of the score matrix using the Needleman-Wunsch algorithm
  for (i in 2:nrow) {
    for (j in 2:ncol) {
      match_score <- ifelse(substr(seq1, i-1, i) == substr(seq2, j-1, j),
                            match_reward, mismatch_penalty)
      score_matrix[i,j] <- max(score_matrix[i-1,j-1] + match_score,
                               score_matrix[i-1,j] + gap_penalty,
                               score_matrix[i,j-1] + gap_penalty)
    }
  }
  
  # Traceback to find the optimal alignment
  i <- nrow
  j <- ncol
  aligned_seq1 <- ""
  aligned_seq2 <- ""
  score <- score_matrix[i,j]
  while (i > 1 || j > 1) {
    if (i > 1 && j > 1 && score_matrix[i-1,j-1] + ifelse(substr(seq1, i-1, i) == substr(seq2, j-1, j),
                                                         match_reward, mismatch_penalty) == score_matrix[i,j]) {
      aligned_seq1 <- paste0(substr(seq1, i-1, i), aligned_seq1)
      aligned_seq2 <- paste0(substr(seq2, j-1, j), aligned_seq2)
      i <- i - 1
      j <- j - 1
    } else if (i > 1 && score_matrix[i-1,j] + gap_penalty == score_matrix[i,j]) {
      aligned_seq1 <- paste0(substr(seq1, i-1, i), aligned_seq1)
      aligned_seq2 <- paste0("-", aligned_seq2)
      i <- i - 1
    } else if (j > 1 && score_matrix[i,j-1] + gap_penalty == score_matrix[i,j]) {
      aligned_seq1 <- paste0("-", aligned_seq1)
      aligned_seq2 <- paste0(substr(seq2, j-1, j), aligned_seq2)
      j <- j - 1
    }
  }
  
  # Return the aligned sequences and the corresponding score
  return(list(aligned_seq1, aligned_seq2, score))
}

# Perform global alignment of the input sequences and get the result
result <- global_alignment(seq1, seq2, match_reward, mismatch_penalty, gap_penalty)

# Write the aligned sequences and the alignment score to a file
aligned_file <- "aligned_seqs.txt"
write.table(data.frame(seq1=result[[1]], seq2=result[[2]], score=result[[3]]),
            file=aligned_file, sep="\t", quote=FALSE, row.names=FALSE)

# Print the file path
cat("Aligned sequences saved to file: ", aligned_file, "\n")