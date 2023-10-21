# Define your own BWT sequence
bw <- "T$TGAGAAATTAGAACGATGAAGAAGACCTACATAATACATCCATACCTCGGGAGAAAGAGAACCCAACAAACGACACTACCTGTCGTAAGCATTCCACACCATCCTCAAGAGAAAAACGGAGTAACGCTCCACATTTACCTATC$CCCCTCGACTCGGCCCGGATGAAGGCTAAAGCACTGCCGCGAAGCGCTCATGAACCTCCCCCTGATAGATCCCGACATGACCGGGATAGCTCGCTCCGCTAGAAACTGATCCGAGGATGGCAACCGACCGCGCTGACAACAAAAGACGGATAAAAGGAGGCGCTGGGGCCGGCCGGAGGTATAACCTGAGCTTGGGGCACTCCTGGAGACCGTCAAGGCGGAAGGACTCCGAAACTTTCGTTGACTCGTTGCATAGAGAGAGTATAGTGCATCATGATCTTGTTCCTAACATGCCCCCGGTTGTGACCACTAGGGGATCGCGGTCTCTATTTTCCGATTGTGAGAACTATTGCCGTGCCCATAGATAGCATTGAGGATTCTTTTCCGGCCGGCCATCGAAACCACATTGCAGGGACGCCGGCTCTTTCAGATAGCGATACGGATTTACGTAATCGGTGGGTGGTTTTATTAAATATTAATAGATGAGAAGAAATATGGTGGACTTCGTTGCTTCTTACTCCTCTGCACCACATCGATTCTACGGCACGGAGAGGCTCCAAAGGCGGTAATAAATGAAACAATTGTGTCATAAAGTATTGTGTCCTTTCTTATAGACTGCTTATTTC"

# Define function to compute rank and totals vectors
rank_bw <- function(bw){
  bwv <- strsplit(bw, "")[[1]] # split string in vector
  totals <- c() # empty totals vector
  rank <- rep(NA, length(bwv)) # empty rank vector with predefined size
  for(i in seq(bwv)){
    if(!(bwv[i] %in% names(totals))){ # add to item to totals if doesn't exist
      totals[bwv[i]] <- 1 # add make it 1
    } else {
      totals[bwv[i]] <- totals[bwv[i]] + 1 # add 1 to totals for that character
    }
    rank[i] <- totals[bwv[i]] # fill the rank vector with rank
  }
  return(list(totals = totals, rank = rank)) # return a list of with totals and rank
}

# Define function to create First data.frame
get_first <- function(totals){
  totals <- totals[order(names(totals))] # order alpabetically on character
  FirstL <- list() # empty list
  for(char in names(totals)){
    FirstL[[char]] <- data.frame(c = rep(char, totals[char]),
                                 n = 1:totals[char]) # make a data.frame for each character with ascending rank
  }
  return(do.call(rbind, FirstL)) # return concatenated data.frame
}

# Define function to reverse Burrows-Wheeler Transform
reverse_bwt <- function(bw){
  bwv <- strsplit(bw, "")[[1]] # split string in vector
  rank_list <- rank_bw(bw) # call rank_bw
  totals <- rank_list$totals # get totals vector
  rank <- rank_list$rank # get rank vector
  First <- get_first(totals) # get data.frame of First with rank
  i <- 1
  out <- "$" # start building from last character, so with the dollarsign (eof)
  while(bwv[i] != "$"){ # if dollarsign is found again, you're finished
    appchar <- bwv[i] # get character that appends before last found character
    out <- paste0(appchar, out) # append character 
    i <- which(First$c == appchar)[1] + rank[i] - 1 # find character with rank in First based on rank of character in last
  }
  return(out)
}

# Reverse the Burrows-Wheeler Transform
result <- reverse_bwt(bw)

# Display the result in the console
cat("Original string: ", result, "\n")