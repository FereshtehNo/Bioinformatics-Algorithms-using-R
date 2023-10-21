# Assign the nucleotide sequence to a variable and append an end-of-string character ($)
seq <- "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGCTTCTGAACTGGTTACCTGCCGTGAGTAAATTAAAATTTTATTGACTTAGGTCACTAAATACTTTAACCAATATAGGCATAGCGCACAGACAGATAAAAATTACAGAGTACACAACATCCATGAAACGCATTAGCACCACCATTACCACCACCATCACCATTACCACAGGTAACGGTGCGGGCTGACGCGTACAGGAAACACAGAAAAAAGCCCGCACCTGACAGTGCGGGCTTTTTTTTTCGACCAAAGGTAACGAGGTAACAACCATGCGAGTGTTGAAGTTCGGCGGTACATCAGTGGCAAATGCAGAACGTTTTCTGCGTGTTGCCGATATTCTGGAAAGCAATGCCAGGCAGGGGCAGGTGGCCACCGTCCTCTCTGCCCCCGCCAAAATCACCAACCACCTGGTGGCGATGATTGAAAAAACCATTAGCGGCCAGGATGCTTTACCCAATATCAGCGATGCCGAACGTATTTTTGCCGAACTTTTGACGGGACTCGCCGCCGCCCAGCCGGGGTTCCCGCTGGCGCAATTGAAAACTTTCGTCGATCAGGAATTTGCCCAAATAAAACATGTCCTGCATGGCATTAGTTTGTTGGGGCAGTGCCCGGATAGCATCAACGCTGCGCTGATTTGCCGTGGCGAGAAAATGTCGATCGCCATTATGGCCGGCGTATTAGAAGCGCGCGGTCACAACGT$"
seq <- paste0(seq, "$")

# Generate all possible cyclic rotations and save to a vector
rotations <- sapply(0:(nchar(seq)-1), function(i) paste0(substr(seq, i+1, nchar(seq)), substr(seq, 1, i)))

# Print the rotations and save to file horizontally
#cat("Rotations: ")
#cat(paste(rotations, collapse = " "))
write.table(rotations, "rotations-BIO.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

# Generate the Burrows-Wheeler transform (BWT) and save to file horizontally
sorted_rotations <- sort(rotations)
bwt <- substring(sorted_rotations, nchar(seq))
#cat("\nBWT: ")
cat(paste(bwt, collapse = ""))
write.table(bwt, "bwt.txt", sep = "\t", row.names = FALSE, col.names = FALSE)