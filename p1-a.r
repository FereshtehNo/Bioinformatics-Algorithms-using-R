create_and_save_adj_matrix <- function(csv_file, output_file) {
  # Read the CSV file into a data frame
  df <- read.csv(csv_file, header=TRUE, sep="\t")
  
  # Check for missing or non-numeric values
  if (any(is.na(df$node1)) || any(is.na(df$node2)) || any(!is.character(df$node1)) || any(!is.character(df$node2))) {
    stop("Edges data frame contains missing or non-character value")
  }
  
  # Create a vector of all the unique nodes in the graph
  nodes <- unique(c(df$node1, df$node2))
  
  # Create an empty adjacency matrix with the correct dimensions
  matrix <- matrix(0, nrow=length(nodes), ncol=length(nodes))
  
  # Iterate through the edges in the data frame and fill in the adjacency matrix
  for (i in 1:nrow(df)) {
    source <- match(df[i, "node1"], nodes)
    target <- match(df[i, "node2"], nodes)
    # Since the graph is undirected, we need to add edges in both directions
    matrix[source, target] <- 1
    matrix[target, source] <- 1
  }
  
  # Write the adjacency matrix to a CSV file
  write.csv(matrix, output_file, row.names=FALSE)
  
  return(matrix)
}
matrix <- create_and_save_adj_matrix("question1.csv", "output-p1-a.csv")