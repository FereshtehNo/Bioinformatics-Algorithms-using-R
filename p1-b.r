dfs_adj_matrix <- function(adj_matrix, output_file) {
  # Initialize the visited and parent arrays
  n_nodes <- nrow(adj_matrix)
  visited <- rep(FALSE, n_nodes)
  parent <- rep(NA, n_nodes)
  
  # Define the DFS function
  dfs <- function(vertex) {
    visited[vertex] <<- TRUE
    for (neighbor in which(adj_matrix[vertex,] != 0)) {
      if (!visited[neighbor]) {
        parent[neighbor] <<- vertex
        dfs(neighbor)
      }
    }
  }
  
  # Perform DFS on each unvisited vertex in the graph
  for (vertex in 1:n_nodes) {
    if (!visited[vertex]) {
      dfs(vertex)
    }
  }
  
  # Construct the resulting tree as an adjacency matrix
  tree_matrix <- matrix(0, nrow=n_nodes, ncol=n_nodes)
  for (i in 1:n_nodes) {
    if (!is.na(parent[i])) {
      tree_matrix[i, parent[i]] <- 1
      tree_matrix[parent[i], i] <- 1
    }
  }
  
  # Write the resulting tree to a CSV file
  write.csv(tree_matrix, output_file, row.names=TRUE)
  
  return(tree_matrix)
}

# Read the adjacency matrix from the CSV file with a new "id" column
adj_matrix <- read.csv("output-p1-a.csv", header=TRUE)
adj_matrix$id <- paste0("node_", seq_len(nrow(adj_matrix)))
row.names(adj_matrix) <- adj_matrix$id
adj_matrix <- adj_matrix[,-1]

# Perform DFS on the adjacency matrix and save the resulting tree
tree_matrix <- dfs_adj_matrix(adj_matrix, "output-p1-b.csv")

# Add a prefix to each row name to make them unique
#row.names(tree_matrix) <- paste0("node_", row.names(tree_matrix))