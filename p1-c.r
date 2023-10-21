bfs_adj_matrix <- function(adj_matrix) {
  # Initialize the visited, parent, and level arrays
  n_nodes <- nrow(adj_matrix)
  visited <- rep(FALSE, n_nodes)
  parent <- rep(NA, n_nodes)
  level <- rep(NA, n_nodes)
  
  # Define the BFS function
  bfs <- function(start) {
    # Initialize the queue with the starting node
    queue <- start
    visited[start] <<- TRUE
    level[start] <<- 0
    
    # Perform BFS on the graph
    while (length(queue) > 0) {
      # Get the next node from the queue
      current <- queue[1]
      queue <- queue[-1]
      
      # Traverse the neighbors of the current node
      for (neighbor in which(adj_matrix[current,] != 0)) {
        if (!visited[neighbor]) {
          visited[neighbor] <<- TRUE
          parent[neighbor] <<- current
          level[neighbor] <<- level[current] + 1
          queue <<- c(queue, neighbor)
        }
      }
    }
  }
  
  # Perform BFS on each unvisited vertex in the graph
  for (vertex in 1:n_nodes) {
    if (!visited[vertex]) {
      bfs(vertex)
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
  
  return(tree_matrix)
}
# Read the adjacency matrix from the CSV file with a new "id" column
adj_matrix <- read.csv("output-p1-a.csv", header=TRUE)
adj_matrix$id <- paste0("node_", seq_len(nrow(adj_matrix)))
row.names(adj_matrix) <- adj_matrix$id
adj_matrix <- adj_matrix[,-1]

# Perform BFS on the adjacency matrix and save the resulting tree
tree_matrix <- bfs_adj_matrix(adj_matrix)

# Add a prefix to each row name to make them unique
#row.names(tree_matrix) <- paste0("node_", row.names(tree_matrix))

# Save the resulting tree matrix as a CSV file
write.csv(tree_matrix, "output-p1-c.csv", row.names=TRUE)