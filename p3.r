# Simulate gene expression data for ten healthy participants (rows 1 to 10) and ten cancer patients (rows 11 to 20) and 12500 genes
set.seed(123) # Set seed for reproducibility
n_genes <- 12500
n_participants <- 20
healthy_expression <- matrix(rnorm(n_genes * 10, mean = 10, sd = 1), nrow = 10)
cancer_expression <- matrix(rnorm(n_genes * 10, mean = 12, sd = 1), nrow = 10)
gene_expression <- rbind(healthy_expression, cancer_expression)
gene_expression[1:10, 1:1250] <- rnorm(12500, mean = 10, sd = 1) # Set the first 1250 genes to a different distribution for the healthy group
gene_expression[11:20, 1:1250] <- rnorm(12500, mean = 12, sd = 1) # Set the first 1250 genes to a different distribution for the cancer group

# Define the three statistics to test for differences between healthy and cancer groups
# Difference of median between two groups
statistic1 <- function(x){
  median(x[1:10]) - median(x[11:20])
}

# Difference of means between two groups
statistic2 <- function(x){
  mean(x[1:10]) - mean(x[11:20])
}

# Difference of mean and median of expression rank between two groups
statistic3 <- function(x){
  mean(rank(x[1:10])) - mean(rank(x[11:20])) - (median(rank(x[1:10])) - median(rank(x[11:20])))
}

# Compute the test statistics for each of the three approaches
test_statistic1 <- apply(gene_expression, 2, statistic1)
test_statistic2 <- apply(gene_expression, 2, statistic2)
test_statistic3 <- apply(gene_expression, 2, statistic3)

# Compute the p-values for each test statistic using random permutations of group labels
n_permutations <- 1000
set.seed(123) # Set seed for reproducibility
p_values1 <- replicate(n_permutations, {
  shuffled_labels <- sample(c(rep("healthy", 10), rep("cancer", 10)))
  permuted_data <- apply(gene_expression, 2, function(x) x[shuffled_labels == "healthy"] - x[shuffled_labels == "cancer"])
  abs(statistic1(permuted_data)) >= abs(test_statistic1)
})
p_values2 <- replicate(n_permutations, {
  shuffled_labels <- sample(c(rep("healthy", 10), rep("cancer", 10)))
  permuted_data <- apply(gene_expression, 2, function(x) x[shuffled_labels == "healthy"] - x[shuffled_labels == "cancer"])
  abs(statistic2(permuted_data)) >= abs(test_statistic2)
})
p_values3 <- replicate(n_permutations, {
  shuffled_labels <- sample(c(rep("healthy", 10), rep("cancer", 10)))
  permuted_data <- apply(gene_expression, 2, function(x) x[shuffled_labels == "healthy"] - x[shuffled_labels == "cancer"])
  abs(statistic3(permuted_data)) >= abs(test_statistic3)
})

# Compute the type I and type II error rates for each approach based on the empirical p-values
# Assuming a significance threshold of 0.05
significance_threshold <- 0.05
type1_error_rate1 <- mean(p_values1 <= significance_threshold)
type1_error_rate2 <- mean(p_values2 <= significance_threshold)
type1_error_rate3 <- mean(p_values3 <= significance_threshold)
type2_error_rate1 <- mean(test_statistic1[1:1250] < 0)
type2_error_rate2 <- mean(test_statistic2[1:1250] < 0)
type2_error_rate3 <- mean(test_statistic3[1:1250] < 0)

# Compute the t-test and Wilcoxon test statistics and p-values
t_test_results <- apply(gene_expression, 2, function(x) t.test(x[1:10], x[11:20]))
wilcoxon_test_results <- apply(gene_expression, 2, function(x) wilcox.test(x[1:10], x[11:20]))

t_test_statistic <- sapply(t_test_results, function(x) x$statistic)
t_test_p_value <- sapply(t_test_results, function(x) x$p.value)
wilcoxon_test_statistic <- sapply(wilcoxon_test_results, function(x) x$statistic)
wilcoxon_test_p_value <- sapply(wilcoxon_test_results, function(x) x$p.value)

# Compute the number of significant genes for each approach based on a significance threshold of 0.05
num_significant_genes1 <- sum(p_values1 <= significance_threshold)
num_significant_genes2 <- sum(p_values2 <= significance_threshold)
num_significant_genes3 <- sum(p_values3 <= significance_threshold)
num_significant_genes_ttest <- sum(t_test_p_value <= significance_threshold)
num_significant_genes_wilcoxon <- sum(wilcoxon_test_p_value <= significance_threshold)

# Compare the performance of each approach based on other criteria of interest
# Statistical power
power1 <- sum(test_statistic1[1251:12500] > quantile(test_statistic1[1:1250], 1 - significance_threshold)) / 11250
power2 <- sum(test_statistic2[1251:12500] > quantile(test_statistic2[1:1250], 1 - significance_threshold)) / 11250
power3 <- sum(test_statistic3[1251:12500] > quantile(test_statistic3[1:1250], 1 - significance_threshold)) / 11250
power_ttest <- sum(t_test_statistic[1251:12500] > quantile(t_test_statistic[1:1250], 1 - significance_threshold)) / 11250
power_wilcoxon <- sum(wilcoxon_test_statistic[1251:12500] > quantile(wilcoxon_test_statistic[1:1250], 1 - significance_threshold)) / 11250

# Print or display the results
cat("Type I error rate for approach 1:", type1_error_rate1, "\n")
cat("Type I error rate for approach 2:", type1_error_rate2, "\n")
cat("Type I error rate for approach 3:", type1_error_rate3, "\n")
cat("Type II error rate for approach 1:", type2_error_rate1, "\n")
cat("Type II error rate for approach 2:", type2_error_rate2, "\n")
cat("Type II error rate for approach 3:", type2_error_rate3, "\n")
cat("Number of significant genes for approach 1:", num_significant_genes1, "\n")
cat("Number of significant genes for approach 2:", num_significant_genes2, "\n")
cat("Number of significant genes for approach 3:", num_significant_genes3, "\n")
cat("Number of significant genes for t-test:", num_significant_genes_ttest, "\n")
cat("Number of significant genes for Wilcoxon test:", num_significant_genes_wilcoxon, "\n")
cat("Statistical power for approach 1:", power1, "\n")
cat("Statistical power for approach 2:", power2, "\n")
cat("Statistical power for approach 3:", power3, "\n")
cat("Statistical power for t-test:", power_ttest, "\n")
cat("Statistical power for Wilcoxon test:", power_wilcoxon, "\n")
# Run the code to generate the results
# ...

# Save the results as a CSV file
results <- data.frame(
  approach = c("Approach 1", "Approach 2", "Approach 3", "t-test", "Wilcoxon test"),
  type1_error_rate = c(type1_error_rate1, type1_error_rate2, type1_error_rate3, NA, NA),
  type2_error_rate = c(type2_error_rate1, type2_error_rate2, type2_error_rate3, NA, NA),
  num_significant_genes = c(num_significant_genes1, num_significant_genes2, num_significant_genes3, num_significant_genes_ttest, num_significant_genes_wilcoxon),
  power = c(power1, power2, power3, power_ttest, power_wilcoxon)
)
write.csv(results, "results.csv", row.names = FALSE)