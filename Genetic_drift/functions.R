allele_dynamics <- function(eps, q, ng, ns) {
  # Initialize empty data frame to store results
  results <- data.frame(generation = numeric(0), A1 = numeric(0), A2 = numeric(0), simulation = numeric(0))
  
  # Loop through number of simulations
  for (i in 1:ns) {
    # Calculate proportion of A2
    p <- 1 - q
    # Initialize vectors to store proportion of each allele for each generation
    q_vec <- numeric(ng)
    p_vec <- numeric(ng)
    # Set initial proportion of alleles
    q_vec[1] <- q
    p_vec[1] <- p
    # Loop through generations
    for (j in 2:ng) {
      # Calculate change in allele proportion based on genetic drift
      q_vec[j] <- q_vec[j-1] + rnorm(1, mean = 0, sd = sqrt(q_vec[j-1]*p_vec[j-1]/eps))
      p_vec[j] <- 1 - q_vec[j]
    }
    # Add results to data frame
    results <- rbind(results, data.frame(generation = 1:ng, A1 = q_vec, A2 = p_vec, simulation = i,stringsAsFactors = F))
  }
  
  return(results)
}
