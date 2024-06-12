########################################################################################
# For VSC User
########################################################################################

# Run all chunks in the document
# rmd ... name of document
runAllChunks <- function(rmd, envir = globalenv()) {
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  knitr::purl(rmd, output = tempR)
  sys.source(tempR, envir = envir)
  unlink(tempR)
}

# Run all chunks in THIS document
run <- function() {
  runAllChunks("ue5_angabe.Rmd")
}

########################################################################################
# Analysis of the Model
########################################################################################

# Function to create combined histogram plots
create_histograms <- function(data) {
  # Get the names of the numeric columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]

  # Loop over the numeric columns
  for (col in numeric_cols) {
    # Create a histogram for the current column
    p <- ggplot(data, aes_string(x = col, fill = "group")) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
      labs(x = col, y = "Count", fill = "Group") +
      theme_minimal()

    # Print the histogram
    print(p)
  }
}

########################################################################################
# Helper functions for the exercises
########################################################################################

#' Calculate the accuracy of clustering
#'
#' @param group_data data.frame: data frame containing the original group labels
#' @param result_data list: list containing the clustering results
#' @param groups character: vector containing the names of the groups
#'
#' @return accuracy of the clustering results compared to the original group labels
calculate_accuracy <- function(group_data, result_data, groups) {
  # Convert the group names to factors
  group_data$group <- factor(group_data$group, levels = groups)

  # Calculate the means of the groups
  group_means <- aggregate(. ~ group, group_data, mean)

  # Create a data frame containing the cluster means
  cluster_means <- get_cluster_means(group_data, result_data)

  # Initialize an empty vector to store the groups
  clusters <- c()

  # Drop group column
  group_means <- dplyr::select(group_means, -group)
  cluster_means <- dplyr::select(cluster_means, -cluster)

  # Loop over all rows of group_means
  for (i in 1:nrow(group_means)) {
    # Get the i-th row of the dataframe
    group_means_row <- group_means[i, ]

    # Overwrite all rows with the i-th row
    group_means_rows <- do.call("rbind", replicate(nrow(group_means), group_means_row, simplify = FALSE))

    # Calculate the squared differences
    differences <- (group_means_rows - cluster_means)^2

    # Calculate the Euclidean distances
    euclidian_distances <- sqrt(rowSums(differences))

    # Find the cluster mean with the smallest distance to the given group mean
    clusters[i] <- which.min(euclidian_distances)
  }

  # Create a named vector for recoding
  recode_vector <- setNames(clusters, groups)

  # Recode the 'group' variable to the matching cluster
  group <- recode(group_data$group, !!!recode_vector)

  # Compare the accuracy of the clustering results with the ground truth
  accuracy <- sum(result_data$cluster == group) / nrow(group_data)

  return(accuracy)
}

#' Calculate the means of the clusters
#'
#' @param group_data data.frame: data frame containing the original group labels
#' @param result_data list: list containing the clustering results.
#'
#' @return data.frame: data frame containing the means of each cluster.
get_cluster_means <- function(group_data, result_data) {
  # Drop the 'group' column from the dataframe
  # This is done because we want to calculate the means of the clusters, not the original groups
  cluster_data <- dplyr::select(group_data, -group)

  # Add the cluster assignments to the dataframe
  cluster_data$cluster <- result_data$cluster

  # Calculate the means of the clusters
  # The aggregate function is used to calculate the mean of each variable for each cluster
  cluster_means <- aggregate(. ~ cluster, cluster_data, mean)

  # Return the means of the clusters
  return(cluster_means)
}
