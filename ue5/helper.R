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
#' @param cluster list: list containing the clustering results
#' @param groups character: vector containing the names of the groups
#'
#' @return accuracy of the clustering results compared to the original group labels
calculate_accuracy <- function(group_data, cluster, groups) {
  # Recode the group labels to match the cluster assignments
  group <- recode_group_to_cluster(group_data, cluster, groups)

  # Compare the accuracy of the clustering results with the ground truth
  accuracy <- sum(cluster == group) / nrow(group_data)

  return(accuracy)
}

recode_group_to_cluster <- function(group_data, cluster, groups) {
  # Convert the group names to factors
  group_data$group <- factor(group_data$group, levels = groups)

  # Calculate the means of the groups
  group_means <- aggregate(. ~ group, group_data, mean)

  # Create a data frame containing the cluster means
  cluster_means <- get_cluster_means(group_data, cluster)

  # Drop group column
  group_means <- dplyr::select(group_means, -group)
  cluster_means <- dplyr::select(cluster_means, -cluster)

  # # Loop over all rows of group_means
  # for (i in 1:nrow(group_means)) {
  #   # Get the i-th row of the dataframe
  #   group_means_row <- group_means[i, ]

  #   # Overwrite all rows with the i-th row
  #   group_means_rows <- do.call("rbind", replicate(nrow(group_means), group_means_row, simplify = FALSE))

  #   # Calculate the squared differences
  #   differences <- (group_means_rows - cluster_means)^2

  #   # Calculate the Euclidean distances
  #   euclidian_distances <- sqrt(rowSums(differences))

  #   # Find the cluster mean with the shortest distance to the given group mean
  #   clusters <- recursive_function(euclidian_distances, clusters)
  # }

  # Initialize an empty matrix to store the Euclidean distances
  euclidian_distances_matrix <- matrix(nrow = nrow(group_means), ncol = nrow(cluster_means))

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

    # Store the Euclidean distances in the matrix
    euclidian_distances_matrix[i, ] <- euclidian_distances
  }

  print(euclidian_distances_matrix)

  # Find the lowest sum of n elements in an nxn matrix where all of these elements share
  # neither the same column index nor the same row index
  # Did not work as expected. Not the best accuarcies are yielded
  # clusters <- get_clusters_3(euclidian_distances_matrix) # --> playground

  clusters <- get_clusters_0(euclidian_distances_matrix)

  print(clusters)

  # Create a named vector for recoding
  recode_vector <- setNames(clusters, groups)

  # Recode the 'group' variable to the matching cluster
  group <- recode(group_data$group, !!!recode_vector)

  return(group)
}

# Simple but sufficiently effective method
get_clusters_0 <- function(euclidian_distances_matrix) {
  clusters <- c()

  # Loop over all rows of the matrix
  for (i in 1:nrow(euclidian_distances_matrix)) {
    # Get the i-th row of the matrix
    row <- euclidian_distances_matrix[i, ]

    # Find the index of the minimum value in the row
    cluster_candidate <- which.min(row)

    continue <- TRUE

    while (continue) {
      # If the cluster_candidate is already in clusters
      if (cluster_candidate %in% clusters) {
        # Remove the cluster_candidate from the row
        row[cluster_candidate] <- NA
        # Find the index of the minimum value in the row
        cluster_candidate <- which.min(row)
      } else {
        # If the cluster_candidate is not in clusters, append it to clusters
        clusters <- c(clusters, cluster_candidate)
        continue <- FALSE
      }
    }
  }

  return(clusters)
}


#' Calculate the means of the clusters
#'
#' @param group_data data.frame: data frame containing the original group labels
#' @param cluster: list containing the clustering results.
#'
#' @return data.frame: data frame containing the means of each cluster.
get_cluster_means <- function(group_data, cluster) {
  # Drop the 'group' column from the dataframe
  # This is done because we want to calculate the means of the clusters, not the original groups
  cluster_data <- dplyr::select(group_data, -group)

  # Add the cluster assignments to the dataframe
  cluster_data$cluster <- cluster

  # Calculate the means of the clusters
  # The aggregate function is used to calculate the mean of each variable for each cluster
  cluster_means <- aggregate(. ~ cluster, cluster_data, mean)

  # Return the means of the clusters
  return(cluster_means)
}

create_scatter_plot <- function(group_data, cluster, cluster_name, accuracy_result, groups, ignore) {
  group_data$group <- recode_group_to_cluster(group_data, cluster, groups)

  # Add the cluster labels to the data frame
  group_data$cluster <- cluster

  # Add a column to the data frame that indicates whether each data point was correctly labeled
  group_data$correctly_labeled <- ifelse(group_data$cluster == group_data$group, "Correct", "Incorrect")

  if (!ignore) {
    print(group_data)
  }


  ggplot(group_data, aes(x = fpg, y = glucose, color = as.factor(cluster), shape = correctly_labeled)) +
    geom_point() +
    scale_shape_manual(values = c("Correct" = 19, "Incorrect" = 4)) +
    labs(color = "Cluster", shape = "Correctly Labeled") +
    theme_minimal() +
    ggtitle(paste("Scatter plot of fpg vs glucose: ", cluster_name, " (Accuracy: ", 100 * round(accuracy_result, 2), "%)", sep = ""))
}
