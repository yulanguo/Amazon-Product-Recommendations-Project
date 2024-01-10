rm(list=ls())

######################################################################################
# The first time you run this file, you will need to install several packages.
# To do that, run the code section below. It may take up a couple of minutes.
# You only need to install packages once, next time you should skip those lines.
list.of.packages <- c("readr","tidytext", "tidygraph","ggraph","igraph","tidyverse","topicmodels","textstem","udpipe")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Now run the lines below to load the packages you have installed.
# You need to load packages every time you run the script or restart R.
library(readr)
library(tidytext)
library(tidygraph)
library(ggraph)
library(igraph)
library(tidyverse)
library(topicmodels)
library(textstem)
library(udpipe)
library(stats)


# To check whether your R loads these packages, run the following code
sessionInfo() ## check other attached packages. If readr, tidytext, tidygraph, ggraph, 
## igraph, tidyverse, topicmodels and textstem are listed there, you're ready!

# In this step you tell R where to look for your files.
# From the menu, select "Session > Set Working Directory... > To Source File Location".

# Read the text file, skipping commented lines and using tab separator
data_frame <- read.table("amazon0601.txt", header = FALSE, comment.char = "#", sep = "\t")

# Convert columns to numeric if needed
data_frame$FromNodeId <- (data_frame$FromNodeId)
data_frame$ToNodeId <- (data_frame$ToNodeId)

# Print the resulting data frame
colnames(data_frame) <- c("From", "To")
print(data_frame)

# Create an igraph graph object
graph <- graph_from_data_frame(data_frame, directed = TRUE)

# Find the number of nodes
num_nodes <- vcount(graph)

# Find the number of edges
num_edges <- ecount(graph)

# Print the results
cat("Number of nodes:", num_nodes, "\n")
cat("Number of edges:", num_edges, "\n")

# Calculate Density
graph.density(graph)

# Calculate the number of components in the graph
amazon_comp <- components(graph); amazon_comp

# Take out the largest component from each graph

# Network
amazon_comp <- components(graph)
giantGraph <- graph %>% 
  induced.subgraph(., which(amazon_comp$membership == which.max(amazon_comp$csize)))

# Calculate the number of nodes in giant graph
num_nodes <- vcount(giantGraph)

# Calculate the number of nodes to keep (quarter of the total nodes)
num_nodes_to_keep <- num_nodes / 4


# Randomly select nodes to keep
set.seed(123)  # For reproducibility
nodes_to_keep <- sample(1:num_nodes, size = num_nodes_to_keep, replace = FALSE)

# Create a subgraph with the selected nodes
subgraph <- induced_subgraph(giantGraph, nodes_to_keep)


# Calculate communities using the walktrap algorithm
cluster_amazon_walktrap <- subgraph %>% cluster_walktrap()

# Find the number of clusters
num_clusters <- length(cluster_amazon_walktrap)

# Print the number of clusters
cat("Number of clusters (Walktrap):", num_clusters, "\n")

# Find the size of each cluster
cluster_sizes <- sizes(cluster_amazon_walktrap)
cat("Cluster sizes (Walktrap):", cluster_sizes, "\n")

# Specify the cluster number you want to extract (the one with 355 nodes)
desired_cluster_number <- 11

# Get the nodes belonging to the desired cluster
nodes_in_desired_cluster <- which(membership(cluster_amazon_walktrap) == desired_cluster_number)

# Create a subgraph with nodes from the desired cluster
subgraph_desired_cluster <- induced_subgraph(subgraph, nodes_in_desired_cluster)

# Print the number of nodes and edges in the desired cluster
num_nodes_desired <- vcount(subgraph_desired_cluster)
num_edges_desired <- ecount(subgraph_desired_cluster)
cat("Number of nodes in desired cluster:", num_nodes_desired, "\n")
cat("Number of edges in desired cluster:", num_edges_desired, "\n")

# Extract the edge list from the subgraph
edge_list_desired_cluster <- as_edgelist(subgraph_desired_cluster)

# Convert the edge list to a data frame
edge_df <- as.data.frame(edge_list_desired_cluster, stringsAsFactors = FALSE)

# Rename the columns
colnames(edge_df) <- c("From", "To")

# Save the edge data frame to a CSV file
write.csv(edge_df, file = "amazon_cleaned_data.csv", row.names = FALSE)







