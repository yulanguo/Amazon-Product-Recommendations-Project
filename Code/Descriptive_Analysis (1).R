# Start with a clear environment
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

######################################################################################

# Read the csv file
amazon_data <- read_csv("amazon_cleaned_data.csv")

# Create an igraph graph object
amazon_graph <- graph_from_data_frame(amazon_data, directed = TRUE)

# Find the number of nodes
amazon_num_nodes <- vcount(amazon_graph)

# Find the number of edges
amazon_num_edges <- ecount(amazon_graph)

# Print the results
cat("Number of nodes:", amazon_num_nodes, "\n")
cat("Number of edges:", amazon_num_edges, "\n")

# Calculate Density
graph.density(amazon_graph)


######################################################################################
#
# Local Network Properties
#
######################################################################################

# Calculate the number of components in the graph
amazon_comp <- components(amazon_graph); amazon_comp

# Take out the largest component from each graph

# Network
amazon_comp <- components(amazon_graph)
giantGraph <- amazon_graph %>% 
  induced.subgraph(., which(amazon_comp$membership == which.max(amazon_comp$csize)))

# Take out the largest component from the graph


#Plotting
## plot the original network
plot(amazon_graph, vertex.size = 3, vertex.label = NA,)

## plot the largest component of the network
plot(giantGraph, vertex.size = 3, vertex.label = NA,)

sna_amazon <- igraph::get.adjacency(amazon_graph, sparse=FALSE) %>% network::as.network.matrix()

detach('package:igraph')
library(statnet)

# Calculate in-degree centrality
idegScores_amazon <- degree(sna_amazon, cmode = 'indegree')

# Store the information
centralities_amazon <- data.frame('node_name' = as.character(network.vertex.names(sna_amazon)),
                               'in_degree' = degree(sna_amazon, cmode = 'indegree'))

# Calculate out-degree centrality and store it in the data.frame called 'centralities'
centralities_amazon$out_degree <- degree(sna_amazon, cmode = 'outdegree')

# Calculate betweenness centrality and store it in the data.frame called 'centralities'
centralities_amazon$betweenness <- betweenness(sna_amazon)

# Calculate closeness centrality and store it in the data.frame called 'centralities'
centralities_amazon$incloseness <- igraph::closeness(amazon_graph, mode = 'in')

# Calculate eigenvector centrality and store it in the data.frame called 'centralities'
# using 'igraph' because the code implemented in 'sna' is unreliable
centralities_amazon$eigen <- igraph::eigen_centrality(amazon_graph)$vector

# Calculate Burt's network constraint and store it in the data.frame called 'centralities'
centralities_amazon$netconstraint <- igraph::constraint(amazon_graph)
help(constraint) 

# Calculate authority and store it in the data.frame called 'centralities'
centralities_amazon$authority <- igraph::authority_score(amazon_graph, scale = TRUE)$`vector`

# Calculate hub and store it in the data.frame called 'centralities'
centralities_amazon$hub <- igraph::hub_score(amazon_graph, scale = TRUE)$`vector`

View(centralities_amazon)

######################################################################################
#
# Global Network Properties
#
######################################################################################
detach('package:statnet', unload = TRUE)
library(igraph)

## calculate k-cores
kcore_amazon <- amazon_graph %>% graph.coreness(.); kcore_amazon ## show the results of k-core decomposition

## Plot a graph colored by the k-core decomposition results
amazon_graph %>% 
  plot(.,
       layout = layout_with_gem(.),
       # layout = layout_with_sugiyama(.),
       edge.arrow.size = .3,
       vertex.size = 4,
       vertex.label = NA,
       vertex.color = adjustcolor(graph.coreness(.), alpha.f = .3),
       vertex.label.cex = .5,
       vertex.label.color = 'black',
       mark.groups = by(seq_along(graph.coreness(.)), graph.coreness(.), invisible),
       mark.shape = 1/4,
       mark.col = rainbow(length(unique(graph.coreness(.))),alpha = .1),
       mark.border = NA
  )

# Plot the number of clusters in the graph and their size
cluster_amazon <- amazon_graph %>% cluster_walktrap()

# modularity measure
modularity(cluster_amazon)

# Find the number of clusters
membership(cluster_amazon)   # affiliation list
length(cluster_amazon) # number of clusters

# Find the size the each cluster 
sizes(cluster_amazon) 

# Visualize clusters - that puts colored blobs around the nodes in the same community.
cluster_amazon %>% plot(.,amazon_graph,
                     # layout = layout_with_gem(.),
                     layout = layout_with_fr(amazon_graph),
                     edge.arrow.size = .3,
                     vertex.size = 4,
                     vertex.label = NA,
                     vertex.color = adjustcolor(membership(.), alpha.f = .3),
                     vertex.label.cex = .5,
                     vertex.label.color = 'black',
                     mark.groups = by(seq_along(membership(.)), membership(.), invisible),
                     mark.shape = 1/4,
                     mark.col = rainbow(length(.),alpha = .1),
                     mark.border = NA
)

# Examine the in-degree distribution
amazon_graph %>% degree.distribution(.,mode="in") %>% 
  plot(., col = 'black', pch = 19, cex = 1.5,
       main = 'In-degree Distribution',
       ylab = 'Density',
       xlab = 'In-degree')

# CCDF - Complementary Cumulative Distribution Function
# Plot a log-log plot of in-degree distribution
amazon_graph %>% 
  degree.distribution(.,cumulative = TRUE,mode ='in') %>% 
  plot(1:(max(degree(amazon_graph,mode='in'))+1),., ## since log doesn't take 0, add 1 to every degree
       log='xy', type = 'l',
       main = 'Log-Log Plot of In-degree',
       ylab = 'CCDF',
       xlab = 'In-degree')


# Fit a power law to the degree distribution
in_power_amazon <- amazon_graph %>% 
  degree.distribution(., mode='in') %>%
  power.law.fit(., xmin=0.00000001)
in_power_amazon

# Examine the out-degree distribution
amazon_graph %>% degree.distribution(.,mode="out") %>% 
  plot(., col = 'black', pch = 19, cex = 1.5,
       main = 'Out-degree Distribution',
       ylab = 'Density',
       xlab = 'Out-degree')

# Plot a log-log plot
amazon_graph %>% 
  degree.distribution(.,cumulative = TRUE,mode ='out') %>% 
  plot(1:(max(degree(amazon_graph,mode='out'))+1), ## since log doesn't take 0, add 1 to every degree
       ., log='xy', type = 'l',
       main = 'Log-Log Plot of Out-degree',
       ylab = 'CCDF',
       xlab = 'Out-degree')

# Fit a power law to the degree distribution
out_power_amazon <- amazon_graph %>% 
  degree.distribution(., mode='out') %>%
  power.law.fit(.)
out_power_amazon

# Small-world Characteristics
ntrials <- 1000 ## set a value for the repetition
cl.rg_amazon <- numeric(ntrials) ## create an estimated value holder for clustering coefficient
apl.rg_amazon <- numeric(ntrials) ## create an estimated value holder for average path length
for (i in (1:ntrials)) {
  g.rg <- rewire(amazon_graph, keeping_degseq(niter = 100))
  cl.rg_amazon[i] <- transitivity(g.rg, type = 'average')
  apl.rg_amazon[i] <- average.path.length(g.rg)
}

# plot a histogram of simulated values for clustering coefficient + the observed value
hist(cl.rg_amazon,
     main = 'Histogram of Clustering Coefficient',
     xlab = 'Clustering Coefficient',
     xlim = c(0.1,0.5))
par(xpd = FALSE)

# the line indicates the mean value of clustering coefficient for your network
abline(v = amazon_graph %>% transitivity(., type = 'average'), col = 'red', lty = 2)

# this tests whether the observed value is statistically different from the simulated distribution
t.test(cl.rg_amazon, mu=amazon_graph %>% transitivity(., type = 'average'),
       alternative = 'greater') ## pick either 'less' or 'greater' based on your results


# plot a histogram of simulated values for average path length + the observed value
hist(apl.rg_amazon,
     main = 'Histogram of Average Path Length',
     xlab = 'Average Path Length',
     xlim = c(0,15))
# the line indicates the mean value of average path length for your network
abline(v = amazon_graph %>% average.path.length(), col = 'red', lty = 2)
# this tests whether the observed value is statistically different from the simulated distribution
t.test(apl.rg_amazon, mu=amazon_graph %>% average.path.length(.),
       alternative = 'greater') ## pick either 'less' or 'greater' based on your results
## (you want to use the one that generates the smaller p-value)





