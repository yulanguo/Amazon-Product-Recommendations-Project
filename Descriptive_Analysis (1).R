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
plot(amazon_graph, vertex.size = 3, vertex.label = NA,
     # Settings for layouts:
     #      Running this command multiple times will produce slightly different networks,
     #      based on the layout algorithm used. You can swap algorithms by uncommenting one of the
     #      lines below. Which algorithm works best often depends on the data
     # layout = layout_nicely(gpt_graph)      ## Automated layout recommendation from iGraph
     # layout = layout_with_fr(gpt_graph)    ## Fruchterman-Reingold algorithm
     # layout = layout_with_dh(gpt_graph)    ## Davidson and Harel algorithm
     # layout = layout_with_drl(gpt_graph)   ## Force-directed algorithm
     # layout = layout_with_kk(gpt_graph)    ## Spring algorithm
     # layout = layout_with_lgl(gpt_graph)   ## Large graph layout
)

## plot the largest component of the network
plot(giantGraph, vertex.size = 3, vertex.label = NA,
     # Settings for layouts:
     #      Running this command multiple times will produce slightly different networks,
     #      based on the layout algorithm used. You can swap algorithms by uncommenting one of the
     #      lines below. Which algorithm works best often depends on the data
     # layout = layout_nicely(giantGraph_gpt)      ## Automated layout recommendation from iGraph
     # layout = layout_with_fr(giantGraph_gpt)    ## Fruchterman-Reingold algorithm
     # layout = layout_with_dh(giantGraph_gpt)    ## Davidson and Harel algorithm
     # layout = layout_with_drl(giantGraph_gpt)   ## Force-directed algorithm
     # layout = layout_with_kk(giantGraph_gpt)    ## Spring algorithm
     # layout = layout_with_lgl(giantGraph_gpt)   ## Large graph layout
)


# For this part, you switch 'igraph' to 'sna' package because we are going to use 
# some functions that only are available in sna package
# As a first step, create a 'sna' graph object from an 'igraph' object
sna_amazon <- igraph::get.adjacency(amazon_graph, sparse=FALSE) %>% network::as.network.matrix()

# this detaching is a necessary step since the two packages have some same function names
# R is often confuesed
detach('package:igraph')
library(statnet)

# Compute centralities based on 'network' package
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
# using 'igraph' because 'sna' doesn't have the function
centralities_amazon$netconstraint <- igraph::constraint(amazon_graph)
help(constraint) # Be careful with the interpretation for constraint:
# High constraint = redundant contacts, low constraint = acting as a broker

# Calculate authority and store it in the data.frame called 'centralities'
# using 'igraph' because 'sna' doesn't have the function
# 'igraph::' allows calling for any igraph function without loading the package
centralities_amazon$authority <- igraph::authority_score(amazon_graph, scale = TRUE)$`vector`

# Calculate hub and store it in the data.frame called 'centralities'
# using 'igraph' because 'sna' doesn't have the function
centralities_amazon$hub <- igraph::hub_score(amazon_graph, scale = TRUE)$`vector`

View(centralities_amazon)

######################################################################################
#
# Global Network Properties
#
######################################################################################
# To go back to igraph analysis, don't forget detaching 'sna' and 'network' first
# before recalling 'igraph'
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
# We will use walktrap because this is a directed graph
cluster_amazon <- amazon_graph %>% cluster_walktrap()

# modularity measure
modularity(cluster_amazon)

# Find the number of clusters
membership(cluster_amazon)   # affiliation list
length(cluster_amazon) # number of clusters

# Find the size the each cluster 
# Note that communities with one node are isolates, or have only a single tie
sizes(cluster_amazon) 

# Visualize clusters - that puts colored blobs around the nodes in the same community.
# You may want to remove vertex.label=NA to figure out what terms are clustered.
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
# The output of the power.law.fit() function tells us what the exponent of the power law is ($alpha)
# and the log-likelihood of the parameters used to fit the power law distribution ($logLik)
# Also, it performs a Kolmogov-Smirnov test to test whether the given degree distribution could have
# been drawn from the fitted power law distribution.
# The function thus gives us the test statistic ($KS.stat) and p-vaule ($KS.p) for that test
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
## (you want to use the one that generates the smaller p-value)



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





