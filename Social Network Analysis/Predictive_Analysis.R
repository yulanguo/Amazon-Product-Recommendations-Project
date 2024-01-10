# ERGM Modeling
#
######################################################################################

# Start with a clear environment
rm(list=ls())

# Install packages below if you do not have them:
# -------------------------------------------------
if (!"statnet" %in% installed.packages()) install.packages("statnet") # For fitting ERGMs
if (!"igraph" %in% installed.packages()) install.packages("igraph") # For network plotting
if (!"texreg" %in% installed.packages()) install.packages("texreg") # For printing "nicer" model output

library(statnet)

# ----------------------------------------------------------------------------------------------------
######################## PART I: Building and Visualizing the Networks ########################
# ----------------------------------------------------------------------------------------------------

#Import file
amazon_list <- read.csv("amazon_cleaned_data.csv")

# View the first rows of the edgelist to make sure it imported correctly:
head(amazon_list)

# Create edgelist
el <- amazon_list 

# Convert the edgelist to a network object in statnet format:
item <- as.network(el, directed=TRUE)
item # View a summary of the network object


# Visualize networks
# ---------------------------------------------------------------------------------------
library('igraph') # Ignore messages on any objects that are masked

# Set default plot options
igraph_options(vertex.size = 3, vertex.color = 'grey', # vertex.size changes the size of nodes; vertex.color changes the color of nodes
               edge.color='gray80', edge.arrow.size=.1, # edge.color changes the color of ties; edge.arrow.size changes the size of tie arrow heads
               vertex.label = NA)                       # vertex.label = NA specifies not to display vertex labels in the plot

# Plot the Advice network
amazon_igraph <- graph.adjacency(as.matrix.network(item)) # make an igraph network object from statnet network object
net_layout <- layout_with_fr(amazon_igraph) # Calculates and stores a spring-embedded layout
# We will re-use this same layout for each plot, so nodes are always in the same place
plot(amazon_igraph, layout=net_layout, edge.color='black', vertex.label = NA)


# -------------------------------------------------------------------------------------------------
######################## PART II: Build the ERGM models ########################
# ----------------------------------------------------------------------------------------------------

detach(package:igraph) # Remove the 'igraph' package from your environment. 
library(statnet)
options(ergm.loglik.warn_dyads=FALSE) #Whether or not a warning should be issued when sample space constraints render the observed number of dyads ill-defined

# Ergm Terms are statistics: They are some deterministic function of the ties, node attributes, and edge covariates of a network.
help("ergm-terms",package = "ergm") # Documentation that contains definitions for all of the terms we are using
# ex. what does "mutual" test and how is it calculated
# We will use the ergm-terms to perform hypothesis testing using ERGMs
# But we can note that any of the ERGM terms can also be examined directly for your observed network, by creating a formula in R

# Look at Endogenous statistics: terms based on only ties in the advice network
summary(item ~ edges)                     # Number of edges (ties)
summary(item ~ mutual)                    # Number of pairs of reciprocated ties
summary(item ~ odegree(0:5))              # Outdegree distribution. (# of nodes with outdegree of 0, # nodes outdegree of 1, etc.)
# Remember, respondents could nominate at most five employees in our survey
summary(item ~ idegree(0:65))             # Indegree distribution.
summary(item ~ gwodegree(log(2),fixed=T)) # One parameter summarizing outdegree distribution - tendency against outdegree hubs
summary(item ~ gwidegree(log(2),fixed=T)) # One parameters summarizing indegree distribution - tendency against indegree hubs
summary(item ~ desp(1:5))                 # Pairs of nodes with one shared partner, two shared partners, etc.
summary(item ~ dgwesp(log(2),fixed = T))  # One parameter summarizing 

# We can't really look at Exogenous statistics: terms based on advice ties AND other ties / node attributes

# The above are statistics - counts of these patterns for our networks
# What fitting the ERGM model will tell is whether these counts are relatively high/low
# in comparison to what we would expect based on random chance, controlling for the other effects in our model.
# This type of analysis can be helpful for understanding your network, as well as troubleshooting issues with ERGM regression

model1 <- ergm(item ~ edges                 # This is  a tendency towards a greater number of advice ties existing. Based on a statistic counting the number of ties.
               # Structural patterns
               + mutual                      # This is a tendency towards reciprocity for the advice ties. Based on a statistic counting the number of reciprocated ties.
               + gwodegree(log(2),fixed=T)
               + gwidegree(log(2),fixed=T)
               # + dgwesp(log(2),fixed = T) weird
               #+ idegree(0:10) also weird
               #+ desp(1:2) #slow
) 
summary(model1) 


