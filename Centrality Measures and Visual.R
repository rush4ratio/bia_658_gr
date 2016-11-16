#install igraph
install.packages('networkD3')
library(networkD3)
library(igraph)
library(dplyr)
library(readr)
library(igraph)
library(tidytext)

setwd("/Users/Matt/Documents/Stevens/BIA 658 Social Network Analytics/Instagram/R Code")

insta <- read_csv("InstaOutputlist.csv")
insta_graph <- graph.data.frame(insta, directed = FALSE)

summary(insta_graph)

#create source, target
#https://christophergandrud.github.io/networkD3/
src <- insta$rephoto
target <- insta$rephotography

networkData <- data.frame(src,target)

#plot
simpleNetwork(networkData, zoom = TRUE, textColour = "#000099", opacity=0.8, nodeColour = "#b3d9ff",
              linkColour = "#f2f2f2", fontSize = 10, linkDistance = 100)


# centralization measure, use mode option for in/out degree
degree(insta_graph)
# use table to get a degree distribution
table(degree(insta_graph))

degree_cent <- as.data.frame(degree(insta_graph, normalized = TRUE))
betweenness(graph = insta_graph, normalized = FALSE)
betweenness(graph = insta_graph, normalized = TRUE)
closeness<- as.data.frame(closeness(insta_graph, normalized= TRUE))
# eigenvalue centrality
evcent(insta_graph)


#Note that `evcent(g)` gives a list that contains multiple outputs (see help for evcent using `?evcent`), in order to extract the element we want from a list we use the $.
evcent(insta_graph)$vector


# load graph from (Graph Modelling Language) GML format

# number of edges and nodes(vertices)
ecount(insta_graph)
vcount(insta_graph)
# visualization
#plot(g2)
# clustering coefficient
transitivity(insta_graph, type = "local")
# degree centralization
centralization.degree(insta_graph)

