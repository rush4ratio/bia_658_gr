library(dplyr)
library(readr)
library(igraph)
library(tidytext)

insta <- read_csv("InstaOutputlist.csv")


# not sure what the proper column names should be.
names(insta) <- c("subject", "tag")


unique_subj <- unique(insta$subject)
# There are 225 unique subjects
length(unique_subj)

subj_unigram = insta %>% unnest_tokens(tokens, tag, to_lower = TRUE)

places <- read_csv("http://simplemaps.com/static/demos/resources/us-cities/cities.csv")

place_names = tolower(places$city)
rm(places)

# Because people normally don't put spaces in instagram hash tags.
place_names_no_spaces <- gsub(" ", "", place_names)

subj_tokens <- subj_unigram %>% filter(tokens %in% place_names_no_spaces)

create_adj_list = function(df){
  # Input: a dataframe with a column "tokens"
  # Output: all possible 2-combinations (sorted) of the unique tokens
  unique_tokens = unique(df$tokens)
  adj_list = data.frame()
  if(length(unique_tokens) >= 2) {
    all_combins = t(combn(unique_tokens, 2))
    all_combins = t(apply(all_combins, 1, sort))
    adj_list = data.frame(all_combins, stringsAsFactors = FALSE)
  }
  return(adj_list)
}

# create unweighted and weighted adjacency lists
adj_list <- subj_tokens %>% group_by(subject) %>% do(create_adj_list(.))
adj_list_weighted = data.frame(adj_list) %>% group_by(X1, X2) %>% summarise(weight = n())

places_graph <- graph.data.frame(adj_list[, c("X1", "X2")], directed = FALSE)


plot(simplify(places_graph))


place_graph_weighted = graph.data.frame(adj_list_weighted[, c("X1", "X2")], directed = FALSE)
E(place_graph_weighted)$weight = adj_list_weighted$weight


l <- layout_in_circle(place_graph_weighted)
plot(place_graph_weighted, layout = l, 
     edge.color="#d3d3d3")
##################### community1 ###################################
g=place_graph_weighted

community = edge.betweenness.community(g, directed=F)
community$membership

community2 = fastgreedy.community(g)
community2$membership

set.seed(1) # Since layout.fruchterman.reingold is a random algorithm, set seed can 'fix' the layout
plot(g,
     vertex.color = community$membership, vertex.size = log(degree(g) + 1),
     mark.groups = by(seq_along(community$membership), community$membership, invisible),
     layout=layout.fruchterman.reingold)

set.seed(1)
plot(g,
     vertex.color = community2$membership, vertex.size = log(degree(g) + 1),
     mark.groups = by(seq_along(community2$membership), community2$membership, invisible),
     layout=layout.gem)

#combine the nodes to meta-nodes accroding to communities
comm.graph = contract.vertices(g, community$membership, vertex.attr.comb=list(size="sum", "ignore"))
comm.graph = simplify(comm.graph)
plot(comm.graph)

############################################################################################################

community = edge.betweenness.community(g, directed=F)
community$membership

community2 = fastgreedy.community(g)
community2$membership

set.seed(1) # Since layout.fruchterman.reingold is a random algorithm, set seed can 'fix' the layout
plot(g,
     vertex.color = community$membership, vertex.size = log(degree(g) + 1),
     mark.groups = by(seq_along(community$membership), community$membership, invisible),
     layout=layout.fruchterman.reingold)

set.seed(1)
plot(g,
     vertex.color = community2$membership, vertex.size = log(degree(g) + 1),
     mark.groups = by(seq_along(community2$membership), community2$membership, invisible),
     layout=layout.fruchterman.reingold)

#combine the nodes to meta-nodes accroding to communities
comm.graph = contract.vertices(g, community$membership, vertex.attr.comb=list(size="sum", "ignore"))
comm.graph = simplify(comm.graph)
plot(comm.graph) ##### image 1 ####

################################################### Cliques ################################

cliques(g, min = 6)
sapply(cliques(g, min = 6), length) # clique sizes
largest_cliques(g) # cliques with max number of nodes

vcol <- rep("grey80", vcount(g))
vcol[unlist(largest_cliques(g))] <- "gold"
plot(as.undirected(g), vertex.label=V(g)$name, vertex.color=vcol) ##### image 2 ####

####################### Community Detection ################

ceb <- cluster_edge_betweenness(g) 
dendPlot(ceb, mode="hclust")     ##### image 3 ####
plot(ceb, g)                    ##### image 4 ####

#######
V(g)$community <- cfg$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(g, vertex.color=colrs[V(g)$community])

##################

