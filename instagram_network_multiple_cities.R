library(dplyr)
library(readr)
library(igraph)
library(tidyr)
library(shiny)
library(ggplot2)
library(ggmap)

# If error, install with install.packages("tidytext", dependency = TRUE)
library(tidytext)

########### Get places data set   ##################
places <- read_csv("http://simplemaps.com/static/demos/resources/us-cities/cities.csv")
place_names = tolower(places$city)
places$city <- tolower(places$city)
places$city <- gsub(" ", "", place_names)
places<-unite(places, id, c(city, state), remove=FALSE)


#rm(places)
place_names_no_spaces <- gsub(" ", "", place_names)

#get top cities
top_cities <- read.csv("http://img.ezlocal.com/data/Top5000Population.csv", header=FALSE,encoding="UTF-8", stringsAsFactors=FALSE)
colnames(top_cities) <- c("city","state","population")
top_cities$city <- tolower(top_cities$city)
top_cities$city <- gsub(" ", "", top_cities$city)
top_cities<-unite(top_cities, id, c(city, state), remove=FALSE)
top_cities<- top_cities[-(1000:5000), ]

#combine lat_long into top_cities
top_cities_2 <- (merge(top_cities, places, by = 'id'))
top_cities_2<- subset(top_cities_2,!duplicated(top_cities_2$id))
top_cities_2 <- subset(top_cities_2, select= c("id", "city.x","state.x","zip","lat","lng"))
colnames(top_cities_2) <- c("id","city","state","zip","lat","lng")

##################################################

########### read csv and transform to network   #
network_from_csv <- function(file_name){
  insta <- read_csv(file_name)
  # not sure what the proper column names should be.
  names(insta) <- c("subject", "tag")
  
  subj_unigram = insta %>% unnest_tokens(tokens, tag, to_lower = TRUE)
  # Because people normally don't put spaces in instagram hash tags.
  subj_tokens <- subj_unigram %>% filter(tokens %in% place_names_no_spaces)
  
  # create unweighted and weighted adjacency lists
  adj_list <- subj_tokens %>% group_by(subject) %>% do(create_adj_list(.))
  adj_list_weighted = data.frame(adj_list) %>% group_by(X1, X2) %>% summarise(weight = n())
  
  places_graph <- graph.data.frame(adj_list_weighted[, c("X1", "X2")], directed = FALSE)
  E(places_graph)$weight = adj_list_weighted$weight

  
  return(places_graph)
  
}
##################################################

####### create adjacency list ######################
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
##################################################
# remember to set your respective directory

dir=setwd("/Users/Matt/Documents/Stevens/BIA 658 Social Network Analytics/Instagram")
############## Main Program ########################
insta_csvs <- list.files(path=paste(getwd(), "/Data", sep=""),pattern="InstaOutputlist_.*\\.csv") 

city_networks = NULL

for(index in 1:length(insta_csvs)){
  city <- gsub("InstaOutputlist_", "", gsub(".csv", "", insta_csvs[index]))
  
  city_network <- network_from_csv(paste("Data/", insta_csvs[index], sep=""))
  
  city_networks[[city]] <- city_network 
}

###############generate map################
map_from_network <- function(network_graph){
  
  network_selected <- network_graph
  degree_cent <- as.data.frame(degree(network_selected, normalized = TRUE))
  degree_cent['city'] <- rownames(degree_cent)
  colnames(degree_cent)<-c("Degree_Centrality","city")
  degree_cent <-degree_cent[,c(2,1)]
  arrange(degree_cent,desc(Degree_Centrality))
  #degree_cent<- degree_cent[(0:5), ]
  #arrange(deg_cent,desc(Degree_Centrality))
  
  map_table <- (merge(degree_cent, top_cities_2, by = 'city'))
  
  # getting the map
  mapgilbert <- get_map(location = c(lon = mean(map_table$lng), lat = mean(map_table$lat)), zoom = 4,
                        scale = 2)
  ggmap(mapgilbert) +
    geom_point(data = map_table, aes(x = lng, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE)

  
}

setwd("/Users/Matt/Documents/Stevens/BIA 658 Social Network Analytics/Instagram/Git/bia_658_gr")
###### Shiny   #####
runApp()



#map_from_network(city_networks[['Chapel Hill']]) #test the map

