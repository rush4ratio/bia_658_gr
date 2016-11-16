library(dplyr)
library(readr)
library(igraph)

library(shiny)

# If error, install with install.packages("tidytext", dependency = TRUE)
library(tidytext)

########### Get places data set   ##################
places <- read_csv("http://simplemaps.com/static/demos/resources/us-cities/cities.csv")
place_names = tolower(places$city)
rm(places)
place_names_no_spaces <- gsub(" ", "", place_names)
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
  
  places_graph <- graph.data.frame(adj_list[, c("X1", "X2")], directed = FALSE)
  
  return(simplify(places_graph))
  
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

dir=setwd("/Users/Matt/Documents/Stevens/BIA 658 Social Network Analytics/Instagram")
############## Main Program ########################
insta_csvs <- list.files(path=paste(dir, "/Data", sep=""),pattern="InstaOutputlist_.*\\.csv") 

city_networks = NULL

for(index in 1:length(insta_csvs)){
  city <- gsub("InstaOutputlist_", "", gsub(".csv", "", insta_csvs[index]))
  
  city_network <- network_from_csv(paste("Data/", insta_csvs[index], sep=""))
  
  city_networks[[city]] <- city_network 
}

###### Shiny   #####
shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("selectNetwork", label = h3("City"), 
                    choices = list("Chapel Hill" = "Chapel Hill", "Hoboken" = "Hoboken", "La Crosse" = "La_Crosse",
                                   "Lakewood" = "Lakewood", "Lancaster" = "Lancaster", "Manhattan KS"="Manhattan, KS",
                                   "New Brunswick" = "New Brunswick", "Royal Oak" = "Royal Oak", "San Marcos" = "San Marcos",
                                   "Sarasota" = "Sarasota"), 
                    selected = "Chapel Hill"),
        
        selectInput("selectVisAlgo", label = h3("Visualization Algorithm"), 
                    choices = list("Circle" = 1, "Sphere"= 2,"Fruchterman Reingold" = 3), 
                    selected = 1)
        
      ),
      mainPanel(tabsetPanel(
        tabPanel("Network",
                 plotOutput("network",width = "750px", height = "600px")),
        tabPanel("Centrality", tableOutput('degreeTable'))
      ))
    )),
  server = function(input, output){
    output$network <- renderPlot({
      if(is.null(input$selectNetwork)){
        return()
      }
      
      choice <- input$selectVisAlgo
      network_selected <- city_networks[[input$selectNetwork]]
      
      if(choice == 1)
        l <- layout_in_circle(network_selected )
      else if(choice == 2)
        l <- layout.sphere(network_selected)
      else if(choice == 3)
        l <- layout.fruchterman.reingold(network_selected)

      
      plot(network_selected,
           edge.color="#d3d3d3", layout = l)
      
    })
    output$degreeTable = renderTable({
      network_selected <- city_networks[[input$selectNetwork]]
      degree_cent <- as.data.frame(degree(network_selected, normalized = TRUE))
      degree_cent['City'] <- rownames(degree_cent)
      colnames(degree_cent)<-c("Degree_Centrality","Hashtag")
      deg_cent <-degree_cent[,c(2,1)]
      arrange(deg_cent,desc(Degree_Centrality))
    })
    
  }
)



