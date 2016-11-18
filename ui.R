shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
      fluidRow(
        selectInput("selectNetwork", label = h3("City"), 
                    choices = list("Chapel Hill" = "Chapel Hill", "Hoboken" = "Hoboken", "La Crosse" = "La Crosse",
                                   "Lakewood" = "Lakewood", "Lancaster" = "Lancaster", "Manhattan KS"="Manhattan, KS",
                                   "New Brunswick" = "New Brunswick", "Royal Oak" = "Royal Oak", "San Marcos" = "San Marcos",
                                   "Sarasota" = "Sarasota"), 
                    selected = "Chapel Hill"),
        
        selectInput("selectVisAlgo", label = h3("Visualization Algorithm"), 
                    choices = list("Circle" = 1, "Sphere"= 2,"Fruchterman Reingold" = 3,"Kamada Kawai" = 4, "Spring" = 5, "Gem" = 6), 
                    selected = 1),
        hr(),
        checkboxInput("showLabels", label = p("Show Labels?"), value = TRUE),
        hr()
      ),
      fluidRow(
        tableOutput('degreeTable')
      )
      ),
      mainPanel(tabsetPanel(
        tabPanel("Network",
                 plotOutput("network",width = "750px", height = "750px")),
        tabPanel("Community Detection",
                 plotOutput("communityDetection",width = "750px", height = "750px"))
      ))
    ))
)