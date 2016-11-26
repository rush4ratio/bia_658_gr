shinyUI(
  fluidPage(theme = shinytheme("darkly"),
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
                    choices = list("Kamada Kawai" = 1, "Sphere"= 2,"Fruchterman Reingold" = 3,"Circle" = 4,"Gem" = 5), 
                    selected = 1),
        hr(),
        checkboxInput("showLabels", label = p("Show Labels?"), value = TRUE),
        checkboxInput("showEdgeWeight", label = p("Show edge weight?"), value = FALSE),
        checkboxInput("showPEdgeWeight", label = p("Show prominent edge weights?"), value = FALSE),
        conditionalPanel(
          condition = "input.showPEdgeWeight == true",
          sliderInput("pomm", label="x times the mean edge weight",
                      min = 0.5, max = 3.5, value = 1)
        ),
        hr()
      ),
      fluidRow(
        tableOutput('degreeTable')
      )
      ),
      mainPanel(tabsetPanel(
        tabPanel("Network",
                 plotOutput("network",width = "800px", height = "750px")),
        tabPanel("Community Detection",
                 plotOutput("communityDetection",width = "800px", height = "750px")),
        tabPanel("Geographic View",
                 plotOutput("geographicView", width = "800px", height = "750px")),
        tabPanel("Degree Distribution",
                 plotOutput("degreeDistribution"))
      ))
    ))
)