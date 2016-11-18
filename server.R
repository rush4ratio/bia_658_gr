shinyServer(
  function(input, output){
    
    data_layout <- reactive({
      if(input$selectVisAlgo == 1)
        layout_in_circle(city_networks[[input$selectNetwork]] )
      else if(input$selectVisAlgo == 2)
        layout.sphere(city_networks[[input$selectNetwork]])
      else if(input$selectVisAlgo == 3)
        layout.fruchterman.reingold(city_networks[[input$selectNetwork]])
      else if(input$selectVisAlgo == 4)
        layout.kamada.kawai(city_networks[[input$selectNetwork]])
      else if(input$selectVisAlgo == 5)
        layout.spring(city_networks[[input$selectNetwork]])
      else if(input$selectVisAlgo == 6)
        layout.gem(city_networks[[input$selectNetwork]])
    })
    
    output$network <- renderPlot({
      if(is.null(input$selectNetwork)){
        return()
      }
      choice <- input$selectVisAlgo
      network_selected <- city_networks[[input$selectNetwork]]
      
      
      label_toggle <- NULL
      if(input$showLabels)
        label_toggle <- V(network_selected)$name
      else
        label_toggle <- ""
      
      data_layout <- data_layout()
      
      plot(network_selected,
           vertex.label = label_toggle,
           edge.color="#d3d3d3", layout =  data_layout)
      
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