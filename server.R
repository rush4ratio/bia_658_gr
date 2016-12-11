shinyServer(
  function(input, output){
    
    data_layout <- reactive({
      if(input$selectVisAlgo == 1)
        layout.kamada.kawai(city_networks[[input$selectNetwork]])
      else if(input$selectVisAlgo == 2)
        layout.sphere(city_networks[[input$selectNetwork]])
      else if(input$selectVisAlgo == 3)
        layout.fruchterman.reingold(city_networks[[input$selectNetwork]])
      else if(input$selectVisAlgo == 4)
        layout_in_circle(city_networks[[input$selectNetwork]] )
      else if(input$selectVisAlgo == 5)
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
      
      E(network_selected)$edge.color <- "#d3d3d3"
      E(network_selected)[E(network_selected)$weight > 2 * mean(E(network_selected )$weight)]$edge.color <- "red"
      
      show_edge <- NULL
      if(input$showEdgeWeight && input$showPEdgeWeight){
        E(network_selected)$edge.color <- "#d3d3d3"
        E(network_selected)[E(network_selected)$weight > input$pomm * mean(E(network_selected )$weight)]$edge.color <- "red"
        show_edge$color <- E(network_selected)$edge.color
        show_edge$width <- E(network_selected)$weight/8
      }
      else if(input$showEdgeWeight){
        show_edge$color <- "#d3d3d3"
        show_edge$width <- E(network_selected)$weight/8
      }
      else if(input$showPEdgeWeight){
        E(network_selected)$edge.color <- "#d3d3d3"
        E(network_selected)[E(network_selected)$weight > input$pomm * mean(E(network_selected )$weight)]$edge.color <- "red"
        show_edge$color <- E(network_selected)$edge.color
        show_edge$width <- NA
      }else{
        show_edge$color <- "#d3d3d3"
        show_edge$width <- NA
      }
      

      plot(network_selected,
           vertex.frame.color = NA,
           vertex.label.cex = .9,
           vertex.size = 10,
           vertex.label = label_toggle,
           edge.color=show_edge$color,
           edge.width = show_edge$width,
           layout =  data_layout)
      
    })
    output$degreeTable = renderTable({
      network_selected <- city_networks[[input$selectNetwork]]
      degree_cent <- as.data.frame(degree(network_selected, normalized = TRUE))
      degree_cent['City'] <- rownames(degree_cent)
      colnames(degree_cent)<-c("Degree_Centrality","Hashtag")
      deg_cent <-degree_cent[,c(2,1)]
      arrange(deg_cent,desc(Degree_Centrality))
    })
    
    output$communityDetection <- renderPlot({
      network_selected <- city_networks[[input$selectNetwork]]
      
      if(input$selectCD == 1){
        community = edge.betweenness.community(network_selected, network_selected$weight)
      }else if(input$selectCD == 2){
        community = fastgreedy.community(network_selected, weights = network_selected$weight)
      }
      
      # show labels
      label_toggle <- NULL
      if(input$showLabels)
        label_toggle <- V(network_selected)$name
      else
        label_toggle <- ""
      
      set.seed(5)
      data_layout <- data_layout()
      
      # edge attributes
      
      show_edge <- NULL
      if(input$showEdgeWeight && input$showPEdgeWeight){
        E(network_selected)$edge.color <- "#d3d3d3"
        E(network_selected)[E(network_selected)$weight > input$pomm * mean(E(network_selected )$weight)]$edge.color <- "red"
        show_edge$color <- E(network_selected)$edge.color
        show_edge$width <- E(network_selected)$weight/8
      }
      else if(input$showEdgeWeight){
        show_edge$color <- "#d3d3d3"
        show_edge$width <- E(network_selected)$weight/8
      }
      else if(input$showPEdgeWeight){
        E(network_selected)$edge.color <- "#d3d3d3"
        E(network_selected)[E(network_selected)$weight > input$pomm * mean(E(network_selected )$weight)]$edge.color <- "red"
        show_edge$color <- E(network_selected)$edge.color
        show_edge$width <- NA
      }else{
        show_edge$color <- "#d3d3d3"
        show_edge$width <- NA
      }
      
      plot(network_selected,
           vertex.color = community$membership, vertex.size = log(degree(network_selected) + 1),vertex.frame.color = NA,
           vertex.label = label_toggle,
           mark.groups = by(seq_along(community$membership), community$membership,   invisible),mark.border = NA,
           edge.color=show_edge$color,
           edge.width = show_edge$width,
           layout=data_layout )
      
    })
    output$geographicView <- renderPlot({
      network_selected <- city_networks[[input$selectNetwork]]
      #degree_d <- degree.distribution(network_selected, cumulative=F, mode="all")
      plot(map_from_network(network_selected))
      
    })
    output$degreeDistribution <- renderPlot({
      network_selected <- city_networks[[input$selectNetwork]]
      degree_d <- as.numeric(names(table(degree(network_selected))))
      plot(degree_d ,as.numeric(table(degree(network_selected))), pch=19, cex=1, col="blue", xlab="Degree", ylab="No of Nodes")
      
    })
    
  }
)

