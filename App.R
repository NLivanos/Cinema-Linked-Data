#install.packages("igraph")
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("DT")
#install.packages("plotrix")
cat("\014")
library(igraph)
library(shiny)
library(shinydashboard)
library(DT)
library(plotrix)

ui <- dashboardPage(
  dashboardHeader(title = 'Movie Distributors',
                  tags$li(class = "dropdown",
                          tags$a(href="http://okfn.gr/", target="_blank", 
                                 tags$img(height = "35px", src="http://s29.postimg.org/q1fx6yf1j/Copy_of_OK_LG_LOGO_GREECE_WHITE_RGB.png")
                          )
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Network", tabName = "Graphs", icon = icon("share-alt")),
      menuItem("Bar Chart", tabName = "Bar", icon = icon("bar-chart")),
      menuItem("Pie Chart", tabName = "Charts", icon = icon("pie-chart")),
      menuItem("Data", tabName = "Matrices", icon = icon("table")),
      menuItem("Time Series", tabName = "Lines", icon = icon("line-chart")),
      menuItem("Statistics", tabName = "Stat", icon = icon("area-chart"))
               )
                  ),
  dashboardBody(
    tabItems(
    tabItem(tabName = "Graphs",
            column(width = 3,
                   box(width = "100%", background = "maroon",
                       collapsible = T,
                        selectInput("layout", "Layout",
                               choices = c("Fruchterman-Reingold",
                                           "Circle",
                                           "Sphere",
                                           "Kamada-Kawai"),
                               selected = "Circle"),
                        sliderInput("edgecut", "Delete Weak Links",
                               min = 1, max = 8, value = 0, step = 1)),
                   box(width = "100%", background = "blue",
                       collapsible = T,
                        selectInput("vcolor", "Vertex Color",
                               choices = c("Green"=1,
                                           "Red"=2,
                                           "Blue"=3,
                                           "Pink"=4,
                                           "Yellow"=5,
                                           "Orange"=6,
                                           "Magenta"=7),
                               selected = 3),
                        sliderInput("vsize", "Vertex Size",
                               min = 1, max = 25, value = 15, step = 1)),
                   box(width = "100%", background = "purple",
                       collapsible = T,
                        selectInput("ecolor", "Edge Color",
                               choices = c("Green"=1,
                                           "Red"=2,
                                           "Blue"=3,
                                           "Pink"=4,
                                           "Yellow"=5,
                                           "Orange"=6,
                                           "Magenta"=7),
                               selected = 4),
                        sliderInput("esize", "Edge Size",
                               min = 1, max = 10, value = 5, step = 1))),
            column(width = 9,
                   box(width = "100%", 
                       height="715px",
                       background = "light-blue",
                       h2("Amount of movies distributed by the same company", align = "center"),
            plotOutput("plot1", width = "100%",height = "630px"))),
                   br(),
                   br(),
                   br(),
                   br(),
                   h6("Livanos Nikolaos", align = "right")
            ),
    tabItem(tabName = "Bar",
            column(width = 3,
                   box(width= "100%",
                       collapsible = T,
                       background = "maroon",
                       radioButtons ("barsel", "Select Chart",
                                    choices = list("Amount of commonly distributed movies" = 1,
                                                   "Percentage of commonly distributed movies" = 2),
                                    selected = 1)
                       ),
                   
                   box(width = "100%", 
                       collapsible = T,
                       background = "light-blue",
                       checkboxGroupInput ("barplot", "Select Company",
                                           choices = list("20th Century Fox" = 1, 
                                                          "Disney" = 2,
                                                          "DreamWorks" = 3,
                                                          "Focus Features" = 4,
                                                          "Fox Searchlight" = 5,
                                                          "Lionsgate" = 6,
                                                          "Metro Goldwyn Mayer" = 7,
                                                          "Miramax" = 8,
                                                          "New Line Cinema" = 9,
                                                          "Paramount" = 10,
                                                          "Relativity Media" = 11,
                                                          "Sony Pictures" = 12,
                                                          "Summit Entertainment" = 13,
                                                          "Universal" = 14,
                                                          "Warner Bros." = 15,
                                                          "Weinstein" = 16),
                                           selected = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)))
            ),
            column(width = 9,
                   box(width = "100%",
                       height = "715px", 
                       background = "light-blue",
                       h2("Commonly Distributed Movies", align = "center"),
                       plotOutput("barplot",width = "100%",height = "630px")
                   )
            ),
            br(),
            br(),
            br(),
            br(),
            h6("Livanos Nikolaos", align = "right")
            ),
    tabItem(tabName = "Charts",
            column(width = 3,
                   box(width = "100%", background = "maroon",
                       collapsible = T,
                        sliderInput("spin", "Spin (Degrees)",
                                    min = 1, max = 360, value = 180, step = 1),
                        sliderInput("radius", "Radius",
                                    min = 0, max = 1, value = 0.8, step = 0.1)),
                   box(width = "100%", background = "blue",
                       collapsible = T,
                       checkboxInput("norm", "Normalized Values for:", FALSE),
                       br(),
                       selectInput("piedist", "Select Company",
                                   choices = list("20th Century Fox" = 1, 
                                                  "Disney" = 2,
                                                  "DreamWorks" = 3,
                                                  "Focus Features" = 4,
                                                  "Fox Searchlight" = 5,
                                                  "Lionsgate" = 6,
                                                  "Metro Goldwyn Mayer" = 7,
                                                  "Miramax" = 8,
                                                  "New Line Cinema" = 9,
                                                  "Paramount" = 10,
                                                  "Relativity Media" = 11,
                                                  "Sony Pictures" = 12,
                                                  "Summit Entertainment" = 13,
                                                  "Universal" = 14,
                                                  "Warner Bros." = 15,
                                                  "Weinstein" = 16),
                                   selected = 1)
                   ), 
                   checkboxInput("threeD", "3D", FALSE),
                   conditionalPanel(condition="input.threeD == true",
                      box(width = "100%", background = "purple",
                          collapsible = T,
                            sliderInput("explode", "Explode (3D only)",
                                   min = 0, max = 10, value = 5, step = 1),
                            sliderInput("flip", "Flip (3D only)",
                                   min = 0, max = 6, value = 3, step = 1))
                   )
                   ), 
            column(width=9,
                   box(width = "100%", 
                       height="715px",
                       background = "light-blue",
                       h2("Amount of movies distributed by the same company", align = "center"),
            plotOutput("plot2", width = "100%", height = "630px"))),
            br(),
            br(),
            br(),
            br(),
            h6("Livanos Nikolaos", align = "right")
                  
            ),
    tabItem(tabName = "Matrices",
            column(width = 5,
                   selectInput("dist", "Select Company",
                               choices = list("20th Century Fox" = 1, 
                                              "Disney" = 2,
                                              "DreamWorks" = 3,
                                              "Focus Features" = 4,
                                              "Fox Searchlight" = 5,
                                              "Lionsgate" = 6,
                                              "Metro Goldwyn Mayer" = 7,
                                              "Miramax" = 8,
                                              "New Line Cinema" = 9,
                                              "Paramount" = 10,
                                              "Relativity Media" = 11,
                                              "Sony Pictures" = 12,
                                              "Summit Entertainment" = 13,
                                              "Universal" = 14,
                                              "Warner Bros." = 15,
                                              "Weinstein" = 16,
                                              "All Companies" = 17),
                               selected = 17)
                  ),
            column(width = 5,
                   selectInput("year", "Select Year",
                               choices = list("2000" = 1, 
                                              "2001" = 2,
                                              "2002" = 3,
                                              "2003" = 4,
                                              "2004" = 5,
                                              "2005" = 6,
                                              "2006" = 7,
                                              "2007" = 8,
                                              "2008" = 9,
                                              "2009" = 10,
                                              "2010" = 11,
                                              "2011" = 12,
                                              "2012" = 13,
                                              "2013" = 14,
                                              "2014" = 15,
                                              "All Years" = 16),
                               selected = 16)
                  ),
            DT::dataTableOutput("table1"),
            downloadButton('downloadData', 'Download'),
            h6("Livanos Nikolaos", align = "right")
            ),
    tabItem(tabName = "Lines",
            column(width = 3,
                box(width = "100%", 
                    collapsible = T,
                    background = "light-blue",
            checkboxGroupInput ("lineplot", "Select Company",
                        choices = list("20th Century Fox" = 1, 
                                       "Disney" = 2,
                                       "DreamWorks" = 3,
                                       "Focus Features" = 4,
                                       "Fox Searchlight" = 5,
                                       "Lionsgate" = 6,
                                       "Metro Goldwyn Mayer" = 7,
                                       "Miramax" = 8,
                                       "New Line Cinema" = 9,
                                       "Paramount" = 10,
                                       "Relativity Media" = 11,
                                       "Sony Pictures" = 12,
                                       "Summit Entertainment" = 13,
                                       "Universal" = 14,
                                       "Warner Bros." = 15,
                                       "Weinstein" = 16),
                        selected = 1))
            ),
            column(width = 9,
                   box(width = "100%",
                       height = "630px", 
                       background = "light-blue",
                       h2("Income ($) per Year", align = "center"),
                   plotOutput("lineplot",width = "100%",height = "550px")
                  )
                     ),
            br(),
            br(),
            br(),
            br(),
            h6("Livanos Nikolaos", align = "right")
            ),
    tabItem(tabName = "Stat",
            box(width = "60%", 
                height = "80px", 
                align = "center",
                background = "orange",
                column(width=8,
                       selectInput("selstat", "Select Statistic",
                                   choices = c("Cross Correlation Latency Zero"=1,
                                               "Cross Correlation Latency One"=2,
                                               "Mutual Information"=3,
                                               "Granger Causality"=4),
                                   selected = 1)
                       ),
                column(width=4,
                       checkboxInput("pvalue", "Delete Statistically Insignificant Edges", FALSE)
                       )
                ), 
            
                column(width = 3,
                   box(width = "100%", background = "maroon",
                       collapsible = T,
                       selectInput("layout2", "Layout",
                                   choices = c("Fruchterman-Reingold",
                                               "Circle",
                                               "Sphere",
                                               "Kamada-Kawai"),
                                   selected = "Circle"),
                       sliderInput("edgecut2", "Delete Weak Links",
                                   min = 1, max = 5, value = 0, step = 1)),
                   box(width = "100%", background = "blue",
                       collapsible = T,
                       selectInput("vcolor2", "Vertex Color",
                                   choices = c("Green"=1,
                                               "Red"=2,
                                               "Blue"=3,
                                               "Pink"=4,
                                               "Yellow"=5,
                                               "Orange"=6,
                                               "Magenta"=7),
                                   selected = 3),
                       sliderInput("vsize2", "Vertex Size",
                                   min = 1, max = 25, value = 15, step = 1)),
                   box(width = "100%", background = "purple",
                       collapsible = T,
                       selectInput("ecolor2", "Edge Color",
                                   choices = c("Green"=1,
                                               "Red"=2,
                                               "Blue"=3,
                                               "Pink"=4,
                                               "Yellow"=5,
                                               "Orange"=6,
                                               "Magenta"=7),
                                   selected = 4),
                       sliderInput("esize2", "Edge Size",
                                   min = 1, max = 10, value = 5, step = 1))
                   ),
            column(width = 9,
                   box(width = "100%", 
                       height="650px",
                       background = "light-blue",
                   plotOutput("Statplot",width = "100%",height = "630px"))
            ),
            h6("Livanos Nikolaos", align = "right")
            
            
            
            )
           
        )
      )
)

server <- function(input, output) { 
  
  output$table1<-DT::renderDataTable(All[[as.numeric(input$dist)]][[as.numeric(input$year)]], server = FALSE)


  output$downloadData <- downloadHandler(
    filename = function() {
      paste(DistNames2[as.numeric(input$dist)]," ",Years2[as.numeric(input$year)]," ", Sys.Date(), '.csv', sep='')
    },
    content = function(file){
    write.csv(All[[as.numeric(input$dist)]][[as.numeric(input$year)]], file)
    }
  )

observe (if (input$barsel==1) {  
  observe (if (length(input$barplot)==0) {
  
    output$barplot<-renderPlot ({
      plot(Years,Profit[,2], type="n",
           xaxt="n",
           xlab=" ",
           ylab=" ",
           ylim=c(0,200))
      abline(h=seq(0,200,by=10),lty="dotted",col="gray")
   })
  
  } else {
    output$barplot<-renderPlot ({
      barp(as.numeric(piesum[as.numeric(input$barplot)]),
           ylim=c(0,200),
           ylab="Amount of movies",
           names.arg= DistNames[as.numeric(input$barplot)],
           cex.axis = 0.8,
           col=colors[as.numeric(input$barplot)],
           staxx = T,
           srt = 30,
           do.first = abline(h=seq(0,200,by=10),lty="dotted",col="gray")
      )
      legend("topleft", 
             xpd=TRUE,
             legend=DistNames[as.numeric(input$barplot)], 
             lwd=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), 
             col=colors[as.numeric(input$barplot)],
             cex=0.6)
    })    
  }  
  )  
} else if (input$barsel==2) {
  observe (if (length(input$barplot)==0) {
    
    output$barplot<-renderPlot ({
      plot(Years,Profit[,2], type="n",
           xaxt="n",
           xlab=" ",
           ylab=" ",
           ylim=c(0,100))
      abline(h=seq(0,100,by=10),lty="dotted",col="gray")
    })
    
  } else {
    output$barplot<-renderPlot ({
      barp(as.numeric(normsum[[1]][as.numeric(input$barplot)]),
           ylim=c(0,100),
           ylab="Percentage (%) of movies",
           names.arg= DistNames[as.numeric(input$barplot)],
           cex.axis = 0.8,
           col=colors[as.numeric(input$barplot)],
           staxx = T,
           srt = 30,
           do.first = abline(h=seq(0,100,by=10),lty="dotted",col="gray")
      )
      legend("topleft", 
             xpd=TRUE,
             legend=DistNames[as.numeric(input$barplot)], 
             lwd=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), 
             col=colors[as.numeric(input$barplot)],
             cex=0.6)
    })    
  }  
  )   
}
)
  observe (if (input$threeD == FALSE) {
    
    observe (if (input$norm == FALSE) {
    
      output$plot2 <- renderPlot ({
      
        pie(pie[[1]],pieDistNames1,
            main="Amount of commonly distributed movies",
            edges=200,
            radius=as.numeric(input$radius),
            init.angle = as.numeric(input$spin),
            col=colors)
    })
    
    } else if (input$norm == TRUE) {
    
      output$plot2 <- renderPlot ({
      
        pie(pie[[2]][[as.numeric(input$piedist)]],
            normsum1[[as.numeric(input$piedist)]],
            main="Percentage of commonly distributed movies",
            edges=200,
            radius=as.numeric(input$radius),
            init.angle = as.numeric(input$spin),
            col=c("firebrick","forestgreen"))
        legend("topright", 
               xpd=TRUE,
               legend=words,
               lwd=c(2,2),
               col=c("firebrick","forestgreen"),
               cex=0.8)
    })
    
    })
  } else if (input$threeD == TRUE) {
    
    observe (if (input$norm == FALSE) {
      
      output$plot2 <- renderPlot ({
        
        pie3D(pie[[1]],
              main="Amount of commonly distributed movies",
              labels = pieDistNames1,
              labelcex=0.8,
              edges=200,
              height=0.08,
              radius=as.numeric(input$radius),
              start=as.numeric(input$spin)*pi/180,
              theta=as.numeric(input$flip)*pi/12,
              explode=as.numeric(input$explode)/60,
              shade = 0.4,
              col=colors)
        
      })
      
    } else if (input$norm == TRUE) {
      
      output$plot2 <- renderPlot ({
        
        pie3D(pie[[2]][[as.numeric(input$piedist)]],
              main="Percentage of commonly distributed movies",
              labels = normsum1[[as.numeric(input$piedist)]],
              labelcex=0.8,
              edges=200,
              height=0.08,
              radius=as.numeric(input$radius),
              start=as.numeric(input$spin)*pi/180,
              theta=as.numeric(input$flip)*pi/12,
              explode=as.numeric(input$explode)/60,
              shade = 0.4,
              col=c("firebrick","forestgreen"))
        legend("topright", 
               xpd=TRUE,
               legend=words,
               lwd=c(2,2),
               col=c("firebrick","forestgreen"),
               cex=0.8)
      })
      
    })
    
  })

  observe (if (length(input$lineplot)==0) {
    
    output$lineplot<-renderPlot ({
      plot(Years,Profit[,1], type="n", xlab="Year", ylab="Income",
           xaxt="n",
           ylim=c(-10000000,5000000000))
      axis(1, at = Years,las=2)
      abline(v=seq(2000, 2014),h=seq(0,5000000000,by=1000000000),lty="dotted",col="gray")
    })

  } else if (length(input$lineplot)==1) {
  
    output$lineplot<-renderPlot ({
      plot(Years,Profit[,as.numeric(input$lineplot)],
           type="l", lwd=3, 
           col=colors[as.numeric(input$lineplot)], 
           xlab="Year", ylab="Earnings",
           xaxt="n",
           ylim=c(-10000000,5000000000),
           panel.first = abline(v=seq(2000, 2014),h=seq(0,5000000000,by=1000000000),lty="dotted",col="gray"))
      axis(1, at = seq(2000, 2014),las=2) 
      legend("topleft", 
             xpd=TRUE,
             legend=DistNames[as.numeric(input$lineplot)], 
             lwd=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), 
             col=colors[as.numeric(input$lineplot)],
             cex=0.7)
    })    
    
  } else {
    
    output$lineplot<-renderPlot ({
      plot(Years,Profit[,as.numeric(input$lineplot[1])], 
           type="l", 
           lwd=3, 
           col=colors[as.numeric(input$lineplot[1])], 
           xlab="Year", ylab="Earnings",
           xaxt="n",
           ylim=c(-10000000,5000000000),
           panel.first = abline(v=seq(2000, 2014),h=seq(0,5000000000,by=1000000000),lty="dotted",col="gray"))
      axis(1, at = seq(2000, 2014),las=2) 
      abline(v=seq(2000, 2014),h=seq(0,5000000000,by=1000000000),lty="dotted",col="gray")
      legend("topleft", 
             xpd=TRUE,
             legend=DistNames[as.numeric(input$lineplot)], 
             lwd=c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2), 
             col=colors[as.numeric(input$lineplot)],
             cex=0.7)
      for (i in 2:as.numeric(length(input$lineplot))) {
        lines(Years,Profit[,as.numeric(input$lineplot[i])], lwd=3, 
              col=colors[as.numeric(input$lineplot[i])])
      }
    })
  })

  
  observe (if (input$layout=="Circle"){
    
    output$plot1<-renderPlot({
      plot.igraph(Graphs[[as.numeric(input$edgecut)]],
                  layout=layout.circle,
                  vertex.color=GraphColors[as.numeric(input$vcolor)],
                  vertex.size=input$vsize,
                  vertex.label=DistNames,
                  vertex.label.color="black",
                  vertex.label.cex=1.5, 
                  edge.color=GraphColors[as.numeric(input$ecolor)],
                  edge.width=input$esize*E(Graphs[[as.numeric(input$edgecut)]])$weight/15)
    })

  }else if(input$layout=="Fruchterman-Reingold"){
    
    output$plot1<-renderPlot({
      plot.igraph(Graphs[[as.numeric(input$edgecut)]],
                  layout=layout.fruchterman.reingold,
                  vertex.color=GraphColors[as.numeric(input$vcolor)],
                  vertex.size=input$vsize,
                  vertex.label=DistNames,
                  vertex.label.color="black",
                  vertex.label.cex=1.5, 
                  edge.color=GraphColors[as.numeric(input$ecolor)],
                  edge.width=input$esize*E(Graphs[[as.numeric(input$edgecut)]])$weight/15)
    })
    
  }else if(input$layout=="Sphere"){
    
    output$plot1<-renderPlot({
      plot.igraph(Graphs[[as.numeric(input$edgecut)]],
                  layout=layout.sphere,
                  vertex.color=GraphColors[as.numeric(input$vcolor)],
                  vertex.size=input$vsize,
                  vertex.label=DistNames,
                  vertex.label.color="black",
                  vertex.label.cex=1.5, 
                  edge.color=GraphColors[as.numeric(input$ecolor)],
                  edge.width=input$esize*E(Graphs[[as.numeric(input$edgecut)]])$weight/15)
    })
    
  }else if(input$layout=="Kamada-Kawai"){
    
    output$plot1<-renderPlot({
      plot.igraph(Graphs[[as.numeric(input$edgecut)]],
                  layout=layout.kamada.kawai,
                  vertex.color=GraphColors[as.numeric(input$vcolor)],
                  vertex.size=input$vsize,
                  vertex.label=DistNames,
                  vertex.label.color="black",
                  vertex.label.cex=1.5, 
                  edge.color=GraphColors[as.numeric(input$ecolor)],
                  edge.width=input$esize*E(Graphs[[as.numeric(input$edgecut)]])$weight/15)
    })
  }
  )
 # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  observe (if (input$pvalue==FALSE) {
    
    observe (if (input$layout2=="Circle" ){
      
      output$Statplot<-renderPlot({
        plot.igraph(GraphsStat[[as.numeric(input$selstat)]][[1]][[as.numeric(input$edgecut2)]],
                    layout=layout.circle,
                    vertex.color=GraphColors[as.numeric(input$vcolor2)],
                    vertex.size=input$vsize2,
                    vertex.label=DistNames,
                    vertex.label.color="black",
                    vertex.label.cex=1.5, 
                    edge.color=GraphColors[as.numeric(input$ecolor2)],
                    edge.width=input$esize2*E(GraphsStat[[as.numeric(input$selstat)]][[1]][[as.numeric(input$edgecut2)]])$weight/7)
      })
    
    }else if(input$layout2=="Fruchterman-Reingold"){
    
      output$Statplot<-renderPlot({
        plot.igraph(GraphsStat[[as.numeric(input$selstat)]][[1]][[as.numeric(input$edgecut2)]],
                    layout=layout.fruchterman.reingold,
                    vertex.color=GraphColors[as.numeric(input$vcolor2)],
                    vertex.size=input$vsize2,
                    vertex.label=DistNames,
                    vertex.label.color="black",
                    vertex.label.cex=1.5, 
                    edge.color=GraphColors[as.numeric(input$ecolor2)],
                    edge.width=input$esize2*E(GraphsStat[[as.numeric(input$selstat)]][[1]][[as.numeric(input$edgecut2)]])$weight/7)
      })
    
    }else if(input$layout2=="Sphere"){
    
      output$Statplot<-renderPlot({
        plot.igraph(GraphsStat[[as.numeric(input$selstat)]][[1]][[as.numeric(input$edgecut2)]],
                    layout=layout.sphere,
                    vertex.color=GraphColors[as.numeric(input$vcolor2)],
                    vertex.size=input$vsize2,
                    vertex.label=DistNames,
                    vertex.label.color="black",
                    vertex.label.cex=1.5, 
                    edge.color=GraphColors[as.numeric(input$ecolor2)],
                    edge.width=input$esize2*E(GraphsStat[[as.numeric(input$selstat)]][[1]][[as.numeric(input$edgecut2)]])$weight/7)
      })
    
    }else if(input$layout2=="Kamada-Kawai"){
    
      output$Statplot<-renderPlot({
        plot.igraph(GraphsStat[[as.numeric(input$selstat)]][[1]][[as.numeric(input$edgecut2)]],
                    layout=layout.kamada.kawai,
                    vertex.color=GraphColors[as.numeric(input$vcolor2)],
                    vertex.size=input$vsize2,
                    vertex.label=DistNames,
                    vertex.label.color="black",
                    vertex.label.cex=1.5, 
                    edge.color=GraphColors[as.numeric(input$ecolor2)],
                    edge.width=input$esize2*E(GraphsStat[[as.numeric(input$selstat)]][[1]][[as.numeric(input$edgecut2)]])$weight/7)
      })
    }
    )
    
  }else if(input$pvalue==TRUE) {
    
    observe (if (input$layout2=="Circle" ){
      
      output$Statplot<-renderPlot({
        plot.igraph(GraphsStat[[as.numeric(input$selstat)]][[2]][[as.numeric(input$edgecut2)]],
                    layout=layout.circle,
                    vertex.color=GraphColors[as.numeric(input$vcolor2)],
                    vertex.size=input$vsize2,
                    vertex.label=DistNames,
                    vertex.label.color="black",
                    vertex.label.cex=1.5, 
                    edge.color=GraphColors[as.numeric(input$ecolor2)],
                    edge.width=input$esize2*E(GraphsStat[[as.numeric(input$selstat)]][[2]][[as.numeric(input$edgecut2)]])$weight/7)
      })
      
    }else if(input$layout2=="Fruchterman-Reingold"){
      
      output$Statplot<-renderPlot({
        plot.igraph(GraphsStat[[as.numeric(input$selstat)]][[2]][[as.numeric(input$edgecut2)]],
                    layout=layout.fruchterman.reingold,
                    vertex.color=GraphColors[as.numeric(input$vcolor2)],
                    vertex.size=input$vsize2,
                    vertex.label=DistNames,
                    vertex.label.color="black",
                    vertex.label.cex=1.5, 
                    edge.color=GraphColors[as.numeric(input$ecolor2)],
                    edge.width=input$esize2*E(GraphsStat[[as.numeric(input$selstat)]][[2]][[as.numeric(input$edgecut2)]])$weight/7)
      })
      
    }else if(input$layout2=="Sphere"){
      
      output$Statplot<-renderPlot({
        plot.igraph(GraphsStat[[as.numeric(input$selstat)]][[2]][[as.numeric(input$edgecut2)]],
                    layout=layout.sphere,
                    vertex.color=GraphColors[as.numeric(input$vcolor2)],
                    vertex.size=input$vsize2,
                    vertex.label=DistNames,
                    vertex.label.color="black",
                    vertex.label.cex=1.5, 
                    edge.color=GraphColors[as.numeric(input$ecolor2)],
                    edge.width=input$esize2*E(GraphsStat[[as.numeric(input$selstat)]][[2]][[as.numeric(input$edgecut2)]])$weight/7)
      })
      
    }else if(input$layout2=="Kamada-Kawai"){
      
      output$Statplot<-renderPlot({
        plot.igraph(GraphsStat[[as.numeric(input$selstat)]][[2]][[as.numeric(input$edgecut2)]],
                    layout=layout.kamada.kawai,
                    vertex.color=GraphColors[as.numeric(input$vcolor2)],
                    vertex.size=input$vsize2,
                    vertex.label=DistNames,
                    vertex.label.color="black",
                    vertex.label.cex=1.5, 
                    edge.color=GraphColors[as.numeric(input$ecolor2)],
                    edge.width=input$esize2*E(GraphsStat[[as.numeric(input$selstat)]][[2]][[as.numeric(input$edgecut2)]])$weight/7)
      })
    }
    )
  }
  )  
  }

shinyApp(ui, server)