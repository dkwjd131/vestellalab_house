
library(shiny)
library(leaflet)
library(plyr)
library(dplyr)
library(lubridate)
library(ggmap)
library(sqldf)
library(devtools)
library(DT)

ui<-fluidPage(
  sidebarPanel(
    column(6,
           
           # Copy the line below to make a slider range 
           sliderInput("st", label = h3("st"), min = 0, 
                       max = 24, value = c(7, 10))
    ),
    column(6,
           
           # Copy the line below to make a slider range 
           sliderInput("ed", label = h3("ed"), min = 0, 
                       max = 24, value = c(17, 20))
    ),
    hr(),
    textInput("x_coord", label = h3("x"), value = "Enter x coord"),
    textInput("y_coord", label = h3("y"), value = "Enter y coord"),
    selectInput("max_t", label = h3("max time"), 
                choices = list("0" = 0,"1" = 1, "2" = 2, "3" = 3,"4" = 4),
                selected = 0),
    textInput("max_dur", label = h3("max dur"), value = "Enter Max Duration (minute)"),
    DTOutput('dt'),
    actionButton("button1","open map"),
    leafletOutput(outputId = "mymap"),
    actionButton("button2","Submit")
  ),
  mainPanel(
    leafletOutput(outputId = "mymap2")
  )
)

server<- function(input, output){
  
  type_color<-colorFactor("Set1",STN$TYPE)
  type_color_2<-colorFactor(palette = c("red","blue","green","black","yellow","cyan","orange"),levels = c("A","B","C","D","E","F","Z"))
  
  #
  reactive_data <- eventReactive(input$button1, {
    leaflet(STN) %>% addTiles() %>% addCircles(data=STN%>% filter(TYPE=="BUS"),lng = ~Y_COORD, lat=~X_COORD,group="BUS",color=~type_color(TYPE),popup = ~CODE_NAME) %>% addCircles(data=STN%>% filter(TYPE=="SUB"),lng = ~Y_COORD, lat=~X_COORD,group="SUB",color=~type_color(TYPE),popup = ~CODE_NAME)%>%addLayersControl(overlayGroups = c("BUS","SUB"),options = layersControlOptions(collapsed = FALSE))
  })
  
  output$mymap <- renderLeaflet({
    reactive_data()
  })
  
  DTproxy<-dataTableProxy("dt")
  output$dt = renderDT(STN)
  
  output$range<-renderPrint({ input$st })
  output$range<-renderPrint({ input$ed })
  
  reactive_data2 <- eventReactive(input$button2, {
    
    st_table <- r0401[between((as.numeric(hour(r0401$BGNG_RIDE_DTM))*60 + as.numeric(minute(r0401$BGNG_RIDE_DTM))),
                               as.numeric(input$st[1])*60, as.numeric(input$st[2])*60),]
    
    ed_table<- r0401[between((as.numeric(hour(r0401$BGNG_RIDE_DTM))*60 + as.numeric(minute(r0401$BGNG_RIDE_DTM))),
                             as.numeric(input$ed[1])*60, as.numeric(input$ed[2])*60),]
    
    isolate({
      
      st_table <- head(st_table, 10)
      
      leaflet(r0401) %>% addTiles() %>% addCircles(data=st_table,lng = ~RIDE_Y, lat=~RIDE_X)
      
    })
  })
  
  output$mymap2 <- renderLeaflet({
    reactive_data2()
  })
  
}

shinyApp(ui,server)

