library(shiny)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)
library(png)
library(gganimate)

#set wd
setwd('C:/Users/JP/Documents/codstats/wwii_structured')
img <- readPNG('cwl-data/maps/ww2/london_docks.png')
#make df to put output in
op <- data.frame()

# Go through all games all events
location <- list.files(path = 'data', pattern = "structured")
for(j in 1:length(location)) {
  
  filenames <- list.files(path = paste('data/',location[j], sep = ""),pattern="*.json", full.names=TRUE)
  print(location[j]) # show when you start each new event
  
  
  for (i in 1:length(filenames)) {
    
    #read each json file in as list of lists
    data_json <- fromJSON(filenames[i], simplifyVector = T)
    # I filter out by mode,
    # but you can change this to whatever
    if(data_json$mode == "Hardpoint" & !is.null(nrow(data_json$events))){
      
      # get just spawns and deaths
      events <- (data_json$events)
      data <- subset(events, events$type == 'death')
      
      # make df of players and teams to match 
      team_players <- data.frame(name = data_json$players$name, player.team = data_json$players$team, gun = data_json$players$fave_weapon, map=data_json$map)
      
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      data = do.call(cbind.data.frame, data)
      
      start_time=5000
      
      # Split by hp and set
      if (length(data_json$hp_hill_names)==4) {
        hp <- rep(seq(1,4),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        set <- rep(seq(1,4),each = 60*1000*4)
        df <- data.frame(time,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
      } else {
        hp <- rep(seq(1,5),4, each = 60*1000)
        time <- seq(start_time, start_time+length(hp)-1) # shift 10 secs to account for rotation
        set <- rep(seq(1,5),each = 60*1000*4)
        df <- data.frame(time,hp, set)
        data = merge(data, df, by.x = 'time_ms', by.y = 'time')
      }
      data<-merge(data, team_players, by.x ='data.attacker.id', by.y = 'name')
      op<-rbind(op,data)
    }}}

ui<-fluidPage(
  titlePanel("Call Of Duty Data Demo",windowTitle = "COD-Demo"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mapInput","Map",choices=c("All","Sainte Marie du Mont","London Docks","Ardennes Forest","Gibraltar","Valkyrie"),selected="All"),
      selectInput("playerInput","Player",choices=unique(op$data.attacker.id)),
      selectInput("opponentInput","Opponent",choices=unique(op$data.id))),
      mainPanel(htmlOutput("record"),
              br(),br(),
              tableOutput("results"))
  )
)
server <- function(session,input, output) {
  filtered<-reactive(
    op%>%
      filter(
        if(input$mapInput!="All"){map==input$mapInput}else{map!=input$mapInput},
        data.attacker.id==input$playerInput | data.id==input$playerInput,
        data.id==input$opponentInput | data.attacker.id==input$opponentInput))
  
  output$record<-renderText({    
    x<-(sum(filtered()$data.attacker.id==input$playerInput))
    x1<-(sum(filtered()$data.attacker.id==input$opponentInput))
    xa<-(x/x1)
    xr <- round(xa,2)
    paste0("<b>Record: ","</b>",x,"-",x1," (",xr,")")
  })
}
shinyApp(ui = ui, server = server)
