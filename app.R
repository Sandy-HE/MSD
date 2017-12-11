#
# This is a Shiny web application that is used for data exploratory analysis for music emotion.
#
# Author: Sandy HE
#

library(shiny)
library(tidyverse)
library(ggvis)

#==== collect and tidy data ====
songsData <- read_csv("./songsdata.csv")
tagsData <- read_csv("./emotiondata.csv")
genreData <- read_csv("./genredata.csv")

data_tempo <- songsData$tempo
data_loudness <- songsData$loudness
allData <- merge(songsData, tagsData,by="track_id")
allData <- merge(allData, genreData, by="track_id",all.x = TRUE)

#==== javascript for visualization ====
jscode <- 
  "$(function() {
$('#plot').click(function(){ $('#ggvis-tooltip').hide(); });
})"

#==== Define UI for application ====
ui <- fluidPage(
  
  tags$script(jscode),
  
  # Application title
  titlePanel("Emotion Show"),
  
  # Sidebar with a series of parameters setting 
  sidebarLayout(
    
    position="right",
    sidebarPanel(
      wellPanel(
        h4("Number of songs"),
        sliderInput("num", "Number of songs", min = 1, max = nrow(allData),
                    value = c(1,as.integer(nrow(allData)/2)), step = 1),
        sliderInput("tempo", "tempo", min = min(data_tempo), max = max(data_tempo),
                    value = c(min(data_tempo),mean(data_tempo)), step = 1),
        sliderInput("loudness", "loudness", min = min(data_loudness), max = max(data_loudness),
                    value = c(mean(data_loudness),max(data_loudness)), step = 1.001)
      ),
      wellPanel(
        h4("Emotion Type Selection"),
        checkboxGroupInput("emotiontype", "Select emotion type you want to observe",
                           #choices = list("Happy" = "happy", "Sad" = "sad","Angry"="angry","Relax"="relax","Sleep"="sleep")
                           unique(allData$tag_name)
                           ),
        h5("No option means all options")
      ),
      wellPanel(
        checkboxGroupInput("infotype", "More Features",
                           choices = list("Genre" = "genre")
                           )
      )
    ),
  
    mainPanel(
    
      # two output, one for ggplot2, one for ggvis
      plotOutput(outputId ="distPlot"),
      ggvisOutput("plot")
    )
  )
)

#==== Define server logic ====
server <- function(input, output) {
  
  aData <- reactive({
    
    #sidebar filter
    min_num <- input$num[1]
    max_num <- input$num[2]
    min_tempo <- input$tempo[1]
    max_tempo <- input$tempo[2]
    min_loudness <- input$loudness[1]
    max_loudness <- input$loudness[2]
    
    d <- allData[min_num:max_num,] %>%
      filter(
        tempo >= min_tempo,
        tempo <= max_tempo,
        loudness >= min_loudness,
        loudness <= max_loudness
      )%>%
     arrange(tag_name)
    
    #emotion type filter
    if (!is.null(input$emotiontype) && input$emotiontype != "") {
      et <- paste0(input$emotiontype)
      d <- d %>% filter(tag_name %in% et)
    }
    
    d
  })
  
  #ggplot for overall songs distribution
  output$distPlot <- renderPlot({
    ggplot(aData()) + 
      geom_point(mapping = aes(x = tempo, y = loudness))+
      facet_grid(tag_name ~ mode)
 })
  
  #build up hyperlink for each song in the tooltip
  linkInfo <- function(x) {
    if(is.null(x)) return(NULL)
    actsongs <- isolate(aData())
    actsongs <- distinct(actsongs,track_id, .keep_all = TRUE)
    songs <- actsongs[actsongs$track_id == x$track_id, ]
    paste0("<b>", songs$artist_name, "-</b><br>",
           "<a href=\"https://www.youtube.com/results?search_query=",
           songs$artist_name," - ",songs$title,"\">",songs$title,"</a>")
    }  
   
    
   vis <- reactive({
     
     #more features filter, here is just for genre
     if(is.null(input$infotype)){
     aData%>%
      ggvis(~tempo,~loudness,fill = ~tag_name, size= ~weight, key:= ~track_id ) %>%
      layer_points() %>%
      #add_legend(c("size","fill"), title = c("weight","emotion type")) %>%
      add_legend("size", title = c("weight"),properties = legend_props(legend=list(x=469,y=0.5))) %>%
      add_legend("fill", title = c("emotion type"),properties = legend_props(legend=list(x=468,y=100))) %>%
      add_tooltip(linkInfo, "click")}
     else{
       aData%>%
         ggvis(~tempo,~loudness, shape = ~tag_name, size= ~weight, key:= ~track_id) %>%
         layer_points(fill= ~genre) %>%
         #add_legend(c("size","fill"), title = c("weight","emotion type")) %>%
         add_legend("size", title = c("weight"),properties = legend_props(legend=list(x=469,y=0.5))) %>%
         add_legend("shape", title = c("emotion type"),properties = legend_props(legend=list(x=468,y=100))) %>%
         add_legend("fill", title = c("genre"),properties = legend_props(legend=list(x=469,y=180))) %>%
         add_tooltip(linkInfo, "click")}
   })
   
   vis%>% bind_shiny("plot")
    
}


# Run the application 
shinyApp(ui = ui, server = server)

