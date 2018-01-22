#
# This is a Shiny web application which shows statistical result of audio features for types of emotion.
# The audio features includes tempo and loudness
# Emotion types includes happy, sad, angry, relax
#
# Author: Sandy HE
# Date: 22/01/2018
#

library(shiny)
library(tidyverse)
library(data.table)

#==== collect and tidy data ====
#songsData <- fread("songsdata.csv")
#alltags <- fread("./alltags.csv")
#allData <- merge(songsData, tagsData,by="track_id")

allData <- fread("./allsongswithtag.csv")
allData <- as.data.frame(allData)


#define confidence interval
ci <- 0.95

#==== Define UI for application ====
ui <- fluidPage(
  # Application title
  titlePanel("Songs Emotion Statistic Show"),
  
  # Sidebar with a series of parameters setting
  sidebarLayout(
    #position="right",
    sidebarPanel(width = 3,
                 
                 wellPanel(
                   h4("Emotion Selection"),
                   checkboxGroupInput(
                     "emotiontype",
                     "Select emotion you want to observe",
                     unique(allData$tag)
                   ),
                   h5("No option means all options")
                 )),
    
    mainPanel(
      width = 9,
      # two output, one for ggplot2, one for ggvis
      fluidRow(
        column(
          width = 6,
          h4("tempo_mean summary"),
          plotOutput("distPlot1", height = 300)
        ),
        column(
          width = 6,
          h4("loudness_mean summary"),
          plotOutput("distPlot2", height = 300)
        )
        
        
      ),
      
      fluidRow(
        br(),
        column(
          width = 6,
          h4("loudness_tempo Mean and Confidence Interval"),
          #ggvisOutput("plot")
          plotOutput("distPlot3", height = 300)
        ),
        column(
          width = 6,
          h4("tempo_loudness Mean and Confidence Interval"),
          plotOutput("distPlot4", height = 300)
        )
      ),
      
      fluidRow(
        br(),
        h4("tempo_loudness distribution for each emotion"),
        plotOutput("distPlot5", height = 300)
      ),
      
      fluidRow(h4("Summary Report"),
               tableOutput("view"))
      
    )
  )
)

#==== Define server logic ====
server <- function(input, output) {
  
  #get dataset based on selected emotion
  #no option cover all emotion types
  aData <- reactive({
    d <- allData
    #emotion type filter
    if (!is.null(input$emotiontype) && input$emotiontype != "") {
      et <- paste0(input$emotiontype)
      d <- allData %>% filter(tag %in% et)
    }
    
    d
  })
  
  #get statistic report
  report <- reactive({
    d <- data.frame(
      aData() %>% dplyr::group_by(tag) %>%
        dplyr::summarize(
          size = n(),
          tempo_m = mean(tempo),
          loudness_m = mean(loudness),
          tempo_md = median(tempo),
          loudness_md = median(loudness),
          tempo_sd = sd(tempo),
          loudness_sd = sd(loudness)
        )
    )
    
    #calculate confidence interval for tempo
    tempo_err <- qnorm(1 - (1 - ci) / 2) * d$tempo_sd / sqrt(d$size)
    d$tempo_cil <- d$tempo_m - tempo_err
    d$tempo_cir <- d$tempo_m + tempo_err
    
    #calculate confidence interval for loudness
    loudness_err <- qnorm(1 - (1 - ci) / 2) * d$loudness_sd / sqrt(d$size)
    d$loudness_cil <- d$loudness_m - loudness_err
    d$loudness_cir <- d$loudness_m + loudness_err
    
    d
  })
  
  
  #ggplot for tempo and its mean for each emotion type
  output$distPlot1 <- renderPlot({
    aData() %>%
      ggplot(aes(x = tag, y = tempo)) +
      geom_jitter(aes(color = tag),alpha = 0.05) +
      stat_summary(
        fun.y = mean,
        colour = "red",
        geom = "point",
        size = 5
      )
  })
  
  #ggplot for loudness and its mean for each emotion type
  output$distPlot2 <- renderPlot({
    aData() %>%
      ggplot(aes(x = tag, y = loudness)) +
      geom_jitter(aes(color = tag),alpha = 0.05) +
      stat_summary(
        fun.y = mean,
        colour = "red",
        geom = "point",
        size = 5
      )
  })
  
  #ggplot for loudness-tempo mean and confidence interval for each emotion type
  output$distPlot3 <- renderPlot({
    report() %>%
      ggplot(aes(x = loudness_m, y = tempo_m, colour = tag)) +
      geom_point() +
      ylab("tempo") +
      xlab("loudness") +
      geom_rect(
        aes(
          ymin = tempo_cil,
          ymax = tempo_cir,
          xmin = loudness_cil,
          xmax = loudness_cir
        ),
        fill = "yellow",
        alpha = 0.2
      )
  })
  
  #ggplot for tempo-loudness mean and confidence interval for each emotion type
  output$distPlot4 <- renderPlot({
    report() %>%
      ggplot(aes(y = loudness_m, x = tempo_m, colour = tag)) +
      geom_point() +
      xlab("tempo") +
      ylab("loudness") +
      geom_rect(
        aes(
          xmin = tempo_cil,
          xmax = tempo_cir,
          ymin = loudness_cil,
          ymax = loudness_cir
        ),
        fill = "yellow",
        alpha = 0.2
      )
  })
  
  #ggplot for songs distribution for each emotion type
  output$distPlot5 <- renderPlot({
    aData() %>%
      ggplot(aes(x = tempo, y = loudness)) +
      geom_point(aes(color = tag), alpha = 0.1) +
      facet_grid(. ~ tag)
  })
  
  # statistic report form
  output$view <- renderTable({
    report()
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
