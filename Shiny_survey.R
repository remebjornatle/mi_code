

####find path for R####
.libPaths("C:/R")
library(haven)
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)


df <- read_sav("C:/Users/t130054/Desktop/df17.sav")

questions = df %>% 
  select(matches('Q0|hcountry')) %>% 
  select_if(is.numeric)

test = gather(questions,key = survey_q, value = score, -hcountry)

u.n <-  as.character(unique(test$survey_q))

ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput('var','Question',choices =u.n)
    ),
    mainPanel(
      plotOutput('distPlot')  
    )
  )
))


server <- shinyServer(function(input,output,session){
  
  output$distPlot <- renderPlot({
    label = questions %>% 
      select(matches(input$var))
    
    colnames(label) = "z"
    
    question.labels = attr(label$z,"label")
    
    newdata <- subset(test, test$survey_q == input$var)
    
    p1 = ggplot(newdata, aes(x=score),environment = environment()) +
      ggtitle(question.labels) +
      geom_bar() +
      theme_bw()
    
    p2 = ggplot(newdata, aes(x=score),environment = environment()) +
      ggtitle(question.labels) +
      geom_bar() +
      theme_bw() +
      facet_wrap(~hcountry)
    
    grid.arrange(p1,p2, ncol=1)
    
  })
})

shinyApp(ui=ui,server = server)