library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(ggplot2)
library(shiny)
library(hms)
library(DT)
library(stringr)
library(tools)
library(readr)
library(tidyverse)

# Load and clean  data ----------------------------------------------
tweet <- read_csv("Stweets2.csv")
names(tweet) <- str_to_title(names(tweet))
tweet$Candidate<-str_to_title(tweet$Candidate)
tweets_data <- mutate(tweet, Created_at = as.Date(Created_at, format= "%m/%d/%Y"))
tweets_data <- tweets_data %>% rename(Rtweet = `Retweet_no`)
tweets_data <- tweets_data %>% rename(BotP = `Bot_probability`)
tweets_data <- tweets_data %>% rename(Poso = `Pos_sent`)
tweets_data <- tweets_data %>% rename(Nego = `Neg_sent`)

# Avoid plotly issues ----------------------------------------------
pdf(NULL)


# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Bot Analyzer",
                          
                          # Drop down menu with hard coded values ------------------------------
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "New sentiments detected ", 
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 110, color = "green",
                                                "Need to update data")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Government Communication Office ",
                                         message = HTML("Bot Update! <br> Any indiaction bots are increasing in the networ?. "),
                                         icon = icon("exclamation-circle"))
                          )
)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plots", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Holistic Stats", icon = icon("cog"), tabName = "widgets",badgeLabel = "", badgeColor = "green"),
    menuItem("Tweet Table", icon = icon("table"), tabName = "table", badgeLabel = "", badgeColor = "green"),
    
    hr(),
    hr(),
    # Inputs: select variables to plot ----------------------------------------------
    radioButtons(inputId = "selected_type",
                 label = "Select Candidate:",
                 choices = c("Biden", "Trump" ),
                 selected = "Biden"),
    
    
    # Select Tweeter Bot Probability ----------------------------------
    sliderInput(inputId = "bot_prob",
                label = "Bot Probability of the Tweeter:", 
                min = 0, max = 1, 
                value = c(0, 1)),
    
    
    
    # Select Manufacturer for Y-axis ----------------------------------
    sliderInput(inputId = "startdate",
                label = "Select Tweet Date Range:", 
                min = as.Date("2020-10-13"), max = as.Date("2020-10-20"), 
                value = c(as.Date("2020-10-13"), as.Date("2020-10-17")))
    
    
  )
)

# Dashboard body ----------------------------------------------
dash_body <- dashboardBody(tabItems(
  
  
  # Plots page ----------------------------------------------
  tabItem("plot",          
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Distribution by Date", plotlyOutput("plot_dist")),
                   tabPanel("Distribution by Positive - Negative Score", plotlyOutput("plot_NegPos")),
                   tabPanel("Distribution by Sentiment",plotOutput(outputId = "plot_senti")))
          )
  ),
  
  
  # Widgets page ----------------------------------------------
  tabItem("widgets",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("Avg_Pos", width = 4),
            infoBoxOutput("Avg_Neg", width = 4),
            infoBoxOutput("Avg_Neutral", width = 4),
            infoBoxOutput("Avgretweet", width = 4),
            infoBoxOutput("Avg_bot_prob", width = 4),

          )
  ),
  
  
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Filtered Tweet Table", DT::dataTableOutput("table"), width = 12))
  )
)
)

ui <- dashboardPage(header, sidebar, dash_body)


# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  tweets_subset <- reactive({
    tweets <-  tweets_data %>%
      
      # Slider Filter ----------------------------------------------
    filter(Candidate %in% input$selected_type & Created_at >= input$startdate[1] & Created_at <= input$startdate[2] & BotP >= input$bot_prob[1] & BotP <= input$bot_prob[2])
    
    
    # Return dataframe ----------------------------------------------
    return(tweets)
  })
  
  # Reactive melted data ----------------------------------------------
  twtInput <- reactive({
    tweets_subset() %>%
      melt(id = "Candidate")
  })
  
  
  
  # A Barchart for number of tweets by day -----------------------------
  output$plot_dist <- renderPlotly({
    dat <- subset(twtInput(), variable == "Created_at")
    
    # Generate Plot ----------------------------------------------
    ggplot(data = tweets_subset(), aes(x = Created_at)) +
      geom_bar(color = 4, fill = "4") +
      ggtitle("Number of Partisan Tweets") +
      xlab("Date") +
      ylab("Tweet Count")+
      theme_classic()+
      theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.y = element_text(face = "bold"))
    
  })  
  
  # A Scatter Plot showing distribution of each tweet on positive - negative axes -----------------------------  
  output$plot_NegPos <- renderPlotly({
    dat <- subset(twtInput(),  variable == "Positive/Negative")
    
    # Generate Plot ---------------------------------------------- 
    ggplot(data = tweets_subset(), aes_string(x = "Poso", y = "Nego")) +
      theme_classic()+
      theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.y = element_text(face = "bold"))+
      geom_point(color = 4)
    
  })
  
  
  
  # A pie-chart showing distribution by sentiment -----------------------------
  output$plot_senti <- renderPlot({
    
    pie_data <- tweets_subset() %>%
      count(Sentiment) %>% 
      mutate(percent = n/sum(n))
    
    # Generate Plot ---------------------------------------------- 
    ggplot(data = pie_data, aes(x ="", y = percent,  fill = Sentiment)) +
      geom_bar(position = "fill", width = 1, stat = "identity", color = "white") +
      geom_text(aes(x = 1.0, label = scales::percent(percent, accuracy = .1)), position = position_stack(vjust = .5)) +
      coord_polar(theta = "y")+
      theme_void()
    
    
  })
  
  
  # Data table of characters ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(tweets_subset(),select = c(Tweet, Hashtags, Candidate, Rtweet, BotP))
  })
  

  
  # Average Positive mean info box ----------------------------------------------
  output$Avg_Pos <- renderInfoBox({
    twt <- tweets_subset()
    num <- round(mean(twt$Positive, na.rm = T), 2)
    
    infoBox("Average Positivity Score", value = num, subtitle = paste(nrow(twt),"Tweets Filtered"), icon = icon("thumbs-up"), color = "blue")
  })
  
  # Average Negative mean  info box ----------------------------------------------
  output$Avg_Neg <- renderInfoBox({
    twt <- tweets_subset()
    num <- round(mean(twt$Negative, na.rm = T), 2)
    
    infoBox("Average Negativity Score", value = num, subtitle = paste(nrow(twt), "Tweets Filtered"), icon = icon("thumbs-down"), color = "blue")
  })

  # Average Neutral mean  info box ----------------------------------------------
  output$Avg_Neutral <- renderInfoBox({
    twt <- tweets_subset()
    num <- round(mean(twt$Neutral, na.rm = T), 2)
    
    infoBox("Average Neutral Score", value = num, subtitle = paste(nrow(twt), "Tweets Filtered"), icon = icon("arrow-right"), color = "blue")
  })  
  
  
  
  # Average Retweet info box ----------------------------------------------
  output$Avgretweet <- renderInfoBox({
    twt <- tweets_subset()
    num <- round(mean(twt$Rtweet, na.rm = T), 2)
    
    infoBox("Average Retweet ", value = num, subtitle = paste(nrow(twt), "Tweets Filtered"), icon = icon("paper-plane"), color = "blue")
  })

  # Average bot mean  info box ----------------------------------------------
  output$Avg_bot_prob <- renderInfoBox({
    twt <- tweets_subset()
    num <- round(mean(twt$BotP, na.rm = T), 2)
    
    infoBox("Average Bot Probability", value = num, subtitle = paste(nrow(twt), "Tweets Filtered"), icon = icon("arrow-right"), color = "blue")
  })
        
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
