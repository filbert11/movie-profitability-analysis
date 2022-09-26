library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(matrixStats)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(tidyverse)
library(MASS)
library(data.table)
library(wordcloud2)


#setwd("C:/Users/kohjw/OneDrive/Desktop/Applied Stats with R/Project")
setwd("C:/Users/a/Downloads/Singapore Management University/Courses/ISSS616 Applied Statistical Analysis with R/Project/Files")
movie_data <- read_csv('IMDb movies_ratings_joined_v2.csv')

select <- dplyr::select

names(movie_data)[2] <- "title"
names(movie_data)[6] <- "Genre"
names(movie_data)[7] <- "Duration"
names(movie_data)[9] <- "Continent"
names(movie_data)[15] <- "Rating"
names(movie_data)[17] <- "Budget"
names(movie_data)[19] <- "Profit"
names(movie_data)[20] <- "meta_score"

###################################### Data Cleaning ######################################
n = nrow(movie_data)
p = ncol(movie_data)

movie_data$year <- as.factor(movie_data$year)

movie_data$Profit_million <- round(movie_data$Profit/1000000, -2)
movie_data$Budget_million <- round(movie_data$Budget/1000000, -2)


movie_data <- movie_data %>%
  mutate(profit_in_millions = Profit/1000000)

movie_data <- movie_data %>%
  mutate(budget_in_millions = Budget/1000000)

profit_data<- subset(movie_data, Profit_million > 0)

nn = nrow(profit_data)
pp = ncol(profit_data)

##### Grouping budget and reviews
group4profit <- c('(0,100]', '(100,250]','(250,2600]')
group4budget <- c('(0,100]', '(100,200]','(200,300]', '(300,400]')
group4critics <- c('(0,155]', '(155,237]', '(237,353]', '(353,999]')

new_group <- rep(NA,nn)
##### Grouping Budgets
grouping_budget <- function(budgetlist){
  for(i in 1:length(budgetlist)){
    if(budgetlist[i]>0 && budgetlist[i]<=100){
      new_group[i] = group4budget[1]
    } else if(budgetlist[i]>100 && budgetlist[i]<=200){
      new_group[i] = group4budget[2]
    } else if(budgetlist[i]>200 && budgetlist[i]<=300){
      new_group[i] = group4budget[3]
    } else {
      new_group[i] = group4budget[4]
    }
  }
  return(new_group)
}
new_group2 <- rep(NA,nn)
##### Grouping Critics Reviews
grouping_criticsReviews <- function(reviewlist){
  for(i in 1:length(reviewlist)){
    if(reviewlist[i]>0 && reviewlist[i]<=155){
      new_group2[i] = group4budget[1]
    } else if(reviewlist[i]>155 && reviewlist[i]<=237){
      new_group2[i] = group4budget[2]
    } else if(reviewlist[i]>237 && reviewlist[i]<=353){
      new_group2[i] = group4budget[3]
    }else {
      new_group2[i] = group4budget[4]
    }
  }
  return(new_group2)
}
profit_data$budget_grouping <- grouping_budget(profit_data$Budget_million)
profit_data$critics_grouping <- grouping_criticsReviews(profit_data$reviews_from_critics)

profit_budget <- aggregate(Profit_million~budget_grouping,data=profit_data,FUN=mean)
profit_genre <- aggregate(Profit_million~Genre,data=profit_data,FUN=mean)
profit_critics <- aggregate(Profit_million~critics_grouping,data=profit_data,FUN=mean)

############################# Extracting Names of the Writers, Directors and Actors#########################
######## Writer Extracting 
writers = as.data.frame(matrix(nrow = nrow(profit_data), ncol = 2))
ww_names = NA
for(i in 1:nrow(profit_data)){
  writers$V1[i] = strsplit(profit_data$writer[i], ",")
  for (w in 1:length(writers$V1[[i]])){
    ww_names = append(ww_names, writers$V1[[i]][w])
  }
}
ww_namesSort<-sort(table(ww_names),decreasing = TRUE)[1:500]
ww_namesSort=as.data.frame(ww_namesSort)

######## Actor Extracting
Actors = as.data.frame(matrix(nrow = nrow(profit_data), ncol = 2))
aa_names = NA
for(i in 1:nrow(profit_data)){
  Actors$V1[i] = strsplit(profit_data$actors[i], ",")
  for (w in 1:length(Actors$V1[[i]])){
    aa_names = append(aa_names, Actors$V1[[i]][w])
  }
}
aa_namesSort<-sort(table(aa_names),decreasing = TRUE)[1:500]
aa_namesSort=as.data.frame(aa_namesSort)

######## Director Extracting
directors = as.data.frame(matrix(nrow = nrow(profit_data), ncol = 2))
dd_names = NA
for(i in 1:nrow(profit_data)){
  directors$V1[i] = strsplit(profit_data$director[i], ",")
  for (w in 1:length(directors$V1[[i]])){
    dd_names = append(dd_names, directors$V1[[i]][w])
  }
}
dd_namesSort<-sort(table(dd_names),decreasing = TRUE)[1:500]
dd_namesSort=as.data.frame(dd_namesSort)

############################################ Shiny App Design ########################

sidebar <- dashboardSidebar(width = 300,
  sidebarMenu(
    menuItem("Descriptive1" ,tabName = "Descriptive1", icon = icon("chart-line")),
    menuItem("Descriptive2", tabName = "Descriptive2", icon = icon("globe")),
    menuItem("Inferential" ,tabName = "Inferential", icon = icon("star"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'Descriptive1',
      fluidPage(
        titlePanel("Analyze Movie Parameters Affecting Profit"),
        fluidRow(
          column(
            width=12,
            tabsetPanel(
              tabPanel("Profit by Parameters",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("variable1", "Select Parameter:",
                                       choices = c('Budget' = 'budget_in_millions',
                                                   'Duration' = 'Duration',
                                                   'Average Vote' = 'Rating',
                                                   'Meta Score' = 'meta_score'
                                       )
                           ),
                           selectInput("variable2", "Select Genre:",
                                       choices = c('All' = 'All',
                                                   'Action' = 'Action',
                                                   'Adventure' = 'Adventure',
                                                   'Animation' = 'Animation',
                                                   'Biography' = 'Biography',
                                                   'Comedy' = 'Comedy',
                                                   'Crime' = 'Crime',
                                                   'Drama' = 'Drama',
                                                   'Family' = 'Family',
                                                   'Fantasy' = 'Fantasy',
                                                   'History' = 'History',
                                                   'Horror' = 'Horror',
                                                   'Music' = 'Music',
                                                   'Musical' = 'Musical',
                                                   'Mystery' = 'Mystery',
                                                   'Romance' = 'Romance',
                                                   'Sci-FI' = 'Sci-Fi',
                                                   'Sport' = 'Sport',
                                                   'Thriler' = 'Thriller',
                                                   'War' = 'War',
                                                   'Western' = 'Western'
                                                   
                                                   
                                       )
                           ),
                           selectInput("variable3", "Select Year:",
                                       choices = c('All' = 'All',
                                                   '2000' = '2000',
                                                   '2001' = '2001',
                                                   '2002' = '2002',
                                                   '2003' = '2003',
                                                   '2004' = '2004',
                                                   '2005' = '2005',
                                                   '2006' = '2006',
                                                   '2007' = '2007',
                                                   '2008' = '2008',
                                                   '2009' = '2009',
                                                   '2010' = '2010',
                                                   '2011' = '2011',
                                                   '2012' = '2012',
                                                   '2013' = '2013',
                                                   '2014' = '2014',
                                                   '2015' = '2015',
                                                   '2016' = '2016',
                                                   '2017' = '2017',
                                                   '2018' = '2018',
                                                   '2019' = '2019',
                                                   '2020' = '2020'
                                       )
                           ),
                           selectInput("variable6", "Select Continent:",
                                       choices = c('All' = 'All',
                                                   'North America' = 'North America',
                                                   'Europe' = 'Europe',
                                                   'Oceania' = 'Oceania',
                                                   'Asia' = 'Asia',
                                                   'Africa' = 'Africa',
                                                   'South America' = 'South America'
                                       )
                           )
                         ),
                         mainPanel(
                           plotOutput(outputId = "correlation_plot",
                                      height = 800),
                         )
                       )
              ),
              tabPanel("Top Directors and Writers",
                       selectInput("variable5", "Select Genre:",
                                   choices = c('All' = 'All',
                                               'Action' = 'Action',
                                               'Adventure' = 'Adventure',
                                               'Animation' = 'Animation',
                                               'Biography' = 'Biography',
                                               'Comedy' = 'Comedy',
                                               'Crime' = 'Crime',
                                               'Drama' = 'Drama',
                                               'Family' = 'Family',
                                               'Fantasy' = 'Fantasy',
                                               'History' = 'History',
                                               'Horror' = 'Horror',
                                               'Music' = 'Music',
                                               'Musical' = 'Musical',
                                               'Mystery' = 'Mystery',
                                               'Romance' = 'Romance',
                                               'Sci-FI' = 'Sci-Fi',
                                               'Sport' = 'Sport',
                                               'Thriler' = 'Thriller',
                                               'War' = 'War',
                                               'Western' = 'Western'
                                   )
                       ),
                       plotOutput(outputId  = "director_plot",
                                  height = "350"),
                       plotOutput(outputId  = "writer_plot",
                                  height = "350"),
              ),
              tabPanel("Profit by Genre",
                       selectInput("variable4", "Select Year:",
                                   choices = c('All' = 'All',
                                               '2000' = '2000',
                                               '2001' = '2001',
                                               '2002' = '2002',
                                               '2003' = '2003',
                                               '2004' = '2004',
                                               '2005' = '2005',
                                               '2006' = '2006',
                                               '2007' = '2007',
                                               '2008' = '2008',
                                               '2009' = '2009',
                                               '2010' = '2010',
                                               '2011' = '2011',
                                               '2012' = '2012',
                                               '2013' = '2013',
                                               '2014' = '2014',
                                               '2015' = '2015',
                                               '2016' = '2016',
                                               '2017' = '2017',
                                               '2018' = '2018',
                                               '2019' = '2019',
                                               '2020' = '2020'
                                   )
                       ),
                       plotOutput(outputId  = "boxplot",
                                  height = "600")
              )
            )
          )
        )
      )      
    ),
    tabItem(tabName = "Descriptive2",
            fluidPage(
              titlePanel("Profit Comparisons"),
              fluidRow(
                column(
                  width = 12,
                  height = 100,
                  tabsetPanel(
                    tabPanel("Historical Data",
                             selectInput("variable7", "Please select one category you would like to explore:",
                                         choices = c('Genre' = 'Genre',
                                                     'Budget' = 'Budget',
                                                     'Critic Reviews' = 'critic',
                                                     'Year' = 'year'),'Genre', 
                             ),
                             fluidRow(
                               box(
                                 plotOutput(outputId = 'history_plot1', height = '600', width = '1300'),
                                 width = 1000),
                             )
                    ),
                    tabPanel("Metascore vs Profit",
                             sliderInput("year", "Year range that the movies were released", 2000, 2020, 
                                         value = c(2000, 2001),
                                         sep = "", width = '1300'),
                             fluidRow(
                               box(
                                 plotOutput(outputId = "metascore_profit_plot", height = '600', width = '1300'),
                                 width = 1000
                               )
                             )
                    ),
                    tabPanel("Best Cast/Crew to invest in",
                             selectInput("wordcloud1", "Please select the cast/crew role you are interested in:",
                                         choices = c("Writers" = "writers", "Directors" = "directors",
                                                     "Actors" = "actors"),'Writers'),
                             fluidRow(
                               box(
                                 wordcloud2Output(outputId = "wordcloud_plot1", height = '600', width = '1300'),
                                 width = 1000
                               )
                             )
                    )
                  )
                )
              )
            )
    ),
    tabItem(
      tabName = 'Inferential',
      fluidPage(
        titlePanel("95% Confidence Interval (Assume that Sample Size > 30, population standard deviation is unknown)"),
        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              tabPanel("Continent",
                       selectInput("variable10", "Select Variable:",
                                   choices = c('Profit' = 'Profit',
                                               'Rating' = 'Rating'
                                   )
                       ),
                       fluidRow(
                         tableOutput(outputId = "summary1"),
                         box(
                           plotOutput(outputId = "ci_plot1",
                                      height = "600")
                         ),
                         box(
                           plotOutput(outputId  = "boxplot1",
                                      height = "600")
                         )
                       )
              ),
              tabPanel("Movie Genre",
                       selectInput("variable11", "Select Variable:",
                                   choices = c('Profit' = 'Profit',
                                               'Rating' = 'Rating'
                                   )
                       ),
                       fluidRow(
                         tableOutput(outputId = "summary2"),
                         box(
                           plotOutput(outputId = "ci_plot2",
                                      height = "600")
                         ),
                         box(
                           plotOutput(outputId  = "boxplot2",
                                      height = "600")
                         )
                       )
              ),
              tabPanel("Multiple Linear Regression (Profit)",
                       box(
                         width = 12,
                         height = 180,
                         selectInput("mlr", "Independent Variables:",
                                     choices = c("Genre","Duration","Continent","Budget")
                                     ,multiple = TRUE,selected = c("Genre","Duration","Continent","Budget")
                         ),
                         selectInput("variable_selection", "Choose Stepwise regression:",
                                     choices =  list("Forward Selection", "Backward Selection", "Stepwise Selection")
                         )),
                       box(
                         width = 12,
                         h3('Regression Summary:'),
                         verbatimTextOutput("lb"),
                       )
              )
            )
          )
        )
      )
    )
    
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(skin = 'yellow',
  dashboardHeader(title = 'IMDB Movie dataset Analysis', titleWidth = 300),
  sidebar,
  body
)



# create the server functions for the dashboard  
server <- function(input, output) { 
  output$correlation_plot <- renderPlot({
    if (input$variable2 == "All" & input$variable3 == 'All' & input$variable6 == "All") {
      ggplot(movie_data, aes_string(x = input$variable1, y='profit_in_millions')) +
        geom_point(size = 2) +
        stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, size = 8) +
        geom_smooth(method = "lm",color = 'blue') +
        ggtitle(paste("Scatter plot of profit ($) against", input$variable1, 'based on', input$variable2, 'Genre and All Year and All Continents')) +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text = element_text(size =18), plot.title = element_text(size = 16))
      
    } else if (input$variable2 != "All" & input$variable3 == 'All' & input$variable6 == "All") {
      ggplot(filter(movie_data, Genre == input$variable2), aes_string(x = input$variable1, y='profit_in_millions')) +
        geom_point(size = 2) +
        stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, size = 8) +
        geom_smooth(method = "lm",color = 'blue') +
        ggtitle(paste("Scatter plot of profit ($) against", input$variable1, 'based on', input$variable2, 'Genre and All Year and All Continents')) +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text = element_text(size =18), plot.title = element_text(size = 16))
      
    } else if (input$variable2 == "All" & input$variable3 != 'All' & input$variable6 == "All") {
      ggplot(filter(movie_data, year == input$variable3), aes_string(x = input$variable1, y='profit_in_millions')) +
        geom_point(size = 2) +
        stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, size = 8) +
        geom_smooth(method = "lm",color = 'blue') +
        ggtitle(paste("Scatter plot of profit ($) against", input$variable1, 'based on All Genre and Year', input$variable3, "and All Continents")) +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text = element_text(size =18), plot.title = element_text(size = 16))
      
    } else if (input$variable2 == "All" & input$variable3 == 'All' & input$variable6 != "All") {
      ggplot(filter(movie_data, Continent == input$variable6), aes_string(x = input$variable1, y='profit_in_millions')) +
        geom_point(size = 2) +
        stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, size = 8) +
        geom_smooth(method = "lm",color = 'blue') +
        ggtitle(paste("Scatter plot of profit ($) against", input$variable1, 'based on All Genre and Year', input$variable3, "and", input$variable6,"Continent")) +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text = element_text(size =18), plot.title = element_text(size = 16))
      
    } else if (input$variable2 != "All" & input$variable3 == 'All' & input$variable6 != "All") {
      ggplot(filter(movie_data, Genre == input$variable2, Continent == input$variable6), aes_string(x = input$variable1, y='profit_in_millions')) +
        geom_point(size = 2) +
        stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, size = 8) +
        geom_smooth(method = "lm",color = 'blue') +
        ggtitle(paste("Scatter plot of profit ($) against", input$variable1, 'based on', input$variable2, 'Genre and All Year', input$variable6,"Continent")) +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text = element_text(size =18), plot.title = element_text(size = 16))
      
    } else if (input$variable2 == "All" & input$variable3 != 'All' & input$variable6 != "All") {
      ggplot(filter(movie_data, year == input$variable3, Continent == input$variable6), aes_string(x = input$variable1, y='profit_in_millions')) +
        geom_point(size = 2) +
        stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, size = 8) +
        geom_smooth(method = "lm",color = 'blue') +
        ggtitle(paste("Scatter plot of profit ($) against", input$variable1, 'based on All Genre and Year', input$variable3, 'and', input$variable6,"Continent")) +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text = element_text(size =18), plot.title = element_text(size = 16))
      
    } else if (input$variable2 != "All" & input$variable3 != 'All' & input$variable6 == "All") {
      ggplot(filter(movie_data, Genre == input$variable2, year == input$variable3), aes_string(x = input$variable1, y='profit_in_millions')) +
        geom_point(size = 2) +
        stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, size = 8) +
        geom_smooth(method = "lm",color = 'blue') +
        ggtitle(paste("Scatter plot of profit ($) against", input$variable1, 'based on', input$variable2, 'Genre and Year', input$variable3, 'and All Continent')) +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text = element_text(size =18), plot.title = element_text(size = 16))
      
    } else {
      ggplot(filter(movie_data, Genre == input$variable2, year == input$variable3, Continent == input$variable6), aes_string(x = input$variable1, y='profit_in_millions')) +
        geom_point(size = 2) +
        stat_poly_eq(formula = y ~ x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE, size = 8) +
        geom_smooth(method = "lm",color = 'blue') +
        ggtitle(paste("Scatter plot of profit ($) against", input$variable1, 'based on', input$variable2, 'Genre and Year', input$variable3, 'and', input$variable6)) +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text = element_text(size =18), plot.title = element_text(size = 16))
      
    }
  })
  
  output$director_plot <- renderPlot({
    dir_profit <- aggregate(movie_data$profit_in_millions, by = list(movie_data$director, movie_data$Genre), FUN = sum)
    colnames(dir_profit) <- c('director', 'genre', 'profit_in_millions')
    dir_profit <- dir_profit[order(-dir_profit$profit_in_millions),]
    
    if (input$variable5 == 'All') {
      ggplot(dir_profit[1:10,], aes(x = profit_in_millions, y = reorder(director, profit_in_millions))) +
        geom_bar(stat = 'identity') +
        xlab('Profit') +
        ylab('Director') +
        ggtitle('Top 10 Directors for All Movies based on Profitability') +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text = element_text(size =18), plot.title = element_text(size = 18))
      
    } else {
      ggplot(filter(dir_profit[dir_profit$genre == input$variable5,][1:10,]), aes(x = profit_in_millions, y = reorder(director, profit_in_millions))) +
        geom_bar(stat = 'identity') +
        xlab('Profit') +
        ylab('Director') +
        ggtitle(paste("Top 10 Directors for", input$variable5, "Movies based on Profitability")) +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text = element_text(size =18), plot.title = element_text(size = 18))
    }
  })
  
  output$writer_plot <- renderPlot({
    writer_profit <- aggregate(movie_data$profit_in_millions, by = list(movie_data$writer, movie_data$Genre), FUN = sum)
    colnames(writer_profit) <- c('writer', 'genre', 'profit_in_millions')
    writer_profit <- writer_profit[order(-writer_profit$profit_in_millions),]
    writer_profit[writer_profit$genre == input$variable5,][1:10,]
    
    if (input$variable5 == 'All') {
      ggplot(writer_profit[1:10,], aes(x = profit_in_millions, y = reorder(writer, profit_in_millions))) +
        geom_bar(stat = 'identity') +
        xlab('Profit') +
        ylab('Writer') +
        ggtitle('Top 10 Writers for All Movies based on Profitability') +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text = element_text(size =18), plot.title = element_text(size = 18))
      
    } else {
      ggplot(filter(writer_profit[writer_profit$genre == input$variable5,][1:10,]), aes(x = profit_in_millions, y = reorder(writer, profit_in_millions))) +
        geom_bar(stat = 'identity') +
        xlab('Profit') +
        ylab('Writer') +
        ggtitle(paste("Top 10 Writers for", input$variable5, "Movies based on Profitability")) +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text = element_text(size =18), plot.title = element_text(size = 18))
    }
  })
  
  output$boxplot <- renderPlot({
    if (input$variable4 == 'All') {
      ggplot(movie_data, aes(x=Genre, y=profit_in_millions)) +
        geom_boxplot() +
        ggtitle(paste("Box plot of profit ($) against Genre based on All Year")) +
        xlab('Genre') +
        ylab('Profit') +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text.y = element_text(size =18), axis.text.x = element_text(size =12), plot.title = element_text(size = 18))
      
    } else {
      ggplot(filter(movie_data, year == input$variable4), aes(x = Genre, y=profit_in_millions)) +
        geom_boxplot() +
        ggtitle(paste("Box plot of profit ($) against Genre based on Year", input$variable4)) +
        xlab('Genre') +
        ylab('Profit') +
        theme_classic() +
        theme(axis.title = element_text(size = 18), axis.text.y = element_text(size =18), axis.text.x = element_text(size =12), plot.title = element_text(size = 18))
    }
  })
  
  output$history_plot1 <- renderPlot({
    if(input$variable7 == 'Genre'){
      ggplot(data = profit_genre, mapping = aes(x = Genre, y = Profit_million, 
                                                fill=Genre)) + geom_col() +
        labs(
          title = "Mean of Profits in different genres",
          x = "Genres",
          y = "Profits (million $)") +
        theme(legend.title = element_blank())
    } else if (input$variable7 == 'Budget'){
      ggplot(data = profit_budget,
             mapping = aes(x = budget_grouping,
                           y = Profit_million,
                           fill=budget_grouping)) +
        geom_col() +
        labs(
          title = "Mean of Profits in different budget groups",
          x = "Budget Groups",
          y = "Profits (million $)") +
        scale_fill_discrete (labels =c('less than 100m$', 'between 100 and 200m$', 'between 200 and 300m$', 'more than 300m$')) +
        theme(legend.title = element_blank())
    } else if (input$variable7 == 'critic'){
      ggplot(data = profit_critics,
             mapping = aes(x = critics_grouping,
                           y = Profit_million,
                           fill=critics_grouping)) +
        geom_col() +
        labs(
          title = "Mean of Profits of different Groups that are seperated based on the number of Reviews from critics",
          x = "Critics Reviews Group",
          y = "Profits (million $)") +
        scale_fill_discrete (labels =c('less than 155 reviews', 'between 255 and 237 reviews', 'between 237 and 353 reviews', 'more than 353 reviews')) +
        theme(legend.title = element_blank())
    } else if (input$variable7 == 'year'){
      ggplot(data = profit_data,
             mapping = aes(x = year,
                           y = Profit_million)) +
        geom_col() +
        labs(
          title = "Mean of Profits across the years",
          x = "Year",
          y = "Profits (million $)") +
        theme(legend.title = element_blank())
    }
  })
  
  ################################# workings for Graph 2 and 3 ##########################################
  ####### Scatter Plot for Profit v.s. Metascores [through years]
  #### Selecting years
  yearT<-c(2000,2012)
  profit_data$year <- as.numeric(as.character(profit_data$year))
  testD <- profit_data %>%
    dplyr::filter(year >= yearT[1],
                  year <= yearT[2]) %>%
    dplyr::select(Genre, Profit_million, meta_score)
  
  
  output$metascore_profit_plot <- renderPlot(
    {
      yearData <- reactive({
        # Filter to the desired year, and put the columns
        # in the order that Google's Bubble Chart expects
        # them (name, x, y, color, size). Also sort by region
        # so that Google Charts orders and colors the regions
        # consistently.
        profit_data$year <- as.numeric(as.character(profit_data$year))
        df <- profit_data %>%
          filter(year >= input$year[1],
                 year <= input$year[2]) %>%
          dplyr::select(Genre, Profit_million, meta_score)
      })
      
      data = as.data.frame(yearData())
      
      ggplot(data, aes(x = meta_score, y = Profit_million, color= "#D55E00")) +  theme(legend.position = "none") +
        geom_point(size = 4, shape = 17) +
        geom_hline(aes(yintercept = mean(Profit_million, na.rm = TRUE)), col = "blue", linetype = "dashed") +
        geom_vline(aes(xintercept = mean(meta_score, na.rm= TRUE)), col = "blue", linetype = "dashed") +
        labs(
          title = "MetaScore Vs Profit",
          x = "Metascore",
          y = "Profits (million $)") 
      
    }
  )
  
  ######################################### Graph 3###########################################
  output$wordcloud_plot1 <- renderWordcloud2({
    if (input$wordcloud1 == "writers"){
      wordcloud2(ww_namesSort,shape = 'circle', size = 1)
    }else if (input$wordcloud1 == "directors"){
      wordcloud2(dd_namesSort,shape = 'star',size = 1)
    }else if (input$wordcloud1 == "actors"){
      wordcloud2(aa_namesSort,shape = 'square',size = 1)
    }
  })
  
  output$summary1 <- renderTable({
    
    if(input$variable10 == "Profit") {
      summary_continent_1 <- movie_data %>%
        dplyr::select(Profit, Continent) %>%
        group_by(Continent) %>%
        summarize(Min = min(Profit, na.rm = TRUE), 
                  Max = max(Profit, na.rm = TRUE), 
                  Mean = mean(Profit, na.rm = TRUE),
                  StdDev = sd(Profit, na.rm = TRUE),
                  Median = median(Profit, na.rm = TRUE))
      summary_continent_2 <- movie_data %>%
        dplyr::count(Continent)
      summary_continent <- merge(summary_continent_1, summary_continent_2, by = "Continent")
      summary_continent$lower_CI <- summary_continent$Mean - qnorm(0.975)* summary_continent$StdDev/sqrt(summary_continent$n)
      summary_continent$upper_CI <- summary_continent$Mean + qnorm(0.975)* summary_continent$StdDev/sqrt(summary_continent$n)
      
    } else if (input$variable10 == "Rating") {
      summary_continent_1 <- movie_data %>%
        dplyr::select(Rating, Continent) %>%
        group_by(Continent) %>%
        summarize(Min = min(Rating, na.rm = TRUE), 
                  Max = max(Rating, na.rm = TRUE), 
                  Mean = mean(Rating, na.rm = TRUE),
                  StdDev = sd(Rating, na.rm = TRUE),
                  Median = median(Rating, na.rm = TRUE))
      summary_continent_2 <- movie_data %>%
        dplyr::count(Continent)
      summary_continent <- merge(summary_continent_1, summary_continent_2, by = "Continent")
      summary_continent$lower_CI <- summary_continent$Mean - qnorm(0.975)* summary_continent$StdDev/sqrt(summary_continent$n)
      summary_continent$upper_CI <- summary_continent$Mean + qnorm(0.975)* summary_continent$StdDev/sqrt(summary_continent$n)
      
    }
    return(summary_continent)
  })
  
  output$ci_plot1 <- renderPlot({
    
    if(input$variable10 == "Profit") {
      summary_continent_1 <- movie_data %>%
        dplyr::select(Profit, Continent) %>%
        group_by(Continent) %>%
        summarize(Mean = mean(Profit, na.rm = TRUE),
                  Sd = sd(Profit, na.rm = TRUE))
      summary_continent_2 <- movie_data %>%
        dplyr::count(Continent)
      summary_continent <- merge(summary_continent_1, summary_continent_2, by = "Continent")
      summary_continent$lower_CI <- summary_continent$Mean - qnorm(0.975)* summary_continent$Sd/sqrt(summary_continent$n)
      summary_continent$upper_CI <- summary_continent$Mean + qnorm(0.975)* summary_continent$Sd/sqrt(summary_continent$n)
      
      
      ci_plot <- ggplot(summary_continent, aes(Mean, Continent))
      ci_plot <- ci_plot + geom_point() +
        geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI, height = .3))+
        labs(title = 'Confidence Interval plots of movie profits by Continent', x = "Profit")
      
    } else if (input$variable10 == "Rating") {
      summary_continent_1 <- movie_data %>%
        dplyr::select(Rating, Continent) %>%
        group_by(Continent) %>%
        summarize(Mean = mean(Rating, na.rm = TRUE),
                  Sd = sd(Rating, na.rm = TRUE))
      summary_continent_2 <- movie_data %>%
        dplyr::count(Continent)
      summary_continent <- merge(summary_continent_1, summary_continent_2, by = "Continent")
      summary_continent$lower_CI <- summary_continent$Mean - qnorm(0.975)* summary_continent$Sd/sqrt(summary_continent$n)
      summary_continent$upper_CI <- summary_continent$Mean + qnorm(0.975)* summary_continent$Sd/sqrt(summary_continent$n)
      
      
      ci_plot <- ggplot(summary_continent, aes(Mean, Continent))
      ci_plot <- ci_plot + geom_point() +
        geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI, height = .3))+
        labs(title = 'Confidence Interval plots of movie ratings by Continent', x = "Rating")
    }
    
    return(ci_plot)
  })
  
  output$boxplot1 <- renderPlot({
    
    if (input$variable10 == "Profit") {
      bplot <- ggplot(movie_data, aes(y = Profit, x = Continent))+
        geom_boxplot()+
        labs(title = 'Boxplots of movie profits by Continent',
             x = 'Continent', y = 'Profit')
    } else if (input$variable10 == "Rating") {
      bplot <- ggplot(movie_data, aes(y = Rating, x = Continent))+
        geom_boxplot()+
        labs(title = 'Boxplots of movie ratings by Continent',
             x = 'Continent', y = 'Rating')
    }
    
    return(bplot)
  })
  
  output$summary2 <- renderTable({
    
    if(input$variable11 == "Profit") {
      summary_genre_1 <- movie_data %>%
        dplyr::select(Profit, Genre) %>%
        group_by(Genre) %>%
        summarize(Min = min(Profit, na.rm = TRUE), 
                  Max = max(Profit, na.rm = TRUE), 
                  Mean = mean(Profit, na.rm = TRUE),
                  StdDev = sd(Profit, na.rm = TRUE),
                  Median = median(Profit, na.rm = TRUE))
      summary_genre_2 <- movie_data %>%
        dplyr::count(Genre)
      summary_genre <- merge(summary_genre_1, summary_genre_2, by = "Genre")
      summary_genre$lower_CI <- summary_genre$Mean - qnorm(0.975)* summary_genre$StdDev/sqrt(summary_genre$n)
      summary_genre$upper_CI <- summary_genre$Mean + qnorm(0.975)* summary_genre$StdDev/sqrt(summary_genre$n)
      
    } else if (input$variable11 == "Rating") {
      summary_genre_1 <- movie_data %>%
        dplyr::select(Rating, Genre) %>%
        group_by(Genre) %>%
        summarize(Min = min(Rating, na.rm = TRUE), 
                  Max = max(Rating, na.rm = TRUE), 
                  Mean = mean(Rating, na.rm = TRUE),
                  StdDev = sd(Rating, na.rm = TRUE),
                  Median = median(Rating, na.rm = TRUE))
      summary_genre_2 <- movie_data %>%
        dplyr::count(Genre)
      summary_genre <- merge(summary_genre_1, summary_genre_2, by = "Genre")
      summary_genre$lower_CI <- summary_genre$Mean - qnorm(0.975)* summary_genre$StdDev/sqrt(summary_genre$n)
      summary_genre$upper_CI <- summary_genre$Mean + qnorm(0.975)* summary_genre$StdDev/sqrt(summary_genre$n)
      
    }
    return(summary_genre)
  })
  
  output$ci_plot2 <- renderPlot({
    
    if(input$variable11 == "Profit") {
      summary_genre_1 <- movie_data %>%
        dplyr::select(Profit, Genre) %>%
        group_by(Genre) %>%
        summarize(Mean = mean(Profit, na.rm = TRUE),
                  Sd = sd(Profit, na.rm = TRUE))
      summary_genre_2 <- movie_data %>%
        dplyr::count(Genre)
      summary_genre <- merge(summary_genre_1, summary_genre_2, by = "Genre")
      summary_genre$lower_CI <- summary_genre$Mean - qnorm(0.975)* summary_genre$Sd/sqrt(summary_genre$n)
      summary_genre$upper_CI <- summary_genre$Mean + qnorm(0.975)* summary_genre$Sd/sqrt(summary_genre$n)
      
      
      ci_plot <- ggplot(summary_genre, aes(Mean, Genre))
      ci_plot <- ci_plot + geom_point() +
        geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI, height = .3))+
        labs(title = 'Confidence Interval plots of movie profits by Genre', x = "Profit")
      
    } else if (input$variable11 == "Rating") {
      summary_genre_1 <- movie_data %>%
        dplyr::select(Rating, Genre) %>%
        group_by(Genre) %>%
        summarize(Mean = mean(Rating, na.rm = TRUE),
                  Sd = sd(Rating, na.rm = TRUE))
      summary_genre_2 <- movie_data %>%
        dplyr::count(Genre)
      summary_genre <- merge(summary_genre_1, summary_genre_2, by = "Genre")
      summary_genre$lower_CI <- summary_genre$Mean - qnorm(0.975)* summary_genre$Sd/sqrt(summary_genre$n)
      summary_genre$upper_CI <- summary_genre$Mean + qnorm(0.975)* summary_genre$Sd/sqrt(summary_genre$n)
      
      
      ci_plot <- ggplot(summary_genre, aes(Mean, Genre))
      ci_plot <- ci_plot + geom_point() +
        geom_errorbarh(aes(xmax = upper_CI, xmin = lower_CI, height = .3))+
        labs(title = 'Confidence Interval plots of movie ratings by Genre', x = "Rating")
    }
    
    return(ci_plot)
  })
  
  output$boxplot2 <- renderPlot({
    
    if (input$variable11 == "Profit") {
      bplot <- ggplot(movie_data, aes(y = Profit, x = Genre))+
        geom_boxplot()+
        labs(title = 'Boxplots of movie profits by Genre',
             x = 'Genre', y = 'Profit')
    } else if (input$variable11 == "Rating") {
      bplot <- ggplot(movie_data, aes(y = Rating, x = Genre))+
        geom_boxplot()+
        labs(title = 'Boxplots of movie ratings by Genre',
             x = 'Genre', y = 'Rating')
    }
    
    return(bplot)
  })
  
  output$lb <- renderPrint({
    variablesx<-c("Profit",input$mlr)
    Rdata = movie_data[,variablesx]
    Rdata<-na.omit(Rdata)
    fit1<-lm(Profit ~.,data=Rdata)
    fit2<-lm(Profit ~1,data=Rdata)
    
    if (input$variable_selection == "Backward Selection") {
      step<- stepAIC(fit1,direction = "backward")
      
    } else if (input$variable_selection == "Forward Selection") {
      step<- stepAIC(fit2,direction = "forward",scope = list(upper = fit1,lower = fit2))
      
    } else {
      step<- stepAIC(fit2,direction = "both",scope = list(upper = fit1,lower = fit2))
    }
    return(summary(step))
    
  })
  

}
shinyApp(ui, server)

