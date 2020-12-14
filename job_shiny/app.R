
library(shiny)

library(tidytext)
library(leaflet)
library(tnum)
library(httr)
library(rjson)
library(RCurl)
library(XML)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)


r <- jsonlite::fromJSON(readLines("https://jobs.github.com/positions.json?description=r"))
node <- jsonlite::fromJSON("https://jobs.github.com/positions.json?search=node")
stat <- jsonlite::fromJSON("https://jobs.github.com/positions.json?description=stat")
whole <- jsonlite::fromJSON("https://jobs.github.com/positions.json?")
dt <-jsonlite::fromJSON("https://jobs.github.com/positions.json?description=data")
al <-jsonlite::fromJSON("https://jobs.github.com/positions.json?description=algorithm")
ml <-jsonlite::fromJSON("https://jobs.github.com/positions.json?description=machine")
whole2 <- jsonlite::fromJSON("https://jobs.github.com/positions.json?company=Amazon")
whole3 <- jsonlite::fromJSON("https://jobs.github.com/positions.json?company=google")
muse <- jsonlite::fromJSON("https://www.themuse.com/api/public/jobs?page=10")



total <- rbind(r, node, stat, whole, dt, al, ml, whole2, whole3)
total2 <- data.frame(total[1], total[2], total[5], total[6], total[7], total[8], total[9])



#total2
a <- total2 %>% unnest_tokens(word, description)

data(stop_words)     ##  Remove stop-words, e.g. the, a, this, and that ...

a <- a %>%anti_join(stop_words)

a<- a %>%
  count(word, sort = TRUE) 
b <- data.frame(a) %>% filter(n<2000) %>% top_n(10, n) #remove 'li'


prog_pop <- a %>% filter((word=="java")|(word=="python")|(word=="javascript")|(word=="php")|(word=="swift")|(word=="go")|(word=="ruby"))
prog_ggplot <- ggplot(data=prog_pop, aes(x=word, y=n))+geom_bar(stat='identity', aes(fill=word))+geom_text(aes(label=word), hjust=0, vjust=0)
#top 10 https://towardsdatascience.com/top-10-in-demand-programming-languages-to-learn-in-2020-4462eb7d8d3
#python, javascript, java, c#, c, c++, php, swift, go, ruby
#couldn't find c#,c,c++

word_ggplot <- ggplot(data=b,aes(x=word, y=n))+geom_point()+geom_text(aes(label=word), hjust=0, vjust=0)+
  theme(axis.text.x=element_blank())

# Total 2
# which company most 
total2_summary <- total2 %>% group_by(company) %>% summarise(company_count = n()) %>%top_n(10, company_count)
ggplot(total2_summary, aes(company, company_count))+geom_bar(stat='identity', aes(fill=company))+
  theme(axis.text.x=element_blank())

total2_location <- total2 %>%group_by(location)
total2_location$location[total2_location$location == "Berlin / Remote"] <- "Berlin"
total2_location$location[total2_location$location == "Berlin | Remote"] <- "Berlin"
total2_location$location[total2_location$location == "Berlin, BE, DE"] <- "Berlin"
total2_location$location[total2_location$location == "Garching, Munich"] <- "Munich"
total2_location$location[total2_location$location == "München"] <- "Munich"
total2_location$location[total2_location$location == "Munch Germany "] <- "Munich"
total2_location$location[total2_location$location == "Munich Germany"] <- "Munich"
total2_location$location[total2_location$location == "NYC / Remote"] <- "New York City"
total2_location$location[total2_location$location == "remote" ] <- "Remote"
total2_location$location[total2_location$location == "Remote (USA)" ] <- "Remote"
total2_location$location[total2_location$location == "Remote in U.S." ] <- "Remote"
total2_location$location[total2_location$location == "Remote, EU" ] <- "Remote"
total2_location$location[total2_location$location == "Utrecht (The Netherlands)" ] <- "Utrecht"
total2_location$location[total2_location$location == "Croeselaan 18, 3521CB, Utrecht" ] <- "Utrecht"
total2_location$location[total2_location$location == "Utrecht " ] <- "Utrecht"
total2_location$location[total2_location$location == "San Francisco | Remote (US/Canada)" ] <- "San Francisco"
total2_location$location[total2_location$location == "Soho, London" ] <- "London"
total2_location$location[total2_location$location == "Europe (remote)" ] <- "Remote"
total2_location$location[total2_location$location == "Seattle / Fully Remote" ] <- "Romote"
total2_location$location[total2_location$location == "Toronto, ON - REMOTE" ] <- "Toronto"
total2_location$location[total2_location$location == "Toronto, Canada (or remote within Canada)" ] <- "Toronto"

total2_location <- total2_location %>%group_by(location) %>% summarise(location_count =n())
location_top <- total2_location %>% slice_max(location_count, n=10)

ggplot(location_top, aes(location, location_count))+geom_bar(stat='identity', aes(fill=location))+
  theme(axis.text.x=element_blank())

dt.map <- data.frame(
  city = c('Berlin', 'Munich', 'Dresen', 'Oldenburg','Leverkusen',
           'Cologne', 'Erfurt', 'Frankfurt', 'Hamburg', 'Holzwickede',
           'Utrecht',
           'Barcelona', 'Madrid', 'Poland',
           'Kyiv', 'Budapest'),
  
  lat = c(52.52, 48.1351, 51.0504, 53.1435, 51.0459,
          50.9375, 50.9848, 50.1109, 53.5511, 51.4998,
          52.0907,
          41.3851, 40.4168, 51.9194,
          50.4501, 47.4979),
  
  lng = c(13.4050, 11.5820,13.7373, 8.2146,7.0192,
          6.9603, 11.0299, 8.6821, 9.9937, 7.6209,
          5.1214,
          2.1734, 3.7038, 19.1451,
          30.5234, 19.0402),
  
  col = c("red", "red","red","red","red",
          "red", "red","red","red","red",
          "orange",
          "blue", "blue", "purple",
          "green", "pink")
)

dt.map$popup <- with(dt.map, paste("<b>", city, "</b>"))

markers <- awesomeIcons(
  icon='map-marker',
  iconColor = 'black',
  markerColor = dt.map$col,
  library='fa')

map <- leaflet(data = dt.map, width = "100%" ) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addAwesomeMarkers(
    lng = ~lng, 
    lat = ~lat, 
    popup = ~popup,
    icon = markers
  ) %>%
  addLegend(
    position='topright',
    colors= c("red","red","red","red","red",
              "red", "red","red","red","red",
              "orange",
              "blue", "blue", "purple",
              "green", "grey"),
    labels= dt.map$city,
    opacity = 0.75,
    title="Legend"
  )

map  # Show map



ui <- fluidPage(
  title = "Job Postings of Data Scientist",
  sidebarLayout(
    tabsetPanel(
      conditionalPanel(
        'input.dataset === "total2"'),
      conditionalPanel(
        'input.dataset === "total2"'),
      conditionalPanel(
        'input.dataset === "total2"')
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Companies Map in Europe",
                 leafletOutput("map"),
                 
                 # Creating a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("location",
                                      "Locations:",
                                      c("All",
                                        unique(as.character(total2$location))))
                   ),
                   column(4,
                          selectInput("company",
                                      "Company:",
                                      c("All",
                                        unique(total2$company)))
                          
                          
                   ),
                   column(4,
                          selectInput("title",
                                      "Title:",
                                      c("All",
                                        unique(as.character(total2$title))))
                   ),
                 ),
                 # Creating a new row for the table
                 DT::dataTableOutput("table1")),
        tabPanel("Programming Popularity",
                 plotOutput("prog_ggplot"),
                 
                 # Creating a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("location",
                                      "Locations:",
                                      c("All",
                                        unique(as.character(total2$location))))
                   ),
                   column(4,
                          selectInput("company",
                                      "Company:",
                                      c("All",
                                        unique(total2$company)))
                          
                          
                   ),
                   column(4,
                          selectInput("title",
                                      "Title:",
                                      c("All",
                                        unique(as.character(total2$title))))
                   ),
                 ),
                 # Creating a new row for the table
                 DT::dataTableOutput("table2")),
        tabPanel("Words in Descriptions",
                 plotOutput("word_ggplot"),
                 
                 # Creating a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("location",
                                      "Locations:",
                                      c("All",
                                        unique(as.character(total2$location))))
                   ),
                   column(4,
                          selectInput("company",
                                      "Company:",
                                      c("All",
                                        unique(total2$company)))
                          
                          
                   ),
                   column(4,
                          selectInput("title",
                                      "Title:",
                                      c("All",
                                        unique(as.character(total2$title))))
                   ),
                 ),
                 # Creating a new row for the table
                 DT::dataTableOutput("table3"))
        
      )
    )   
  ))

server <- function(input, output) {
  
  # Filtering data based on selections
  output$map = renderLeaflet({
    leaflet(data = dt.map, width = "100%" ) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addAwesomeMarkers(
        lng = ~lng, 
        lat = ~lat, 
        popup = ~popup,
        icon = markers
      ) %>%
      addLegend(
        position='topright',
        colors= c("red","red","red","red","red",
                  "red", "red","red","red","red",
                  "orange",
                  "blue", "blue", "purple",
                  "green", "pink"),
        labels= dt.map$city,
        opacity = 0.75,
        title="Legend"
  )})
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- total2
    if (input$location != "All") {
      data <- data[total2$location == input$location,]
    }
    if (input$company != "All") {
      data <- data[total2$company == input$company,]
    }
    
    data
  }))
  
  # Sorted columns are colored now because CSS are attached to them
  # Filtering data based on selections
  output$prog_ggplot = renderPlot({
    ggplot(data=prog_skill, aes(x=word, y=n))+
      geom_bar(stat='identity', aes(fill=word))+geom_text(aes(label=word), hjust=0, vjust=0)
  })
  output$table2 <- DT::renderDataTable(DT::datatable({
    data2 <- total2
    if (input$location != "All") {
      data2 <- data2[total2$location == input$location,]
    }
    if (input$company != "All") {
      data2 <- data2[total2$company == input$company,]
    }
    
    data2
  }))
  
  output$word_ggplot = renderPlot({
    ggplot(data=b,aes(x=word, y=n))+geom_point()+geom_text(aes(label=word), hjust=0, vjust=0)+
      theme(axis.text.x=element_blank())
    
  })
  output$table3 <- DT::renderDataTable(DT::datatable({
    data3 <- total2
    if (input$location != "All") {
      data3 <- data3[total2$location == input$location,]
    }
    if (input$company != "All") {
      data3 <- data3[total2$company == input$company,]
    }
    
    data3
  }))
  
  
}

# Running the application 
shinyApp(ui = ui, server = server) 
