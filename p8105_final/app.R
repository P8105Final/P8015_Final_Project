library(shiny)

library(leaflet)
library(tidyverse)

#Read in dataset
Data = read_csv("data/NYPD_Shooting_Incident_Data__Historic_.csv")


data_hist = 
    read_csv("data/NYPD_Shooting_Incident_Data__Historic_.csv") %>%
    janitor::clean_names() %>%
    rename("date" = "occur_date",
           "time" = "occur_time") %>%
    separate(date, into = c("month", "day", "year"), sep = "/") %>%
    separate(time, into = c("hour", "minute"), sep = ":") %>% 
    mutate(
        boro = str_to_lower(boro),
        location_desc = str_to_lower(location_desc),
        perp_race = str_to_lower(perp_race),
        vic_race = str_to_lower(vic_race),
        perp_age_group = str_replace(perp_age_group, "UNKNOWN","unknown"),
        perp_sex = recode(perp_sex, "M" = "male","F" = "female","U" = "unknown"), 
        vic_sex = recode(vic_sex, "M" = "male","F" = "female","U" = "unknown"),
        jurisdiction_code = case_when(
            jurisdiction_code == 0 ~ "patrol", 
            jurisdiction_code == 1 ~ "transit",
            jurisdiction_code == 2 ~ "housing")
    ) %>% 
    filter(year > 2014) %>% 
    arrange(year,month,day,hour,minute)

data_map = 
    data_hist %>% 
    mutate(
        date_paste = as.Date(paste(year,month,day,sep = "-")),
        time_paste = paste(hour,minute,sep = ":"),
        statistical_murder_flag = case_when(
            statistical_murder_flag == "TRUE" ~"murdered",
            statistical_murder_flag == "FALSE" ~"survived"
        )) %>% 
    rename("lat" = "latitude",
           "lng" = "longitude",
           "outcome" = "statistical_murder_flag") %>%
    select(date_paste,time_paste, boro, outcome, lat,lng) 

boro = data_map %>% distinct(boro) %>% pull()

outcome = data_map %>% distinct(outcome) %>% pull()



shinyApp(options = list(height = 1000), 
#Define the user interface
         ui = fluidPage(
             
             titlePanel("NYC Shooting Incident Map"),
             
             sidebarLayout(
                 sidebarPanel(
                     helpText("Explore the shotting incident across NYC yourself! Choose the borough, time, and incident outcome of interest!"),
                     
                     # select boro
                     selectInput("boro_choice", 
                                 label = h3("Choose a borough"),
                                 choices = as.list(boro),
                                 selected = "manhattan"),
                     
                     #select incident outcome
                     selectInput("outcome_choice",
                                 label = h3("Choose incident outcome"),
                                 choices = as.list(outcome),
                                 selected = "murdered",
                                 multiple = TRUE),
                     helpText("Multiple selections are avaliable. Select both to see the total incidents!")
                                ),
                 
                 
                 mainPanel(
                     leafletOutput('map', height = 550))
             )
         ),
         
         server = function(input, output,session){
             
             df1 = reactive({
                 df = data_map
                 df = subset(df, boro == input$boro_choice)
             })
             
             df2 = reactive({
                 x = df1()
                 x = subset(x, outcome %in% input$outcome_choice)
             })

             
             output$map = renderLeaflet({
                 df = df2()
                 lat = vector()
                 lng = vector()
                 for (i in 1:nrow(df)){
                     lat[i] = df$lat[i]
                     lng[i] = df$lng[i]
                 }
        
                 
                 df$lat = as.numeric(lat)
                 df$lng = as.numeric(lng)
                 
                 leaflet() %>% 
                     addTiles() %>%
                     addProviderTiles(providers$CartoDB.Positron) %>% 
                     addCircleMarkers(
                         lng = df$lng,
                         lat = df$lat,
                         data = df,
                         fillOpacity = 0.5,
                         radius = 0.5) 
             })
         }
)
    
