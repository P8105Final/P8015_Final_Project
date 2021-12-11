library(shiny)

library(leaflet)
library(tidyverse)
library(RColorBrewer)

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
    filter(year >=2006) %>% 
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
    select(date_paste,time_paste, year, hour, boro, outcome, lat,lng) 

boro = data_map %>% distinct(boro) %>% pull()

outcome = data_map %>% distinct(outcome) %>% pull()




shinyApp(options = list(height = 1000), 
#Define the user interface
         ui = fluidPage(
             
             titlePanel("NYC Shooting Incident Map"),
             
             sidebarLayout(
                 sidebarPanel(
                     helpText("Enter the interested borough, time range, and incident outcome to explore the NYC shooting incident map yourself! "),
                     
                     # select boro
                     radioButtons("boro_choice", 
                                 label = h3("Choose a borough"),
                                 choices = as.list(boro),
                                 selected = "manhattan"),
                     
                     #select incident outcome
                     selectInput("outcome_choice",
                                 label = h3("Choose incident outcome"),
                                 choices = as.list(outcome),
                                 selected = "murdered",
                                 multiple = TRUE),
                     helpText("Multiple selections are avaliable. Select both to see the total incidents!"),
                     
                     #select date range 
                     sliderInput(
                         "date_choice", 
                         label = h3("Choose a date range"), 
                         min = 2006, max = 2020, 
                         value = c(2006, 2020)),
                     
                     #select time range
                     
                     sliderInput(
                         "time_choice", 
                         label = h3("Choose a time range"), 
                         min = 00, max = 23, 
                         value = c(00, 23))
                                
                     ),
                 
                 
                 mainPanel(
                     h3("Click on each point to see exact occurence date and time"),
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
                 df = subset(df,
                             as.numeric(year) >= input$date_choice[1] & as.numeric(year) <= input$date_choice[2]
                             & as.numeric(hour) >= input$time_choice[1] & as.numeric(hour) <= input$time_choice[2]) %>% 
                     mutate(label = str_c("<b>date: ", date_paste, "</b><br>time: ", time_paste , sep = ""))
                 
            #set color
            pal2 = colorFactor(c("sienna2", "steelblue3"), domain = c("murdered", "survived"))
                 
            lat = vector()
            lng = vector()
            for (i in 1:nrow(df)) {
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
                    color = ~pal2(df$outcome),
                    opacity = 0.8,
                    radius = 0.6,
                    fillColor = ~pal2(df$outcome),
                    popup = ~ label) %>%
                     addLegend(position = 'bottomright', pal = pal2, values = df$outcome, title = "Incident Outcome")
             })
         }
)
    
