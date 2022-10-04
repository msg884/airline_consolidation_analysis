## Eurocontrol STATFOR R&D dataset

#Load packages
library(readr) #Read data
library(readxl) #Read Excel sheets
library(dplyr) #Clean and manipulate data
library(airportr) #Airport IATA-ICAO correlations
library(shiny) #interactive visualisation 
library(ggplot2) #plot
library(scales) #Format numbers as percent
library(shinyWidgets) #better UI
library(DT) #interactive datatable
library(treemapify) #Create treemaps
library(shinythemes) #Theme for UI


#PREPARE DATA FOR APP

#Define regional groups in vectors
Europe <- readLines("Europe.txt")
EUplus <- readLines("EUplus.txt")
EU <- readLines("EU27.txt")

#Prepare disclaimer text
text_about <- ("This app allows you to explore how airline mergers or acquisitions would change the baseline 2019 market structure. <br/> <br/>
                Some health warnings: this app is just for fun and speculation. It is not designed for business or policy considerations. 
                Airline market concentration is better analysed at the route or country level and considering growth through fleet expansion or increased utilisation rates, in addition to acquisitions.
                The data comes from Eurocontrol's Flights R&D Archive. Only scheduled airline services are considered. Thank you to Eurocontrol for making this data available publicly. --Michael S-G
                <br/> <br/> EUROCONTROL's Disclaimer: This document_R&D product has been created with or contains elements of ATM Datasets made available by EUROCONTROL. 2020, EUROCONTROL; EUROCONTROL does not necessarily support and/or endorse the conclusion of this document/R&D product. EUROCONTROL shall not be liable for any direct, indirect, incidental or consequential damages arising out of or in connection with this document/product and/or underlying the ATM Datasets.")

#Import airline groups and alliances information
airline_alliance_codes <- read_excel("airlines_groups_correlations.xlsx")

#Import flight data, simplify names and expect
df_Flights201903 <- read_csv("Flights_20190301_20190331.csv") %>% rename_with( ~ tolower(gsub(" ", "_", .x, fixed = TRUE)))

#Add other variables to identify AIRPORTS by IATA codes
Airport_Correlations <- airports %>% rename_with( ~ tolower(gsub(" ", "_", .x, fixed = TRUE))) #Load airport data from airportr package - from source https://openflights.org/data.html

#Create dataframe
df <- 
  #Add IATA airport codes departing airports (adep) for user ease
  left_join(df_Flights201903 %>% select(adep, ades, ac_type, ac_operator, icao_flight_type, actual_off_block_time, statfor_market_segment),
                Airport_Correlations %>% select(name, iata, icao, country, `country_code_(alpha-3)`),
                by = c("adep" = "icao")) %>%
                rename(iata_dep = iata) %>% #Specify that the IATA code is for the departing airport / adep
  #Add IATA airport codes arriving airports (ades) for user ease
  left_join(.,  
            Airport_Correlations %>% select(name, iata, icao, country, `country_code_(alpha-3)`),
            by = c("ades" = "icao")) %>%
            rename(iata_arr = iata)%>%  #Specify that the IATA code is for the arrival airport / ades
  #Select only the relevant variables
  select(adep, iata_dep, country.x, `country_code_(alpha-3).x`, 
         ades, iata_arr, country.y, `country_code_(alpha-3).y`,
         ac_type, ac_operator, icao_flight_type, statfor_market_segment,
         actual_off_block_time) %>%
  #Add variables related to airline groups and airline alliances
  left_join(. ,airline_alliance_codes, by = c("ac_operator" = "ICAO_airline")) %>% 
  mutate(
    Airline_Alliance = case_when(
      Alliance != 0 ~ Alliance,
      is.na(Alliance) ~ Airline_Name)
  ) %>%
  mutate(
    Airline_Group = case_when(
      Group != 0 ~ Group,
      is.na(Group) ~ Airline_Name)
  ) %>% 
  #Filter out data so that dataset includes only relevant rows
  filter(
    statfor_market_segment %in% c("Traditional Scheduled", "Lowcost"),  #Filter to include on passenger services
    ac_operator != "ZZZ",  #Flight to exclude flights without an identified aircraft operator
    icao_flight_type == "S", #Filter to include only scheduled flights
    country.x %in% Europe) %>% #Filter to only flights departing Europe
  select(-`country_code_(alpha-3).x`, -`country_code_(alpha-3).y`, -actual_off_block_time, -ac_type, -icao_flight_type, -statfor_market_segment)

glimpse(df)

#Define list of input values for World Destinations and Airline buyers/sellers
World <- unique(df$country.y)
Buyers <- df %>% count(Airline_Group, name = 'Flights') %>% arrange(desc(Flights)) %>% slice_head(n=20) %>% select(Airline_Group)


############################################################################################################################
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

# UI
ui <- fluidPage(
  theme = shinytheme("readable"),

      titlePanel("Market concentration after Theoretical Airline Consolidation"),
  sidebarLayout(
    sidebarPanel(
        helpText("To create a scenario for market shares, choose the acquiring airline and the airline that is purchased."),
        actionButton('show_about', 'About', class = "btn-warning"),
        pickerInput("airline_buyer", "Select Buyer:", 
                    arrange(Buyers), multiple = F,
                    selected = "AF-KLM",
                      options = pickerOptions(
                        Title = "Merger/Acquisition",
                        header = "Select buyer and acquired airlines")),
        pickerInput("airline_seller", "Select Target:", 
                    arrange(Buyers), multiple = F,
                    selected = "Alitalia"),
        pickerInput("airline_buyer2", "2nd Merger? Select Buyer:", 
                    arrange(Buyers), multiple = F,
                    selected = NULL),
        pickerInput("airline_seller2", "2nd Merger? Select Target:", 
                    arrange(Buyers), multiple = F,
                    selected = NULL),
        pickerInput("airline_buyer3", "3rd Merger? Select Buyer:", 
                    arrange(Buyers), multiple = F,
                    selected = NULL),
        pickerInput("airline_seller3", "3rd Merger? Select Target:", 
                    arrange(Buyers), multiple = F,
                    selected = NULL),

      radioButtons("dep_region", "Select Departure Region", 
                     choices = c("Europe", "EUplus", "EU")),
      radioButtons("arr_region", "Select Arrival Region", 
                   choices = c("World" = "World", "Europe" = "Europe", "EUplus" = "EUplus", "EU" = "EU"))
                ),
    
  mainPanel(
    tabsetPanel(
      tabPanel("Column",
               h3(textOutput("Your_Scenario")),
               plotOutput("Columns_Future"),
               h3(textOutput("Original_Scenario")),
               plotOutput("Columns_2019")),
      tabPanel("Table",
              h3(textOutput("Your_Scenario_1")),
              DT::DTOutput("Table_Future"),
              h3(textOutput("Original_Scenario_1")),
              DT::DTOutput("Table_2019")),
      tabPanel("Treemap",
               h3(textOutput("Your_Scenario_2")),
               plotOutput("Treemap_Future"),
               h3(textOutput("Original_Scenario_2")),
               plotOutput("Treemap_2019"))
      )
    )
  )
)

  
server <- function(input, output, session) {
  
#About Text  
  observeEvent(input$show_about, {
    showModal(modalDialog(HTML(text_about), title = 'About'))
  })
  
#Captions for the tables and treemaps


  # Build reactive dataframe based on inputs
  data1 <- reactive({
    
    df %>% filter(country.x %in% get(input$dep_region) &
                          country.y %in% get(input$arr_region)) %>%
      mutate(
        Total_Flights = n(),
        `Theoretical Owner` = case_when(
          Airline_Group == input$airline_seller ~ input$airline_buyer,
          Airline_Group == input$airline_seller2 ~ input$airline_buyer2,
          Airline_Group == input$airline_seller3 ~ input$airline_buyer3,
          TRUE ~ (Airline_Group))
      ) %>% 
      group_by(`Theoretical Owner`) %>%
      mutate(Airline_flights = n()) %>%
      ungroup() %>%
      mutate(Market_Share = Airline_flights/Total_Flights) %>%
      group_by(`Theoretical Owner`) %>%
      summarise(
        `Market Share` = mean(Market_Share)) %>%
      arrange(desc(`Market Share`)) %>%
      head(n=10) %>%
      print()
    
  })  
  
  #Text about the scenario selected
  
  #Create a reactive value of the sum of the market share of the top 10 (head) airlines
  
  Top10_Shares <- reactive({ 
    
    df %>% filter(country.x %in% get(input$dep_region) &
                     country.y %in% get(input$arr_region)) %>%
      mutate(
        Total_Flights = n(),
        `Theoretical Owner` = case_when(
          Airline_Group == input$airline_seller ~ input$airline_buyer,
          Airline_Group == input$airline_seller2 ~ input$airline_buyer2,
          Airline_Group == input$airline_seller3 ~ input$airline_buyer3,
          TRUE ~ (Airline_Group))
      ) %>% 
      group_by(`Theoretical Owner`) %>%
      mutate(Airline_flights = n()) %>%
      ungroup() %>%
      mutate(Market_Share = Airline_flights/Total_Flights) %>%
      group_by(`Theoretical Owner`) %>%
      summarise(
        `Market Share` = mean(Market_Share)) %>%
      arrange(desc(`Market Share`)) %>%
      head(n=10) %>%  #Limit to the top 10 airlines
    mutate(
      SUM10 = sum(`Market Share`)) %>%
      head(n=1) %>%
    select(SUM10)
  })


  
  #Create a reactive value of the sum of the market share of the top 5 (head) airlines
  
  Top5_Shares <- reactive({ 
    
    df %>% filter(country.x %in% get(input$dep_region) &
                     country.y %in% get(input$arr_region)) %>%
      mutate(
        Total_Flights = n(),
        `Theoretical Owner` = case_when(
          Airline_Group == input$airline_seller ~ input$airline_buyer,
          Airline_Group == input$airline_seller2 ~ input$airline_buyer2,
          Airline_Group == input$airline_seller3 ~ input$airline_buyer3,
          TRUE ~ (Airline_Group))
      ) %>% 
      group_by(`Theoretical Owner`) %>%
      mutate(Airline_flights = n()) %>%
      ungroup() %>%
      mutate(Market_Share = Airline_flights/Total_Flights) %>%
      group_by(`Theoretical Owner`) %>%
      summarise(
        `Market Share` = mean(Market_Share)) %>%
      arrange(desc(`Market Share`)) %>%
      head(n=5) %>%  #Limit to the top 5 airlines
      mutate(
        SUM5 = sum(`Market Share`)) %>%
      head(n=1) %>%
      select(SUM5)
  })
  
    #Create text for all 3 Tab Panels
  
  output$Your_Scenario <- renderText({
    paste("In the scenario that you have created, the top 10 airlines would account for ", 
    scales::percent(Top10_Shares()$SUM10,accuracy = .1),
    "and the top 5 airlines for ",
    scales::percent(Top5_Shares()$SUM5,accuracy = .1),
    "of all flights departing ", input$dep_region,
    "and arriving in ", input$arr_region)
      })
  
  output$Your_Scenario_1 <- renderText({
    paste("In the scenario that you have created, the top 10 airlines would account for ", 
          scales::percent(Top10_Shares()$SUM10,accuracy = .1),
          "and the top 5 airlines for ",
          scales::percent(Top5_Shares()$SUM5,accuracy = .1),
          "of all flights departing ", input$dep_region,
          "and arriving in ", input$arr_region)
  })
  
  output$Your_Scenario_2 <- renderText({
    paste("In the scenario that you have created, the top 10 airlines would account for ", 
          scales::percent(Top10_Shares()$SUM10,accuracy = .1),
          "and the top 5 airlines for ",
          scales::percent(Top5_Shares()$SUM5,accuracy = .1),
          "of all flights departing ", input$dep_region,
          "and arriving in ", input$arr_region)
  })
  
  
  #Create text for all 3 Tab Panels with the 2019 market share headlines
  
  output$Original_Scenario <- renderText({
    paste("In 2019, airlines' share of flights departing", input$dep_region,
          "and arriving in ", input$arr_region, "was:")
  })

  
  output$Original_Scenario_1 <- renderText({
    paste("In 2019, airlines' share of flights departing", input$dep_region,
          "and arriving in ", input$arr_region, "was:")
  })
  
  
  output$Original_Scenario_2 <- renderText({
    paste("In 2019, airlines' share of flights departing", input$dep_region,
          "and arriving in ", input$arr_region, "was:")
  })
  
  #Create bar/column plots
  
    output$Columns_Future <- renderPlot({
    ggplot(data = data1(), aes(x=reorder(`Theoretical Owner`, `Market Share`), y=`Market Share`)) + geom_col() +
      scale_y_continuous(labels = scales::percent) +
      coord_flip() +
      labs(title = "Theoretical Market Shares Post Acquisition", x = "Airline", y = "Market Share")  +
        geom_text(aes(label = scales::percent(`Market Share`, accuracy = 0.1)), hjust = -0.1) +
        ggthemes::theme_tufte(base_family = "sans", ticks = F, base_size = 20)
    })
  
  
  output$Columns_2019 <- renderPlot({
    
    df %>% filter(country.x %in% get(input$dep_region) &
                     country.y %in% get(input$arr_region)) %>%
      mutate(
        Total_Flights = n()) %>%
      group_by(Airline_Group) %>%
      mutate(Airline_flights = n()) %>%
      ungroup() %>%
      mutate(Market_Share = Airline_flights/Total_Flights) %>%
      group_by(Airline_Group) %>%
      summarise(`Market Share` = mean(Market_Share)) %>%
      arrange(desc(`Market Share`)) %>%
      head(n=10) %>%
      print() %>%
      ggplot(aes(x=reorder(Airline_Group, `Market Share`), y=`Market Share`)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "2019 Market Shares", x = "Airline", y = "Market Share")  +
      geom_text(aes(label = scales::percent(`Market Share`, accuracy = 0.1)), hjust = -0.1) +
      ggthemes::theme_tufte(base_family = "sans", ticks = F, base_size = 20)
    
  })
  
  #Create Tables
  
  output$Table_Future <- DT::renderDataTable({
    
    datatable(
    df %>% filter(country.x %in% get(input$dep_region) &
                     country.y %in% get(input$arr_region))%>%
      mutate(
        Total_Flights = n(),
        `Theoretical Owner` = case_when(
          Airline_Group == input$airline_seller ~ input$airline_buyer,
          Airline_Group == input$airline_seller2 ~ input$airline_buyer2,
          Airline_Group == input$airline_seller3 ~ input$airline_buyer3,
          TRUE ~ (Airline_Group))
      ) %>% 
      group_by(`Theoretical Owner`) %>%
      mutate(Airline_flights = n()) %>%
      ungroup() %>%
      mutate(Market_Share = Airline_flights/Total_Flights) %>%
      group_by(`Theoretical Owner`) %>%
      summarise(
        `Market Share` = mean(Market_Share)) %>%
      arrange(desc(`Market Share`))
    ) %>%
      formatPercentage("Market Share", 2)
      
    })
  
  
  output$Table_2019 <- DT::renderDataTable ({
    
    datatable(
    df %>% filter(country.x %in% get(input$dep_region) &
                     country.y %in% get(input$arr_region)) %>%
      mutate(
        Total_Flights = n()) %>%
      group_by(Airline_Group) %>%
      mutate(Airline_flights = n()) %>%
      ungroup() %>%
      mutate(Market_Share = Airline_flights/Total_Flights) %>%
      group_by(Airline_Group) %>%
      summarise(`Market Share` = mean(Market_Share)) %>%
      arrange(desc(`Market Share`))
    ) %>%
      formatPercentage("Market Share", 2)
  })
  
  
  #Create treemaps
  
  output$Treemap_Future <- renderPlot({
    ggplot(data = data1(), 
           aes(
             area = `Market Share`, 
             group = `Theoretical Owner`,  
             fill = `Theoretical Owner`, 
             label = paste(`Theoretical Owner`, scales::percent(`Market Share`, accuracy = 0.1), sep = "\n"))
                ) + 
      geom_treemap(fill = "#81d4fa") +
      geom_treemap_text(color = "dark blue", fontface = "italic",  place = "centre") +
      labs(title = "Theoretical Market Shares Post Acquisition", x = "Airline", y = "Market Share") +
      theme(legend.position = "none", plot.title = element_text(size = 20))
  })
  
  
  output$Treemap_2019 <- renderPlot({
    
    df %>% filter(country.x %in% get(input$dep_region) &
                     country.y %in% get(input$arr_region)) %>%
      mutate(
        Total_Flights = n()) %>%
      group_by(Airline_Group) %>%
      mutate(Airline_flights = n()) %>%
      ungroup() %>%
      mutate(Market_Share = Airline_flights/Total_Flights) %>%
      group_by(Airline_Group) %>%
      summarise(`Market Share` = mean(Market_Share)) %>%
      arrange(desc(`Market Share`)) %>%
      head(n=10) %>%
      print() %>%
      ggplot(
        aes(
          area = `Market Share`,
          group = Airline_Group, 
          fill = `Market Share`, 
          label = paste(Airline_Group, scales::percent(`Market Share`, accuracy = 0.1), sep = "\n"))
            ) +
      geom_treemap(fill = "#0000CC") +
      geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
      labs(title = "2019 Market Shares", x = "Airline", y = "Market Share") +
      theme(legend.position = "none", plot.title = element_text(size = 20))
  })
  
}

  
# Run the application 
shinyApp(ui = ui, server = server)
  
  
