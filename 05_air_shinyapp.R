#### VISUALIZING INFORMATION IN A DASHBOARD -- SHINY APP ####

# Take a look at this website to get a compelling navbar:
# https://shiny.rstudio.com/gallery/navbar-example.html

library("tidyverse")
library("lubridate")
library("shiny")

pollutants.dash.df <- read_csv("./WorkingData/pollutants_31103_dashboard.csv")
glimpse(pollutants.dash.df)

pollutants.dash.df$Station_ID <- as.factor(pollutants.dash.df$Station_ID)

pollutants.dash.df$NO2.level <- factor(pollutants.dash.df$NO2.level,
                                       levels = c("Low","Medium","High","Very High"),
                                       ordered = TRUE)
pollutants.dash.df$PM25.level <- factor(pollutants.dash.df$PM25.level,
                                        levels = c("Low","Medium","High","Very High"),
                                        ordered = TRUE)
pollutants.dash.df$O3.level <- factor(pollutants.dash.df$O3.level,
                                      levels = c("Low","Medium","High","Very High"),
                                      ordered = TRUE)

# Get the maximum and the average values for each pollutant to be displayed
no2.avg <- paste("Avg. value: ", signif(mean(pollutants.dash.df$NO2, na.rm = TRUE), digits = 4),"ppb")
no2.max <- paste("Max. value: ", as.integer(max(pollutants.dash.df$NO2, na.rm = TRUE)),"ppb")
pm25.avg <- paste("Avg. value: ", signif(mean(pollutants.dash.df$PM25, na.rm = TRUE), digits = 4),"ug/m3")
pm25.max <- paste("Max. value: ", as.integer(max(pollutants.dash.df$PM25, na.rm = TRUE)),"ug/m3")
o3.avg <- paste("Avg. value: ", signif(mean(pollutants.dash.df$O3, na.rm = TRUE), digits = 4),"ppb")
o3.max <- paste("Max. value: ", as.integer(max(pollutants.dash.df$O3, na.rm = TRUE)),"ppb")

# Create the dataframe to be used to plot the daily average through the year
pollutants.daily.df <- pollutants.dash.df[,c("Date","NO2","PM25","O3")]

pollutants.daily.df <- pollutants.daily.df %>% group_by(Date) %>% 
  summarize(NO2 = mean(NO2, na.rm = TRUE), PM25 = mean(PM25, na.rm = TRUE), O3 = mean(O3, na.rm = TRUE)) %>% 
  group_by(Date) %>% gather("Pollutant","Value",2:4)

pollutants.daily.df$Pollutant <- factor(pollutants.daily.df$Pollutant)
pollutants.daily.df$Value[is.na(pollutants.daily.df$Value)] <- 0                #Assign zeros to NAs for plotting

## Get current time from system to create a more interactive experience for users
ara.mateix <- Sys.time()

ara.hora <- hour(ara.mateix)
ara.data <- paste0("2012-", str_pad(month(ara.mateix),2,"left","0"), "-", 
                   str_pad(day(ara.mateix),2,"left","0"))

## Define a pollutants palette, ie: NO2 = red, PM25 = blue, O3 = dark orange
pollutants.palette <- c("#8a0f0f","#000099","#707d1c")

## Here starts the code for the Shiny App!!
ui <- navbarPage("Air Quality Dashboard", selected = "Tab 1",
  
  # Header :: Shiny Apps
  tags$head(tags$style(HTML("
                            .shiny-text-output {
                            background-color:#fff;
                            }
                            "))),
  
  h1("Toronto Downtown", span("Air Quality", style = "font-weight: 300"), 
     style = "font-family: 'Source Sans Pro';
     color: #fff; text-align: center;
     background-color: #275d8b;
     padding: 20px"),
  br(),
  
  tabPanel("Instant Air Quality", value = "Tab 1",
           # Short text introducing this tab
           fluidRow(
             column(8, offset = 2,
                    p("For a given hour, get the values of NO2, PM2.5 and O3 in the air", 
                      style = "font-family: 'Source Sans Pro';")
             )
           ),
           
           
           br(),
           
  # Filter + Information row
  fluidRow(
    
    column(6,
           wellPanel(
             fluidRow(
               column(6,
                      h4("Select Day"),
                      dateInput("dateId",label = NULL,value = ara.data, min = "2012-01-01", 
                                max = "2012-12-31", format = "yyyy-mm-dd", startview = "month", 
                                weekstart = 1, language = "en", autoclose = TRUE), 
                      actionButton("actionId", "Let's go!", 
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
               ),
               column(6,
                      h4("Select Hour"),
                      sliderInput("hourId",label = NULL, min = 0, max = 23, value = ara.hora)
               )
             )
           )
    ),
    
    column(6,
           wellPanel(
             textOutput("selectedDayId"),
             br(),
             h4("Current Air Quality Health Index", style = "text-align: center;"),
             h3(textOutput("aqhi.valueId"), style = "text-align: center;")
           )
    )
    
  ),
  
  # First line of widgets
  fluidRow(
    
    column(4,
           wellPanel(
             h3("NO2"),
             hr(),
             #h2("11 ppb", style = "color:#888888; text-align:center"), 
             h2(textOutput("no2.valueId"), style = "color:#888888; text-align:center"),
             hr(),
             p(no2.avg, style = "color:#888888;"),
             p(no2.max, style = "color:#888888;")
           )
    ),
    
    column(4,
           wellPanel(
             h3("PM2.5"),
             hr(),
             h2(textOutput("pm25.valueId"), style = "color:#888888; text-align:center"), 
             hr(),
             p(pm25.avg, style = "color:#888888;"),
             p(pm25.max, style = "color:#888888;")
           )
    ),
    
    column(4,
           wellPanel(
             h3("O3"),
             hr(),
             h2(textOutput("o3.valueId"), style = "color:#888888; text-align:center"), 
             hr(),
             p(o3.avg, style = "color:#888888;"),
             p(o3.max, style = "color:#888888;")
           )
    )
    
  )
  
  ),
  
  tabPanel("Yearly Evolution", value = "Tab 2",
           fluidPage(
             # Short text introducing this tab
             fluidRow(
               column(8, offset = 2,
                      p("Evolution of the daily average during the year", 
                        style = "font-family: 'Source Sans Pro';")
               )
             ),
             
             
             br(),
             
             sidebarLayout(
               sidebarPanel(
                 
                 helpText("Choose a pollutant from the list below to see its evolution through
                          the year 2012 in Toronto Downtown station."),
                 
                 radioButtons("pollutantId", "Pollutant:",
                              choices = list("Dioxide Nitrogen" = "NO2",
                                             "Particulate 2.5" = "PM25",
                                             "Ground Ozone" = "O3"),
                              # choices = list("NO2" = "NO2", 
                              #                "PM25" = "PM25", 
                              #                "O3" = "O3"), 
                              selected = "NO2")
                 
               ),
               
               mainPanel(
                 plotOutput("yearlyplotId")
              )
              
             )
           )
  )
)

server <- function(input, output, session) {
  
  # The ignoreNULL parameter set to FALSE in the eventReactive function
  # allows us to show the default value when loading the app
  selectedDate <- eventReactive(input$actionId, {
    paste("You have selected this time... ", input$dateId, " at ", input$hourId, "h")
    
  }, ignoreNULL = FALSE)
  
  selectedRow <- eventReactive(input$actionId, {
    #filter df!!!
    pollutants.dash.df[pollutants.dash.df$Date == input$dateId & pollutants.dash.df$Hour == input$hourId,]
    
  }, ignoreNULL = FALSE)
  
  selectedPollutant <- eventReactive(input$pollutantId, {
    # Take the good plot and filter by pollutant!!
    pollutants.daily.df[pollutants.daily.df$Pollutant == input$pollutantId,]
  }, ignoreNULL = FALSE)
  
  colorPollutant <- eventReactive(input$pollutantId, {
    # input$pollutantId == "NO2" (value of radioButton )
    if (input$pollutantId == "NO2") { pollutants.palette[1] }
    else if (input$pollutantId == "PM25") { pollutants.palette[2] }
    else { pollutants.palette[3] }
  }, ignoreNULL = FALSE)
  
  titlePlot.dyn <- eventReactive(input$pollutantId, {
    # input$pollutantId == "NO2" (value of radioButton )
    if (input$pollutantId == "NO2") { "Dioxide Nitrogen (ppb)" }
    else if (input$pollutantId == "PM25") { "Particulate Matter 2.5 (micrograms / m3)" }
    else { "Ground Ozone (ppb)" }
  }, ignoreNULL = FALSE)
  
  observe({
    output$selectedDayId <- renderText({selectedDate()})
    
    output$no2.valueId <- renderText({ 
                            if(!is.na(selectedRow()$NO2)) {
                              paste(selectedRow()$NO2,"ppb") 
                            } else {
                              "Not available"
                            }
                          })
    
    output$pm25.valueId <- renderText({ 
                              if(!is.na(selectedRow()$PM25)) {
                                paste(selectedRow()$PM25,"ug/m3") 
                              } else {
                                "Not available"
                              }
                            })
    
    output$o3.valueId <- renderText({ 
                            if(!is.na(selectedRow()$O3)) {
                              paste(selectedRow()$O3,"ppb") 
                            } else {
                              "Not available"
                            }
                          })
    
    output$aqhi.valueId <- renderText({ 
      if(!is.na(selectedRow()$aqhi)) {
        selectedRow()$aqhi 
      } else {
        "Not available"
      }
    })
    
    output$yearlyplotId <- renderPlot({
      
      # ggplot(data = selectedPollutant(), aes(Date,Value)) + geom_line(aes(color=Pollutant)) +
      #   scale_colour_manual(values = pollutants.palette) + theme(axis.title = element_blank())
      ggplot(data = selectedPollutant(), aes(Date,Value)) + 
        geom_line(color=colorPollutant()) + ggtitle(titlePlot.dyn()) +
        theme(axis.title = element_blank())
      
    })
    
  })
  
}

## Run the ShinyApp & Have Fun!!
shinyApp(ui, server)
