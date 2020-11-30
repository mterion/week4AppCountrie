# Online version: https://manuaccount.shinyapps.io/testDispo/

library(leaflet)
library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(htmltools)
library(caret)
library(SQUAREM)
library(rpart)       # for fitting decision trees
library(ipred)       # for fitting bagged decision trees
# install.packages('e1071', dependencies=TRUE)    # Need this to run carret of shinyapps.io
library(e1071)

# If problems loading it into shinyapps.io, type
  # rsconnect::showLogs()
  # my problem: 2020-11-29T06:39:53.599240+00:00 shinyapps[system]: Out of memory!


# Global data and fun
    # Run at start up because does not rely on user input

    rm(list = ls())
    
    # The path for the data dir needs to be in the shinyApp dir, if not, it won't work
    path1 <- ("./data/country-capitals.csv")
    df <- read.csv(path1, stringsAsFactors = FALSE)
    
    path2 <- ("./data/gdpPop.csv")
    gdpPopDf <- read.csv(path2, stringsAsFactors = FALSE)
    gdpPopDf <- gdpPopDf %>%
        mutate(militaryExpAbsolute = gdp * militaryExpenditure / 100) %>% # in millions, not billions
        drop_na(militaryExpAbsolute) %>% # remove the row where milExp is NA
        filter(!countryCode %in% c("ARB", "EAS", "ECS", "MEA", "SAS", "NAC"))
    
    # Create a function for filtering of the data frame
        filterContinent <- function(continent){
                contCapDf <- df %>%
                    filter(ContinentName == continent) %>%
                    rename(lat = CapitalLatitude, long = CapitalLongitude) %>%
                    mutate (lat = as.numeric(lat)) %>%
                    mutate (popupText = paste0("<b>", CountryName,"</b>", "<br/>", CapitalName)) # Need to add col with html tags for pop up on the map
                return(contCapDf)
        }

#=====================================================================================
# UI
#=====================================================================================
  ui <- fluidPage(
      h2("Diverse Informations on Countries"),
      h6("Manu, 26.11.2020"),
    fluidRow(class = "myRow1", 
      column(width = 3,
             div(
                style = "height:320px;", # background-color: yellow; add this in the " " to ajust the size of col/row: background-color: yellow;
                 
                sidebarPanel(
                    radioButtons("radio", h3("Capitals:"),
                                choices = list(
                                  "Africa" = 1, 
                                  "Antarctica" = 2,
                                  "Asia" = 3,
                                  "Australia" = 4,
                                  "Central America" = 5, 
                                  "Europe" = 6,
                                  "North America" = 7,
                                  "South America" = 8
                                  ),selected = 6), width = 13)
        )), # end 1st col
      
      column(9,
             div(
                style = "height:450px;", # background-color: blue;
          
                mainPanel(
                    leafletOutput("mapCont"),
                    width = 13
                )

          
          
      )) # end 2nd col
    ), # end 1st row
    
    
    fluidRow(class = "myRow2",
      column(5,div(
                style = "height:230px;", # background-color: green;
                   
                sidebarPanel(
                    h3("Predict Military Expenditures"),
                    h4("Bivariate relationship"),
                    helpText("Select a model and plot based on either population or gdp and hover on the plot to identify outliers"),
                    
                            radioButtons("radioModel", "",
                                         choices = list(
                                             "Population" = 1, 
                                             "GDP" = 2
                                         ), selected = 1),
                    width = 13
                    ), # end sidebarPannel 2nd row
  
                   
       )), # end 1st col
      
      column(7,
             div(
                style = "height:450px;", #background-color: red; 
                   
                mainPanel(
                    plotlyOutput("plot", width = "100%"),
                    width = 14
                )                   
                   
        )) # end of 2nd col
      ), # end of 2nd row
    
    fluidRow(class = "myRow3",
             column(5,
                    div(
                      style = "height:500px;", # background-color: pink;
                      sidebarPanel(
                        h4("Multivariate relationship"),
                        helpText("Move the sliders to get the prediction and click submit"),
                    
                            sliderInput("popSlider", "Population (millions): ",
                                        min = min(gdpPopDf$population), max = max(gdpPopDf$population),
                                        value=500),
                    
                            sliderInput("gdpSlider", "GDP (billions): ",
                                        min = min(gdpPopDf$gdp), max = max(gdpPopDf$gdp),
                                        value= 10000),
                        br(),
                        tags$b("Prediction for military expenditures (millions): "),
                        br(), 
                        # submitButton("Submit"),  # not included because it acted on the whole App, and to change continent we had to scroll down to the submit button
                        br(),
                    
                        verbatimTextOutput("pred3", placeholder = TRUE),
                        helpText("A machine learning procedure (bootstrap aggregating prediction model, cv:5, nbagg:500)  is used to improve the accuracy of the predictions"),
                        width = 13),
                      h6("Data sources: ", 
                         tags$a(href="http://techslides.com/list-of-countries-and-capitals", "Techslides,"),
                         tags$a(href="https://datacatalog.worldbank.org/", "World Bank"),
                         )
                      ) # end of div
                    
              ), # end of 1st col
             column(7,
                    div(
                          
                      style = "height:650px;", # background-color: red;

                      mainPanel(
                          plotlyOutput("plot3D", height = "auto", width = "auto" ),
                          width = 14)
                      
                    )

                    
              ) # end of 2nd col
     ) # end of 3rd row
  ) # end fluid page


# For issue of row/col height-width, see: https://stackoverflow.com/questions/25340847/control-the-height-in-fluidrow-in-r-shiny
#=====================================================================================
# Server
#=====================================================================================  
  server <- function(input, output) {
        #popUpContent <- paste0(ContCapDf$CountryCode, " / ", ContCapDf$CapitalName)

# Map
        continent <- reactive({input$radio})
        output$mapCont <- renderLeaflet({
            
          # Data -> adjust the output according to selection
                # This need to be donw with the render environment and the call is a fun : continent()
          if(continent() == 1){
              ContCapDf <- filterContinent("Africa")    
          }
          if(continent() == 2){
              ContCapDf <- filterContinent("Antarctica")    
          }
          if(continent() == 3){
              ContCapDf <- filterContinent("Asia")    
          }
          if(continent() == 4){
              ContCapDf <- filterContinent("Australia")    
          }
          if(continent() == 5){
              ContCapDf <- filterContinent("Central America")    
          }
          if(continent() == 6){
              ContCapDf <- filterContinent("Europe")    
          }
          if(continent() == 7){
              ContCapDf <- filterContinent("North America")    
          }
          if(continent() == 8){
              ContCapDf <- filterContinent("South America")    
          }
        
        # Map the selection
            # need to use popup to use html tag(col popupText)
                # using labels (~htmlEscape(Name)) for hovering on it do not work with html tags https://rstudio.github.io/leaflet/popups.html
                # need to use popups if I want to be able to use html tags (those html tags are the col popupText in the Df)
        ContCapDf %>%
          leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
          addTiles() %>%
          addMarkers(
            clusterOptions = markerClusterOptions(),
            popup = ContCapDf$popupText
            ) 

        })
        
# Models
        modelChoice <- reactive({input$radioModel})
        
        # lm
        model1 <- lm(militaryExpAbsolute ~ population, data = gdpPopDf)
        model2 <- lm(militaryExpAbsolute ~ gdp, data = gdpPopDf)
        
        # Df
        dfSelect <- gdpPopDf %>%
                  select(c(gdp, population, militaryExpAbsolute))
                
        # Scale Df into matrix
                sDfSelect <- scale(dfSelect)   # It convert it to a matrix
                
        # Save scale attributes
                scaleList <- list(
                        scale = attr(sDfSelect, "scaled:scale"),
                        center = attr(sDfSelect, "scaled:center"))  
                
        # Convert matrix back into Df for modling    
                sDfSelect <- as.data.frame(sDfSelect) # Convert it back to df
                #head(sDfSelect)
 
#########       
        # Fit scaled model
                # Upload webpage time on shinyapps.io: for 1000 nbag
                        # 11 second if model not saved and uploaded
                        # 5 second if model saved and uploaded
                
        #         sMilExModel <- train(
        #                 militaryExpAbsolute ~.,
        #                 data = sDfSelect,
        #                 method = "treebag",
        #                 trControl = trainControl(method="cv", number=5),
        #                 nbagg=500,
        #                 control=rpart.control(minsplit=2, cp=0.01),
        #                 metric = "accuracy"
        # 
        #         )
        #         sMilExModel
        # 
        # # save Model
        #         saveRDS(sMilExModel, "./final/modelSaved/sMilExModel.rds")   # Memory storage limitation on shinyapps.io -> ok for 500

        # loadModel
                  sMilExModel <- readRDS("./modelSaved/sMilExModel.rds")   # Does not work with sinyapps.io -> out of memory

        # New data
                model3Pred <- reactive({
                        # User inputs
                        popInput <- input$popSlider
                        gdpInput <- input$gdpSlider

                        # Scale inputed values
                        sPop <- (popInput - scaleList$center["population"]) / scaleList$scale["population"]
                        sGdp <- (gdpInput - scaleList$center["gdp"]) / scaleList$scale["gdp"]

                        # SInputed value in a Df
                        newDataInputDf <- data.frame(population = sPop, gdp = sGdp)

                        # Predictions on scaled data:
                        sPred <- predict(sMilExModel, newdata = newDataInputDf)

                        # Convert scaled prediction to original data scale:
                        oPred <- sPred * scaleList$scale["militaryExpAbsolute"] + scaleList$center["militaryExpAbsolute"]
                      })

        output$pred3 <- renderText({
          model3Pred()
        })
        
##########
        
        # plots
        output$plot <- renderPlotly({
            if(modelChoice() ==2){
                fig <- plot_ly(gdpPopDf, x = ~gdp, y = ~militaryExpAbsolute, type = 'scatter', mode = 'markers',
                        hoverinfo = 'text',
                        text = ~paste('</br>', country,
                                      '</br> GDP: ', gdp,
                                      '</br> Military Expenditures: ', militaryExpAbsolute)) %>%
                        add_lines(x =~ gdp, y =~ fitted(model2)) %>%
                        layout(
                                showlegend = F,
                                xaxis = list(title= "GDP (billions)"),
                                yaxis = list(title="Mil. Expenditures (millions)"))
                fig
        
            } else{
                fig <- plot_ly(gdpPopDf, x = ~population, y = ~militaryExpAbsolute, type = 'scatter', mode = 'markers',
                        hoverinfo = 'text',
                        text = ~paste('</br>', country,
                                      '</br> Population: ', population,
                                      '</br> Mil. Expenditures: ', militaryExpAbsolute)) %>%
                        add_lines(x =~ population, y =~ fitted(model1)) %>%
                        layout(
                                showlegend = F,
                                xaxis = list(title= "Population (millions)"),
                                yaxis = list(title="Military Expenditures (millions)"))
                fig                
            } # end of else
        }) # end of render plotly
        
        # 3D scatter: for ref see https://plotly-r.com/controlling-tooltips.html / https://plotly.com/r/setting-graph-size/
                # Got problem for the sizing of this plot, I solwed it with list and height
                # However, this kills the responsive device at small scale
                # My idea = better to use html / css directly instead of UI and parmeter the size of the model according to size of screen (iphone...)
        output$plot3D <- renderPlotly({
          
          fig3D <- plot_ly(gdpPopDf, x=~ population, y =~militaryExpAbsolute, z =~ gdp, color=~country)
          fig3D <- fig3D %>% 
                      add_markers(
                      hovertemplate = "Pop: %{x} <br>Mil: %{y} <br>GDP: %{z}")  # Need to remove space bec html code when <br>
          fig3D <- fig3D %>%                
                    layout(scene = list(xaxis = list(title = 'Population'),
                                        yaxis = list(title = 'Military Expenditures'),
                                        zaxis = list(title = 'GDP')),
                           showlegend = F)
          
          m <- list(
                    l = 0,
                    r = 0,
                    b = 10,
                    t = 0,
                    pad = 0
                  )
          
          fig3D <- fig3D %>% 
                    layout(autosize = T, height = 800, margin = m)
          fig3D
          
        })
} # end of server

#=====================================================================================
# Shiny App
#=====================================================================================  

shinyApp(ui, server)