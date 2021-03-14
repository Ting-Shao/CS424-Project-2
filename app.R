# sample R + Shiny example for CS 424 Spring 2020 UIC - Andy Johnson
# www.evl.uic.edu/aej/424

# This is a sample dashboard making use of the evl room temperature data and displaying
# it in a variery of ways to show off some of the different capabilities of R and Shiny
# and the Shiny Dashboard.

#libraries to include

library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(usmap)
library(stringr)

#read data
egrid2018<- read_excel("egrid2018_data_modified.xlsx", sheet = "PLNT18", col_names = TRUE,skip = 1)
egrid2000<- read_excel("eGRID2000_plant_modified.xls", sheet = "EGRDPLNT00", col_names = TRUE,skip = 2)
egrid2010<- read_excel("eGRID2010_Data_modified.xls", sheet = "PLNT10", col_names = TRUE,skip = 4)

#preprocess data
colnames(egrid2000)<-sub("\n.*", "", colnames(egrid2000))
egrid2000$LON <- as.numeric(egrid2000$LON)
egrid2000$LAT <- as.numeric(egrid2000$LAT)
egrid2000$LON <- -egrid2000$LON
mapping <-c("1"= "COAL",
            "2"= "OIL",
            "3"= "GAS",
            "4"= "NUCLEAR",
            "5"= "HYDRO",
            "6"= "BIOMASS",
            "7"= "WIND",
            "8"= "SOLAR",
            "9"= "GEOTHERMAL",
            "10"= "OTHER",
            "11"= "OTHER"
)
egrid2000$TYPE <-unname(mapping [max.col(egrid2000[,9:18], ties.method = "first")])
egrid2010$TYPE <-unname(mapping [max.col(egrid2010[,11:21], ties.method = "first")]) 
egrid2018$TYPE <-unname(mapping [max.col(egrid2018[,10:20], ties.method = "first")])

egrid2018_IL <- subset(egrid2018, PSTATABB=="IL")
egrid2000_IL <- subset(egrid2000, PSTATABB=="IL")
egrid2010_IL <- subset(egrid2010, PSTATABB=="IL")



color_pal <-c("COAL"= "darkblue",
              "OIL"="purple",
              "GAS"= "lightgreen",
              "NUCLEAR"= "cadetblue",
              "HYDRO"= "lightblue",
              "BIOMASS"="beige",
              "WIND"= "darkgreen",
              "SOLAR"="pink",
              "GEOTHERMAL"= "red",
              "OTHER" = "gray",
              "OTHF"="gray",
              "OFSL"="gray"
              )

Source <- names(color_pal)[c(1:9)]
Source <- append(Source, "OTHER")
colors<- unname(color_pal)[c(1:9)]
colors <- append(colors, "gray")
renewables  <- c("NUCLEAR",
                 "HYDRO",
                 "BIOMASS",
                 "WIND",
                 "SOLAR",
                 "GEOTHERMAL")
n_renewables  <- c("COAL",
                   "OIL",
                   "GAS")

c_state<-setNames(state.abb, state.name)[state.name]
#c_state<-append(c_state, c("Washington DC"="DC"))
#c_state<-append(c("US-TOTAL"="US-TOTAL"),c_state)


getColor <- function(data) {
    sapply(data$TYPE, function(TYPE) {
        if(is.na(TYPE)){"gray"}
        else {
            color_pal[TYPE] 
        } }, USE.NAMES = FALSE)
}



#================================================================================
# Create the shiny dashboard
ui <- dashboardPage(
    dashboardHeader(title = "CS424 Spring 2021 Project1"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     sidebarMenu(
                         id="tabs",
                         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("Split screen visualization", tabName = "split", icon = icon("dashboard")),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("The entire US", tabName = "entire_US", icon = icon("dashboard")),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("Plants added or idled", tabName = "Plants_variation", icon = icon("dashboard")),
                         
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                         menuItem("About page", tabName = "about", icon = icon("question"))
                     ),
                     hr()
    ),
    #==========    ==========    ==========    dashboard    ==========    ==========    ==========    
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    fluidRow(
                        column(12,
                               box( width = NULL, status = "primary", solidHeader = TRUE, title= "Plants in Illinois",
                                    column(2,
                                           checkboxInput("All_e", "All", TRUE),
                                           actionButton("button", "Reset"),),
                                    column(6,checkboxGroupInput("variable_e", "Energy source to show:",
                                                                Source,
                                                                selected =Source,
                                                                inline = TRUE),),
                                    column(4,checkboxGroupInput("variable_r", "Renewability:",
                                                                c("renewables","non-renewables"),
                                                                selected = c("renewables","non-renewables"),
                                                                inline = TRUE),),
                                    
                                    #helpText("Graphical representation of the reactive expressions called in the app. It is a minimal example with only the color and horizon setting as adjustable value. To build the graphic please use the mouse and drag the blue bar to the right."),
                                    leafletOutput("Plants_in_Illinois", height = 500)
                               )
                               )
                    )
            ),
            tabItem(tabName = "split",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title= "Split screen visualization",
                         #helpText("Graphical representation of the reactive expressions called in the app. It is a minimal example with only the color and horizon setting as adjustable value. To build the graphic please use the mouse and drag the blue bar to the right."),
                         column(6,
                                column(6,
                                       selectInput('year1', 'Year1', c("2000", "2010", "2018"), selected = "2000")),
                                column(6,
                                       selectInput('state1', 'State1', c_state, selected = "IL")),
                                
                                column(3,
                                       checkboxInput("All_e1", "All", TRUE),
                                       actionButton("button1", "Reset"),),
                                column(6,checkboxGroupInput("variable_e1", "Energy source to show:",
                                                            Source,
                                                            selected =Source,
                                                            inline = TRUE),),
                                column(3,checkboxGroupInput("variable_r1", "Renewability:",
                                                            c("renewables","non-renewables"),
                                                            selected = c("renewables","non-renewables"),
                                                            inline = TRUE),),
                                leafletOutput("Plants_in_Illinois1", height = 450)
                                ),
                         column(6,
                                column(6,
                                       selectInput('year2', 'Year2', c("2000", "2010", "2018"), selected = "2018")
                                       ),
                                column(6,
                                       selectInput('state2', 'State2', c_state, selected = "IL")
                                       ),
                                
                                column(3,
                                       checkboxInput("All_e2", "All", TRUE),
                                       actionButton("button2", "Reset"),
                                       actionButton("buttons", "Same setting"),),
                                column(6,checkboxGroupInput("variable_e2", "Energy source to show:",
                                                            Source,
                                                            selected =Source,
                                                            inline = TRUE),),
                                column(3,checkboxGroupInput("variable_r2", "Renewability:",
                                                            c("renewables","non-renewables"),
                                                            selected = c("renewables","non-renewables"),
                                                            inline = TRUE),),
                                leafletOutput("Plants_in_Illinois2", height = 450)
                         ),
                    )
            ),
            tabItem(tabName = "entire_US",
                    fluidRow(
                        column(12,
                               box( width = NULL, status = "primary", solidHeader = TRUE, title= "Plants in the US",
                                    column(4,
                                           column(6,
                                                  selectInput('statel', 'State', append(c("US"="US"),c_state), selected = "US"),
                                                  ),
                                           column(6,
                                                  selectInput('yearl', 'Year', c("2000", "2010", "2018"), selected = "2018"),
                                           ),
                                           sliderInput("sliderl", "Generation amount (MWh)",
                                                       min = 0, max = 31199935, value = c(0, 31199935)),
                                    ),
                                    column(1,
                                           checkboxInput("All_el", "All", FALSE),
                                           actionButton("buttonl", "Reset"),),
                                    column(5,
                                           checkboxGroupInput("variable_el", "Energy source to show:",
                                                                Source,
                                                                #selected =Source,
                                                                inline = TRUE),
                                           textOutput("wrong")
                                           ),
                                    column(2,checkboxGroupInput("variable_rl", "Renewability:",
                                                                c("renewables","non-renewables"),
                                                                #selected = c("renewables","non-renewables"),
                                                                inline = TRUE),),
                                    
                                    #helpText("Graphical representation of the reactive expressions called in the app. It is a minimal example with only the color and horizon setting as adjustable value. To build the graphic please use the mouse and drag the blue bar to the right."),
                                    leafletOutput("Plants_in_US", height = 500)
                               )
                        )
                    ),
                    fluidRow(
                      box(title = "Table of raw numbers", solidHeader = TRUE, status = "primary", 
                          width = 12, 
                          collapsible = TRUE,
                          #h5("The amount of each energy source"),
                          dataTableOutput("Table", height = 200)
                      )
                    ),
            ),
            tabItem(tabName = "Plants_variation",
                    fluidRow(
                      column(12,
                             box( width = NULL, status = "primary", solidHeader = TRUE, title= "Plants added or idled",
                                  
                                  
                                  column(6,
                                         helpText("This leaflet shows which plants were added or idled in 2010"),
                                         leafletOutput("pv2010", height = 500),
                                         ),
                                  column(6,
                                         helpText("This leaflet shows which plants were added or idled in 2018"),
                                         leafletOutput("pv2018", height = 500),
                                         ),
                             )
                      )
                    )
            ),

            
            tabItem(tabName = "about",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title= "About page",
                         #helpText("Graphical representation of the reactive expressions called in the app. It is a minimal example with only the color and horizon setting as adjustable value. To build the graphic please use the mouse and drag the blue bar to the right."),
                         mainPanel(
                             h1("Data reference"),
                             h3("https://www.eia.gov/electricity/data/state/ "),
                             h1("App developer"),
                             h3("Ting-Shao, Lee"),
                             h3("This application is part of my CS424 project 1 at the University of Illinois at Chicago, Spring 2021."),
                         )
                    )
            )
        )     
    ))




#================================================================================
server <- function(input, output, session) {
    
    # increase the default font size
    theme_set(theme_grey(base_size = 18)) 
    
    # calculate the values one time and re-use them in multiple charts to speed things up
    
    TEPI_state1 <- reactive({subset(TEPI, STATE==input$state1)})
    TEPI_state2 <- reactive({subset(TEPI, STATE==input$state2)})
    
    TEPI_state1Reactive <- reactive({subset(TEPI,STATE==input$state1&ENERGY.SOURCE!="Total")})
    TEPI_state2Reactive <- reactive({subset(TEPI,STATE==input$state2&ENERGY.SOURCE!="Total")})
    
    justOneYearReactive <- reactive({subset(allData, year(allData$newDate) == input$Year)})
    newNoonsReactive <- reactive({subset(allData, year(allData$newDate) == input$Year & Hour == 12)})
    oneRoomNoonReactive <- reactive({subset(allData$input$Room, year(allData$newDate) == input$Year & Hour == 12)})
    
    # in 2017 it was y=justOneYear["Hour"] - needed to make a change for 2018
    
    
    
  
    
    output$Plants_in_Illinois <- renderLeaflet({
        dataset<-egrid2018_IL
        dataset <- subset(dataset, dataset$TYPE %in% input$variable_e)
        
        icons <- awesomeIcons(
            icon = 'power-outline',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(dataset)
        )
        icons$markerColor <-unname(icons$markerColor )
        
        
        map <- leaflet(data=dataset) %>% addTiles() %>%
            addLegend(colors = colors, label = Source,
                      title = "Illinois (2018)",
                      position = "bottomright",
                      opacity = 1
            )
        if (!is.null(input$variable_e)&dim(dataset)[1]!=0){
          map <- addAwesomeMarkers(map, ~LON, ~LAT, icon=icons, popup = paste("Plant :", dataset$PNAME, "<br>",
                                                                              "coal", dataset$PLGENACL, "<br>",
                                                                              "oil", dataset$PLGENAOL, "<br>",
                                                                              "gas", dataset$PLGENAGS, "<br>",
                                                                              "nuclear", dataset$PLGENANC, "<br>",
                                                                              "hydro", dataset$PLGENAHY, "<br>",
                                                                              "wood", dataset$PLGENABM, "<br>",
                                                                              "wind", dataset$PLGENAWI, "<br>",
                                                                              "solar", dataset$PLGENASO, "<br>",
                                                                              "geothermal", dataset$PLGENAOF, "<br>",
                                                                              "Renewables generation:", dataset$PLTRPR*100,"%", "<br>",
                                                                              "Nonrenewables generation:", dataset$PLTNPR*100,"%"), 
                                   label = ~TYPE,
                                   clusterOptions = markerClusterOptions()
                                   )
        }
        map <- setView(map, lng = -89.3985, lat = 40.6331, zoom = 6)
        map
    })
    
    
    output$Plants_in_Illinois1 <- renderLeaflet({
        if (input$year1=="2000") {
            dataset<-egrid2000
        }else if ( input$year1=="2010") {
            dataset<-egrid2010
        }else{
            dataset<-egrid2018
        }
        STATE=input$state1
        dataset <- subset(dataset, PSTATABB==STATE)
        dataset <- subset(dataset, dataset$TYPE %in% input$variable_e1)
        
        icons <- awesomeIcons(
            icon = 'power-outline',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(dataset)
        )
        icons$markerColor <-unname(icons$markerColor)
        
        map <- leaflet(data=dataset) %>% addTiles(group = "OSM (default)") %>%
          addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
          addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
          addLayersControl(
            baseGroups = c("OSM (default)", "OpenTopoMap", "Toner Lite"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
            addLegend(colors = colors, label = Source,
                      title = paste(input$state1, input$year1),
                      position = "bottomright",
                      opacity = 1
            )
        if (!is.null(input$variable_e1)&dim(dataset)[1]!=0){
          map <- addAwesomeMarkers(map, ~LON, ~LAT, icon=icons, popup = ~PNAME, label = ~TYPE, clusterOptions = markerClusterOptions())
        }
        #map <- setView(map, lng = -87.647998, lat = 41.870, zoom = 18)
        map
    })
    
    output$Plants_in_Illinois2 <- renderLeaflet({
        if (input$year2=="2000") {
            dataset<-egrid2000
        }else if ( input$year2=="2010") {
            dataset<-egrid2010
        }else{
            dataset<-egrid2018
        }
        STATE=input$state2
        dataset <- subset(dataset, PSTATABB==STATE)
        dataset <- subset(dataset, dataset$TYPE %in% input$variable_e2)
        
        icons <- awesomeIcons(
            icon = 'power-outline',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(dataset)
        )
        icons$markerColor <-unname(icons$markerColor )
        
        map <- leaflet(data=dataset) %>% 
          addTiles(group = "OSM (default)") %>%
          addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
          addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
          addLayersControl(
            baseGroups = c("OSM (default)", "OpenTopoMap", "Toner Lite"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
            addLegend(colors = colors, label = Source,
                      title = paste(input$state2, input$year2),
                      position = "bottomright",
                      opacity = 1
            )
        if (!is.null(input$variable_e2)&dim(dataset)[1]!=0){
          map <- addAwesomeMarkers(map, ~LON, ~LAT, icon=icons, popup = ~PNAME, label = ~TYPE, clusterOptions = markerClusterOptions())
        }
        #map <- setView(map, lng = -87.647998, lat = 41.870, zoom = 18)
        map
    })

    
    
    
    
    output$Plants_in_US <- renderLeaflet({
        if (input$yearl=="2000") {
            dataset<-egrid2000
        }else if ( input$yearl=="2010") {
            dataset<-egrid2010
        }else{
            dataset<-egrid2018
        }
        STATE=input$statel
        if( STATE!="US"){dataset <- subset(dataset, PSTATABB==STATE)}

        if (is.null(input$variable_el)){
          dataset <- subset(dataset, TYPE %in% c())
        }else{
          dataset <- subset(dataset, TYPE %in% input$variable_el)
        }
        dataset <- subset(dataset, PLNGENAN >input$sliderl[1]&PLNGENAN <input$sliderl[2])

        icons <- awesomeIcons(
            icon = 'power-outline',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(dataset)
        )
        icons$markerColor <-unname(icons$markerColor )

        map <- leaflet(data=dataset) %>% addTiles(group = "OSM (default)") %>%
          addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
          addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
          addLayersControl(
            baseGroups = c("OSM (default)", "OpenTopoMap", "Toner Lite"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
            addLegend(colors = colors, label = Source,
                      title = paste("Energy source", input$yearl),
                      position = "bottomright",
                      opacity = 1
            )
        if (!is.null(input$variable_el)&dim(dataset)[1]!=0){
          map <- addAwesomeMarkers(map, ~LON, ~LAT, icon=icons, popup = ~PNAME, label = ~TYPE,clusterOptions = markerClusterOptions())
        }
        
        map <- setView(map, lng = -98.58, lat = 39.82, zoom = 4)
        map
    })
    
    output$pv2010 <- renderLeaflet({
      ORISPL00<-unique(egrid2000$ORISPL)
      ORISPL10<-unique(egrid2010$ORISPL)
      idled10<-subset(egrid2000, !(ORISPL %in% ORISPL10))
      added10<-subset(egrid2010, !(ORISPL %in% ORISPL00))
      
      
      
      
      map <- leaflet() %>% 
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") 
      
      
      
      map <-addLayersControl(map,
          baseGroups = c("OSM (default)", "OpenTopoMap", "Toner Lite"),
          overlayGroups = c("added10", "idled10"),
          options = layersControlOptions(collapsed = FALSE)
        ) 
      
    
      icons <- awesomeIcons(
        icon = 'power-outline',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(idled10) #rep(c("black"),dim(idled10)[1])
      )
      icons$markerColor <-unname(icons$markerColor)
      map <- addAwesomeMarkers(map, idled10$LON, idled10$LAT, icon=icons, group = "idled10", popup = idled10$PNAME, label = idled10$TYPE, clusterOptions = markerClusterOptions())

      icons <- awesomeIcons(
        icon = 'power-outline',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(added10) #rep(c("black"),dim(added10)[1])
      )
      icons$markerColor <-unname(icons$markerColor)
      map <- addAwesomeMarkers(map, added10$LON, added10$LAT, icon=icons, group = "added10", popup = added10$PNAME, label = added10$TYPE, clusterOptions = markerClusterOptions())
      map <- addLegend(map, colors = colors, label = Source,
                title = "US (2010)",
                position = "bottomright",
                opacity = 1
      )
      map <- setView(map, lng = -98.58, lat = 39.82, zoom = 4)
      map
    })
    
    
    output$pv2018 <- renderLeaflet({
      ORISPL10<-unique(egrid2010$ORISPL)
      ORISPL18<-unique(egrid2018$ORISPL)
      idled18<-subset(egrid2010, !(ORISPL %in% ORISPL18))
      added18<-subset(egrid2018, !(ORISPL %in% ORISPL10))
      
      
      map <- leaflet() %>% 
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") 
      
      
      
      map <-addLayersControl(map,
                             baseGroups = c("OSM (default)", "OpenTopoMap", "Toner Lite"),
                             overlayGroups = c("added18", "idled18"),
                             options = layersControlOptions(collapsed = FALSE)
      ) 
      
      
      icons <- awesomeIcons(
        icon = 'power-outline',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(idled18) #rep(c("black"),dim(idled18)[1])
      )
      icons$markerColor <-unname(icons$markerColor)
      map <- addAwesomeMarkers(map, idled18$LON, idled18$LAT, icon=icons, group = "idled18", popup = idled18$PNAME, label = idled18$TYPE, clusterOptions = markerClusterOptions())
      
      icons <- awesomeIcons(
        icon = 'power-outline',
        iconColor = 'black',
        library = 'ion',
        markerColor = getColor(added18) #rep(c("black"),dim(added18)[1])
      )
      icons$markerColor <-unname(icons$markerColor)
      map <- addAwesomeMarkers(map, added18$LON, added18$LAT, icon=icons, group = "added18", popup = added18$PNAME, label = added18$TYPE, clusterOptions = markerClusterOptions())
      
      map <- addLegend(map, colors = colors, label = Source,
                       title = "US (2018)",
                       position = "bottomright",
                       opacity = 1
      )
      map <- setView(map, lng = -98.58, lat = 39.82, zoom = 4)
      map
    })
    
    
    
    
    
    
    
    
    output$wrong <- renderText({ 
      if (input$yearl=="2000") {
        dataset<-egrid2000
      }else if ( input$yearl=="2010") {
        dataset<-egrid2010
      }else{
        dataset<-egrid2018
      }
      STATE=input$statel
      if( STATE!="US"){dataset <- subset(dataset, PSTATABB==STATE)}
      
      if (is.null(input$variable_el)){
        dataset <- subset(dataset, TYPE %in% c())
      }else{
        dataset <- subset(dataset, TYPE %in% input$variable_el)
      }
      dataset <- subset(dataset, PLNGENAN >input$sliderl[1]&PLNGENAN <input$sliderl[2])
      input$variable_el
      #paste("You have selected", !is.null(input$variable_el))
    })
    
    output$Table <- DT::renderDataTable(
      DT::datatable({ 
        if (input$yearl=="2000") {
          dataset<-egrid2000
        }else if ( input$yearl=="2010") {
          dataset<-egrid2010
        }else{
          dataset<-egrid2018
        }
        STATE=input$statel
        if( STATE!="US"){dataset <- subset(dataset, PSTATABB==STATE)}
        
        if (is.null(input$variable_el)){
          dataset <- subset(dataset, TYPE %in% c())
        }else{
          dataset <- subset(dataset, TYPE %in% input$variable_el)
        }
        dataset <- subset(dataset, PLNGENAN >input$sliderl[1]&PLNGENAN <input$sliderl[2])
        dataset
      }, 
      options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(0, 'asc')), scrollX = T
      ), rownames = FALSE 
      )
    )
    
    #Update reset button ==================================================
    observeEvent(input$button, {
      updateCheckboxInput(session, "All_e", "All", TRUE)
      updateCheckboxGroupInput(session, "variable_e", "Energy source to show:",
                               Source,
                               selected =Source,
                               inline = TRUE
      )
      updateCheckboxGroupInput(session, "variable_r", "Renewability:",
                               c("renewables","non-renewables"),
                               selected = c("renewables","non-renewables"),
                               inline = TRUE
      )
    })
    observeEvent(input$button1, {
      updateCheckboxInput(session, "All_e1", "All", FALSE)
      updateSelectInput(session,'state1', 'State1', c_state, selected = "IL")
      updateSelectInput(session,'year1', 'Year1', c("2000", "2010", "2018"), selected = "2000")
      updateCheckboxGroupInput(session, "variable_e1", "Energy source to show:",
                               Source,
                               selected =Source,
                               inline = TRUE
      )
      updateCheckboxGroupInput(session, "variable_r1", "Renewability:",
                               c("renewables","non-renewables"),
                               selected = c("renewables","non-renewables"),
                               inline = TRUE
      )
    })
    observeEvent(input$button2, {
      updateCheckboxInput(session, "All_e2", "All", FALSE)
      updateSelectInput(session,'state2', 'State1', c_state, selected = "IL")
      updateSelectInput(session,'year2', 'Year2', c("2000", "2010", "2018"), selected = "2018")
      updateCheckboxGroupInput(session, "variable_e2", "Energy source to show:",
                               Source,
                               selected =Source,
                               inline = TRUE
      )
      updateCheckboxGroupInput(session, "variable_r2", "Renewability:",
                               c("renewables","non-renewables"),
                               selected = c("renewables","non-renewables"),
                               inline = TRUE
      )
    })
    observeEvent(input$buttonl, {
      updateCheckboxInput(session, "All_el", "All", FALSE)
      updateSelectInput(session,'statel', 'State', append(c("US"="US"),c_state), selected = "US")
      updateSelectInput(session,'yearl', 'Year', c("2000", "2010", "2018"), selected = "2018")
      updateCheckboxGroupInput(session, "variable_el", "Energy source to show:",
                               Source,
                               #selected =Source,
                               inline = TRUE
      )
      updateCheckboxGroupInput(session, "variable_rl", "Renewability:",
                               c("renewables","non-renewables"),
                               #selected = c("renewables","non-renewables"),
                               inline = TRUE
      )
      updateSliderInput(session, "sliderl", label = "Generation amount (MWh)", min = 0, max = 31199935, value = c(0, 31199935))
    })
    
    
    #Update Same setting ==================================================
    observeEvent(input$buttons, {
      updateCheckboxInput(session, "All_e2", "All", input$All_e1)
      updateCheckboxGroupInput(session, "variable_r2", "Renewability:",
                               c("renewables","non-renewables"),
                               selected = input$variable_r1,
                               inline = TRUE
      )
      updateCheckboxGroupInput(session, "variable_e2", "Energy source to show:",
                               Source,
                               selected =input$variable_e1,
                               inline = TRUE
      )
    })
    
    
    #Update ALL ==================================================
    observe({
        if (input$All_e){
            updateCheckboxGroupInput(session, "variable_e","Energy source to show:",
                                     Source,
                                     selected =Source,
                                     inline = TRUE
            )
            updateCheckboxGroupInput(session, "variable_r", "Renewability:",
                               c("renewables","non-renewables"),
                               selected = c("renewables","non-renewables"),
                               inline = TRUE
            )
        }
        if (input$All_e1){
            updateCheckboxGroupInput(session, "variable_e1", "Energy source to show:",
                                     Source,
                                     selected =Source,
                                     inline = TRUE
            )
            updateCheckboxGroupInput(session, "variable_r1", "Renewability:",
                                     c("renewables","non-renewables"),
                                     selected = c("renewables","non-renewables"),
                                     inline = TRUE
            )
        }
        if (input$All_e2){
            updateCheckboxGroupInput(session, "variable_e2", "Energy source to show:",
                                     Source,
                                     selected =Source,
                                     inline = TRUE
            )
            updateCheckboxGroupInput(session, "variable_r2", "Renewability:",
                                     c("renewables","non-renewables"),
                                     selected = c("renewables","non-renewables"),
                                     inline = TRUE
            )
        }
        if (input$All_el){
            updateCheckboxGroupInput(session, "variable_el", "Energy source to show:",
                                     Source,
                                     selected =Source,
                                     inline = TRUE
            )
            updateCheckboxGroupInput(session, "variable_rl", "Renewability:",
                                     c("renewables","non-renewables"),
                                     selected = c("renewables","non-renewables"),
                                     inline = TRUE
            )
        }
      
    })
    
    #Update Renewability ==================================================
    observe({
      #Update Plants in Illinois Renewability
      if(is.null(input$variable_r)){
        updateCheckboxInput(session, "All_e", "All", FALSE)
        updateCheckboxGroupInput(session, "variable_e", "Energy source to show:",
                                 Source,
                                 #selected =Source,
                                 inline = TRUE
        )
      }else{
        if(length(input$variable_r)==1){
          updateCheckboxInput(session, "All_e", "All", FALSE)
         if (input$variable_r%in%c("renewables")){
           updateCheckboxGroupInput(session, "variable_e", "Energy source to show:",
                                    Source,
                                    selected =renewables,
                                    inline = TRUE
           )
         }else if (input$variable_r%in%c("non-renewables")){
           updateCheckboxGroupInput(session, "variable_e", "Energy source to show:",
                                    Source,
                                    selected =n_renewables,
                                    inline = TRUE
           )
         }
        }else{
          updateCheckboxInput(session, "All_e", "All", TRUE)
        }
      }
      
      
      
      
      #Update Split Renewability
      #split1
      if(is.null(input$variable_r1)){
        updateCheckboxInput(session, "All_e1", "All", FALSE)
        updateCheckboxGroupInput(session, "variable_e1", "Energy source to show:",
                                 Source,
                                 #selected =Source,
                                 inline = TRUE
        )
      }else{
        if(length(input$variable_r1)==1){
          updateCheckboxInput(session, "All_e1", "All", FALSE)
          if (input$variable_r1%in%c("renewables")){
            updateCheckboxGroupInput(session, "variable_e1", "Energy source to show:",
                                     Source,
                                     selected =renewables,
                                     inline = TRUE
            )
          }else if (input$variable_r1%in%c("non-renewables")){
            updateCheckboxGroupInput(session, "variable_e1", "Energy source to show:",
                                     Source,
                                     selected =n_renewables,
                                     inline = TRUE
            )
          }
        }else{
          updateCheckboxInput(session, "All_e1", "All", TRUE)
        }
      }
      
      #split2
      
      if(is.null(input$variable_r2)){
        updateCheckboxInput(session, "All_e2", "All", FALSE)
        updateCheckboxGroupInput(session, "variable_e2", "Energy source to show:",
                                 Source,
                                 #selected =Source,
                                 inline = TRUE
        )
      }else{
        if(length(input$variable_r2)==1){
          updateCheckboxInput(session, "All_e2", "All", FALSE)
          if (input$variable_r2%in%c("renewables")){
            updateCheckboxGroupInput(session, "variable_e2", "Energy source to show:",
                                     Source,
                                     selected =renewables,
                                     inline = TRUE
            )
          }else if (input$variable_r2%in%c("non-renewables")){
            updateCheckboxGroupInput(session, "variable_e2", "Energy source to show:",
                                     Source,
                                     selected =n_renewables,
                                     inline = TRUE
            )
          }
        }else{
          updateCheckboxInput(session, "All_e2", "All", TRUE)
        }
      }
      
      #Plants in the US
      if(is.null(input$variable_rl)){
        updateCheckboxInput(session, "All_el", "All", FALSE)
        updateCheckboxGroupInput(session, "variable_el", "Energy source to show:",
                                 Source,
                                 #selected =Source,
                                 inline = TRUE
        )
      }else{
        if(length(input$variable_rl)==1){
          updateCheckboxInput(session, "All_el", "All", FALSE)
          if (input$variable_rl%in%c("renewables")){
            updateCheckboxGroupInput(session, "variable_el", "Energy source to show:",
                                     Source,
                                     selected =renewables,
                                     inline = TRUE
            )
          }else if (input$variable_rl%in%c("non-renewables")){
            updateCheckboxGroupInput(session, "variable_el", "Energy source to show:",
                                     Source,
                                     selected =n_renewables,
                                     inline = TRUE
            )
          }
        }else{
          updateCheckboxInput(session, "All_el", "All", TRUE)
        }
      }
    })
    
    
    
    
    
   
    
    
    
    
    
    
    
    
    
    
    #    Line chart 
    # observe({
    #     if (input$All){
    #         updateCheckboxGroupInput(session, "variable_e", "Variables to show:",
    #                                  c("Coal",
    #                                    "Geothermal",
    #                                    "Hydro",
    #                                    "NaturalGas",
    #                                    "Nuclear",
    #                                    "Petroleum",
    #                                    "Solar",
    #                                    "Wind",
    #                                    "Wood"),
    #                                  selected = c("Coal",
    #                                               "Geothermal",
    #                                               "Hydro",
    #                                               "NaturalGas",
    #                                               "Nuclear",
    #                                               "Petroleum",
    #                                               "Solar",
    #                                               "Wind",
    #                                               "Wood"),
    #                                  inline = TRUE
    #         )
    #     }
    #     if (input$All2){
    #         updateCheckboxGroupInput(session, "variable2", "Variables to show:",
    #                                  c("Coal",
    #                                    "Geothermal",
    #                                    "Hydro",
    #                                    "NaturalGas",
    #                                    "Nuclear",
    #                                    "Petroleum",
    #                                    "Solar",
    #                                    "Wind",
    #                                    "Wood"),
    #                                  selected = c("Coal",
    #                                               "Geothermal",
    #                                               "Hydro",
    #                                               "NaturalGas",
    #                                               "Nuclear",
    #                                               "Petroleum",
    #                                               "Solar",
    #                                               "Wind",
    #                                               "Wood"),
    #                                  inline = TRUE
    #         )
    #     }
    # })
    # 
    # observe({
    #     # TRUE if input$controller is odd, FALSE if even.
    #     if(length(input$variable)<9){
    #         updateCheckboxInput(session, "All", "All", FALSE)
    #     }
    #     else if(length(input$variable)==9){
    #         updateCheckboxInput(session, "All", "All", TRUE)
    #     }
    #     
    #     if(length(input$variable2)<9){
    #         updateCheckboxInput(session, "All2", "All", FALSE)
    #     }
    #     else if(length(input$variable2)==9){
    #         updateCheckboxInput(session, "All2", "All", TRUE)
    #     }
    # })
    # 
    
    
   
    
    
    
    
    
    
    # 5 interesting comparisons
    # observe({
    #     if (input$tabs=="com1"){
    #         updateSelectInput(session, 'es1', label = 'Energy Source1', choices = c_es,
    #                           selected = "Coal")
    #         updateSelectInput(session, "year1", label = "Year1", choices = years,
    #                           selected = 1990)
    #         updateSelectInput(session, 'es2', label = 'Energy Source1', choices = c_es,
    #                           selected = "Coal")
    #         updateSelectInput(session, "year2", label = "Year1", choices = years,
    #                           selected = 2019)
    #     }
    #     if (input$tabs=="com2"){
    #         updateSelectInput(session, 'es1', label = 'Energy Source1', choices = c_es,
    #                           selected = "Natural Gas")
    #         updateSelectInput(session, "year1", label = "Year1", choices = years,
    #                           selected = 1990)
    #         updateSelectInput(session, 'es2', label = 'Energy Source1', choices = c_es,
    #                           selected = "Natural Gas")
    #         updateSelectInput(session, "year2", label = "Year1", choices = years,
    #                           selected = 2019)
    #     }
    # })
    
    
    
    
    
}

shinyApp(ui = ui, server = server)
