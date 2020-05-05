###############################################################################
###   AUTHOR: Nathan Basch
###   DATE: 5/6/2002
###
###   DESCRIPTION: SHINY APP TO VIEW USDA QUICKSTATS DATA
###############################################################################

### Set Environment -------------------------
options(stringsAsFactors = F)
options(shiny.usecairo=TRUE)

#Load libraries
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(tigris)
library(DT)

#Read Data ---------------------------------------------------
df_names<-c("national", "sp_counties", "county_all")
u<-paste0("https://github.com/ngbasch/indigo-case/raw/master/",df_names,".Rda")

for (i in (1:length(df_names))){
  load(url(u[i]))
}

#Function to clean units --------------------------------------
addUnits <- function(n, dec = 0) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3, dec), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6, dec), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9, dec), 'B'), # in billions
                                        paste0(round(n/1e12, dec),'T')
                                 ))))
  return(labels)
}

#Set units and crop colors --------------------------------------
units<-county_all%>%group_by(statisticcat_desc, unit_desc, commodity_desc)%>%summarise(t = max(value_num))
classes<-county_all%>%distinct(commodity_desc, class_desc)

#APP -----------------------------------------------------------------------------------------------------
ui<- navbarPage("Indigo Case Study", id="nav",
           
       tabPanel("Interactive Map",
            div(class="outer",
              tags$head(
            # Include custom CSS
              includeCSS("https://raw.githubusercontent.com/ngbasch/indigo-case/master/styling.css")
              ),
          #Interactive Map UI -----------------------------------------------------------------------
              leafletOutput("map", width="100%", height="100%"),
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                            width = 330, height = "auto",
                                      
                            h2("Control"),
                            #Control Panel:
                            selectInput("year", "Year", 
                                        seq(1990, 2019, 1),
                                        selected = 2010),
                            selectInput("crop", "Crop", sort(unique(county_all$commodity_desc)), selected = "CORN"),
                            selectizeInput("variety", "Variety","ALL CLASSES", selected = "ALL CLASSES"),
                            selectInput("metric", "Metric", sort(unique(county_all$statisticcat_desc)), selected = "PRODUCTION"),
                            plotOutput("temporalPlot", height = 200)
                        ),
                        #Footnote
                        tags$div(id="cite",
                                 'Source: NASS QuickStats Data. Compiled by Nathan Basch')
                    )),
        #National Trends UI -----------------------------------------------------------
        tabPanel("National Trends",
                sidebarLayout(
                  #Panel Choices
                  sidebarPanel(
                       sliderTextInput("year_nat", "Years", 
                                 choices = as.character(seq(1990,2019,1)),
                                 selected = c("1990", "2019")),
                       pickerInput("crop_nat", 
                                   "Crop", 
                                   multiple = TRUE,
                                    choices = sort(unique(county_all$commodity_desc)),
                                    selected = "CORN",
                                    options = list(`live-search` = TRUE,
                                                   `actions-box` = TRUE)),
                        uiOutput("variety_nat"),
                        selectInput("metric_nat", "Metric", sort(unique(county_all$statisticcat_desc)), selected = "PRODUCTION"),
                        width = 2
                       ),
                  #Plotly Output
                  mainPanel(
                    fluidRow(column(12, plotlyOutput("national_plot", height = '600px')))
                  )
                )
              ),
       #County Table UI -----------------------------------------------------------
       tabPanel("All Counties Table",
            uiOutput("link"),
            hr(),
            DT::dataTableOutput("county_table")
       )
)
        
                  

server<-  function(input, output, session) {

  #Interactive Map Server ----------------------------------------------------------------
  #Update input to display the correct varieties based on crop 
  observeEvent(input$crop,{
     updateSelectizeInput(session = session, inputId = 'variety',
                        choices = county_all%>%
                          filter(commodity_desc==input$crop,year == input$year)%>%
                          pull(class_desc)%>%unique()%>%sort(),
                        selected = "ALL CLASSES")
   })
  
  #Only allow user to choose years where data exists for a given crop (Cotton data does not exist in 2019) 
  observeEvent(input$crop,{
    years= county_all%>%filter(commodity_desc==input$crop)%>%pull(year)%>%unique()%>%sort()
      
    updateSelectizeInput(session = session, inputId = 'year',
                         choices = years,
                         selected = input$year
                         )
  })
  
  #Only allow user to choose crops where data exists for a given year
  observeEvent(input$year,{
    crops= county_all%>%filter(year==input$year)%>%pull(commodity_desc)%>%unique()%>%sort()
    
    updateSelectizeInput(session = session, inputId = 'crop',
                         choices = crops,
                         selected = input$crop)
    
  })
  
  #For Logs, print user input:
  observe({print(c(input$year, input$crop,input$variety, input$metric))})
  
    #Reactive expression for the data subsetted to what the user selected
    df_reactive<-reactive({
      
      #Change class to "ALL CLASSES" if user selects a class that doesn't exist for a crop.
      class<-ifelse(nrow(classes%>%filter(commodity_desc == input$crop, class_desc== input$variety))>0, 
                    input$variety,
                    "ALL CLASSES")
      
      nass<-
        county_all%>%
        filter(year == input$year, 
               commodity_desc == input$crop,
               statisticcat_desc == input$metric,
               class_desc == class,
               #Remove "Other combined Counties" for graphing purposes
               county_code != "998")%>%
        mutate(fips = paste0(state_fips_code, county_code))%>%
        select(state_alpha, fips, value_num)

      #Join county NASS data and spatial data
      joined <- geo_join(counties_tigris, nass, "GEOID","fips")
      
      #For rendering speed, remove data where counties are missing
      joined<-joined%>%
        subset(!is.na(value_num))
      
      
      return(joined)
    })
    
    #Reactive DF for national data (for Interactive Map tab)
    national_df<-reactive({
      national%>%
        filter(commodity_desc == input$crop,
               statisticcat_desc == input$metric,
               class_desc == input$variety)
        
    })
    

    

    # This reactive expression represents the palette function,
    mypal <- reactive({
      range = df_reactive()[["value_num"]]
      colorNumeric(palette = c("#fffdd0", "#FF8C00", "#006400"), domain = range, na.color = "grey")
    })
    
  
    
    # Create the map
    output$map <- renderLeaflet({
      
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)

      
    })
    
    # Add Polygons (counties) to the map
    observe({
      req(df_reactive)
      
      pal <- mypal()
      temp<-df_reactive()
      
      leafletProxy("map", data = temp) %>%
        clearShapes() %>%
        addPolygons(data = temp, #stroke = FALSE, smoothFactor = 0.2,
                    fillColor = ~pal(temp$value_num),
                    weight = 2,
                    color = "white",
                    dashArray = 4,
                    #dashArray = 3,
                    fillOpacity = 0.7,
                    label = ~temp$NAME,
                    popup = paste("Region: ", temp$NAME, "<br>",
                                  "State: ", temp$state_alpha, "<br>",
                                  paste0(input$metric,": "), comma(temp$value_num), "<br>"),
                    highlight =
                      highlightOptions(
                        weight = 1,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE)
        )
    })
    
    #Add Legend to the map
    observe({
      req(df_reactive)
      
      pal <- mypal()
      temp<-df_reactive()
      unit<-units%>%filter(statisticcat_desc == input$metric, commodity_desc == input$crop)%>%pull(unit_desc)
      
      leafletProxy("map", data = temp) %>%
        clearControls() %>%
        addLegend(position = "bottomleft", 
                  pal = pal, 
                  values = temp$value_num,
                  title = paste0(input$metric, " (", unit,")"),
                  opacity = 1)
      
      
    })
    
    #Create plot of national totals on the "Interactive Map" tab
    output$temporalPlot <- renderPlot({
      req(national_df)
      
      unit<-units%>%filter(statisticcat_desc == input$metric, commodity_desc == input$crop)%>%pull(unit_desc)
      
      gg<-national_df()%>%
        mutate(highlight = ifelse(year == input$year, "Y","N"))%>%
          ggplot(aes(x = year, y = value_num, fill = highlight))+
            geom_col()+
            scale_fill_manual( values = c( "Y"="tomato", "N"="gray" ), guide = FALSE )+
            scale_y_continuous(label = addUnits)+
            labs(x = "Year", y = paste0(input$metric, " (", unit,")"), 
                title = "National Total")+
            theme_minimal()
      
      return(gg)
        
    })
    
    # National Trends Server ----------------------------------------------------------

    #Update Varieties based on crops chosen
    output$variety_nat <- renderUI({
      
      #Make a tiered list of classes. ("WINTER" will be under "COTTON")
      v<-national%>%filter(commodity_desc %in% input$crop_nat, class_desc != "ALL CLASSES")%>%distinct(commodity_desc, class_desc)
      x<-list()
      for (i in unique(v$commodity_desc)){
        temp<-list(TEMP = v%>%filter(commodity_desc == i)%>%pull(class_desc))
        names(temp) <- i
        
        x<-c(x, temp)
      }
      
      pickerInput("variety_nat_var",
                  "Variety",
                  multiple = TRUE,
                  selected = "ALL CLASSES",
                  choices = c("ALL CLASSES",x),
                  options = list(`live-search` = TRUE,
                                 `actions-box` = TRUE)
      )
    })
    
    #Set Group Colors for each crop
    group_colors<- reactive({
      
      group_colors <- c(CORN = "#bc8d3d", COTTON = "#69a75b", RICE ="#6785d0", SOYBEANS = "#b75fb3", WHEAT = "#cc5658")
      
      #Change titles if unit is added for Production and Yield Graph
      names(group_colors) <-  national%>%
        filter(statisticcat_desc == input$metric_nat)%>%
        mutate(commodity_desc = paste0(commodity_desc," (",unit_desc,")"))%>%
        pull(commodity_desc)%>%
        unique()%>%
        sort()
      
      if (input$metric_nat == "AREA HARVESTED" | input$metric_nat == "AREA PLANTED"){
        group_colors <- c(CORN = "#bc8d3d", COTTON = "#69a75b", RICE ="#6785d0", SOYBEANS = "#b75fb3", WHEAT = "#cc5658")
      }
      
      return(group_colors)
      
    })
    
    #Plot national statistics using plotly ("National Trends" tab)
    output$national_plot<-renderPlotly({
      #Wait until user fills out these inputs:
      req(input$crop_nat, input$variety_nat_var, input$metric_nat, input$year_nat)
      
    
      gg<-national%>%
        filter(commodity_desc %in% input$crop_nat, 
               class_desc %in% input$variety_nat_var, 
               statisticcat_desc == input$metric_nat,
               year>= as.numeric(input$year_nat[1]), year <= as.numeric(input$year_nat[2])
               )
        
      #For Are Harvested and Planted, show all crops on one graph, since units are the same.
      if (input$metric_nat %in% c("AREA HARVESTED", "AREA PLANTED")){
        
        p<-ggplot(gg, aes(x = year, y = value_num, color = commodity_desc, linetype = class_desc))+
              geom_line()+
              geom_point(size = .8,aes(text = paste0("Year: ", year, "<br>",
                                                 "Value: ", addUnits(value_num, dec = 3))))+
              scale_y_continuous(label = addUnits)+
              theme(legend.title = element_blank())+
              scale_color_manual(values=group_colors())+
              labs(x = "", y = "Acres",color = "", linetype = "")+
              theme_bw()
        }else{
      
      #For Production and Yield, show all crops on different graphs, since units are different.
      p<- 
        gg%>%
          mutate(commodity_desc = paste0(commodity_desc," (",unit_desc,")"))%>%
            ggplot(aes(x = as.integer(year), y = value_num, color = commodity_desc, linetype = class_desc))+
            geom_line()+
            geom_point(size = .8,aes(text = paste0("Year: ", year, "<br>",
                                                   "Value: ", addUnits(value_num, dec = 3))))+
            scale_y_continuous(label = addUnits)+
            labs(x = "", y= "", color = "", linetype = "")+
            theme(legend.title = element_blank())+
            facet_wrap(~commodity_desc, scales = "free")+
            scale_color_manual(values=group_colors())+
            theme_bw()+
            theme(strip.text = element_text(face="bold", size=7))
        
        }
      
      #If someone chooses interval of two or fewer years, show those years on x axis (instead of some decimal difference)
      if (as.numeric(input$year_nat[2]) - as.numeric(input$year_nat[1]) <= 2){
        p<-p+scale_x_continuous(breaks = seq(as.numeric(input$year_nat[1]), as.numeric(input$year_nat[2]), 1))
      }
      #Replace yield with commas instead of "K" 
      if(input$metric_nat == "YIELD"){
        p<-p+scale_y_continuous(label = comma)
      }
      
      ggplotly(p, tooltip = "text")
      
    })
    
    #Data table server --------------------------------------------------------------------------------------
    #Hyperlink to GitHub
    url <- a("https://github.com/ngbasch/indigo-case", href="https://github.com/ngbasch/indigo-case")
    output$link <- renderUI({
      tagList("See GitHub repository for data cleaning code:", url)
    })
    
    #Data Table output
    output$county_table<-renderDataTable({
      
      datatable(county_all%>%
        mutate(year = as.integer(year),
              value_num = round(value_num),
               asd_code = as.character(asd_code))%>%
        select(-source_desc, -util_practice_desc, -prodn_practice_desc),    
      options = list(pageLength = 100,
                     columnDefs=list(list(targets=0:11, className="dt-left"))),
      filter = "top",
      rownames= FALSE
    )%>%
      formatCurrency("value_num", currency = "", interval = 3, mark = ",", digits = 0)
  })
    
  }
  
shinyApp(ui, server)
  