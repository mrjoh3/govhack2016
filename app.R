library(shiny)
library(dplyr)
library(ggmap)
library(sp)
library(leaflet)
library(DT)
library(rgdal)

source('R/import_spatial.R')


# http://stackoverflow.com/questions/34348737/r-leaflet-how-to-click-on-map-and-add-a-circle
ui <- shinyUI(
    navbarPage("My Livability", id="nav",

        tabPanel('Definition', align = 'center',
                 tags$style(type = "text/css", "
      .irs-bar {width: 100%; height: 25px;}

                            .irs-single, .irs-bar-edge, .irs-bar {
background: transparent; height: 25px; border: 0px;
                            }
                            .irs-line {border: 0px solid grey; height: 25px; border-radius: 10px;}
                            .irs-grid-text {font-family: 'arial'; color: white; bottom: 17px; z-index: 1;}
                            .irs-grid-pol {display: none;}
                            .irs-max {font-family: 'arial'; color: grey;}
                            .irs-min {font-family: 'arial'; color: grey;}
                            .irs-single {color:grey; background:lightgrey;}
                            .irs-slider {width: 30px; height: 30px; top: 22px;}
                            "),
                h3('Define Your Livability'),
                tags$p('The idea of "livability" is inherantly personal. Each person will value a different range of preferences. Using the sliders below please define your own preferences'),
                sliderInput("env", h3("The Environment"),
                            -1,
                            1,
                            value = 0, step = .1, ticks = FALSE, width = '80%'
                ),
                tags$p('choose between the built environment on the left and the natural environment on the right'),
                sliderInput("trans", h3("Transport"),
                            -1,
                            1,
                            value = 0, step = .1, ticks = FALSE, width = '80%'
                ),
                tags$p('choose between Public Transport on the left and driving your own car on the right'),
                sliderInput("comm", h3("Community"),
                            0,
                            1,
                            value = 0, step = .1, ticks = FALSE, width = '80%'
                ),
                tags$p('choose between the "Lone Wolf" on the left and the "Engaged Community" on the right'),
                sliderInput("rec", h3("Recreation"),
                            0,
                            1,
                            value = 0, step = .1, ticks = FALSE, width = '80%'
                ),
                tags$p('choose your preference for recreational facilities such as sporting fields and walking paths'),
                sliderInput("food", h3("Food"),
                            0,
                            1,
                            value = 0, step = .1, ticks = FALSE, width = '80%'
                ),
                tags$p('choose your preference for Restaurant and Dining Options'),
                sliderInput("ent", h3("Education"),
                            0,
                            1,
                            value = 0, step = .1, ticks = FALSE, width = '80%'
                ),
                tags$p('choose your preference for Education Options'),
                actionButton('do', 'Click Here then View Map Tab', label = h3('Calculate Livability'), width = '30%', height = '50px'),
                selectInput("get", label = h3("How Many Suburbs do you want to compare?"), 
                            choices = list("Handfull" = 20, "Some" = 150, "Half" = 800, "All" = 2000), 
                            selected = 20, width = '30%'),
                sliderInput("buffer", h3("Buffer Distance"),
                            0.1,
                            10,
                            value = 1, step = 0.1
                )
               ),
        tabPanel('Map',
            div(class="outer",
                tags$head(
                    # Include our custom CSS
                    includeCSS("styles.css")
                ),
                #tags$style('.leaflet {height: 100%; width: 100%;}'),
                leafletOutput("map", width = "100%", height = '100%'),
                # absolutePanel(id = "controls", left = class = "panel panel-default", fixed = TRUE,
                #               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                #               width = 330, height = "auto",
                #               sliderInput("buffer", h3("Buffer Distance"),
                #                           0.1,
                #                           10,
                #                           value = 1, step = 0.1
                #               )
                # ),
                tags$div(id="cite",
                         'Data compiled by ', tags$em('Psychedelic Prosthetics'), ', GovHack 2016.'
                )
            )

        ),
        tabPanel('Data',
                 h3('Data Summary'),
                 DT::dataTableOutput("table1"),
                 h3('Livibility Choices'),
                 DT::dataTableOutput("table2")
                 )
        )
)

server <- shinyServer(function(input, output, session) {

    # get suburbs calcs
    source('R/data_clean.R')
    
    dat <- reactiveValues(circs = data.frame(lng=numeric(),
                                             lat=numeric(),
                                             address=character()))

    dat$foi <- data.frame(latitude = numeric(),
                          longitude = numeric(),
                          Name = character(),
                          Type = character(),
                          distance = character())
    
    dat$choices <- data.frame(Environment = numeric(),
                              Transport = numeric(),
                              Community = numeric(),
                              Recreation = numeric(),
                              Food = numeric(),
                              Education = numeric())
    
    dat$subs <- subs
    sp.subs <- readOGR('shp', 'SSC_2011_VIC_SIM', stringsAsFactors = F)
    
    observeEvent(input$do, {
            subs <- dat$subs
            subs <- calc_slider(input$env, 'built.p', 'reserve.p', 'env',  subs)
            subs <- calc_slider(input$trans, 'rail.p', 'roads.p', 'trans',  subs)
            subs <- mutate(subs, comm = community.p * input$comm)
            subs <- mutate(subs, rec = sport.p * input$rec)
            #subs <- mutate(subs, food = zomato * input$food)
            #subs <- calc_slider(input$ent, 'rail.p', 'roads.p', 'ent',  subs)
            
            subs <- subs %>%
                mutate(livibility = (env + trans + comm + rec) * 100,
                       livibility = ifelse(is.na(livibility), 0, livibility))
        
            dat$subs <- subs

            dat$sp.subs <- sp.subs
            
            # save values
            isolate(dat$choices <- rbind(dat$choices, data.frame(Environment = input$env,
                                                                 Transport = input$trans,
                                                                 Community = input$comm,
                                                                 Recreation = input$rec,
                                                                 Food = input$food,
                                                                 Education = input$ent)))
    })
    
    
    ## Make your initial map
    output$map <- renderLeaflet({
        
        # if ('livibility' %in% colnames(dat$subs)) {
        isolate(
            sp.subs@data <- left_join(mutate(sp.subs@data, SSC_CODE = as.integer(SSC_CODE)),
                                      select(dat$subs, SSC_CODE, livibility))
        )
        # isolate(
        #     sp.subs <- sp.subs[sp.subs$livibility > sort(sp.subs$livibility, decreasing = T)[input$get], ]
        # )
        
        pal <- colorQuantile('RdYlBu', sp.subs$livibility, n = 12, na.color = 'grey')
        #     
        # } else {
        #     sp.subs$livibility <- 100
        #     pal <- colorQuantile('Greens', dat$sp.subs$livibility, n = 5, na.color = 'grey')
        # }
        
        leaflet() %>%
            setView(lng = 145, lat = -38, zoom = 11) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addScaleBar('bottomright') %>%
            addPolygons(data = sp.subs,
                        color = ~pal(livibility),
                        group = 'Livibility') %>%
            addLayersControl(
                overlayGroups = 'Livibility',
                options = layersControlOptions(collapsed = TRUE)
            ) %>%
            addLegend("bottomright", pal = pal, values = sp.subs$livibility,
                      title = "Livability",
                      #labFormat = labelFormat(prefix = "$"),
                      opacity = 1
            )
        
    })
    
    ## Observe mouse clicks and add circles
    observeEvent(input$map_click, {

        withProgress(message = 'Retrieving Data', value = 0, {
        ## Get the click info like had been doing
        click <- input$map_click
        clat <- click$lat
        clng <- click$lng
        address <- revgeocode(c(clng,clat))

        isolate(dat$circs <- rbind(dat$circs, data.frame(longitude = clng,
                                                         latitude = clat,
                                                         address = address)))

        db <- "db.sqlite"

        # check which buffer distance to query
        D = input$buffer / 100 # this is very rough conversion from km to DD

        # setup map
        popup <- paste(sep = '<br>',
                       '<b>%s</b>',
                       'Type: %s',
                       'Distance: %.0fm')
        iconSet = awesomeIconList(
            'primary school' = makeAwesomeIcon(markerColor = 'cadetblue',
                                               icon = 'graduation-cap', library = 'fa'),
            'secondary school' = makeAwesomeIcon(markerColor = 'darkred',
                                                 icon = 'graduation-cap', library = 'fa'),
            'aged care' = makeAwesomeIcon(markerColor = 'red',
                                          icon = 'bed', library = 'fa'),
            'general hospital' = makeAwesomeIcon(markerColor = 'lightred',
                                                 icon = 'h-square', library = 'fa'),
            'Other' = makeAwesomeIcon(markerColor = 'grey',
                                      icon = 'question-circle', library = 'fa')
        )
        ## Add the circle to the map proxy
        ## so you dont need to re-render the whole thing
        ## I also give the circles a group, "circles", so you can
        ## then do something like hide all the circles with hideGroup('circles')
        leafletProxy('map') %>% # use the proxy to save computation
            addMarkers(lng = clng, lat = clat,
                       group = 'circles',
                       popup = address)



                foi <- get.points(db, 'foi', clng, clat, D) %>%
                    dplyr::select(Name = NAME_LABEL,
                                  Type = FEATURE_SU,
                                  longitude, latitude, distance) %>%
                    dplyr::filter(!is.na(longitude),
                                  !is.na(latitude),
                                  Type != 'emergency marker') %>%
                    dplyr::mutate(icon = ifelse(Type %in% c('primary school',
                                                            'secondary school',
                                                            'aged care',
                                                            'general hospital'), Type, "Other"),
                                  risk = ifelse(Type %in% c('primary school','aged care'), 5,
                                                ifelse(Type %in% c('secondary school','general hospital'), 4, 1)))

                

                
                if (nrow(foi) > 0) {
                    leafletProxy('map') %>%
                        addAwesomeMarkers(data = foi, lat = ~latitude, lng = ~longitude,
                                          popup = ~sprintf(popup, Name, Type, distance),
                                          icon = ~iconSet[icon]) %>%
                        fitBounds(min(foi$longitude),
                                  min(foi$latitude),
                                  max(foi$longitude),
                                  max(foi$latitude))
                    # store data
                    isolate(dat$foi <- rbind(dat$foi, foi))

                }
 
                
                
                # parse suburb e.g
                cells <- revgeocode(c(clng,clat),output = 'all')$results[[1]]$address_components[[3]]$short_name
                
                # import data
                zomato <- read_csv("csv/suburbs_br_rest_zomato2.csv")
                
                # find suburb
                suburb_find <- zomato[zomato$suburb == cells,]
                
                # if statement (if suburb not available, show no map, otherwise create json file)
                if (nrow(suburb_find) > 0) {
                    leafletProxy('map') %>%
                        addMarkers(data = suburb_find,
                                   lng = ~lon, lat = ~lat,
                                   popup = paste("Name:", suburb_find$name, "<br>",
                                               "Cuisines:", suburb_find$cuisines, "<br>",
                                               "User Rating:", suburb_find$user_rating, "<br>",
                                               "Link:", suburb_find$href)
                        )
                }
                    
                

        })
    })
    
    
    output$table1 <- DT::renderDataTable({
        datatable(dat$subs,
                  class = 'dt-body nowrap',
                  extensions = c('Buttons','Responsive'),
                  options = list(pageLength = 5,
                                 dom = 'Bfrtip',
                                 buttons = c('csv', 'excel'))) #%>%
            #formatRound(c('longitude','latitude'), 4)
    })

    output$table2 <- DT::renderDataTable({
        datatable(dat$choices,
                  class = 'dt-body nowrap',
                  extensions = c('Buttons','Responsive'),
                  options = list(pageLength = 5,
                                 dom = 'Bfrtip',
                                 buttons = c('csv', 'excel'))) #%>%
        #formatRound(c('longitude','latitude'), 4)
    })



})

shinyApp(ui = ui, server = server)
