library(shiny)

library(dplyr)
library(ggmap)
library(sp)
library(leaflet)
library(DT)
library(rcdimple)
library(howfar)
library(c3)

# http://stackoverflow.com/questions/34348737/r-leaflet-how-to-click-on-map-and-add-a-circle

#folder <- 'G:/Advisory and Intelligence/Intelligence and Analysis/DATA_STORE'

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
                tags$p('choose between Punlic Transport on the left and driving your own car on the right'),
                sliderInput("comm", h3("Community"),
                            -1,
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
                sliderInput("ent", h3("Entertainment"),
                            0,
                            1,
                            value = 0, step = .1, ticks = FALSE, width = '80%'
                ),
                tags$p('choose your preference for Entertainment Options')
               ),
        tabPanel('Map',
            div(class="outer",
                tags$head(
                    # Include our custom CSS
                    includeCSS("styles.css")
                ),
                #tags$style('.leaflet {height: 100%; width: 100%;}'),
                leafletOutput("map", width = "100%", height = '100%'),
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 330, height = "auto",
                              sliderInput("buffer", h3("Buffer Distance"),
                                          0.5,
                                          10,
                                          value = 5, step = 0.5
                              )
                ),
                tags$div(id="cite",
                         'Data compiled by ', tags$em('Psychedelic Prosthetics'), ', GovHack 2016.'
                )
            )

        ),
        tabPanel('Report',
                 h3('Risk Profile'),
                 dimpleOutput('risk_plot', width="100%")
                 )
        )
)

server <- shinyServer(function(input, output, session) {

    # get unigue foi types for dropdown
    #foi.types <- list(get.unique(system.file("db.sqlite", package = "howfar"),
                                # 'foi', 'FEATURE_SU'))
    #names(foi.types) <- as.character(foi.types)

    #updateSelectInput(session = session, inputId = "select_fld", choices = foi.types)

    ## One alternative: store circles data?
    ## I dont actually implement this, but you would do this in the observer as well
    dat <- reactiveValues(circs = data.frame(lng=numeric(),
                                             lat=numeric(),
                                             address=character()))

    dat$foi <- data.frame(latitude = numeric(),
                          longitude = numeric(),
                          Name = character(),
                          Type = character(),
                          distance = character())

    ## Make your initial map
    output$map <- renderLeaflet({
        leaflet() %>%
            setView(lng = 145, lat = -38, zoom = 11) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addScaleBar('bottomright')
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
 

        })
    })





})

shinyApp(ui = ui, server = server)
