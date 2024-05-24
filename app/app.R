library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)


ui <- page_navbar(
  title = "Sirius: Spatial Rice Crop Simulation ",
  selected = "Schema Builder",
  collapsible = FALSE,
  theme = bslib::bs_theme(),
  sidebar = sidebar(
    title = "Settings",
    textInput(
      inputId = "txt_siriushome",
      label = "Sirius Home",
      value = "W:/Projects/SiRiUS",
      width = "100%"
    ),
    textInput(
      inputId = "txt_oryzaexe",
      label = "Oryza Executable",
      value = "./oryza/ORYZA3.exe",
      width = "100%"
    ),
    textInput(
      inputId = "txt_cropcaldir",
      label = "Crop Calendar Folder",
      value = "./data/cropcalendar",
      width = "100%"
    ),
    textInput(
      inputId = "txt_soilgriddl",
      label = "SoilGrids Download Folder",
      value = "./data/soilgrids",
      width = "100%"
    ),
    textInput(
      inputId = "txt_oryzasoildir",
      label = "ORYZA Soil File Folder",
      value = "./data/soil",
      width = "100%"
    ),
    textInput(
      inputId = "txt_oryzawth",
      label = "ORYZA Weather File Folder",
      value = "./data/weather/agera5",
      width = "100%"
    )
  ),
  nav_panel(
    title = "Home",
    grid_container(
      layout = c(
        "grd_schemas .",
        ".           ."
      ),
      row_sizes = c(
        "1fr",
        "1fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      gap_size = "10px",
      grid_card(
        area = "grd_schemas",
        full_screen = TRUE,
        card_header("Schemas")
      )
    )
  ),
  nav_panel(
    title = "Schema Builder",
    grid_container(
      layout = c(
        "schema_toolbar grd_schemaeditor"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "115px",
        "1fr"
      ),
      gap_size = "5px",
      grid_card(
        area = "schema_toolbar",
        card_header("Tool"),
        card_body(
          actionButton(inputId = "bt_createschema", label = "New"),
          actionButton(inputId = "bt_editschema", label = "Edit"),
          actionButton(inputId = "bt_saveschema", label = "Build"),
          actionButton(inputId = "bt_clear", label = "Clear")
        )
      ),
      grid_card(
        area = "grd_schemaeditor",
        card_body(
          tabsetPanel(
            selected = "Basic",
            nav_panel(
              title = "Schema",
              grid_container(
                layout = c(
                  "schema_basics  schema_basics  schema_location schema_simperiod",
                  "grd_experiment grd_experiment schema_location schema_simperiod",
                  "grd_soil       grd_soil       .               .               ",
                  "grd_weather    grd_weather    .               .               "
                ),
                row_sizes = c(
                  "1.06fr",
                  "0.99fr",
                  "0.95fr",
                  "1fr"
                ),
                col_sizes = c(
                  "1fr",
                  "1fr",
                  "1fr",
                  "1fr"
                ),
                gap_size = "10px",
                grid_card(
                  area = "schema_basics",
                  card_header("Schema Basics"),
                  card_body(
                    textInput(
                      inputId = "txt_schemaname",
                      label = "Schema Name",
                      value = "",
                      width = "100%"
                    ),
                    textInput(
                      inputId = "txt_cropfile",
                      label = "ORYZA Crop File",
                      value = "",
                      width = "100%"
                    )
                  )
                ),
                grid_card(
                  area = "schema_location",
                  card_header("Area of Interest (AOI)"),
                  card_body(
                    textInput(
                      inputId = "txt_aoiname",
                      label = "AOI Name",
                      value = "",
                      width = "100%"
                    ),
                    textInput(
                      inputId = "txt_aoifilename",
                      label = "AOI File",
                      value = "",
                      width = "100%"
                    ),
                    radioButtons(
                      inputId = "rbt_alignaoi",
                      label = "Align AOI to",
                      choices = list(
                        "SoilGrids" = "soilgrid",
                        "CDS AgERA5" = "agera5",
                        "My AOI raster" = "own"
                      ),
                      width = "100%"
                    )
                  )
                ),
                grid_card(
                  area = "schema_simperiod",
                  card_header("Simulation Period"),
                  card_body(
                    numericInput(
                      inputId = "num_startyear",
                      label = "Start Year",
                      value = 2001,
                      min = 1979
                    ),
                    numericInput(
                      inputId = "num_endyear",
                      label = "End Year",
                      value = 2002,
                      min = 1979
                    ),
                    checkboxInput(
                      inputId = "cb_contsim",
                      label = "Continuous simulation",
                      value = FALSE
                    ),
                    checkboxInput(
                      inputId = "cb_usecropcal",
                      label = "Use RiceAtlas Planting Dates",
                      value = FALSE
                    )
                  )
                ),
                grid_card(
                  area = "grd_soil",
                  full_screen = TRUE,
                  card_header("Soil"),
                  card_body(
                    selectInput(
                      inputId = "txt_soilsrc",
                      label = "ORYZA Soil File",
                      choices = list(
                        "SoilGrids.org" = "soilgrids",
                        "Manual " = "manual",
                        "Value3" = "value3"
                      ),
                      selected = "soilgrids"
                    ),
                    checkboxInput(
                      inputId = "cb_usesoilgrids",
                      label = "Use SoilGrids-derived soil files ",
                      value = TRUE
                    )
                  )
                ),
                grid_card(
                  area = "grd_weather",
                  full_screen = TRUE,
                  card_header("Weather"),
                  card_body(
                    checkboxInput(
                      inputId = "cb_useagera5",
                      label = "Use CDS AgERA5 weather",
                      value = TRUE
                    )
                  )
                ),
                grid_card(
                  area = "grd_experiment",
                  full_screen = TRUE,
                  card_header("Experiment"),
                  card_body(
                    textInput(
                      inputId = "txt_expfile",
                      label = "ORYZA Experiment File",
                      value = "",
                      width = "100%"
                    )
                  )
                )
              )
            ),
            nav_panel(title = "Experiment")
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Controller",
    grid_container(
      row_sizes = c(
        "165px",
        "1fr"
      ),
      col_sizes = c(
        "1fr"
      ),
      gap_size = "10px",
      layout = c(
        "facetOption",
        "dists"
      ),
      grid_card_plot(area = "dists"),
      grid_card(
        area = "facetOption",
        card_header("Distribution Plot Options"),
        card_body(
          radioButtons(
            inputId = "distFacet",
            label = "Facet distribution by",
            choices = list("Diet Option" = "Diet", "Measure Time" = "Time")
          )
        )
      )
    )
  ),
  nav_panel(title = "Input Builder"),
  nav_panel(title = "Plotter")
)


server <- function(input, output) {
   
  output$linePlots <- renderPlot({
    obs_to_include <- as.integer(ChickWeight$Chick) <= input$numChicks
    chicks <- ChickWeight[obs_to_include, ]
  
    ggplot(
      chicks,
      aes(
        x = Time,
        y = weight,
        group = Chick
      )
    ) +
      geom_line(alpha = 0.5) +
      ggtitle("Chick weights over time")
  })
  
  output$dists <- renderPlot({
    ggplot(
      ChickWeight,
      aes(x = weight)
    ) +
      facet_wrap(input$distFacet) +
      geom_density(fill = "#fa551b", color = "#ee6331") +
      ggtitle("Distribution of weights by diet")
  })
  
}

shinyApp(ui, server)
  

