library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)


ui <- page_navbar(
  title = "Sirius: Rice Crop Simulation ",
  selected = "Schema Builder",
  collapsible = FALSE,
  theme = bslib::bs_theme(),
  sidebar = sidebar(
    title = "Settings",
    open = "closed",
    width = "350px",
    textInput(
      inputId = "txt_siriushome",
      label = "Sirius Home",
      value = "W:/Projects/SiRiUS"
    ),
    textInput(
      inputId = "txt_oryzaexe",
      label = "Oryza Executable",
      value = "./oryza/ORYZA3.exe"
    ),
    textInput(
      inputId = "txt_soilgriddl",
      label = "SoilGrids Download Folder",
      value = "./data/soilgrids"
    ),
    textInput(
      inputId = "txt_oryzasoildir",
      label = "ORYZA Soil File Folder",
      value = "./data/soil"
    ),
    textInput(
      inputId = "txt_oryzawth",
      label = "ORYZA Weather File Folder",
      value = "./data/weather/agera5"
    )
  ),
  nav_panel(
    title = "Schema Builder",
    grid_container(
      layout = c(
        "num_chicks grd_schemaparams"
      ),
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "250px",
        "1fr"
      ),
      gap_size = "5px",
      grid_card(
        area = "num_chicks",
        card_header("Settings"),
        card_body(
          actionButton(
            inputId = "bt_createschema",
            label = "New Schema"
          ),
          actionButton(
            inputId = "bt_editschema",
            label = "Edit Schema"
          )
        )
      ),
      grid_card(
        area = "grd_schemaparams",
        card_body(
          grid_container(
            layout = c(
              "area_schemabasics area_location",
              "area_timeperiod   grd_actions  "
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
              area = "grd_actions",
              card_body(
                actionButton(inputId = "bt_cancel", label = "Cancel"),
                actionButton(inputId = "bt_saveschema", label = "Save")
              )
            ),
            grid_card(
              area = "area_schemabasics",
              card_header("Basics"),
              card_body(
                textInput(
                  inputId = "txt_schemaname",
                  label = "Schema Name",
                  value = "",
                  width = "100%"
                ),
                textInput(
                  inputId = "txt_expfile",
                  label = "ORYZA Experiment File",
                  value = "",
                  width = "100%"
                ),
                textInput(
                  inputId = "txt_cropfile",
                  label = "ORYZA Crop File",
                  value = "",
                  width = "100%"
                ),
                checkboxInput(
                  inputId = "cb_usesoilgrids",
                  label = "Use SoilGrids-derived soil files ",
                  value = FALSE
                ),
                checkboxInput(
                  inputId = "cb_useagera5",
                  label = "Use CDS AgERA5 weather",
                  value = FALSE
                )
              )
            ),
            grid_card(
              area = "area_location",
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
                  value = 2001,
                  min = 1979
                ),
                checkboxInput(
                  inputId = "cb_contsim",
                  label = "Reset soil state for each run",
                  value = FALSE
                ),
                textInput(
                  inputId = "myTextInput",
                  label = "Text Input",
                  value = ""
                )
              )
            ),
            grid_card(
              area = "area_timeperiod",
              card_header("Area of Interest (AOI)"),
              card_body(
                textInput(
                  inputId = "txt_aoiname",
                  label = "AOI Name",
                  value = "",
                  width = "100%"
                ),
                radioButtons(
                  inputId = "rbt_alignaoi",
                  label = "Align AOI to",
                  choices = list(
                    "SoilGrids" = "soilgrid",
                    "CDS AgERA5" = "agera5",
                    "Other" = "other"
                  ),
                  width = "100%"
                )
              ),
              card_body()
            )
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
  nav_panel(title = "Plotter"),
  nav_panel(title = "Sample")
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
  

