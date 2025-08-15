pacman::p_load(
  shiny,
  shinythemes,
  shinyWidgets,
  shinyBS,
  shinyjs,
  DT,
  openxlsx,
  bslib
)

# load helper functions
source(here::here("R", "helpers.R"))
source(here::here("R", "xlsx_build.R"))

# increase upload size
options(shiny.maxRequestSize = 100 * 1024^2)

ui <- bslib::page_navbar(
  title = "Simmons Segmentation",
  theme = bslib::bs_theme(bootswatch = "flatly", version = 5),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # TAB 1 - LOAD DATA -------------------------------------------------------
  bslib::nav_panel(
    "Start here",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        id = "data_sidebar",
        open = TRUE,
        width = "300px",
        shiny::uiOutput("startHere"),
        shiny::fileInput(
          inputId = 'inFile',
          'Choose xlsx file',
          accept = c(".xlsx", ".xls")
        ),
        shiny::uiOutput("inputComplete")
      ),
      # Main panel content:
      shiny::fluidRow(
        shiny::column(
          width = 11,
          offset = 1,
          shiny::htmlOutput("txt") |>
            shinycssloaders::withSpinner(
              type = 3,
              color = "#29C1A2",
              color.background = "white"
            )
        )
      )
    )
  ),

  # TAB 2 - DEMOGRAPHICS ----------------------------------------------------
  bslib::nav_panel(
    "Demographics",
    DT::DTOutput("demos")
  ),

  # TAB 3 - PSYCHOGRAPHICS --------------------------------------------------
  bslib::nav_panel(
    "Psychographics",
    bslib::layout_sidebar(
      sidebar = sidebar(
        id = "psychographics_sidebar",
        open = TRUE,
        width = "300px",

        shiny::uiOutput("step1"),
        shiny::uiOutput("agreement"),
        shiny::uiOutput("vertical"),
        shiny::uiOutput("index"),
        shiny::uiOutput("compare"),
        shiny::uiOutput("emptyRow"),
        shiny::uiOutput("step2"),
        shiny::uiOutput("run_filters"),
        shiny::uiOutput("step3"),
        shiny::br(),
        shiny::uiOutput("download")
      ),
      DT::DTOutput("psychographics")
    )
  ),

  # TAB 4 - SEGMENTS --------------------------------------------------
  bslib::nav_panel(
    "Segments",
    bslib::navset_tab(
      # Equivalent to tabPanel()
      bslib::nav_panel(
        "Segment Breakdown",
        DT::DTOutput("segments")
      ),
      bslib::nav_panel(
        "Segment Summary",
        DT::DTOutput("agree_matrix")
      )
    )
  ),

  # TAB 5 - ABOUT / FAQ --------------------------------------------------
  bslib::nav_panel(
    "FAQ",
    shiny::uiOutput("about")
  )
)

server <- function(input, output, session) {
  # read in data
  rv <- shiny::reactiveValues(data = data.frame(), name = "data")

  shiny::observeEvent(input$inFile, {
    rv$data <- read_simmons(input$inFile$datapath)
  })

  # SERVER TAB 1 - LOAD DATA ---------------------------------------------

  output$startHere <- shiny::renderUI({
    shiny::h5(
      tags$i(
        "STEP 1: Locate your Simmons Datahaul to read in",
        tags$br(),
        tags$br(),
        "This can take up to a minute to fully read in"
      ),
      style = "color:#045a8d; font-size:13px; font-weight:500;"
    )
  })

  output$inputComplete <- shiny::renderUI({
    shiny::req(input$inFile, rv$data)
    shiny::h5(
      tags$i(
        "Your data is ready and can be found in the tabs above",
      ),
      style = "color:#045a8d; font-size:13px; font-weight:400;"
    )
  })

  output$txt <- shiny::renderUI({
    shiny::req(input$inFile, rv$data)
    shiny::HTML(paste(
      shiny::h3("Data Check:"),
      rv$data$meta,
      glue::glue(
        "<br/><br/><b>There are {nrow(rv$data$grp_def)} audiences included in Datahaul:</b><br/>",
        trimws(paste(rv$data$grp_def$Definition, collapse = "<br/>"))
      )
    ))
  })

  # SERVER TAB 2 - DEMOGRAPHICS ---------------------------------------------

  # demo table
  output$demos <- DT::renderDT({
    shiny::req(input$inFile, rv$data)
    build_demo_tbl(rv$data)
  })

  # SERVER TAB 3 - PSYCHOGRAPHICS -------------------------------------------

  # sidebar information
  output$step1 <- shiny::renderUI({
    shiny::req(input$inFile, rv$data)
    shiny::h5(
      tags$i(
        "STEP 1: Select your criteria to filter the Simmons data",
      ),
      style = "color:#045a8d; font-size:16px; font-weight:400;"
    )
  })
  output$agreement <- shiny::renderUI({
    shiny::req(input$inFile, rv$data)
    shinyWidgets::pickerInput(
      "intensity",
      "Agreement intensity:",
      choices = c(
        "Any Agree",
        "Agree Completely",
        "Agree somewhat",
        "Disagree Somewhat",
        "Disagree Completely",
        "Any Disagree"
      ),
      selected = c("Any Agree"),
      multiple = FALSE
    )
  })
  output$vertical <- shiny::renderUI({
    shiny::req(input$inFile, rv$data)
    shiny::sliderInput(
      "vertical",
      "Vertical % floor:",
      min = 1,
      max = 100,
      value = 10,
      post = "%"
    )
  })
  output$index <- shiny::renderUI({
    shiny::req(input$inFile, rv$data)
    shiny::sliderInput(
      "index",
      "Index thresholds:",
      min = 60,
      max = 200,
      value = c(90, 110)
    )
  })
  output$compare <- shiny::renderUI({
    shiny::req(input$inFile, rv$data)
    shiny::div(
      style = "display: flex; align-items: flex-start; gap: 6px;",
      shiny::checkboxInput(
        inputId = "compare",
        label = "Comparison view",
        value = TRUE
      ),
      shiny::actionButton(
        inputId = "btn_tip",
        label = NULL,
        icon = shiny::icon("question-circle"),
        class = "btn-sm"
      ) |>
        bslib::tooltip(
          "If this is checked, rows with any significant index will retain all data for comparison."
        )
    )
  })

  output$emptyRow <- shiny::renderUI({
    shiny::req(input$inFile, rv$data)
    shiny::div(
      style = "display: flex; align-items: flex-start; gap: 6px;",
      shiny::checkboxInput(
        inputId = "emptyRow",
        label = "Drop empty rows",
        value = FALSE
      ),
      shiny::actionButton(
        inputId = "btn_tip",
        label = NULL,
        icon = shiny::icon("question-circle"),
        class = "btn-sm"
      ) |>
        bslib::tooltip(
          "When checked, any row without an index over/under threshold will be deleted. Too keep all psychographic statements, keep this unchecked."
        )
    )
  })
  output$step2 <- shiny::renderUI({
    shiny::req(input$inFile, rv$data)
    shiny::h5(
      tags$i(
        "STEP 2: Once you've selected all criteria, hit 'Run' below",
      ),
      style = "color:#045a8d; font-size:16px; font-weight:400;"
    )
  })
  output$run_filters <- shiny::renderUI({
    shiny::req(input$inFile, rv$data)
    shiny::actionButton(
      "run_filters",
      "Run",
      icon = shiny::icon("person-running"),
      width = "150px"
    )
  })
  output$step3 <- shiny::renderUI({
    req(input$inFile, rv$data)
    tags$h5(
      style = "color: #045a8d; font-size:16px; font-weight:400;",
      tags$i(
        tags$br(),
        "STEP 3: Change any selection and hit 'Run' again.",
        tags$br(),
        tags$br(),
        "If you need more room, press the ",
        tags$i(class = "fa-solid fa-angle-left"),
        " at the top of this sidebar to close it."
      )
    )
  })

  output$download <- shiny::renderUI({
    shiny::req(input$run_filters, segment_data())
    shiny::downloadButton("downloadData", "Download your data")
  })

  # Demographics table
  output$demographics <- DT::renderDT({
    DT::datatable(
      data.frame(
        Demographic = c("Male", "Female"),
        Count = c(500, 700),
        Percent = c("42%", "58%")
      ),
      options = list(scrollX = TRUE),
      rownames = FALSE
    )
  })

  # main panel psychographic data
  segment_data <- shiny::eventReactive(input$run_filters, {
    psychographic_segments(
      rv$data,
      intensity = input$intensity,
      vertical = input$vertical,
      index1 = input$index[1],
      index2 = input$index[2],
      compare = input$compare,
      empty = input$emptyRow
    )
  })

  # build out tables
  shiny::observeEvent(input$run_filters, {
    output$psychographics <- DT::renderDT(
      build_psychographic_tbl(segment_data())
    )
    output$agree_matrix <- DT::renderDT(
      build_agreement_tbl(segment_data())
    )
    output$segments <- DT::renderDT(
      build_segment_tbl(segment_data())
    )
    output$extra <- shiny::renderPrint(
      names(segment_data())
    )
  })

  # download handler
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      paste("MRI-Simmons_Psychographic_CutDown_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      xlsx_build(segment_data(), file)
    }
  )

  # SERVER TAB 5 - ABOUT / FAQ -------------------------------------------

  output$about <- shiny::renderUI({
    shiny::div(
      shiny::HTML(
        "<h5>What is this?</h5>This is an app that cuts down a Simmons DataHaul and provides you with
         the demographic profile and psychographics of your audiences.<br><h5>How does it work?</h5>
         Use the <span class='inline-code'>Browse</span> button to locate and load the Datahaul into memory. Depending on how
         many audiences are included in the Datahaul it can take up to a minute to fully load. Please
         be patient. When the data is loaded the Simmons metadata will display.<br><br>
         The Audience demographics will automatically populate in the <span class='inline-code'>Demographics</span> tab. 
         Move to the <span class='inline-code'>Psychographics</span> tab to set the Agreement intensity and your Index and 
         Vertical % thresholds.<br><br>Once the psychographic statements have been filtered, the 
         <span class='inline-code'>Download</span> button will appear. Clicking it will download an Excel file with 
         4 tabs - Demographics, Psychographics, Segmented Pscyhographics, and a Segment Summary.
         <h5>What are segmented psychographics?</h5>
         Psychographics are grouped very literally by Simmons, but a lot of pscyhographics carry 
         more nuanced meanings. For example, the statement \"People often copy what I do or wear\"
         is categorized as \"Your Values\" by Simmons. But what values? It can mean that a person 
         sees themselves as ambitious, or a fashionista, or as an influencer, or that he/she is 
         materialistic.<br><br>
         Almost 400 of the psychographic statements have been grouped into 34 different 
         segments. These segmentations are really just rough groupings, but should help you to more quickly break down 
         an audience's personality. The names of each segment are hopefully self-explanatory, but
         if you want to see all statements by segment, make sure the \"Drop empty rows\" box is 
         unchecked.
         <h5>What filters are available for Psychographics?</h5>
         <li>Agreement intensity: Level of agreement from Simmons</li>
         <li>Vertical % floor: Vertical % is greater than or equal to this.</li>
         <li>Index thresholds: The lower and upper index thresholds.</li>
         <li>Drop rows: Drop rows where no audience under- or over-indexes.</li>
         <li>Comparison view: Check to keep all data in the row if one audience index is significant.</li>
         <h5>What if I only want the over-indexing psychographics?</h5>
         Set the lower index threshold to 60. That should filter out the negative values.
         <h5>What if I want to change my thresholds?</h5>
         Simply select new filter levels and hit <span class='inline-code'>Run</span> again.
         <h5>The psychographics filter panel takes up to much space</h5>
         There is a <span class='inline-code'><</span> symbol at the top right of the 
         filter sidebar. Click it and the filter window will shrink. Push it again
         to bring it back.
         <h5>I can't open the excel file on my Mac/My Mac is asking to grant access</h5>
         This is likely an issue related to Excel for Mac. The easiest solution is to clear the 
         Excel cache and configuration file from the Library folder. To do this: 
         <ol><li>Completely quit all Office applications.</li>
         <li>In Finder, press Command +Shift + G and enter <b>~/Library</b> to 
         open the Library folder.</li>
         <li>In the Library folder, open the Containers folder and remove the 
         folder <b>\"Microsoft Excel\"</b>.</li>
         <li>Restart Mac and relaunch the application to see the results.</li><br><br><br>"
      )
    )
  })
}

shiny::shinyApp(ui, server)
