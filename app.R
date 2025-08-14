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

# to keep table headers sticky; DT fixedheader wouldn't hold when sidebar was retracted
JS <- "
$(document).ready(function() {
  var myInterval = setInterval(function() {
    // clear interval after the table's DOM is available
    if ($('thead').length) {
      clearInterval(myInterval);
    }
    // setting css
    $('thead tr th').css('position', 'sticky').css('background', '#2D3D4F').css('color', '#fff');
    var height = 0;
    for (var i = 0, length = $('thead tr').length; i < length; i++) {
      var header = $('thead tr:nth-child(' + i + ')');
      height += header.length ? header.height() : 0;
      $('thead tr:nth-child(' + (i + 1) + ') th').css('top', height);
    }
  }, 500);
});
"

### SHINY UI ###
ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  shiny::navbarPage(
    theme = bslib::bs_theme(bootswatch = "flatly", version = 5),
    collapsible = TRUE,
    title = tagList(
      shiny::actionLink("sidebar_button", "", icon = shiny::icon("bars")),
      "Simmons Segmentation"
    ),
    tags$head(
      # this changes the size of the popovers
      tags$style(".popover{font-size:14px;}")
    ),

    # TAB 1 - LOAD DATA -------------------------------------------------------
    shiny::tabPanel(
      "Start here",
      tags$head(tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      )),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::h5(
            tags$i(
              "STEP 1: Locate your Simmons Datahaul to read in",
              tags$br(),
              tags$br(),
              "This can take up to a minute to fully read in"
            ),
            style = "color:#045a8d; font-size:16px; font-weight:400;"
          ),
          shiny::fileInput(
            inputId = 'inFile',
            'Choose xlsx file',
            accept = c(".xlsx", ".xls")
          ),
          shiny::uiOutput("inputComplete")
        ),
        shiny::mainPanel(
          shiny::fluidRow(
            title = "Data check",
            shiny::column(
              width = 11,
              offset = 1,
              shiny::htmlOutput('txt') |>
                shinycssloaders::withSpinner(
                  type = 3,
                  color = "#29C1A2",
                  color.background = "white"
                )
            )
          )
        )
      )
    ),

    # TAB 2 - DEMOGRAPHICS ----------------------------------------------------
    shiny::tabPanel(
      "Demographics",
      shiny::mainPanel(
        tags$head(
          tags$script(HTML(JS)) # call the JS
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            DT::DTOutput("demos")
          )
        )
      )
    ),

    # TAB 3 - PSYCHOGRAPHICS --------------------------------------------------
    shiny::tabPanel(
      "Psychographics",
      tags$head(tags$script(HTML(JS))), # your sticky header JS

      shiny::sidebarLayout(
        position = "left", # default
        sidebarPanel = shiny::sidebarPanel(
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
          shiny::uiOutput("download"),
          width = 3 # or 2, depending on how tight you want it
        ),
        mainPanel = shiny::mainPanel(
          DT::DTOutput("psychographics"),
          width = 9 # fills the rest automatically
        )
      )
    ),

    # TAB 4 - SEGMENTS --------------------------------------------------
    shiny::tabPanel(
      "Segments",
      shiny::mainPanel(
        tags$head(
          tags$script(HTML(JS)) # call the JS
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::tabsetPanel(
              shiny::tabPanel("Segments", DT::DTOutput("segments")),
              shiny::tabPanel("Summary", DT::DTOutput("agree_matrix"))
            )
          )
        )
      )
    ),
    shiny::tabPanel(
      "FAQ",
      shiny::mainPanel(
        tags$head(
          tags$script(HTML(JS)) # call the JS
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::uiOutput("about")
          )
        )
      )
    )
  )
)

### SHINY SERVER ###
server <- function(input, output) {
  shiny::observeEvent(input$sidebar_button, {
    shinyjs::toggle(selector = ".sidebar")
  })

  # read in data
  rv <- shiny::reactiveValues(data = data.frame(), name = "data")

  shiny::observeEvent(input$inFile, {
    rv$data <- read_simmons(input$inFile$datapath)
  })

  # SERVER TAB 1 - LOAD DATA ---------------------------------------------

  # print metadata as check on uploaded audiences
  output$txt <- shiny::renderUI({
    shiny::req(input$inFile, rv$data)
    shiny::HTML(paste(
      h3("Data Check:"),
      rv$data$meta,
      glue::glue(
        "<br/><br/><b>There are {nrow(rv$data$grp_def)} audiences included in Datahaul:</b><br/>",
        trimws(paste(rv$data$grp_def$Definition, collapse = "<br/>"))
      )
    ))
  })

  # sidebar notes
  output$inputComplete <- shiny::renderUI({
    shiny::req(input$inFile, rv$data)
    shiny::h5(
      tags$i(
        "Your data is ready and can be found in the tabs above",
      ),
      style = "color:#045a8d; font-size:16px; font-weight:400;"
    )
  })

  # SERVER TAB 2 - DEMOGRAPHICS ---------------------------------------------

  # table from helpers
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
        "STEP 1: Select the criteria of interest for your Simmons pull",
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
  output$compare <- renderUI({
    req(input$inFile, rv$data)

    div(
      style = "display: flex; align-items: flex-start; gap: 6px;",

      checkboxInput(
        inputId = "compare",
        label = "Comparison view",
        value = TRUE
      ),
      actionButton(
        inputId = "btn_tip",
        label = NULL,
        icon = icon("question-circle"),
        class = "btn-sm"
      ) |>
        tooltip(
          "If this is checked, rows with any significant index will retain all data for comparison."
        )
    )
  })

  output$emptyRow <- renderUI({
    req(input$inFile, rv$data)

    div(
      style = "display: flex; align-items: flex-start; gap: 6px;",

      checkboxInput(
        inputId = "emptyRow",
        label = "Drop empty rows",
        value = FALSE
      ),
      actionButton(
        inputId = "btn_tip",
        label = NULL,
        icon = icon("question-circle"),
        class = "btn-sm"
      ) |>
        tooltip(
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
        tags$i(class = "fa-solid fa-bars"),
        " in the navbar to collapse this window."
      )
    )
  })

  output$download <- shiny::renderUI({
    shiny::req(input$run_filters, segment_data())
    shiny::downloadButton("downloadData", "Download your data")
  })

  # main panel table
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
    output$extra <- renderPrint(
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

  # SHINY SERVER - ABOUT -------------------------------------------------------

  output$about <- shiny::renderUI({
    shiny::HTML(
      "<h4>What is this?</h4>This is an app that cuts down a Simmons DataHaul and provides you with
         the demographic profile and psychographics of your audiences.<br><h4>How does it work?</h4>
         Use the <code>Browse</code> button to locate and load the Datahaul into memory. Depending on how
         many audiences are included in the Datahaul it can take up to a minute to fully load. Please
         be patient. When the data is loaded the Simmons metadata will display.<br><br>
         The Audience demographics will automatically populate in the <code>Demographics</code> tab. 
         Move to the <code>Psychographics</code> tab to set the Agreement intensity and your Index and 
         Vertical % thresholds.<br><br>Once the psychographic statements have been filtered, the 
         <code>Download</code> button will appear. Clicking it will download an Excel file with 
         4 tabs - Demographics, Psychographics, Segmented Pscyhographics, and a Segment Summary.
         <h4>What are segmented psychographics?</h4>
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
         <h4>What filters are available for Psychographics?</h4>
         <li>Agreement intensity: Level of agreement from Simmons</li>
         <li>Vertical % floor: Vertical % is greater than or equal to this.</li>
         <li>Index thresholds: The lower and upper index thresholds.</li>
         <li>Drop rows: Drop rows where no audience under- or over-indexes.</li>
         <li>Comparison view: Check to keep all data in the row if one audience index is significant.</li>
         <h4>What if I only want the over-indexing psychographics?</h4>
         Set the lower index threshold to 60. That should filter out the negative values.
         <h4>What if I want to change my thresholds?</h4>
         Simply select new filter levels and hit <code>Run</code> again.
         <h4>The psychographics filter panel takes up to much space</h4>
         There is a hamburger menu icon (<i class=\"fa-solid fa-bars\"></i>) on the 
         left side of the navbar. Click it and the filter window will shrink. Push it again
         to bring it back.
         <h4>I can't open the excel file on my Mac/My Mac is asking to grant access</h4>
         This is likely an issue related to Excel for Mac. The easiest solution is to clear the 
         Excel cache and configuration file from the Library folder. To do this: 
         <ol><li>Completely quit all Office applications.</li>
         <li>In Finder, press Command +Shift + G and enter <b>~/Library</b> to 
         open the Library folder.</li>
         <li>In the Library folder, open the Containers folder and remove the 
         folder <b>\"Microsoft Excel\"</b>.</li>
         <li>Restart Mac and relaunch the application to see the results.</li><br><br><br>"
    )
  })
}

# Complete app with UI and server components
shiny::shinyApp(ui, server)
