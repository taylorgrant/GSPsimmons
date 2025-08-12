pacman::p_load(shiny, shinythemes, shinyWidgets, shinyBS, shinyjs, DT, openxlsx)

# load helper functions
source(here::here("R", "helpers.R"))
source(here::here("R", "xlsx_build.R"))

# increase upload size 
options(shiny.maxRequestSize = 100*1024^2)

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
ui <- fluidPage(
  useShinyjs(),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             # HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Simmons Segmentation</a>'), id="nav",
             title = tagList(actionLink("sidebar_button","",icon = icon("bars")), "Simmons Segmentation"),
             tags$head(
               # this changes the size of the popovers
               tags$style(".popover{font-size:14px;}")
             ),
             
             # TAB 1 - LOAD DATA -------------------------------------------------------
             tabPanel("Start here",
                      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
                      shiny::sidebarLayout(
                        
                        shiny::sidebarPanel(
                          
                          span(tags$i(h5("STEP 1: Locate your Simmons Datahaul to read in",
                                         tags$br(),
                                         tags$br(),
                                         "This can take up to a minute fully read in")), style="color:#045a8d"),             
                          shiny::fileInput(inputId = 'inFile', 'Choose xlsx file',
                                           accept = c(".xlsx", ".xls")),
                          uiOutput("inputComplete")
                        ),
                        shiny::mainPanel(
                          shiny::fluidRow( title = "Data check",
                                           shiny::column(
                                             width = 11, 
                                             offset = 1,
                                             htmlOutput('txt') |> 
                                               shinycssloaders::withSpinner(type = 3, 
                                                                            color="#29C1A2",
                                                                            color.background = "white")
                                           ))
                        )
                      )
             ),
             
             # TAB 2 - DEMOGRAPHICS ----------------------------------------------------
             tabPanel("Demographics",
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
             tabPanel("Psychographics",
                      div(
                        class = "sidebar",
                        shiny::sidebarPanel(
                          uiOutput("step1"),
                          uiOutput('agreement'),
                          uiOutput('vertical'),
                          uiOutput('index'),
                          uiOutput('compare'),
                          uiOutput('emptyRow'),
                          uiOutput("step2"),
                          uiOutput("run_filters"),
                          uiOutput('step3'),
                          br(),
                          uiOutput('download')
                        )
                      ), 
                      shiny::mainPanel(
                        tags$head(
                          tags$script(HTML(JS)) # call the JS
                        ),
                        shiny::fluidRow(
                          shiny::column(width = 12,
                                        DT::DTOutput('psychographics')
                          )
                        )
                      )),
             
             # TAB 4 - SEGMENTS --------------------------------------------------
             tabPanel("Segments",
                      shiny::mainPanel(
                        tags$head(
                          tags$script(HTML(JS)) # call the JS
                        ),
                        shiny::fluidRow(
                          shiny::column(width = 12,
                                        shiny::tabsetPanel(
                                          shiny::tabPanel("Segments",
                                                          DT::DTOutput("segments")
                                          ),
                                          shiny::tabPanel("Summary",
                                                          DT::DTOutput("agree_matrix")
                                          )
                                          
                                        )
                                        
                          )
                        )
                      )
             ),
             tabPanel("FAQ",
                      shiny::mainPanel(
                        tags$head(
                          tags$script(HTML(JS)) # call the JS
                        ),
                        shiny::fluidRow(
                          shiny::column(
                            width = 12,
                            uiOutput("about") 
                          )
                        )
                      ))
  ))

### SHINY SERVER ###
server <- function(input, output) {
  
  # upping data limit on uploads
  options(shiny.maxRequestSize=30*1024^2) 
  
  observeEvent(input$sidebar_button,{
    shinyjs::toggle(selector = ".sidebar")
  })
  
  # read in data
  rv <- shiny::reactiveValues(data = data.frame(), name = "data")
  
  shiny::observeEvent(input$inFile, {
    rv$data <- read_simmons(input$inFile$datapath)
  })
  
  # SERVER TAB 1 - LOAD DATA ---------------------------------------------
  
  # print metadata as check on uploaded audiences
  output$txt <- renderUI({
    req(input$inFile, rv$data)
    HTML(paste(h3("Data Check:"), rv$data$meta,
               glue::glue("<br/><br/><b>There are {nrow(rv$data$grp_def)} audiences included in Datahaul:</b><br/>",
                          trimws(paste(rv$data$grp_def$Definition, collapse = "<br/>")))))
  })
  
  # sidebar notes
  output$inputComplete <- renderUI({
    req(input$inFile, rv$data)
    span(tags$i(h5("Your data is ready and can be found in the tabs above")), style="color:#045a8d")
  })
  
  # SERVER TAB 2 - DEMOGRAPHICS ---------------------------------------------
  
  # table from helpers 
  output$demos <- DT::renderDT({
    req(input$inFile, rv$data)
    build_demo_tbl(rv$data)
  })
  
  # SERVER TAB 3 - PSYCHOGRAPHICS -------------------------------------------
  
  # sidebar information
  output$step1 <- renderUI({
    req(input$inFile, rv$data)
    span(tags$i(h5("STEP 1: Select the criteria of interest for your Simmons pull")), style="color:#045a8d")
  })
  output$agreement <- renderUI({
    req(input$inFile, rv$data)
    pickerInput("intensity", "Agreement intensity:",
                choices = c("Any Agree", "Agree Completely", "Agree somewhat", "Disagree Somewhat",
                            "Disagree Completely", "Any Disagree"),
                selected = c("Any Agree"),
                multiple = FALSE)
  })
  output$vertical <- renderUI({
    req(input$inFile, rv$data)
    sliderInput("vertical",
                "Vertical % floor:",
                min = 1,
                max = 100,
                value = 10,
                post = "%")
  })
  output$index <- renderUI({
    req(input$inFile, rv$data)
    sliderInput("index",
                "Index thresholds:",
                min = 60,
                max = 200,
                value = c(90, 110))
  })
  output$compare <- renderUI({
    req(input$inFile, rv$data)
    shiny::div(
      shiny::div(style="display: inline-block; ;", 
                 checkboxInput(inputId = "compare", label = "Comparison view", value = TRUE)),
      shiny::div(style="display: inline-block;", 
                 bsButton("q1", label = "", icon = icon("question"),
                          style = "default", size = "extra-small"),
                 bsPopover(id = "q1", title = "",
                           content = "If this is checked, any row with at least one index under/over the index threshold will retain all data for easy comparison across audiences.",
                           placement = "right", 
                           trigger = "hover", 
                           options = list(container = "body")
                 )
      ))
  })
  output$emptyRow <- renderUI({
    req(input$inFile, rv$data)
    shiny::div(
      shiny::div(style="display: inline-block; ;", 
                 checkboxInput(inputId = "emptyRow", label = "Drop empty rows",
                               value = FALSE)),
      shiny::div(style="display: inline-block;", 
                 bsButton("q2", label = "", icon = icon("question"),
                          style = "default", size = "extra-small"),
                 bsPopover(id = "q2", title = "",
                           content = "When checked, any row without an index over/under threshold will be deleted. Too keep all psychographic statements, keep this unchecked.",
                           placement = "right", 
                           trigger = "hover", 
                           options = list(container = "body")
                 )
      ))
    
  })
  output$step2 <- renderUI({
    req(input$inFile, rv$data)
    span(tags$i(h5("STEP 2: Once you've selected all criteria, hit 'Run' below")), style="color:#045a8d")
  })
  output$run_filters <- renderUI({
    req(input$inFile, rv$data)
    actionButton("run_filters", "Run",
                 icon = icon("person-running"),
                 width = "150px")
  })
  output$step3 <- renderUI({
    req(input$inFile, rv$data)
    HTML("<H5 style=\"color: #045a8d\"><I>STEP 3: Change any selection and hit 'Run' again.<br><br>If you need more room, press the <i class=\"fa-solid fa-bars\"></i> in the navbar to collapse this window.</I></H5")
  })
  output$download <- renderUI({
    req(input$run_filters, segment_data())
    shiny::downloadButton("downloadData", "Download your data")
  })
  
  # main panel table 
  segment_data <- eventReactive(input$run_filters, {
    
    psychographic_segments(rv$data, 
                           intensity = input$intensity, 
                           vertical = input$vertical, 
                           index1 = input$index[1],
                           index2 = input$index[2],
                           compare = input$compare,
                           empty = input$emptyRow)
  })
  
  # build out tables 
  observeEvent(input$run_filters, {
    output$psychographics <- DT::renderDT(
      build_psychographic_tbl(segment_data()))
    
    output$agree_matrix <- DT::renderDT(
      build_agreement_tbl(segment_data()))
    
    output$segments <- DT::renderDT(
      build_segment_tbl(segment_data()))
    
    output$extra <- renderPrint(
      names(segment_data())
    )
  })
  
  # download handler 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("MRI-Simmons_Psychographic_CutDown_", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      xlsx_build(segment_data(), file)
    }
  )
  
  # SHINY SERVER - ABOUT -------------------------------------------------------
  
  output$about <- renderUI({
    HTML("<h4>What is this?</h4>This is an app that cuts down a Simmons DataHaul and provides you with
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
         <li>Restart Mac and relaunch the application to see the results.</li><br><br><br>")
  })
  
  
  
}

# Complete app with UI and server components
shinyApp(ui, server)