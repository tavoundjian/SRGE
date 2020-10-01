## This is a test comment
## This is another test

shinyUI(
  navbarPage("SRGE", id="nav",
    tabPanel("Data Load",
             pageWithSidebar(
               tags$head(tags$script('
                                          Shiny.addCustomMessageHandler("myCallbackHandler",
                                          function(typeMessage) {console.log(typeMessage)
                                          if(typeMessage == 1){
                                          console.log("got here");
                                          $("a:contains(Reactor Grid)").click();
                                          }
                                          if(typeMessage == 2){
                                          $("a:contains(Data Load)").click();
                                          }
                                          });
                                          ')),
               
               sidebarPanel(
                 tabsetPanel(
                   tabPanel(
                     title = "New Data",
                     #Selector for file upload
                     fileInput('datafile', 'Choose CSV file',
                               accept=c("txt/csv", "text/comma-separated-values,text/plain", ".csv")),
                     #These column selectors are dynamically created when the file is loaded
                     fluidRow(
                       uiOutput("req.text")
                     ),
                     fluidRow(
                       column(8,
                              uiOutput("case")
                       )
                     ),
                     fluidRow(
                       column(8,
                              uiOutput("agegrp")
                       )
                       # column(4,
                       #        uiOutput("age.display")
                       # ) 
                     ),
                     fluidRow(
                       column(8,
                              uiOutput("result")
                       )
                       # column(4,
                       #        uiOutput("result.display")
                       # ) 
                     ),
                     fluidRow(
                       uiOutput("opt.text")
                     ),
                     fluidRow(
                       column(8,
                              uiOutput("gender")
                       ),
                       column(4,
                              uiOutput("gender.display")
                       ) 
                     ),
                     fluidRow(
                       column(8,
                              uiOutput("type")
                       ),
                       column(4,
                              uiOutput("type.display")
                       ) 
                     ),
                     fluidRow(
                       column(8,
                              uiOutput("prev")
                       ),
                       column(4,
                              uiOutput("prev.display")
                       ) 
                     ),
                     fluidRow(
                       column(6,
                              checkboxInput("aggregate.chk","Aggregated Data?")
                       ),
                       column(6,
                              uiOutput("agg.var")
                       )
                     ),
                     fluidRow(
                       column(6,
                              downloadButton("SaveGrid","Save Data File")
                              ),
                       column(6,
                              actionButton("createGrid", "Create Grid")
                              )
                     )
                   ),
                   tabPanel(
                     title = "Load Data",
                     fileInput('loadfile', 'Choose CSV file',
                               accept=c("txt/csv", "text/comma-separated-values,text/plain", ".csv")),
                     fluidRow(
                       actionButton("LoadGrid", "Load Data File")
                     )
                   )
                 )
               ),
               mainPanel(
                 #uiOutput("tabs")  
                 
                 tabsetPanel(
                   id = "tabs",
                   tabPanel(
                     title = "Getting Started",
                     includeMarkdown("gettingstarted.Rmd")
                   ),
                   tabPanel(
                     title = "Data",
                     tableOutput("filetable")
                   ),
                   tabPanel(
                     title = "Required Fields",
                     HTML("<h2>Case Status</h2>"),
                     uiOutput("case.panel.title"),
                     uiOutput("case.panel"),
                     HTML("<h2>Age</h2>"),
                     uiOutput("age.factor.title"),
                     uiOutput("age.factor.inputs"),
                     uiOutput("age.panel.title"),
                     uiOutput("age.panel"),
                     HTML("<h2>Test Result</h2>"),
                     uiOutput("result.factor.title"),
                     uiOutput("result.factor.inputs"),
                     uiOutput("result.panel.title"),
                     uiOutput("result.panel")
                   ),
                   tabPanel(
                     title = "Optional Fields",
                     HTML("<h2>Gender</h2>"),
                     uiOutput("gender.factor.title"),
                     uiOutput("gender.factor.inputs"),
                     uiOutput("gender.panel.title"),
                     uiOutput("gender.panel"),
                     HTML("<h2>Test Type</h2>"),
                     uiOutput("type.factor.title"),
                     uiOutput("type.factor.inputs"),
                     uiOutput("type.panel.title"),
                     uiOutput("type.panel"),
                     HTML("<h2>Previous Result</h2>"),
                     uiOutput("prev.factor.title"),
                     uiOutput("prev.factor.inputs"),
                     uiOutput("prev.panel.title"),
                     uiOutput("prev.panel")
                   )
                   # tabPanel(
                   #   title = "Case",
                   #   uiOutput("case.panel.title"),
                   #   uiOutput("case.panel")
                   # ),
                   # tabPanel(
                   #   title = "Age",
                   #   uiOutput("age.factor.title"),
                   #   uiOutput("age.factor.inputs"),
                   #   uiOutput("age.panel.title"),
                   #   uiOutput("age.panel")
                   # ),
                   # tabPanel(
                   #   title = "Test Result",
                   #   uiOutput("result.factor.title"),
                   #   uiOutput("result.factor.inputs"),
                   #   uiOutput("result.panel.title"),
                   #   uiOutput("result.panel")
                   # ),
                   # tabPanel(
                   #   title = "Gender",
                   #   uiOutput("gender.factor.title"),
                   #   uiOutput("gender.factor.inputs"),
                   #   uiOutput("gender.panel.title"),
                   #   uiOutput("gender.panel")
                   # ),
                   # tabPanel(
                   #   title = "Test Type",
                   #   uiOutput("type.factor.title"),
                   #   uiOutput("type.factor.inputs"),
                   #   uiOutput("type.panel.title"),
                   #   uiOutput("type.panel")
                   # ),
                   # tabPanel(
                   #   title = "Prev. Result",
                   #   uiOutput("prev.factor.title"),
                   #   uiOutput("prev.factor.inputs"),
                   #   uiOutput("prev.panel.title"),
                   #   uiOutput("prev.panel")
                   # )
                 )
               )
             )
    ),
    tabPanel("Reactor Grid",
             tabsetPanel(
               
               tabPanel(
                 #### SECTION: Define ####
                 title = 'Define',
                 fluidRow(
                   #### Selectors ####
                   column(4,
                          uiOutput("gen.filt")
                   ),
                   column(4,
                          uiOutput("prev.filt")
                   ),
                   column(4,
                          uiOutput("test.filt")
                   )
                 ),
                 
                 #### Grid ####
                 
                 fluidRow(
                   HTML('<h4>Reactor Grid</h4><br>Number of Verified Cases/Number of Lab Reports')
                 ),
                 fluidRow(
                   column(9,
                          DT::dataTableOutput("table", height=13)
                   ),
                   
                   #### Dynamic Calculations ####  
                   
                   column(3,
                          fluidRow(
                            htmlOutput('totals')
                          ),
                          fluidRow(
                            htmlOutput('cumulative')
                          ),
                          fluidRow(
                            htmlOutput('last_selected')
                          )
                   )
                 ),
                 
                 ##### Action Buttons ####
                 
                 fluidRow(
                   column(2,
                          actionButton('clear', 'Clear Selection')
                   ),
                   column(2,
                          actionButton('save.btn', 'Save Algorithm')
                   )
                 ),
                 
                 #### Legend ####
                 
                 fluidRow(
                   plotOutput('legend',width='75%',height='190px')
                 )
             ),
             tabPanel(
               #### SECTION: Evaluate ####
               title = 'Evaluate',
               fluidRow(
                 column(10,
                        #### Table/Graph Frame ####
                        tabsetPanel(
                          #### TAB: Table ####
                          tabPanel(
                            title = "Table",
                            #### Saved Algorithm Table ####
                            fluidRow(
                              DT::dataTableOutput('save')
                            ),
                            #### Table buttons #####
                            fluidRow(
                              column(4,
                                     actionButton('delete.btn', 'Delete Selected')
                                     )
                            )
                          ),
                          #### TAB: Graph ####
                          tabPanel(
                            title = "Graph",
                            #### Graph Window ####
                            fluidRow(
                              plotOutput('plot')
                            )
                          )
                        )
                 ),
                 column(2,
                        fluidRow(
                          h4('Final Algorithm')
                        ),
                        fluidRow(
                          #### Final Algorithm Window ####
                          htmlOutput('final.alg')
                        ),
                        fluidRow(
                          #### Export Button ####
                          downloadButton('export.alg','Export Algorithm')
                        )
                 )
               )
             )
            )
    )
  )
) 