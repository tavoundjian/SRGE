shinyUI(
  fluidPage(
    titlePanel("Syphilis Reactor Grid Evaluator", windowTitle = 'Syphilis RG'),
    tabsetPanel(
      tabPanel(
        #### SECTION: Define ####
        title = 'Define',
        fluidRow(
          #### Selectors ####
          column(4,
                 selectInput("gender",
                             "Gender:",
                             c("All",
                               unique(as.character(df$gen_comb))))
          ),
          column(4,
                 selectInput("prev",
                             "Change since Prveious Result:",
                             c("All",
                               unique(as.character(df$prev))))
          ),
          column(4,
                 selectInput("test",
                             "Test Type:",
                             c("All",
                               unique(as.character(df$test))))
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
          column(8,
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
                              actionButton('combine.btn', 'Combine Selected')
                              ),
                       column(4,
                              actionButton('delete.btn', 'Delete Selected'))
                     )
                   ),
                   tabPanel(
                     title = "Additional Metrics",
                     fluidRow(
                       DT::dataTableOutput('addl')
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
          column(4,
                 fluidRow(
                   h4('Final Algorithm')
                 ),
                 fluidRow(
                   #### Final Algorithm Window ####
                   htmlOutput('final.alg')
                   ),
                 fluidRow(
                   #### Export Button ####
                   actionButton('export.alg','Export Algorithm')
                   )
                 )
          )
      )
    )
  )
)