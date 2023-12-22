
# Check if the git connection works
## First specify the packages of interest
# packages = c("tidyverse", "tidymodels",
#              "lubridate", "DBI","dbplyr",
#              "rmarkdown", "knitr", "plotly",
#              "kableExtra", "DT", "tidyr",
#              "shiny", "rlang", "esquisse",
#              "tidytext", "viridis", "hrbrthemes", "shinythemes",
#              # also customise your plots to match theme
#              "thematic",
#              # to add point inside the violin not outside 
#              "ggforce",
#              #"GGally"
#              "ggcorrplot",
#              # To add tiles
#              "shinydashboard",
#              # To add the matrixplot 
#              "GGally",
#              "ggside",
#              "tidyquant",
#              # to calculate gini index
#              "DescTools",
#              "kableExtra",
#              # to make the summary table colorful
#              "reactable", "RColorBrewer",
#              #for pca analysis
#              "FactoMineR", "factoextra", "corrplot","tibble",
#              # for creating gt tables
#              "gt",
#              "waiter"
# )

library("tidyverse")
library("tidymodels")
library("lubridate")
library("DBI")
library("dbplyr")
library("rmarkdown")
library("knitr")
library("plotly")
library("kableExtra")
library("DT")
library("tidyr")
library("shiny")
library("rlang")
library("esquisse")
library("tidytext")
library("viridis")
library("hrbrthemes")
library("shinythemes")
library("thematic")
library("ggforce")
library("ggcorrplot")
library("shinydashboard")
library("GGally")
library("ggside")
library("tidyquant")
library("DescTools")
library("kableExtra")
library("reactable")
library("RColorBrewer")
library("FactoMineR")
library("factoextra")
library("corrplot")
library("tibble")
library("gt")
library("waiter")
library("rsconnect")
library("shinyBS")
library("shinyFeedback")
library("bslib")
library("shinycssloaders")
library("reactlog")
library("profvis")





# Increase the size of uploaded file

options(shiny.maxRequestSize=30000*1024^2) 

rsconnect::setAccountInfo(name='xxx', token='xxxx', secret='xxxxx')

#rsconnect::configureApp("Testsegment", size="large")
# Connect to database and pull the data

con <- DBI::dbConnect(odbc::odbc(),
                      #Snowflake
                      #SnowflakeDSIIDriver
                      Driver       = "SnowflakeDSIIDriver",
                      Server       = "ed87949.us-east-1.snowflakecomputing.com",

                      UID          = rstudioapi::askForPassword("Database user"),
                      PWD          = rstudioapi::askForPassword("Database password"),
                      Database     = "EDW",
                      Warehouse    = "shiny_app",
                      Schema       = "dim"
                      #,
                      #authenticator = "externalbrowser"
                      )
mywh <- DBI::dbSendQuery(con, 'use role shiny_app_role')
#mywh <- DBI::dbSendQuery(con, 'use role developer_role')
mywh <- DBI::dbSendQuery(con, 'use warehouse shiny_app')

waiting_screen <- tagList(
  spin_flower(),
  h4("Cool stuff loading...")
) 

# Random seed
set.seed(27)

# Define UI for the application

## adding the first navigation bar

# tabPanel("1. Data Import and preparation",
#          
#          dashboardPage(
#            dashboardHeader(title = "Basic dashboard"),
#            dashboardSidebar(disable = TRUE),
#            dashboardBody(




ui <-  
  
  
  navbarPage(title=span(img(src="newsela-logo.png",
                                 width = 60,
                                 height = 60,
                            style = "margin:-30px 10px"),
                             "NEWSELA ANALYSIS AND CLUSTERING TOOL"),
             id = "navid",
             #theme = bs_theme(bootswatch = "slate"),
             theme = shinythemes::shinytheme("sandstone"),
             #position = "fixed-top",
             
            
             

            
                
                  tabPanel("Home", 
                           icon = icon("home"), 
                           
                           
                           # change navbar css I put here because if I put in introduction in create ghost tabs
                           tags$head(
                             tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:30px !important; 
                            padding-bottom:0 !important;
                            
                            height: 80px;
                            font-size: 25px;
                            font-size: 15px !important;}
                            
                           
                           
                           .navbar {min-height:25px !important;}'
                             ))),
                           #.navbar .navbar-header {float: right}
                           #.navbar .navbar-nav {float: right}
                           
                           # To change EDA page tabs font size
                           tags$head(
                             tags$style(type='text/css',
                                        ".nav-tabs {font-size: 15px} ")),
                           

                           # TO add the scroll on top of tables
                           
    
                           br(),
                           # Summary
                           fluidRow(
                           column(1),
                           column(9,
                           p(style="text-align:center;;  font-size:240%", "Introduction")
                           ),
                           column(2),
                             ),
                           fluidRow(
                             column(1),
                             column(9,
                                    shiny::HTML("<h5>This App helps you to Import, Explore, and Model(Kmeans Clustering and PCA) any data with n rows and m columns. </h5>"),
                                    shiny::HTML("<h5>The Goal is to be able to discover patterns and segments in any data that has at least one numerical column, based on any combination of variables in a simple manner only by selecting the data and clicking a few buttons while keeping your autonomy to change any piece of the process.  The data can be imported using a CSV file or a Snowflake query</h5>"),
                                    ),
                             column(2)
                           ),
                           
                           # fluidRow(
                           #   
                           #   style = "height:5px;"),
                           
                           # PAGE BREAK
                           #tags$hr(),
                           br(),
                           br(),
                           br(),
                           fluidRow(
                             column(1),
                             
                             column(3,
                                    div(class="panel panel-default", 
                                        div(class="panel-body",  width = "600px",
                                            align = "center",
                                            div(
                                              tags$img(src = "1.png", 
                                                       width = "50px", height = "50px")
                                            ),
                                            div(h4("Data Import and Preparation")),
                                            div(
                                              h5(
                                                "Import/Transform your data for further analysis. Make sure you have at least one numerical column(Recommended to have more than 2) and one column with unique values."
                                              )
                                            ),
                                            fluidRow(
                                              #text-align:center;
                                              column(4),
                                              tags$div(align = "center",
                                                       tags$div(align = "center",
                                                                fluidRow( 
                                                                  column(4,   actionButton("gotoimport", "Start HERE", icon("play"), 
                                                                                                    style="color: #0A6EFA; text-align:center; background-color: #E7F1FF; border-color: #2e6da4;  font-size:110%"))
                                                                                         ),
                                                                ))
                                              #,
                                             # column(2)
                                            )
                                        )
                                    )
                             ),
                             column(3,
                                    div(class="panel panel-default",
                                        div(class="panel-body",  width = "600px", 
                                            align = "center",
                                            div(
                                              tags$img(src = "2.png", 
                                                       width = "50px", height = "50px")
                                            ),
                                            div(h4("EDA(Explore And Visualize data)")),
                                            div(
                                              h5(
                                                "Then focus on visualizing your data. There are multiple ways to visualize your data such as boxplot, bar chart, table, correlation matrix and regression analysis. You can also exclude some columns from further analysis."
                                              ),
                                              br(), br()
                                            )
                                        )
                                    )
                             ),
                             column(3,
                                    div(class="panel panel-default",
                                        div(class="panel-body",  width = "600px", 
                                            align = "center",
                                            div(
                                              tags$img(src = "3.png", 
                                                       width = "50px", height = "50px")),
                                            div(h4("Build Segments")),
                                            div(
                                              h5(
                                                "Finally segment your data using some of the columns in your data. PCA is used to help you find significant variables. By Kmeans you discover the groups and finally, you can create a report from all the columns"
                                              ),
                                              br(), br()
                                              
                                            )
                                        )
                                    )
                             ),
                             column(1)
                             
                           ),
                           
                           fluidRow(style = "height:30px;"
                           ),
                           br(),
                           
                           ),
                  tabPanel(value="importtransid",
                           "1. Data Import and preparation",
                           icon = icon("table"), 
                           br(),
                           # fluidPage(
                           #   tags$head(
                           #     tags$link(rel= "stylesheet", type="text/css", href= "bootstrap.min.css")
                           #   )
                           #   #theme = "bootstrap.css"
                           # ),
                           p(strong("On this page, you can perform these initial steps:"),br(),
                             "* Import data using a Snowflake connection or a CSV file",br(),
                             "* Play around with your data to get a better sense",br(),
                             "* Transform the data types of the columns",br(),
                             style="text-align:justify;color:black;background-color:powderblue;padding:15px;border-radius:10px"),
                           
                           br(),
                           
                           
                           
                             
                             #theme = shinytheme("sandstone"),
                             #theme = bslib::bs_theme(bootswatch = "darkly"),
                             
                             # two methods of importing data
                             waiter::use_waiter(),
                             tags$h3("A. First lets import the data:"),
                             wellPanel( 
                             column(9,
                                    offset=3,
                                    radioButtons("importmethod",
                                                 NULL,
                                                 c( "I want to write a query"="querytext" ,
                                                    "I have a CSV file"="csvfile"  ),
                                                 inline = TRUE)
                             ),
                             
                               helpText("Your data should have a unique column for that you want to do the cluster.\n
                                        For example if you want to cluster teachers a user_id column without duplicate is necessary "),
                               # creating UI two ways of importing data
                               shinyFeedback::useShinyFeedback(),
                             withSpinner(htmlOutput("chosenimport")),
                             fluidRow(
                               column (3, withSpinner( htmlOutput("importdetails"))),
                               
                               span(htmlOutput("queryerror"), style="color:red"),
                               span(htmlOutput("querysucess"), style="color:#50D050; font-size: 20px"),
                               span(htmlOutput("numericwarning"), style="color:red; font-size: 20px")
                               #helpText("Remember one of the columns should be unique"),
                               
                             ), 
                               verbatimTextOutput("query")
                             ),
                             
                             hr(),
                             tags$h3("B. Take a look at your data:"),
                             fluidRow(
                               column(4, withSpinner(shinydashboard::valueBoxOutput("tablerow"))),
                               column(2, withSpinner(shinydashboard::valueBoxOutput("tablecol"))),
                               column(2, withSpinner(shinydashboard::valueBoxOutput("numbcolnum"))),
                               column(2, withSpinner(shinydashboard::valueBoxOutput("factcolnum"))),
                               column(2, withSpinner(shinydashboard::valueBoxOutput("datecolnum"))),
                               
                             ),
                             tags$h4("Your current data(Sample of 20,000 rows if the number of rows is more than 20,000):"),
                             dataTableOutput("testo"),
                             #verbatimTextOutput("testo"),
                             waiter::use_waitress(),
                           
                           # these 3 tags comes from here https://github.com/rstudio/DT/issues/554
                           # without these three when we want to filter, scroll resets
                           tags$head(tags$style(HTML( ".dataTables_scroll      {overflow-x:scroll;}"))),
                           tags$head(tags$style(HTML( ".dataTables_scrollBody  {overflow: unset !important;}"))),
                           tags$head(tags$style(HTML( ".dataTables_scrollHead  {
                                       overflow: unset !important;
                                       z-index: 10;
                             }"))),
                           
                           
                             column(12,
                                    withSpinner(dataTableOutput("querydata", height = "400px"))
                             ),
                             br(),
                             hr(),
                             tags$h3("C. Lets transform some columns if necessary:"),
                             tableOutput("file"),
                             wellPanel( 
                               fluidRow(
                                 column(6,
                                        htmlOutput("tofactorinput")),
                                 column(6,
                                        htmlOutput("tostringinput"))
                               ),
                               fluidRow(
                                 column(6,
                                        htmlOutput("tonumericinput")),
                                 column(6,
                                        htmlOutput("todateinput"))
                               ),
                               fluidRow(
                                 column(6, actionButton("convertbutton", "Apply the conversion", class = "btn-warning"))
                               )
                             ),
                             dataTableOutput("typeofcolumns"),
                             br(),
                             fluidRow(
                               column(2, offset = 9, actionButton("gotoeda", "The data looks good Go to EDA Page"))
                             ),
                             br()
                           
                  ),
                  
                  tabPanel(value="edaid",
                           icon = icon("chart-bar"),
                           "2. EDA(Explore And Visualize data)",
                           br(),
                           p(strong("On this page, you can perform these Exploratory Data analyses: :"),br(),
                             "* Visualize Categorical, Date, and Numerical columns in your data",br(),
                             "* Create Correlation Matrix and Correlation Matrix Plot",br(),
                             "* Exclude variables that are not necessary anymore in the modeling page",br(),
                             style="text-align:justify;color:black;background-color:powderblue;padding:15px;border-radius:10px"),
                           br(),
                           p("All the rows of your data will be used to build all the plots and tables on this page. Only if the data has less than 20000 rows a", 
                             strong("SAMPLE TABLE"), "with 20,000 rows will be randomly selected from your data to plot the graphs on this page.",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                          
                           br(),
                           tabsetPanel( 
                             tabPanel("Categorical Explore", 
                                      fluidRow(
                                      column(width=4, htmlOutput("factorexclude")),
                                      column(width=3,
                                             fluidRow(
                                               tags$h4("Status")),
                                             fluidRow(
                                               verbatimTextOutput("nofactorshow")
                                             )
                                      )
                                      ),
                                      tabsetPanel(
                                        
                                        
                                        tabPanel( "Table",
                                                 br(),
                                                 fluidRow(
                                                   column(width=2,   sliderInput("factobs", "Top n values:",
                                                                                            min = 0, max = 100, value = 12
                                                                                      )),
                                                   column(width=2, offset = 1,
                                                          fluidRow("Using n:"),
                                                          fluidRow(" "),
                                                          fluidRow(         
                                                          actionButton("showtablefact", "Show summary table of the categorical variables", class = "btn-warning"))
                                                   )
                                                   ),
                                                br(),
                                                 waiter::use_waiter(),
                                                 fluidRow(
                                                   column(12, withSpinner(dataTableOutput("factorsummary")))
                                                 ),
                                                br()),
                                        tabPanel("Bar",
                                                 br(),
                                                 fluidRow(
                                                   column(width=2,  sliderInput("factobsbar", "Top n values:(Max=100)",
                                                                                     min = 0, max = 100, value = 5
                                                                                      )),
                                                   column(width=2, offset = 1,
                                                          fluidRow("Using n:"),
                                                          fluidRow(  
                                                          actionButton("showbarfact", "Show summary bar chart of the categorical variables", class = "btn-warning"))
                                                   )
                                                   ),
                                                 br(),
                                                 fluidRow(
                                                   waiter::use_waiter(),
                                                   column(12, withSpinner(plotOutput("factorbar", height = "100%")))
                                                 ),
                                                 br())
                                      )),
                             tabPanel("Date Explore",
                                      fluidRow(
                                        column(width=4, htmlOutput("dateexclude")),
                                        column(width=3,
                                               fluidRow(
                                                 tags$h4("Status")),
                                               fluidRow(
                                                 verbatimTextOutput("nodateshow")
                                               )
                                        )
                                      ),
                                      
                                      
                                      tabsetPanel(
                                        tabPanel("Table",
                                                 br(),
                                                 fluidRow(
                                                   column(width=4, actionButton("showdatesummary", "Show summary table of the date columns", class = "btn-warning"))
                                                 ),
                                                 br(),
                                                 fluidRow(
                                                   column(12, withSpinner(dataTableOutput("datesummary")))
                                                 ),
                                                 br()),
                                        tabPanel("Boxplot",
                                                 br(),
                                                 fluidRow(
                                                   column(width=4, actionButton("showdateboxplot", "Show box table of the date columns", class = "btn-warning"))
                                                 ),
                                                 br(),
                                                 fluidRow(
                                                   column(12, withSpinner(plotOutput("dateboxplot", height = "100%")))
                                                 ),
                                                 br()),
                                        tabPanel("Histogram",
                                                 br(),
                                                 fluidRow(
                                                   column(width=2,  sliderInput("BinsHistDate",
                                                                           "Bins, Change aggregation level",
                                                                           min = 0, max = 100, value = 5
                                                                           )),
                                                   column(width=4,
                                                          fluidRow("Using n:"),
                                                          fluidRow(
                                                          actionButton("showdatehistogram", "Show histogram of date columns", class = "btn-warning"))
                                                   ) 
                                                 ),
                                                 br(),
                                                   
                                                 fluidRow(
                                                   column(12, withSpinner(plotOutput("datehistogram", height = "100%")))
                                                 ),br()),
                                        tabPanel("Area",
                                                 fluidRow(
                                                   column(12,  plotOutput("datearea", height = "100%")),
                                                 ),
                                                 br())
                                      )),
                             tabPanel("Numeric Explore",
                                      fluidRow(
                                        column(width=4, htmlOutput("numbexclude")),
                                        column(width=3,
                                               fluidRow(
                                                 tags$h4("Status")),
                                               fluidRow(
                                                 verbatimTextOutput("nonumbershow")
                                               )
                                        )
                                      ),
                                      tabsetPanel(
                                        tabPanel("Table",
                                                 br(),
                                                 fluidRow(
                                                 column(width=2, actionButton("showtablenumb", "Show summary table of the numerical variables", class = "btn-warning"))
                                                 ),
                                                 br(),
                                                 fluidRow(
                                                 column(12, withSpinner(dataTableOutput("numbsummary")))
                                                 )
                                                 
                                                 ,
                                                 fluidRow(
                                                   column(12, tableOutput("giniindex"))
                                                 )
                                                 
                                        ),
                                        tabPanel("Boxplot",
                                                 br(),
                                                 fluidRow(
                                                   column(width=4, actionButton("shownumbboxplot", "Show box table of the Numerical columns", class = "btn-warning"))
                                                 ),
                                                 br(),
                                                 fluidRow(
                                                   column(12, withSpinner(plotOutput("numbboxplot", height = "100%")))),
                                                 br()
                                        ),
                                        tabPanel("Histogram",
                                                 br(),
                                                 fluidRow(
                                                   column(width=2,   sliderInput("BinsHistNumb",
                                                                           "Bins, Change aggregation level",
                                                                           min = 0, max = 100, value = 5
                                                                           )),
                                                   column(width=4, 
                                                          fluidRow("Using n:"),
                                                          fluidRow(
                                                            actionButton("shownumbhistogram", "Show histogram of Numerical columns", class = "btn-warning"))
                                                   )
                                                   
                                                 ),
                                                 br(),
                                                 fluidRow(
                                                   column(12, withSpinner(plotOutput("Numbhistogram", height = "100%")))
                                                 ),
                                                 br()
                                        ),
                                        tabPanel("Violin",
                                                 br(),
                                                 fluidRow(
                                                   column(4,
                                                 actionButton("shownumbviolin", "Show Violin chart of Numerical columns", class = "btn-warning")
                                                 )),
                                                 br(),
                                                 fluidRow(
                                                   column(12,  withSpinner(plotOutput("numbviolin", height = "100%"))),
                                                 ),
                                                 br(),
                                        ),
                                        
                                        # to repeat a graph put outside the tabpanles inside a tabpaneles
                                      )
                             ),
                             
                             tabPanel("Correlation Analysis",
                                      wellPanel( 
                                        fluidRow(
                                          
                                          column(12,
                                                 htmlOutput("cormatrixvars")),
                                          
                                          column(12,
                                                 htmlOutput("correlationfactor"))
                                          
                                        ),
                                        
                                        
                                      ),
                                      hr(),
                                      
                                      tabsetPanel(
                                        tabPanel("Matrix",
                                                 fluidRow(
                                                   br(),
                                                   column(12,
                                                          actionButton("corrun", "Show Correlation Matrix", class = "btn-warning")),
                                                   br(),
                                                   column(12, withSpinner(plotOutput("numbmatrix", height = "100%")))
                                                 )
                                        ),
                                        tabPanel("Matrix.Plot",
                                                 
                                                 fluidRow(
                                                   br(),
                                                   column(12,
                                                          actionButton("matrixplotrun", "Show Correlation Matrix plot", class = "btn-warning")),
                                                   br(),
                                                   column(12,  withSpinner(plotOutput("CorrelationMatrixPlot", height = "100%"))),
                                                 )
                                        )
                                        
                                      )
                             ),
                             tabPanel("Exclude Nominated/Unnecessary variables",
                                      br(),
                                      column(12,
                                      fluidRow(
                                               htmlOutput("excludingvariables"))
                                        ),
                                      br(),
                                      fluidRow(
                                        actionButton("excludevars", "Exclude these variables from the rest of the process", class = "btn-warning")
                                               ),
                                      br(),
                                      fluidRow(
                                        column(12,
                                        verbatimTextOutput("excludingresult"),
                                        tags$head(tags$style("#excludingresult{color:black; font-size:12px;   
overflow-y:scroll;  max-height: 5000px; background: ghostwhite;}"))
                                        )
                                      )
                                      )
                             
                           ),
                           br(),
                           fluidRow(
                             column(12, offset = 9,  actionButton("gotomodel", "Data Exploration is completed Go to modelling page"), height = "100%")
                           ),
                           br()
                  ),
                  tabPanel(value="modelid",
                           
                           icon = icon("layer-group"),
                           "3. Build Segments",
                           br(),
                           p(strong("On this page, you can perform these Modelling analyses:"),br(),
                             "* Run and analyze Principal Component Analysis (PCA) and discover variables that can statistically create more reasonable segments",br(),
                             "* Run and analyze the Kmeans Clustering model to segment your data",br(),
                             "* Build a customized report after creating the clusters for deeper analysis",br(),
                             style="text-align:justify;color:black;background-color:powderblue;padding:15px;border-radius:10px"),
                           
                           br(),
                           
                           p("All the rows of your data will be used on this page except in the Feature Selection tab and the Clustering graph. In the Feature Selection tab if your data has more than 20000 rows a", 
                             strong("SAMPLE TABLE"), "with 20,000 rows will be randomly selected from your data to be used in the PCA model.",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                           
                           br(),
                           tabsetPanel(
                             tabPanel("Feature Selection",
                                      br(),
                                      tags$h3("Get the ordered significant variables for clustering"),
                                      tags$h5("If a value is NA/NULL(in numerical columns) then the corresponding row will be removed")
                                      ,
                                      fluidRow(
                                        column(4, actionButton("runfeatureselect", "Recommend significant features", class = "btn-warning")),
                                        verbatimTextOutput("numpcavar")
                                      ),
                                      hr(),
                                      fluidRow(
                                        column(12, withSpinner(plotOutput("significantvars", height = "800px")))
                                      ),
                                      fluidRow(
                                        column(12, withSpinner(gt_output("signigicanttable")))
                                      ),
                                      hr(),
                                      fluidRow(
                                        column(4, 
                                               withSpinner(gt_output("suggestedvars")),
                                               withSpinner(plotOutput("pcavargraph" , height = "100%"))
                                               ),
                                        column(8, withSpinner(plotOutput("corrplotcos", height = "100%")))
                                      ),
                                        br()
                                      #,
                                        # column(6,
                                        # fluidRow(
                                        #        withSpinner(plotOutput("pcavargraph" , height = "100%"))
                                        #        )),
                                        # br()
                                      ),
                             tabPanel("Kmeans Clustering",
                                      br(),
                                      tags$h3("A: Elbow Graph - Plot Variance covered by Each K(Number of Clusters)"),
                                      tags$h5("It is recommended to choose a K value where an elbow is created in the graph"),
                                      fluidRow(
                             column(4,
                                    wellPanel(
                                      fluidRow(
                                        htmlOutput("uniqevar"),
                                        htmlOutput("modelvars"),
                                        htmlOutput("kmeanalgorithm"),
                                        numericInput("maxelbowk",
                                                     "Max k for elbow plot:",
                                                     value = 3,
                                                     min = 1,
                                                     max = 10),
                                        
                                        numericInput("nstarts",
                                                     "How many random sets should be chosen:",
                                                     value = 6,
                                                     min = 5,
                                                     max = 100),
                                        
                                        
                                        htmlOutput("iterationtimes"),
                                        actionButton("runelbow", "Draw Elbow Graph", class = "btn-warning")
                                      )
                                    )
                             ),
                             column(8,
                                    fluidRow(
                                      column(12,  withSpinner(plotOutput("silhouetteplot")))
                                    ))
                           ),
                           fluidRow(
                             hr()
                           ),
                           
                           fluidRow(
                             column(4,
                                    br(),
                                    tags$h3("B: Create the Clusters(Groups) - Build the summary table"),
                                    br(),
                                    wellPanel(
                                      fluidRow(
                                        
                                        numericInput("k", "K-Number of Clusters:", 3, min = 1, max = 10),
                                        
                                        actionButton("runclust", "Create K clusters", class = "btn-warning")
                                      )
                                    )
                             ),
                             column(8,
                                    withSpinner(reactableOutput("summarycluster"))
                                    )
                             ),
                           hr(),
                           br(),
                           tags$h3("C: Draw a Scatter Plot for clusters - Using the model's variables "),
                           br(),
                           fluidRow(
                             column(4,
                                    wellPanel(
                                      fluidRow(       
                                    htmlOutput("xvar"),
                                    htmlOutput("yvar"),
                                    selectInput("transplot",
                                                    "Log-Transform Axis:",
                                                    c("No transformation" = "base",
                                                      "X axis" = "X",
                                                      "Y axis" = "Y",
                                                      "Both axis" = "XY")),
                                    actionButton("drawclustgraph", "Draw Clusters", class = "btn-warning"),
                                    tags$h5(HTML(paste("30,000 random points are plotted", "(if data has more than 30,000 rows)", sep="<br/>")))
                                    
                                      )
                                    )
                             )
                             ,
                             column(8,
                                    fluidRow(
                                      column(12, withSpinner( plotOutput("clusteringplot", height = "100%")))
                                    ))
                             ),
                           br()
                             
                           ),
                           tabPanel("Ad-hoc report",
                                    tags$h3("Get a table tailored for your need"),
                                    
                                    fluidRow(
                                      column(6, htmlOutput("groupby")),
                                      column(6,htmlOutput("summarizecount"))
                                      ),
                                    fluidRow(
                                      column(4,htmlOutput("summarize.mean.avg")),
                                      column(4, htmlOutput("summarize.mean")),
                                      column(4, htmlOutput("summarize.sd"))
                                    ),
                                    fluidRow(
                                      column(4,actionButton("runreport", "Create Report", class = "btn-warning"))
                                    ),
                                    hr(),
                                    fluidRow(
                                      column(12, withSpinner( dataTableOutput("adhoctable"))),
                                     # column(12, verbatimTextOutput("shorttime"))
                                    ),
                                    fluidRow(
                                      column(4, downloadButton('downloadData', 'Download the Report(CSV)'))
                                    ),
                                    br(),
                                    hr(),
                                    tags$h3("Create a scatterplot from your data"),
                                    br(),
                                    fluidRow(
                                    column(4,
                                           wellPanel(
                                             fluidRow(
                                    htmlOutput("scatterx"),
                                    htmlOutput("scattery"),
                                    htmlOutput("scattercolor"),
                                    selectInput("smoothmethod", "Smoothing method:",
                                                c("", "lm", "glm", "gam", "loess"),
                                                selected = NULL,
                                                selectize = FALSE
                                    ),
                                    checkboxInput("seinclude", "Display confidence interval?", value = FALSE, width = NULL),
                                    selectInput("scattertransplot",
                                                "Log-Transform Axis:",
                                                c("No transformation" = "base",
                                                  "X axis" = "X",
                                                  "Y axis" = "Y",
                                                  "Both axis" = "XY")),
                                    actionButton("createscatter", "Create a scatterplot", class = "btn-warning")
                                    
                                             ))),
                                    column(8,
                                           fluidRow(
                                             column(12, withSpinner( plotOutput("scatterplot", height = "100%")))
                                           )),
                                    br()
                                    )
                           )
                           )
                           ),
tabPanel("Help", 
         icon = icon("question"),
         br(), br(),
         tags$div(column(12,  p(style=" font-size:120%", "This app is built to be as user-fridnly as possible but you might want to know more about the details. So here is a confluence page where can read detailed description of each section of the app:"),
                                 tags$div(
                                          fluidRow(
                                            column(4, actionButton("gotodoc", "Take a look at a handy Doc", icon("book"),
                                                                              onclick ="window.open('https://newsela.atlassian.net/wiki/spaces/DAS/pages/3351412962/Customer+segmentation+App', '_blank')",
                                                                              style="color: #0A6EFA; text-align:center; background-color: #E7F1FF; border-color: #2e6da4;  font-size:110%"))
                                          ),
                                 )
)))
                  
                  )


server <- function(input, output, session) {
  # Customise your plots to match the theme
  #thematic::thematic_shiny()
  # creating UI for data import based on the users choice (Query or CSV)
  

  
  
  ### Transition to IMPORT
  
  
  
  observeEvent(input$gotoimport, {
    updateNavbarPage(session,
                     "navid",
                     selected = "importtransid"
    )
  })
  
  
  output$chosenimport <- renderUI({
    switch(input$importmethod, 
           "querytext" =  textAreaInput("query",
                                        "Write your Query here(Dont use commented lines \"--\")",
                                        "SELECT *\nFROM data_hub.wide.teacher_profile\nLIMIT 200",
                                      #  "SELECT *\nFROM analytics.sandbox.initial_teacher_cluster_data\nLIMIT 200",
                                        width = validateCssUnit("100%"),
                                        height = validateCssUnit('200px')
           ),
           "csvfile" = fileInput("file1",
                                 "Choose CSV File",
                                 accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
           )
    )
  })
  
  output$importdetails <- renderUI({
    switch(input$importmethod, 
           "querytext" =  actionButton("go", "Run and Update Data", class = "btn-warning", width = "100%"),
           "csvfile" = checkboxInput("header", "Does your data have Header?", TRUE)
    )
  })
  
  
  importvalues <- reactiveValues(statusqueryerror= NULL, successmessage= NULL, iserrorquery= NULL)
 
   textquery <- eventReactive(input$go, {
    id <- showNotification("Running the Query(The Peformance depends on SNOWFLAKE server)", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    
    trythequery <- tryCatch(DBI::dbGetQuery(con, input$query),
             error=function(e) NULL)
    
    defaultresult <- tryCatch(DBI::dbGetQuery(con,"SELECT 'There is something wrong in the query' AS ERROR"),
                    error=function(e) NULL)
    
    # validate(
    #   need( is.null(ddd), "Please select a data set")
    # )
    
    
    importvalues$iserrorquery <- if (is.null(tryCatch(trythequery, error=function(e) NULL))) {
      1
      } else {
        0  }
    
    result <- if (is.null(trythequery)) { 
      defaultresult } else {
        trythequery  }
    
    errormessage <- if (is.null(trythequery)) {
      paste("Ooops... There is a problem with the query. Make sure it is not commented.
             It only reads the data so you can not change the warehouse or etc.",
            "Maybe you should try your query in Snowflake first",
            sep="\n") } else {
        ""  }
    
    successmessage <- if (!is.null(trythequery)) {
      paste("Well done your query worked",
            sep="\n") } else {
              ""  }
    
    
    
    importvalues$statusqueryerror <- errormessage
    
    importvalues$statusquerysuccess <- successmessage
    
    
    
    return(result)
    
  })
    
    # my_update_function <- function(x){
    #   result <- tryCatch(
    #     # This is what I want to do...
    #     {
    #       half <- NULL
    #       data <- DBI::dbGetQuery(x, input$query)
    #       half <- "Right"
    #       return(data)
    #       
    #     },
    #     # ... but if an error occurs, tell me what happened: 
    #     error=function(e) {
    #       rightquery <- FALSE
    #       a <- shinyFeedback::feedbackWarning("n", !rightquery, "Please select an even number")
    #       half <- "Wrong"
    #       
    #       
    #       errordefault <- DBI::dbGetQuery(x,"SELECT *\nFROM analytics.sandbox.initial_teacher_cluster_data\nLIMIT 5")
    #       return(errordefault)
    #     }
    #   )
    #   importvalues$statusquery <- half
    #   return(result)
    # }
    # 
    # 
    # my_update_function(con)
    
  # tryCatch( function(x) {
  #  tryCatch( DBI::dbGetQuery(con, input$query)
  #  )
  # },
  #  
  #  error= function(e) {
  #    
  #    wrongquery <- FALSE
  #    a <- shinyFeedback::feedbackWarning("n", !wrongquery, "Please select an even number", color = "#F89406",)
  #    half <- "You commented your query or the table does not exist. Make sure you have "
  #    output$half <- renderText(half)
  #    DBI::dbGetQuery(con,"SELECT *\nFROM analytics.sandbox.initial_teacher_cluster_data\nLIMIT 5" )
  #   
  # })
    
    
  
  output$queryerror <- renderUI({
    switch(input$importmethod, 
           "querytext" =  renderText(importvalues$statusqueryerror),
           "csvfile" = ""
    )
  })
  
  output$querysucess <- renderUI({
    switch(input$importmethod, 
           "querytext" =  renderText(importvalues$statusquerysuccess),
           "csvfile" = ""
    )
  })
  
  warningnumeric <- reactive({ 
    if (is.null(importvalues$iserrorquery)) {
      NULL
    } else {
    
    if (importvalues$iserrorquery==1) {
    NULL
    } else {
    if (numberofNumerical()==0) {
    paste("Notice that You can not perform Clustering Modeling as there are 0 numerical variables ",
          sep="\n")
  } else {
    if (numberofNumerical()==1) { 
    "It is recommended to have at least one more numerical variable. (You have only 1 now)"
    } else {
        NULL
    }
  }
    }
    }
  })
  
  output$numericwarning <- renderUI({
    switch(input$importmethod, 
           "querytext" =  renderText(warningnumeric()),
           "csvfile" = renderText(warningnumeric())
    )
  })
  
  
  
#  output$queryerror <- renderText(importvalues$statusquery)
  
  filedata <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read_delim(inFile$datapath, col_names = input$header)
  })
  
  # based on the button the users clicks(Update or input file)
  values <- reactiveValues(df_data = NULL, datebins= NULL)
  observeEvent(input$file1, {
    values$df_data <- filedata()
    names(values$df_data) <- make.names(names(values$df_data), unique=TRUE)
  })
  observeEvent(input$go, {
    #temp <- textquery()
    values$df_data <- textquery()
    names(values$df_data) <- make.names(names(values$df_data), unique=TRUE)
    
  })
  
  
  # Number of rows
  #rownumb
  output$tablerow <- renderValueBox({
    shinydashboard::valueBox( #VB_style( paste0( '$',format(tmp_ex_tot,big.mark=','), " m"), "font-size: 60%;"  ),
      
      tryCatch(if(is.null(values$df_data)) {
        0 
        } else {
          dim(values$df_data)[1]
                },
        error=function(e) 0 ),       

      "Rows",
      color = "blue"
      # icon = icon('export', lib = 'glyphicon'), # icon("sign-in"),
      #color = "blue"
    )
  })
  
  output$tablecol <- renderValueBox({
    shinydashboard::valueBox( #VB_style( paste0( '$',format(tmp_ex_tot,big.mark=','), " m"), "font-size: 60%;"  ),
      
      tryCatch(if(is.null(values$df_data)) {
        0 
      } else {
        dim(values$df_data)[2]
      },
      error=function(e)0),
      "Columns",
      # icon = icon('export', lib = 'glyphicon'), # icon("sign-in"),
      #color = "blue"
    )
  })
  
  output$numbcolnum <- renderValueBox({
    shinydashboard::valueBox( #VB_style( paste0( '$',format(tmp_ex_tot,big.mark=','), " m"), "font-size: 60%;"  ),
      
      tryCatch(numberofNumerical(), error=function(e)0),
      "Numerical Columns",
      #                     icon = icon('export', lib = 'glyphicon'), # icon("sign-in"),
      #color = "blue"
    )
  })
  
  output$factcolnum <- renderValueBox({
    shinydashboard::valueBox( #VB_style( paste0( '$',format(tmp_ex_tot,big.mark=','), " m"), "font-size: 60%;"  ),
      
      tryCatch(ifelse(is.null(numberoffactors()),0, numberoffactors()), error=function(e)0),
      "Categorical Columns",
      #                     icon = icon('export', lib = 'glyphicon'), # icon("sign-in"),
      #color = "blue"
    )
  })
  
  output$datecolnum <- renderValueBox({
    shinydashboard::valueBox( #VB_style( paste0( '$',format(tmp_ex_tot,big.mark=','), " m"), "font-size: 60%;"  ),
      
      tryCatch(numberofdates(), error=function(e)0),
      "Date Columns",
      #                     icon = icon('export', lib = 'glyphicon'), # icon("sign-in"),
      #color = "blue"
    )
  })
  
  # try to guess which columns are factors ( string with less than 30 distinct values or numerical with less than 5 distinct values)
  
  estimatedfactor <- reactive({
    
    tryCatch(values$df_data %>% 
               select(where(is.character)) %>%
               gather(var, value) %>% 
               distinct() %>% 
               count(var) %>%
               # if less than 30 distinct values    
               filter(n<30) %>%
               select(var) %>%
               pluck("var"),
             error = function(e)
               NULL)

    
    })
  
 
  
  # changing data types
  numericestimatedfactor <- reactive({
    tryCatch(values$df_data %>%
               select(where(is.numeric), where(is.integer)) %>%
               gather(var, value) %>%
               distinct() %>%
               count(var) %>%
               # if less than 5 distinct values
               filter(n<5) %>%
               select(var) %>%
               pluck("var"),
             error = function(e)
               NULL)
   
  })
  
  
  
  
  # Creating input for changing the column type to factor
  output$tofactorinput <- renderUI({
    selectInput(
      "tofact",
      "Convert to Category",
      names(values$df_data),
      selected = NULL,
      multiple = TRUE
    )
    
  } )
  
  # Creating input for changing the column type to string
  output$tostringinput <- renderUI({
    selectInput(
      "tostring",
      "Convert to String(USER_ID, School_ID, etc)",
      names(values$df_data),
      selected = NULL,
      multiple = TRUE
    )
  })
  
  
  # Creating input for changing the column type to Numeric
  output$tonumericinput <- renderUI({
    selectInput(
      "tonumeric",
      "Convert to Number(Number of prints, Duration)",
      names(values$df_data),
      selected = NULL,
      multiple = TRUE
    )
  })
  
  
  
  # Creating input for changing the column type to Numeric
  output$todateinput <- renderUI({
    selectInput(
      "todate",
      "Convert to Date(Should be in YMD format)",
      names(values$df_data),
      selected = NULL,
      multiple = TRUE
    )
  })
  
  #datahandling <- reactiveValues(datachanged = NULL)
  
  # Convert Logical and  strings( that are possibly factor) columns to factor
  middledata <- reactive({
    middle <- values$df_data %>%
      as.data.frame() %>%
      mutate_at(estimatedfactor(), as_factor) %>%
      mutate_at(numericestimatedfactor(), as_factor) %>%
      mutate_if(is.logical, as_factor)
    #datahandling$datachanged <- 0
    
  })
  
  # we need to convert the column types but also make sure if the user does not choose any it still works
  datahandling <- reactiveValues(tofactorconvert = NULL, tostringconvert = NULL,tonumericconvert = NULL,todateconvert = NULL)
  
  # assign the chosen variables and concat them to see if any of them has a value to see if checkfields is null or not
  checkfields <- eventReactive(input$convertbutton, {
    datahandling$tofactorconvert <- input$tofact
    datahandling$tostringconvert <- input$tostring
    datahandling$tonumericconvert <- input$tonumeric
    datahandling$todateconvert <- input$todate
    c(input$tofact, input$tostring, input$tonumeric, input$todate)
  }
    , ignoreNULL = FALSE
  )
  # build the final table based on the filters and transformation  
  finaldata <- reactive({
    if(is.null(checkfields())) {
      middledata()
    } else {
    middledata() %>%
      mutate_at(vars(one_of(datahandling$tofactorconvert)), as_factor)  %>%
      mutate_at(vars(one_of(datahandling$tostringconvert)), as.character) %>%
      mutate_at(vars(one_of(datahandling$tonumericconvert)), as.numeric) %>%
      mutate_at(vars(one_of(datahandling$todateconvert)), ymd)
    }
  }
  
  )
  
  
  
  # Type of each column
  typetable <- reactive ({  
    data.frame(
      subclass = unlist(map(finaldata(), class) %>%
                          map(1) # to get the first element only (If we have POSIXct or similar it can produce more than 1 row per variable)
      )
    ) %>%
      mutate(variable=rownames(.)) %>%
      unique() %>%
      mutate(
        Mainclass= case_when(subclass == 'factor'  ~ 'Categorical',
                             subclass == 'logical'  ~ 'Categorical', 
                             subclass == 'Date'~ 'Date',
                             subclass == 'POSIXct' | subclass == 'POSIXt' ~ 'DateTime',
                             subclass == 'character' ~ 'Character',
                             subclass == 'numeric'| subclass =='integer' ~ 'Numeric',
                             TRUE ~ '0')
      ) %>%
      select(variable, Mainclass, subclass) %>%
      as_tibble()
  })
  
  output$typeofcolumns <- renderDataTable({
    a <- tryCatch(typetable(),
                  error=function(e) NULL)
    datatable(a, options = list(searching = TRUE)) 
  })
  
  
  sampledata <- reactive({
    set.seed(27)
    datarownumber <- dim(finaldata())[1]
    
  tryCatch( if (datarownumber>20000) {
    finaldata() %>% slice_sample(n= min(20000, datarownumber))
  } else {
    finaldata()
  },
  error = function(e)NULL)
  })
  
 # To exclude the variables that are  
  exclude_pca_many_in_one_value <- reactive({
    tryCatch(sampledata() %>%
               select(where(is.numeric), where(is.integer)) %>%
               gather(var, value) %>%
               group_by(var) %>%
               count(value) %>% 
               arrange(desc(n) , .by_group = TRUE) %>%
               slice_max(order_by = n, n = 1) %>%
               mutate(totalrow= dim(sampledata())[1],
                      perc= 100*n/totalrow) %>%
               filter(perc >= 25) %>%
               distinct(var) %>%
               pluck("var"),
             error = function(e)
               NULL)
    
  })
  
  
  ### Transition to EDA
  
  
  observeEvent(input$gotoeda, {
    updateNavbarPage(session,
                      "navid",
                      selected = "edaid"
    )
  })
  
 

  #### EDA
  
  # count of each column types
  counttype <- reactive({
    # very important to keep all pages clean
    firstcheck <- !is.na(sampledata())
    req(firstcheck)
    
    data.frame(
      subclass = unlist(
        map(sampledata(), class) %>%
          map(1))) %>%
      mutate(variable=rownames(.)) %>%
      group_by(subclass) %>%
      summarise(count=n()) %>%
      ungroup()
  })
  
  # Number of Factors
  numberoffactors <- reactive({
    counttype() %>% 
      filter(subclass=="factor")  %>%
      pluck("count")
  })
  
  # Make a adjustable height for factor based on the number of factors
  factorheight <- reactive({
    factnumb <- counttype() %>% 
      filter(subclass=="factor")  %>%
      pluck("count")
    return(50+factnumb*80)
    
  })
  
  # Number of Dates
  numberofdates <- reactive({
    counttype() %>% 
      filter(subclass %in% c("Date", "POSIXct", "POSIXt") ) %>%
      summarize(totalcount=sum(count)) %>%
      pluck("totalcount")
      
  })
  
  # Make a adjustable height for date based on the number of dates
  dateheight <- reactive({
    #reactive
    datenumb <-  counttype() %>% 
      filter(subclass %in% c("Date", "POSIXct", "POSIXt") ) %>%
      summarize(totalcount=sum(count)) %>%
      pluck("totalcount")
    return(200+datenumb*80)
    
  })
  
  # Number of Numerical
  numberofNumerical <- reactive({
    counttype() %>% 
      filter(subclass %in% c("numeric", "integer") ) %>%
      summarize(totalnum=sum(count)) %>%
      pluck("totalnum")
    
  })
  
  # Make a adjustable height for numeric based on the number of numerical columns
  # output$testo <- renderPrint({
  numheight <- reactive({
    #reactive
    numnumb <-  counttype() %>% 
      filter(subclass %in% c("numeric", "integer") ) %>%
      summarize(totalnum=sum(count)) %>%
      pluck("totalnum")
    
    return(200+numnumb*80)
    
  }) 
  
  
  
  
  # showing the output
  output$querydata <- renderDataTable({
    id <- showNotification("Reading the data", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    datatable(sampledata(),filter = list(position = 'top', clear = FALSE), options = list(autoWidth = TRUE, bAutoWidth = FALSE,  scrollX = TRUE) )
  })    
  
  
  # Factor data
  factordata <- reactive({
    waiter::Waiter$new(id = "factorbar")$show()
    #waiter_show(html = waiting_screen, color = "grey", id="factorbar")
    Sys.sleep(3) # do something that takes time
    
    sampledata() %>% 
      select(where(is.logical), where(is.factor))
  })
  
  factord <- eventReactive(input$showtablefact, {
   factord <- factordata() %>%
      summary(maxsum=as.numeric(input$factobs)) %>%
      unclass() %>%
      data.frame(check.names = FALSE, stringsAsFactors = TRUE)
    
    newname <- names(factord)
    finalname <- paste(newname,  "(value:count)", sep="\n")
    names(factord) <- finalname
    
    factord
  })
  
  # suggest excluding variables (By app) less than 2 distinct values for factor variables
  
  appexcludefact <- reactive({
    tryCatch(sampledata() %>%
               select(where(is.logical), where(is.factor)) %>%
               gather(var, value) %>%
               distinct() %>%
               count(var) %>%
               # if less than 5 distinct values
               filter(n<2) %>%
               select(var) %>%
               pluck("var"),
             error = function(e)
               NULL)
    
  })
  
  
  output$factorexclude <- renderUI({
    selectInput("excludefact",
                "Nominate these variables to be excluded from the modeling page",
                names(factordata()),
                selected = appexcludefact(),
                multiple = TRUE,
                width = '100%')
  })
  
  # If there is not factor show this
  output$nofactorshow <- renderPrint({
    checkexistance <- try(numberoffactors())
    req(checkexistance)
    if(is.null(numberoffactors())) {
      cat("There is NO Categorical column in your data")
    } else {
      cat(paste("There are ", numberoffactors(), " categorical columns in your data"))
    }
  })
  
  # show factor summary table
  output$factorsummary <- renderDataTable({
    #checkexistance <- try(factord())
    #req(checkexistance)
    id <- showNotification("Building Categorical summary table", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    # w <- Waiter$new()
    # w$show()
    waiter::Waiter$new(id = "factorsummary")$show()
    tryCatch(datatable( factord(), options = list(pageLength = 12, scrollX=TRUE, scrollCollapse=TRUE)),
             error = function(e) NULL )
    #datatable( factord(), options = list(pageLength = 12, scrollX=TRUE, scrollCollapse=TRUE))
    
  }
  
  )    
  
  
  factorbardata <- eventReactive(input$showbarfact, {
    
    
    id <- showNotification("Calculating data for Categorical bar chart", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    factordata() %>%
      pivot_longer(everything(), names_to = "key", values_to = "value")  %>%
      ungroup() %>%
      group_by(key) %>%
      mutate(value=fct_lump_n(value, n = as.numeric(input$factobsbar), ties.method = "first")) %>%
      count(value, sort = TRUE) %>% 
      ungroup() %>%
      filter(!is.na(value))
  })
  
  
  
  
  # Explore factor
  output$factorbar <- renderPlot({
    checkexistance <- try(factorbardata())
    req(checkexistance)
    
    id <- showNotification("Building Categorical Bar Chart", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    #waiter::Waiter$new(id = "factorbar", color = "black")$show()
    #waiter_show(html = waiting_screen, color = "grey", id="factorbar")
    #Sys.sleep(3) # do something that takes time
    
    
    factplot <- factorbardata() %>%
      ggplot(aes(reorder_within(as.factor(value), -n, as.factor(key)) , n, fill= n))+
      geom_col()+
      scale_x_reordered() +
      facet_wrap( ~ as.factor(key), ncol = 1, scales = "free") +
      scale_fill_gradient(low = "grey", high = "brown")+
      xlab("Values")+
      ylab("Count")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
      labs(title = "Bar chart of the factors in the data",
           subtitle = "Only top values are shown",
           caption = "You can add to categories in Import and Transform page")+
      #coord_flip()+
      #theme_minimal()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            strip.text.x = element_text(size = 20),
            axis.text = element_text(size = 15,face="bold"),
            legend.position="bottom")
    tryCatch(
    plot(factplot, res = 96)
    ,
    error = function(e) NULL )
   waiter_hide()
   
  }
  # without "function" the height does not work but I don't know why 
  
  , height=  function(){if(is.null(factorbardata())){
    200
    } else {
      4*factorheight()}
    }
  
  )
  
  
  # If there is no Date column show this
  output$nodateshow <- renderPrint({
    checkexistance <- try(numberofdates())
    req(checkexistance)
    if(is.null(numberofdates())) {
      cat("There is NO Date column in your data")
    } else {
      cat(paste("There are ", numberofdates(), " date columns in your data"))
    }
  })
  
  # Date data
  datedata <- reactive({
    sampledata() %>% 
      select(where(is.Date), where(is.POSIXct), where(is.POSIXt))
  })
  
  # suggest excluding variables (By app) less than 2 distinct values for date variables
  
  appexcludedate <- reactive({
    tryCatch(sampledata() %>%
               select(where(is.Date), where(is.POSIXct), where(is.POSIXt)) %>%
               gather(var, value) %>%
               distinct() %>%
               count(var) %>%
               # if less than 2 distinct values
               filter(n<2) %>%
               select(var) %>%
               pluck("var"),
             error = function(e)
               NULL)
    
  })
  
  output$dateexclude <- renderUI({
    checkexistance <- try(datedata())
    req(checkexistance)
    selectInput("excludedate",
                "Nominate these variables to be excluded from the modeling page",
                names(datedata()),
                selected = appexcludedate(),
                multiple = TRUE,
                width = '100%')
  })
  
  
  
  dated <-  eventReactive(input$showdatesummary, {
    checkexistance <- try(datedata())
    req(checkexistance)
    id <- showNotification("Calculating data for date columns", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    datedata() %>%
    summary() %>%
    unclass() %>% 
    data.frame(check.names = FALSE, stringsAsFactors = TRUE) 
  }
  )
  
  # show date summary table
  output$datesummary <- renderDataTable({
    # checkexistance <- try(dated())
    # req(checkexistance)
    id <- showNotification("Building date summary table", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    datatable( dated() %>% slice(-4), options = list(scrollX=TRUE, scrollCollapse=TRUE))
  }
  
  )  
  
  
  
  # Date explore base data
  
  DateExploreTable <- reactive({
    checkexistance <- try(datedata())
    req(checkexistance)
    id <- showNotification("Calculating data for date columns", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    waiter::Waiter$new(id = "datehistogram",
                       html = spin_solar()
                       )$show()
    
    Sys.sleep(3)
    
    datedata() %>%
      pivot_longer(everything(), names_to = "key", values_to = "value")  %>%
      ungroup() %>%
      group_by(key) %>%
      count(value, sort = TRUE) %>% 
      ungroup() %>%
      mutate(year= as.factor(year(value))) %>%
      # To remove na value otherwise the scale will not work
      filter(!is.na(value)) %>%
      group_by(key) %>%
      mutate(averagedate=mean(value)) %>%
      ungroup()
  })

  
 dateboxplottable <- eventReactive(input$showdateboxplot, {
    DateExploreTable()
  })
  
  # Explore date boxplot
  output$dateboxplot <- renderPlot({
    checkexistance <- try(dateboxplottable())
    req(checkexistance)
    id <- showNotification("Building date boxplot", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    
    first <- dateboxplottable() %>% 
      ggplot(aes(x=factor(0), y=value, fill=as.factor(key))) + # fill=name allow to automatically dedicate a color for each group
      geom_boxplot(alpha= 0.6) +
      geom_point(alpha=0.4) +
      facet_wrap( ~ as.factor(key), ncol = 1, scales = "free") +
      scale_fill_viridis(direction= -1, discrete = T) +
      coord_flip()+
      theme_minimal()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            strip.text.x = element_text(size = 20),
            axis.text = element_text(size = 15,face="bold"),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position="bottom")

   second <- first +
     scale_y_datetime(date_labels =  "%b %Y")
   
   third <- first +
     scale_y_date(date_labels = "%b %Y")
   
   # datebox <- tryCatch(print(second),
   #          error = function(e)
   #            eval(parse(text = 'first + scale_y_date(date_labels = "%b-%d-%Y")' ))
   # ) +
   
     # scale_y_date(date_labels =  "%b %Y")+
    #tryCatch(scale_y_datetime(breaks = breaks_pretty()),
    #           error = function(e)  NULL
                 #scale_y_date(breaks = breaks_pretty())
    #         ) +
  # scale_y_datetime(breaks = breaks_pretty()) +
   # datebox <- tryCatch(scale_y_datetime(date_labels =  "%b %Y"),
   #             error = function(e) 
   #               scale_y_date(date_labels =  "%b %Y")
   #          ) +
    # scale_y_datetime(date_labels =  "%b %Y") +
      
    
    tryCatch(plot(second, res = 96),
             error = function(e)
               tryCatch(plot(third, res = 96), 
                        error = function(e) NULL )
    )
    
  }
  
  , height=  function(){if(is.null(dateheight())){
    200
  } else {
    dateheight()*1.5}
  }
  
  )
  
   
  
  datehistogramtable <- eventReactive(input$showdatehistogram, {
    values$datebins <- input$BinsHistDate
    DateExploreTable()
  })
  
  # Explore date histogram
  output$datehistogram <- renderPlot({ 
    checkexistance <- try(datehistogramtable())
    req(checkexistance)
    id <- showNotification("Building date histogram(This one is challenging)", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    first <- datehistogramtable() %>% 
      ggplot(aes(x=value, fill=year)) + # fill=name allow to automatically dedicate a color for each group
      geom_histogram(alpha=0.6, bins = values$datebins)  +
      facet_wrap( ~ as.factor(key), ncol = 2, scales = "free") +
      scale_fill_viridis(direction= -1, discrete = T)+
      geom_rug(alpha=0.6) +
      geom_vline(aes(xintercept=averagedate),
                 color="black", linetype="dashed", size=2)+
    #geom_density(stat="bin", bins = input$BinsHistDate, alpha=0.25, position="identity")
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            strip.text.x = element_text(size = 20),
            axis.text = element_text(size = 15,face="bold"),
            legend.position="bottom")
    
    second <- first +
      scale_x_datetime(date_labels =  "%b %Y")
    
    third <- first +
      scale_x_date(date_labels = "%b %Y")
    
    
    tryCatch(plot(second, res = 96),
             error = function(e)
               tryCatch(plot(third, res = 96), 
                        error = function(e) NULL )
    )
    #  scale_x_datetime(breaks = breaks_pretty()) +
    # scale_x_datetime( date_labels =  "%b %Y") +
    
    # plot(datehist, res = 96)
  }
  
  , height=  function(){if(is.null(dateheight())){
    200
  } else {
    dateheight()*2}
  }
  
  
  )
  
  # Explore date
  # output$datearea <- renderPlot({
  #   checkexistance <- try(DateExploreTable())
  #   req(checkexistance)
  #   dateplot <- DateExploreTable() %>%
  #     ggplot(aes(x=value)) +
  #     geom_point(aes(y=n, fill=year), color="black", alpha=0.3, size=1) +
  #     #geom_smooth(geom = 'area', aes(y=n, fill=year), alpha= 0.6, stat = "identity", method = 'loess', span = 1/3) +
  #     stat_smooth(aes(y=n, fill=year),
  #                 geom = 'area', method = 'loess', span = 2/3,
  #                 alpha = 1/2) + 
  #     #, fill = "red"
  #     #geom_histogram(binwidth = 1, alpha = 0.5, stat ="identity")+
  #     
  #     facet_wrap( ~ as.factor(key), ncol = 2, scales = "free") +
  #     scale_x_datetime(breaks = breaks_pretty()) +
  #     scale_x_datetime( date_labels =  "%b %Y") +
  #     scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  #     scale_fill_viridis(direction= -1, discrete = T) +
  #     #theme_minimal()+
  #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #           panel.background = element_blank(), axis.line = element_line(colour = "black"),
  #           strip.text.x = element_text(size = 20),
  #           axis.text = element_text(size = 15,face="bold"))
  #   
  #   plot(dateplot, res = 96)
  # }
  # # without "function" the height does not work but I don't know why 
  # 
  # , height=  function(){if(is.null(dateheight())){
  #   200
  # } else {
  #   dateheight()}
  # }
  # 
  # )
  
  
  
  # Numeric data
  numbdata <- reactive({
    sampledata() %>% 
      select(where(is.integer), where(is.numeric))
  })
  
  # If there is no numerical column show this
  output$nonumbershow <- renderPrint({
    checkexistance <- try(numberofNumerical())
    req(checkexistance)
    if(is.null(numberofNumerical())) {
      cat("There is NO Numerical column in your data")
    } else {
      cat(paste("There are ", numberofNumerical(), " Numerical columns in your data"))
    }
  })
  
  # suggest excluding variables (By app) less than 15 distinct values for numerical variables
  
  appexcludenumb <- reactive({
    tryCatch(sampledata() %>%
               select(where(is.numeric), where(is.integer)) %>%
               gather(var, value) %>%
               distinct() %>%
               count(var) %>%
               # if less than 5 distinct values
               filter(n<15) %>%
               select(var) %>%
               pluck("var"),
             error = function(e)
               NULL)
    
  })
  
  
  output$numbexclude <- renderUI({
    checkexistance <- try(numbdata())
    req(checkexistance)
    selectInput("excludenumb",
                "Nominate these variables to be excluded from the modeling page",
                names(numbdata()),
                selected = appexcludenumb(),
                multiple = TRUE,
                width = '100%')
  })
  
  NumbExploreData <- reactive({
    id <- showNotification("Building data for Numerical columns", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    numbdata() %>%
      pivot_longer(everything(), names_to = "key", values_to = "value")  %>%
      ungroup() %>%
      group_by(key) %>%
      count(value, sort = TRUE) %>% 
      mutate(averagenumb=mean(value)) %>%
      ungroup() 
  })
  
  numbsummarydata <-   eventReactive(input$showtablenumb, {
    id <- showNotification("Calculating data for numerical columns", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    numbd <- numbdata() %>%
      summary() %>%
      unclass() %>% 
      data.frame(check.names = FALSE, stringsAsFactors = TRUE) %>%
      select(order(colnames(numbdata())))
    
    # function to get the mode
    modecalc <- function(x){
      uniqv <- unique(x)
      uniqv[which.max(tabulate(match(x, uniqv)))]
      # which.max(tabulate(x))
    }
    # build the mode and gini table
    ginimodetable <- NumbExploreData() %>%
      group_by(key) %>%
      summarise(gini = round(Gini(value, na.rm = TRUE),2)
                ,
                mode= modecalc(value)
                ) %>%
      ungroup() %>%
      mutate(gini=paste('Gini: ', gini)
             ,
             mode= paste('Mode: ', mode)
             ) %>%
      arrange(key) %>%
      rename(Metric=key, Gini.Index=gini
             , Mode= mode
             )
      columns <- ginimodetable$Metric
    #transpose as it needs to be matched with the numbd table. Also remove the first line as it is the same as column name
    ginimodetable <- ginimodetable %>% as.matrix() %>% t %>% as.data.frame() %>% slice(-1)
    names(ginimodetable) <- str_trim(columns)

    # need to remove the spaces as they want to be mapped together in bind_row
    names(numbd) = str_trim(names(numbd))
    names(ginimodetable) = str_trim(names(ginimodetable))
    # Combine two tables
    numbd <- bind_rows(numbd, ginimodetable)
    numbd
  })
  
  
  # show numerical summary table
  output$numbsummary <- renderDataTable({
    checkexistance <- try(numbsummarydata())
    req(checkexistance)
    id <- showNotification("Building Numerical summary table", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    datatable( numbsummarydata(), options = list(scrollX=TRUE, scrollCollapse=TRUE))
  }
  
  )
  
  
  
  
  
  # # Create Gini index
  # output$giniindex <- renderTable({
  #   
  #   # function to get the mode
  #   modecalc <- function(codes){
  #     which.max(tabulate(codes))
  #   }
  #   
  #   ginimodetable <- NumbExploreData() %>%
  #     group_by(key) %>%
  #     summarise(gini = round(Gini(value),2),
  #               mode= modecalc(value)) %>%
  #     ungroup() %>%
  #     mutate(gini=paste('Gini: ', gini),
  #            mode= paste('Mode: ', mode)) %>%
  #     arrange(key) %>%
  #     rename(Metric=key, Gini.Index=gini, Mode= mode)
  #   columns <- ginimodetable$Metric
  #   #columns
  #   
  #   #rownames(ginimodetable) <- ginimodetable$Metric
  #   
  #   
  #   ginimodetable <- ginimodetable %>% t %>% as.data.frame() %>% slice(-1)
  #   names(ginimodetable) <- columns
  #   ginimodetable
  # 
  # })
  
  #names(ginimodetable) <- ginimodetable %>% slice(1)
  #ginimodetable
  #t %>% as.data.frame %>% row_to_names(row_number = 1)
  #%>%
  #%>%
  #kable()
  # knitr::kable("html") %>%
  # kable_styling("striped", full_width = F)
  
  
  numbboxplottable <- eventReactive(input$shownumbboxplot, {
    NumbExploreData()
  })
  

  
  # Boxplot for Numerical charts
  output$numbboxplot <- renderPlot({
    checkexistance <- try(numbboxplottable())
    req(checkexistance)
    id <- showNotification("Building Boxplot for numerical variables", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    numbbox <- numbboxplottable() %>% 
      ggplot(aes(x=factor(0), y=value, fill=as.factor(key))) + # fill=name allow to automatically dedicate a color for each group
      geom_boxplot(alpha= 0.6) +
      geom_point(alpha=0.4) +
      facet_wrap( ~ as.factor(key), ncol = 1, scales = "free") +
      scale_fill_viridis(direction= -1, discrete = T) +
      coord_flip() +
      scale_y_continuous(breaks = breaks_pretty()) +
      #theme_minimal()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            strip.text.x = element_text(size = 20),
            axis.text = element_text(size = 15,face="bold"),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            legend.position="bottom")
    
    plot(numbbox, res = 96)
  }
  
  , height=  function(){if(is.null(numheight())){
    200
  } else {
    1.5*numheight()}
  }
  
  )
  
  numbhistogramtable <- eventReactive(input$shownumbhistogram, {
    id <- showNotification("Calculating data for numerical Histogram", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    values$numbbins <- input$BinsHistNumb
    NumbExploreData()
  })
  
  # Explore Numerical histogram
  output$Numbhistogram <- renderPlot({
    checkexistance <- try(numbhistogramtable())
    req(checkexistance)
    id <- showNotification("Building Histogram for numerical variables", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    numbhist <- numbhistogramtable() %>% 
      ggplot(aes(x=value)) + # fill=name allow to automatically dedicate a color for each group
      geom_histogram(alpha=0.6, bins = values$numbbins, fill='steelblue')  +
      #geom_area(stat="bin", bins = input$BinsHistDate, position="identity")+
      facet_wrap( ~ as.factor(key), ncol = 2, scales = "free") +
      #scale_fill_viridis(direction= -1, discrete = T)+
      scale_x_continuous(breaks = breaks_pretty()) +
      geom_rug(alpha=0.6)+
      geom_vline(aes(xintercept=averagenumb),
                 color="black", linetype="dashed", size=2)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            strip.text.x = element_text(size = 20),
            axis.text = element_text(size = 15,face="bold"),
            legend.position="bottom")
    #+
    #scale_fill_viridis_c()
    #scale_fill_continuous(type = "viridis")
    #geom_vline(aes(xintercept=averagedate),
    #           color="black", linetype="dashed", size=2)
    #geom_density(stat="bin", bins = input$BinsHistDate, alpha=0.25, position="identity")
    
    
    plot(numbhist, res = 96)
  }
  
  , height=  function(){if(is.null(numheight())){
    200
  } else {
    2*numheight()}
  }
  
  
  )
  
  numbviolintable <- eventReactive(input$shownumbviolin, {
    NumbExploreData()
  })
  
  
  
  
  # Violin chart for numerical variables
  
  output$numbviolin <- renderPlot({
    checkexistance <- try(numbviolintable())
    req(checkexistance)
    id <- showNotification("Building Violin Plot", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    numbplot <- numbviolintable() %>%
      ggplot(aes(x=factor(0), y=value, fill=as.factor(key))) + # fill=name allow to automatically dedicate a color for each group
      geom_violin(alpha= 0.6) +
      
      #geom_jitter() +
      geom_point(alpha=0.4) +
      stat_summary(fun = "mean",
                   geom = "point",
                   aes(x=factor(0), y=value, color = "Mean"),
                   size=7,
                   alpha=0.6,
                   show.legend = TRUE,
                   inherit.aes = FALSE) +
      stat_summary(fun = "median",
                   geom = "point",
                   aes(x=factor(0), y=value, color = "Median"),
                   size=7,
                   alpha=0.6,
                   show.legend = TRUE,
                   inherit.aes = FALSE) +
      # To add the point inisde the violin plot
      #geom_sina() +
      
      facet_wrap( ~ as.factor(key), ncol = 2, scales = "free") +
      scale_fill_viridis(direction= -1, discrete = T)+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            strip.text.x = element_text(size = 20),
            axis.text = element_text(size = 15,face="bold"),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            legend.position="bottom")
    
    plot(numbplot, res = 96)
  }
  # without "function" the height does not work but I don't know why 
  
  , height=  function(){if(is.null(numheight())){
    200
  } else {
    2*numheight()}
  }
  
  )
  
  
  # columns to be added to correlation matrix
  output$cormatrixvars <- renderUI({
    checkexistance <- try(numbdata())
    req(checkexistance)
    selectInput("corcols",
                "Choose correlation matrix variables",
                names(numbdata()),
                multiple = TRUE)
  })
  
  # Correlation factor (for using factor as color for correlation matrix)
  output$correlationfactor <- renderUI({
    checkexistance <- try(factordata())
    req(checkexistance)
    selectInput("corfact",
                "If you want you can choose a column for grouping the result(Should be a category)",
                choices= c("",names(factordata())),
                selectize = FALSE,
                selected = NULL,
                multiple = FALSE)
  })
  
  
  matrixdata <- eventReactive(input$corrun,{
    id <- showNotification("Calculating Correlation", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    numbdata() %>% select(input$corcols) %>% drop_na(input$corcols)
  })
  
  # correlation data
  correlationdata <- eventReactive(input$matrixplotrun,{
    id <- showNotification("Calculating Correlation Table", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    #waiter::Waiter$new(id = "numbmatrix")$show()
    #waiter_show(html = waiting_screen, color = "grey", id="factorbar")
   # Sys.sleep(3)
    
    sampledata() %>%
      select(input$corcols, any_of(input$corfact)) %>%
      mutate(correlationfactor=input$corfact)
    
  })
  
  
  # correlation matrix
  output$numbmatrix <- renderPlot({
    checkexistance <- try(matrixdata())
    req(checkexistance)
    
    id <- showNotification("Building Correlation matrix", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
   
    
    corr <- round(cor(matrixdata()), 1)
    
    #p.mat <- cor_pmat(numbdata())
    ggcorrplot(corr,
               #hc.order = TRUE,
               type = "lower",
               outline.col = "white",
               #p.mat = p.mat,
               lab = TRUE,
               #ggtheme = ggplot2::theme_gray,
               colors = c("#6D9EC1", "white", "#E46726")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            strip.text.x = element_text(size = 20),
            axis.text = element_text(size = 15,face="bold"))
    
  }
  #, width = function(){numheight()}
  , width = function(){940},
  height = function(){940}
  )
  
  
  
  # Creating Correlation Matrix Plot
  output$CorrelationMatrixPlot <- renderPlot({
    checkexistance <- try(correlationdata())
    req(checkexistance)
    
    id <- showNotification("Building Correlation matrixplot", type="message", duration = NULL, closeButton = FALSE)
   # on.exit(removeNotification(id), add = TRUE)
    on.exit(removeNotification(id), add = TRUE)
    # lowerFn <- function(data, mapping, method = "lm", ...) {
    #   p <- ggplot(data = data, mapping = mapping) +
    #     geom_point(alpha= 0.3, size=2) +
    #     geom_smooth(method = method, se=FALSE)+
    #     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #           panel.background = element_blank(), axis.line = element_line(colour = "black"),
    #           strip.text.x = element_text(size = 20),
    #           axis.text = element_text(size = 15,face="bold"))
    #   p
    # }
    
    # upperFn <- function(data, mapping, method = "lm", ...) {
    #     ggplot2::theme_light() +
    #     ggplot2::theme(axis.text=ggplot2::element_text(size=15),
    #                    strip.text=ggplot2::element_text(size=20,face="bold"))
    # }
    
    
      GGally::ggpairs(correlationdata(),
                      legend = 1,
                      columns= 1:(ncol(correlationdata())-1),
                    mapping=ggplot2::aes(colour = unlist( correlationdata() %>% select(any_of(correlationfactor))),  alpha=0.5),
                    lower = list(continuous = wrap("smooth", alpha = 0.1, size=2, se = FALSE)),
                    upper = list(continuous = wrap('cor', size=5)),
                    progress=TRUE)
    
    
      
  }
  , height=  function(){length(names(correlationdata()))*200}
  )
  
  
  # suggest excluding variables (By app) less than 2 distinct values for character variables
  
  appexcludecharacter <- reactive({
    tryCatch(sampledata() %>%
               select(where(is.character)) %>%
               gather(var, value) %>%
               distinct() %>%
               count(var) %>%
               # if less than 2 distinct values
               filter(n<2) %>%
               select(var) %>%
               pluck("var"),
             error = function(e)
               NULL)
    
  })
  
  
  output$excludingvariables <- renderUI({
    checkexistance <- try(finaldata())
    req(checkexistance)
    selectInput("excludingvars",
                "Choose the columns that are going to be excluded from the modelling page(If necessary)",
                choices = names(finaldata()),
                selected = c(appexcludecharacter(), input$excludefact, input$excludenumb, input$excludedate),
                multiple = TRUE,
                width = '100%')
  })    
  
  # Transition to model
  featureselectionvalues <- reactiveValues(allnumeric = NULL, sigdimnumb=NULL, temporarytidy=NULL, tempimportantvalues=NULL, numpcavar=NULL)
  
 
  columnstoberemoved <- eventReactive(input$excludevars,{
      # ro remove excluded columns
    
      input$excludingvars
      
  }
  # very important to add this otherwise It would give us nothing if we dont choose any value for input$excludevars
  , ignoreNULL = FALSE
  )
  
  
  output$excludingresult <- renderPrint({
    if(is.null(columnstoberemoved())){
      cat("You kept all the variables")
    } else {
      cat(paste(paste(columnstoberemoved(), collapse="  and  "), "\n are excluded so from the Modeling pages you wont see them"))
    }
   
  }
  
  )
  
  
  observeEvent(input$gotomodel, {
    updateNavbarPage(session,
                     "navid",
                     selected = "modelid"
    )
  })
  
  ### 3. Model
  
  # Feature selection
  res.pca <- eventReactive(input$runfeatureselect,{
    id <- showNotification("Calculating to pull the significant variables", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    checkexistance <- try(numbdata())
    req(checkexistance)
   featurenumericdata <- numbdata() %>%
     # ro remove excluded columns
      select(-any_of(columnstoberemoved()), -any_of(numericestimatedfactor()), -any_of(exclude_pca_many_in_one_value())) %>%
      drop_na(everything())
   
   featureselectionvalues$numpcavar <- tryCatch(dim(featurenumericdata)[2], error=function(e)0)
   featureselectionvalues$allnumeric <-  featurenumericdata
    res.pca <- PCA(featurenumericdata, scale.unit = TRUE, graph = FALSE)
    
    tryCatch(
    res.pca
    ,
    error=function(e) NULL
    )
  })
  
  
  # If there is no numerical column show this
  output$numpcavar <- renderPrint({
    checkexistance <- try(featureselectionvalues$numpcavar)
    req(checkexistance)
    tryCatch(
    if(is.null(checkexistance)) {
      cat("There is NO appropriate Numerical column that can be used in PCA algorithm")
    } else {
      cat(paste("There are ", featureselectionvalues$numpcavar, " appropriate Numerical columns that can be used in PCA algorithm"))
    }
    ,
    error=function(e) NULL
    )
  })
  
  
  
  output$significantvars <- renderPlot({
    id <- showNotification("Rendering PCA plot", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    checkexistance <- try(res.pca())
    req(checkexistance)
    
    eig.val <- get_eigenvalue(res.pca())
    eig.val <- data.frame(Component = row.names(eig.val),eig.val)
    signigicantdim <- eig.val %>% filter(eigenvalue>1) %>% dim() %>% nth(1)
    #signigicantdim <- eig.val %>%  filter(eigenvalue > 1) %>% dim() %>% nth(1)
    
    # I dont know why it only accept few dimensions so I only kept them
    var <- get_pca_var(res.pca())
    numberofaxes <- ncol(var$coord)
    
    finalaxe <- min(numberofaxes, signigicantdim)
    #head(var$coord)
    
    featureselectionvalues$sigdimnumb <- finalaxe
    tryCatch(
    fviz_contrib(res.pca(), choice = "var", axes = 1:finalaxe, top = 15)+
      theme(axis.text = element_text(size = 12,face="bold"),
            axis.title = element_text(size = 20,face="bold"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    ,
    error=function(e)NULL
    )
  }
  , res = 96
  )
  
  
  output$signigicanttable <-  render_gt({
    id <- showNotification("Rendering PCA table", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    checkexistance <- try(res.pca())
    req(checkexistance)
    
    contribframe <- facto_summarize(
      res.pca(),
      "var",
      node.level = 1,
      group.names,
      result = c("contrib"),
      axes = 1:featureselectionvalues$sigdimnumb,
      select = NULL
    )
    
    corrmat <- cor(featureselectionvalues$allnumeric)
    
    numbcols <- dim(corrmat)[2]
    colstart <- numbcols+2+1
    
    result <- bind_cols(contribframe, round(corrmat,2)) %>%
      as.data.frame() %>%
      arrange(-contrib) %>%
      mutate(contrib= round(contrib,2)) %>%
      rowwise() %>%
      mutate(across(
        -c(1,2), ~ if_else(abs(.x)>=0.6 & name!=cur_column(), cur_column() , NULL)
        , .names = "{.col}_status"))
  #  featureselectionvalues$tempimportantvalues <- result
    result <- result %>%
      unite(Dont.use.together.with, colstart:(colstart+numbcols-1), sep = " , ", na.rm = TRUE) %>%
      ungroup() %>%
      select(name, contrib, Dont.use.together.with, everything()) %>%
      rename(Contribution=contrib, Ranked.Feature=name)
    
    firsvar <- result$Ranked.Feature %>% nth(1)
    numbrow <- nrow(result)
    firstcol <- numbrow+3
    lastcol <-  firstcol+numbrow-1

    featureselectionvalues$tempimportantvalues <-  result[,1:3] %>%
      as_tibble() %>%
      separate(Dont.use.together.with, paste0("x", 1:numbrow), ",") %>% 
      mutate(across(where(is.character), str_trim)) %>%
      mutate(across(
        -c(1,2), ~ if_else( firsvar==.x, 1 , 0)
        , .names = "{.col}_status")) %>%
      rowwise() %>%
      mutate(opposite=max(c_across(firstcol:lastcol), na.rm = TRUE))%>%
      select(Ranked.Feature, opposite) %>%
      filter(opposite!=1) %>%
      ungroup() %>%
      slice(1:2) %>%
      pull("Ranked.Feature") %>%
      as.character()

    tryCatch(
    result %>%
      slice(1:20) %>%
      gt() %>%
      tab_spanner(label = md("Correlation matrix(**Dont** use columns close to `1` or `-1` simultaneously"), columns = 4:(4+numbcols-1)) %>%
      tab_spanner(label = md("Suggested variables `Summary`"), columns = 1:3)
      ,
    error=function(e) NULL
    )
  })
  
  
    output$suggestedvars <- render_gt({
      id <- showNotification("Rendering PCA table", type="message", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      
      checkexistance <- try(res.pca())
      req(checkexistance)
      
    eig.val <- get_eigenvalue(res.pca())
    df <- round(eig.val,2) %>% as.data.frame()
    df$Dimension <- rownames(df)
    
    
    tryCatch(
    df %>% select(Dimension, everything()) %>%  gt() %>%
      tab_header(
        title = md("This is the `pca` result of the dataset"),
        subtitle = md("Top **5** Dimensions with `eigenvalue` more than `1` are used to identify the significant variables")
      )
    ,
    error=function(e) NULL
    )
  })
    
    
    output$corrplotcos <- renderPlot({
      id <- showNotification("Rendering PCA corrplot", type="message", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      
      checkexistance <- try(res.pca())
      req(checkexistance)
      var <- get_pca_var(res.pca())
      tryCatch(
        corrplot(var$cos2, is.corr=FALSE)
        ,
        error=function(e) NULL
      )
      
    }
    , width = function(){940},
    height=  function(){if(is.null(featureselectionvalues$numpcavar)){
      200
    } else {
      100*featureselectionvalues$numpcavar}
    },
    res = 96
    )
  
    
    output$pcavargraph <- renderPlot({
      id <- showNotification("Rendering PCA plot", type="message", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id), add = TRUE)
      
      checkexistance <- try(res.pca())
      req(checkexistance)
      
      tryCatch(
      fviz_pca_var(res.pca(), col.var = "cos2",
                   gradient.cols = c("blue", "yellow", "red"), 
                   repel = TRUE # Avoid text overlapping
                   )
      ,
      error=function(e) NULL
      )
      
    }
    , width = function(){680},
    height = function(){680},
    res = 96
      )
   
   
  
  # Kmeans Modeling
  
  #input filters
  output$uniqevar <- renderUI({
    selectInput("clustunique",
                label = div(icon("info-circle", style = "color:red;", fill =TRUE),
                            # add html and <br/> to bring second part to the next line
                            HTML(paste("The column that is going to be clustered", "(Values of this column should not be duplicated)", sep="<br/>"))
                            #"The column that is going to be clustered \n (Values of this column should not be duplicated)"
                            ),
                choices = c("", names(finaldata() %>%  select(where(is.character)) %>%  select(-any_of(columnstoberemoved())))),
                selectize = FALSE,
                selected = NULL,
                multiple = FALSE)
  })
  
  output$modelvars <- renderUI({
    selectInput("clustvars",
                "Clustering variables",
                choices = names(numbdata() %>%  select(-any_of(columnstoberemoved()))),
                selected = featureselectionvalues$tempimportantvalues,
                multiple = TRUE)
  })
  
  output$kmeanalgorithm <- renderUI({
    selectInput("algorithm",
                "The Kmeans algorithm to be used for clustering",
                choices = c("Hartigan-Wong", "Lloyd", "Forgy",
                            "MacQueen"),
                selected="Hartigan-Wong",
                multiple = FALSE)
  })
  
  output$iterationtimes <- renderUI({
    sliderInput("iterationmax",
                "Maximum number of iterations allowed",
                value = 6,
                min = 1,
                max = 100)
  }) 
  
  

  
  # function for scaling variables of clustering
  scale2 <- function(x, na.rm = TRUE) 
    (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)
  
  # Very important (to get the values wtithin the next chunck of code)
  clustervalues <- reactiveValues(midcluster = NULL, variablechosen=NULL,  originaldata=NULL, origincluster=NULL, resultofcluster=NULL, agumentedvalues=NULL, dimensiontest=NULL, updateddata=NULL)
  # Build the back end for button to run the table
  grandcluster <- eventReactive(input$runelbow,{
    id <- showNotification("Calculating data for Elbow graph. It might takes a little more", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    set.seed(27)
    getclustertable <- finaldata() %>%
      select(any_of(input$clustunique), any_of(input$clustvars)) %>%
      # to remove Na row otherwise it does not work
      drop_na(any_of(input$clustunique), any_of(input$clustvars))
    
    checkexistance <- try(getclustertable)
    req(checkexistance)  
    
    
    rownames(getclustertable) <- getclustertable$input$clustunique
    
    clustervalues$variablechosen <- getclustertable %>% select(any_of(input$clustvars)) %>% names()
    clustervalues$originaldata <- getclustertable
    
    # scaling
    
    fullclustertable <- getclustertable %>% 
      select(any_of(input$clustunique), any_of(input$clustvars)) %>%
      mutate(across(any_of(input$clustvars), scale2, .names = "scale_{col}")) 
    
    clustervalues$origincluster <- fullclustertable
    
    clustertable <-  fullclustertable %>%
      select(starts_with("scale") )
    
    clustervalues$midcluster <- clustertable
    
    kclusts <-  tibble(k = 1:input$maxelbowk) %>%
      mutate(
        kclust = map(k, ~kmeans( clustertable , .x, nstart = input$nstarts, iter.max=input$iterationmax, algorithm =input$algorithm)),
        tidied = map(kclust, tidy),
        glanced = map(kclust, glance)
      )
    
    
    tryCatch(
    kclusts,
    error=function(e) NULL
    )
  })
  # clusters <-   kclusts %>%
  #  unnest(cols = c(tidied))
  
  # assignments <- kclusts %>% 
  #   unnest(cols = c(augmented))
  
  silhouettetable <- reactive({
    id <- showNotification("Calculating data for Elbow graph.", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    checkexistance <- try(grandcluster())
    req(checkexistance)
    
    silhouettedata <- grandcluster()  %>%
      unnest(cols = c(glanced))
    
    tryCatch(
    silhouettedata,
    error=function(e) NULL
    )
  })
  
  
  # build the initial table
  output$silhouetteplot <-  renderPlot({
    id <- showNotification("Creating Silhouett Plot", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    ggplot(silhouettetable(), aes(k,  round(100*betweenss/(tot.withinss+betweenss),2) )) +
      geom_line(color = "darkcyan", size=1) +
      geom_point(color = "firebrick", size=5, alpha=0.5) +
      scale_x_continuous(breaks= pretty_breaks()) +
      scale_y_continuous(breaks= pretty_breaks()) +
      #ylim(NA, 1)+
      ylab("Variance Covered by the clusters")+
      xlab("K-Number of clusters-")+
      labs(title = "Elbow Graph",
             subtitle = "Variance within the clusters decreases as K increases Notice a bend in the graph and choose the corresponding K",
             caption = "Use chosen K in the next step")+
      geom_text(aes(label = round(100*betweenss/(tot.withinss+betweenss),2)),
                position = position_dodge(0.9),
                vjust = 2)+ 
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+ 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+
      theme(plot.title = element_text(size=22),
            plot.subtitle=element_text(size=16),
            plot.caption=element_text(size=16),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
    
  }
  , height = 650, res = 96
  )
  
  scaleback <- function(x, na.rm = TRUE) {
    # keep the columns that we want them to be scaled back
    temx <- x
    names(temx) = gsub(pattern = "scale_", replacement = "", x = names(temx))
    relatedcols <- temx %>% select(-c(size, withinss, cluster)) %>% names()
    average <- clustervalues$origincluster %>% summarise(across(relatedcols, mean, na.rm = TRUE))
    sd <- clustervalues$origincluster %>% summarise( across(relatedcols, sd, na.rm = TRUE))
# Add rows to the average vertically (rep function would add horizontally after we summarize the data)    
    avgrepeat <- average %>%  slice(rep(1:n(), each = dim(temx)[1])) %>% unlist() 
    sdrepeat <- sd %>%  slice(rep(1:n(), each = dim(temx)[1]))  %>% unlist()
    backed <- round( temx[,relatedcols]*sdrepeat+avgrepeat, 2)
    #average <- map(clustervalues$origincluster[,relatedcols], mean, na.rm = TRUE)
    # sd <- map(clustervalues$origincluster[,relatedcols], sd, na.rm = TRUE)
    # backed <- round( temx[,relatedcols]*sd+average, 2)
    # Add a function to rename(to Average) newly calculated(Scaled back) fields
    backed <- backed %>% rename_all(function(zzz){paste("AVG_of_",zzz)})
    
    bind_cols(temx,backed) %>% select(cluster, size, withinss, starts_with("AVG_of_")) %>%
    mutate(across(starts_with("AVG_of_"), ~ rank(-.x, ties.method="min"), .names="Rank_{col}")) %>%
    rename(Cluster = cluster)
  }
  
  
  
  
  augmentedclustertable <- eventReactive(input$runclust,{
    id <- showNotification("Creating K clusters", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    set.seed(27)
    basecluster <- kmeans(clustervalues$midcluster,
                          centers = input$k,
                          nstart = input$nstarts,
                          iter.max=input$iterationmax,
                          algorithm = input$algorithm)
    tidied <- tidy(basecluster)
    featureselectionvalues$temporarytidy <- tidied
    augm <- augment(basecluster, clustervalues$midcluster)
    # to use augm later in the plot
    clustervalues$resultofcluster  <-  augm %>%
      bind_cols(clustervalues$originaldata) %>%
      rename(Cluster=.cluster)
      
    clustervalues$agumentedvalues <- clustervalues$resultofcluster   %>%
      select(Cluster,
             any_of(input$clustvars))
  #           input$X,
  #           input$Y)
    # if we only have one variable then it change the column name to x1 and it makes problem so we make sure the names are correct
   # namingbase <- tidied %>% select(-c(size, withinss, cluster)) %>% names()
   # tidied <- if_else(length(input$clustvars)==1, tidied %>% rename(namingbase=X1), tidied) 
    #names(tidied)[starts_with("scale_")] <- names(clustervalues$midcluster)
    tidied %>% scaleback()
    # augm  %>%
    #   group_by(.cluster) %>%
    #   summarise(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE),2))) %>%
    #   inner_join(tidied, by = c(".cluster" = "cluster"))
  })
  
  output$summarycluster <- renderReactable({
    id <- showNotification("Rendering table of cluster summary", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    #augmentedclustertable()
    updateddata <- augmentedclustertable()
    names(updateddata) = gsub(pattern = "AVG_of_", replacement = "", x = names(updateddata))
    names(updateddata) = str_trim(names(updateddata))
    stylefunc <- function(value, index, name) {
      numberofrows <- dim(updateddata)[1]
      #colmax <- max(rank(-updateddata[name], ties.method = "min"))
      rankingofvalue <- percent_rank(-updateddata[name])[which(updateddata[name]==value)][1]
      # rank(-updateddata[name], ties.method = "min")[which(updateddata[name]==value)][1]
      list(background = if_else( rankingofvalue < 1/ 5,
                                 "#00CC00",
                                 if_else(rankingofvalue < 2*1/ 5,
                                         "#78B7C5",
                                         if_else(rankingofvalue < 3*1/ 5,
                                                 "#CCCCCC",
                                                 if_else(rankingofvalue < 4*1/ 5,
                                                         "#E1AF00",
                                                         if_else(rankingofvalue <= 5*1/ 5,
                                                                 "#F21A00",
                                                                 "White"
                                                         )
                                                 )
                                         )
                                 )
      )
      )
      
    }
    
    # list giving column formatting (using style function) for single column
    coldefs <- list(
      reactable::colDef(style = stylefunc)
    )
    
    # get names of numerical cols
    numcols <- input$clustvars
    #updateddata %>% dplyr::select(where(is.numeric)) %>% colnames()
    
    # replicate list to required length
    coldefs <- rep(coldefs,length(numcols))
    
    # name elements of list according to cols
    names(coldefs) <- numcols
    
    # create table
    updateddata <- updateddata%>%
      mutate(withinss= round(withinss,2))
    
    
    tryCatch(reactable(updateddata, columns = coldefs),
             error = function(e)
               NULL)
    
    
    #datatable(updateddata, options = list(pageLength = 10, scrollX=TRUE, scrollCollapse=TRUE))
    
  })
  
  clustervariables <- reactive({
    nameofvariable <- clustervalues$agumentedvalues %>%
      select(-input$clustunique, -.cluster, -starts_with("scale_")) 
  })
  
  
  output$xvar <- renderUI({
    selectInput(
      "X",
      "Choose X axis",
      clustervalues$variablechosen,
      selected =  names(clustervalues$variablechosen)[1],
      multiple = FALSE
    )
    
  } )
  
  output$yvar <- renderUI({
    selectInput(
      "Y",
      "Choose Y axis",
      clustervalues$variablechosen,
      #selectize = FALSE,
      selected = names(clustervalues$variablechosen)[2],
      multiple = FALSE
    )
    
  } )
  
  # output$testo <- renderDataTable({
  #   grandcluster()       %>%
  #    unnest(cols = c(glanced)) %>%
  #   select(k, tot.withinss)
  # })
  
  #drawclustgraph
  
  clusterplotdata <-  eventReactive(input$drawclustgraph,{
    id <- showNotification("Preparing Plot data", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    # updateddata <- tidyvalues$summarydata
    # names(updateddata) = gsub(pattern = "AVG_of_", replacement = "", x = names(updateddata))
    updateddata <- augmentedclustertable()
    # for the sake of mapping we should remove the AVG_OF_ so when we add geom_text it understands
    names(updateddata) = gsub(pattern = "AVG_of_", replacement = "", x = names(updateddata))
    # there is a space after we change the name so we should remove it for mapping
    names(updateddata) = str_trim(names(updateddata))
    clustervalues$updateddata <- updateddata
    # xvar <- input$X
    # yvar <- input$Y
    # clustervalues$agumentedvalues[[xvar]] <- with(clustervalues$agumentedvalues, input$X)
    # clustervalues$agumentedvalues[[yvar]] <- with(clustervalues$agumentedvalues, input$Y)
    clustervalues$agumentedvalues %>%
      select(Cluster, input$X, input$Y)
  })
  
  
  output$clusteringplot <-  renderPlot({
    id <- showNotification("Rendering Cluster Plot", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    # to increase the performance for big number of rows (getting sample for tables bigger than 20000)
    if (dim(clusterplotdata())[1] <= 30000) {
      plotfinaldata <- clusterplotdata()
    } else {
      table <-  clusterplotdata() %>% group_by(Cluster) %>% count() %>% ungroup()
      number <- clusterplotdata() %>%  distinct(Cluster)  %>% arrange(Cluster) %>% left_join(table)
      finalpointscount <- ceiling(30000*number$n/ sum(number$n))
      plotfinaldata <- clusterplotdata() %>% 
        group_split(Cluster) %>%
        map2_dfr(finalpointscount, ~ slice_sample(.x, n = .y))
    }
    
    second <-  if_else(dim(plotfinaldata)[2] == 2,  names(plotfinaldata)[2], names(plotfinaldata)[3])
    clustervalues$dimensiontest <- second
    ggplot(plotfinaldata, aes_string(names(plotfinaldata[2]), second, label= "Cluster"))+
      geom_point(aes(color=Cluster),alpha=0.2, size=2)+
      geom_smooth(aes(group=Cluster, color= Cluster), size=2, se = FALSE)+
      geom_text(data=clustervalues$updateddata, size=10)+
      
      
      geom_xsidedensity(
        aes(y=after_stat(density),
            fill= Cluster ),
        alpha=0.5,
        size=1,
        position="stack"
      )+
      geom_ysidedensity(
        aes(x=after_stat(density),
            fill= Cluster ),
        alpha=0.5,
        size=1,
        position="stack"
      )+
      switch(input$transplot,
             "XY" = list( scale_x_continuous(trans='log2') , scale_y_continuous(trans='log2')),
             "X" = scale_x_continuous(trans='log2'),
             "Y" = scale_y_continuous(trans='log2'),
             "base" = NULL) +
      scale_color_tq()+
      scale_fill_tq()+
      theme_tq()+
      theme(ggside.panel.scale.x=0.4,
            ggside.panel.scale.y=0.2,
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
  }
  , height=  function(){800}, res = 96
  )
 
  
  ### Ad-hoc report
  output$groupby <- renderUI({
    selectInput(
      "grouping",
      "Group BY",
      c("Cluster", names(finaldata() %>%  select(-any_of(columnstoberemoved())) )),
      selected =  "Cluster",
      multiple = TRUE,
      width='100%'
    )
    
  } )
  
  output$summarizecount <- renderUI({
    selectInput(
      "count",
      "Count Distinct",
      c(names(
        finaldata() %>% 
          select(-any_of(columnstoberemoved())) %>% 
          select(where(is.character), where(is.logical), where(is.factor))
        )
        ),
      selected =  NULL,
      multiple = TRUE,
      width = '100%'
    )
    
  } )
  
  output$summarize.mean.avg <- renderUI({
    selectInput(
      "sum",
      "Sum",
      c(names(numbdata())
      ),
      selected =  NULL,
      multiple = TRUE,
      width = '100%'
    )
    
  } )
  
  output$summarize.mean <- renderUI({
    selectInput(
      "avg",
      "Average",
      c(names(numbdata())
      ),
      selected =  NULL,
      multiple = TRUE,
      width = '100%'
    )
    
  } )
  
  output$summarize.sd <- renderUI({
    selectInput(
      "sd",
      "Standard Deviation",
      c(names(numbdata())
      ),
      selected =  NULL,
      multiple = TRUE,
      width = '100%'
    )
    
  } )
  
  
  reportvalues <- reactiveValues(reportedtable= NULL)
  
  reporttable <- eventReactive(input$runreport,{
    id <- showNotification("Preparing ad-hoc data", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    wholedata <- finaldata() %>%
      drop_na(any_of(input$clustunique), any_of(input$clustvars))
    
    clustersraw <- clustervalues$resultofcluster %>% 
      select(Cluster)
    
    checkexistance <- try(clustersraw)
    req(checkexistance)
    
    
   reportdata <- wholedata %>%
      bind_cols(clustersraw) %>%
      select(any_of(input$grouping), any_of(input$count), any_of(input$sum), any_of(input$avg), any_of(input$sd)) %>%
      group_by_at(input$grouping)  %>%
      summarise(count = n(),
        across(input$count, list( count_distinct = function(x){n_distinct(x)}    )),
        #across(input$sumavg, list(sum= function(x){round(sum(x),2)}, mean = function(x){round(mean(x),2)}, sd =  function(x){round(sd(x),2)}))
        across(input$sum, list(sum= function(x){round(sum(x, na.rm=TRUE),2)})),
        across(input$avg, list(mean = function(x){round(mean(x, na.rm=TRUE),2)})),
        across(input$sd, list(sd =  function(x){round(sd(x, na.rm=TRUE),2)}))
                )
   
   reportvalues$reportedtable <- reportdata
   
   tryCatch(reportdata,
            error=function(e)NULL)
    
    # count= function(x){tally(x)},
   # ,
   # across(input$sumavg, list(sum= function(x){round(sum(x),2)}, mean = function(x){round(mean(x),2)}, sd =  function(x){round(sd(x),2)}))
 })
  
  
  output$adhoctable <- renderDataTable({
    id <- showNotification("Render ad-hoc table", type="message", duration = NULL, closeButton = TRUE)
    on.exit(removeNotification(id), add = TRUE)
    
   
    tryCatch( 
  datatable(reporttable(), filter = list(position = 'top', clear = FALSE), options = list(scrollX = TRUE), editable = "all"    )
  , error=function(e)NULL)
    
  })
  
  
  

 
  
  output$downloadData <- downloadHandler(
       filename = function() {
         paste('data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(reporttable(), con)
       }
     )
  
  output$scatterx <- renderUI({
    selectInput(
      "scatx",
      "X axis",
      c("", tryCatch( names(reportvalues$reportedtable %>% select(where(is.numeric), where(is.integer))), error=function(e)NULL)),
      selected =  NULL,
      selectize = FALSE,
      multiple = FALSE,
      width = '100%'
    )
    
  } )
  
  output$scattery <- renderUI({
    selectInput(
      "scaty",
      "Y axis",
      c("", tryCatch( names(reportvalues$reportedtable %>% select(where(is.numeric), where(is.integer))), error=function(e)NULL)),
      selected =  NULL,
      selectize = FALSE,
      multiple = FALSE,
      width = '100%'
    )
    
  } )
  
  output$scattercolor <- renderUI({
    selectInput(
      "scatcolor",
      "Color",
      c("", tryCatch(names(reportvalues$reportedtable %>% select( where(is.factor))), error=function(e)NULL)),
      selected =  NULL,
      selectize = FALSE,
      multiple = FALSE,
      width='100%'
    )
    
  } )
  
  
  scatterplotdata <-  eventReactive(input$createscatter,{
    set.seed(27)
    id <- showNotification("preparing data for scatter Plot", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    datarownumber <- dim(reportvalues$reportedtable)[1]
    
    if (datarownumber <= 50000) {
      newdata <- reportvalues$reportedtable %>%
        ungroup() %>%
        select(input$scatx, input$scaty, any_of(input$scatcolor))
    } else {
      newdata <- 
        tryCatch( reportvalues$reportedtable %>%
                    ungroup()  %>% 
                    slice_sample(n= min(50000, datarownumber)) %>%
                    select(input$scatx, input$scaty, any_of(input$scatcolor)) ,
        error = function(e)NULL)
      
    }
    
    # newdata <- reportvalues$reportedtable %>%
    #   ungroup() %>%
    #  # mutate(across(where(is_character), as_factor)) %>%
    #   select(input$scatx, input$scaty, any_of(input$scatcolor))
    
    return(newdata)
  })
  
  output$scatterplot <-  renderPlot({
    id <- showNotification("Rendering scatter Plot", type="message", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    colnumb <- dim(scatterplotdata())[2]
    hascol <- colnumb==2
    finalcol <- if(hascol) NULL else names(scatterplotdata())[3]
    tryCatch(
      ggplot(scatterplotdata(), aes_string(names(scatterplotdata())[1], names(scatterplotdata())[2], color= finalcol )) +
      geom_point(alpha=0.4, size=3) +
      geom_smooth(aes_string(group=finalcol, color= finalcol), method=input$smoothmethod, size=1, se = input$seinclude)+
      switch(input$scattertransplot,
             "XY" = list( scale_x_continuous(trans='log2') , scale_y_continuous(trans='log2')),
             "X" = scale_x_continuous(trans='log2'),
             "Y" = scale_y_continuous(trans='log2'),
             "base" = NULL) +
      scale_color_tq()+
      scale_fill_tq()+
      theme_tq()+
      theme(legend.position="bottom",
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
    ,
    error=function(e)NULL)
  }
  , height=  function(){800}, res = 96
  )
  
  
  
 # output$shorttime <- renderPrint({
  #  scatterplotdata()
 # exclude_pca_many_in_one_value()
    #appexcludenumb()
    #checkfields()
    #estimatedfactor()
    # facto_summarize(
    #   res.pca(),
    #   "var",
    #   node.level = 1,
    #   group.names,
    #   result = c("contrib"),
    #   axes = 1:featureselectionvalues$sigdimnumb,
    #   select = NULL
    # )
    # var <- get_pca_var(res.pca())
    # var$coord
    # eig.val <- get_eigenvalue(res.pca())
    # eig.val <- data.frame(Component = row.names(eig.val),eig.val)
    # signigicantdim <- eig.val %>% dim() %>% nth(1)
    # eig.val
    #a <- featureselectionvalues$tempimportantvalues
    #a
    #rownames(a) <- NULL
    #first <- a["Ranked.Feature"][1]
    #listofvars <- a["name"] %>% nth(1)
    #first <- listofvars  %>% nth(1) %>% as.character()
    #second <- listofvars  %>% nth(2) %>% as.character()
    #d <- a %>% select(`Dont use together with:`) %>% str_split(`Dont use together with:`, ",")
    
    #str_split(featureselectionvalues$tempimportantvalues[,3][1], ", ", simplify = TRUE)
    #columnstoberemoved()
    #clustervalues$dimensiontest
    #clustervalues$agumentedvalues
    #clusterplotdata()
    #clustervalues$resultofcluster
    # augmentedclustertable()
    #featureselectionvalues$temporarytidy
 # })
 
  
}
# Run the application 
#if (interactive())
  shinyApp(ui = ui, server = server)

# Done Poxisct to datetime
# Done Default factors to 12  (Exlore)
# Done Add boxplot, table, area to date, numbers, histogram
# Done gini coefficient and mode (Also multiple mode)

# Done vertical factors
# Done Mark the max and min of clustering (Color band)


  # Done Data Import and preparation
  # Done EDA (Explo..)
  # Done (value:count) add to the factor table
  # Done One column per row for bar chart big name and bigger graph
  # Done Filter for columns chosen for factor bar chart
  # Done add button for the graphs
  # Done remove the mean from date
  # Done Add option to add or remove columns in 
  # Done add variables that are more significant for clustering
  
  # factor when there is 1 is very big graph
  
