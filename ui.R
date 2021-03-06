### Application name : CA1 B9DA101 - Statistics for Data Analytics 

### Course : MSc (Data Analytics) - Sep 2019 - Group <<A>>  

### Developed by : Ayush Varhadi (Student#10520671) / Kaif Siddiqui (Student#10520313) / Sadiya Patel (Student#10520312) / Saurabh Gupta (Student#10519384)  

### College : Dublin Business School  


# Use the shiny library
library(shiny)
# Use the markdown library
library(markdown)

# Created the Tabs for each section like Random Variable, Prediction, Hypothesis test, GLM linear regression and About us
navbarPage("Home",
           
           ###>>> Begin >>> Ayush Varhadi(Student#10520671) >>>  
           
           # First Tab Random Variable
           tabPanel("Random  Variable",
                    # Header of this tab
                    headerPanel("Distribution of Continous Random Variables"),
                    # Side bar for Random Variable panel
                    sidebarLayout(
                      sidebarPanel(
                          
                          # Select input for all the Continuous models
                          selectInput("modelCont", "Select Model",
                                      choices = c(
                                        "Uniform" = "uniform",
                                        "Normal" = "normal",
                                        "Exponential" = "exponential",
                                        "Gamma" = "gamma",
                                        "Chi-squared" = "chisquared"
                                      ),
                                      selected = "uniform"
                          ),
                          
                          # Slider input for Number of simulated data
                          sliderInput("s", "Number of simulated data" ,min=1, max=1000, value = 10),
                          # Numeric input for the support
                          numericInput("i", "Support" , value = 2),
                          
                          # Conditional panel for inputs of uniform
                          conditionalPanel(
                            condition = "input.modelCont == 'uniform'",
                            # Numeric input for the Parameter a
                            numericInput("a", "Parameter a in Uniform" , value = -2),
                            # Numeric input for the Parameter b
                            numericInput("b", "Parameter b in Uniform" , value = 0.8)
                          ),
                          
                          # Conditional panel for inputs of normal
                          conditionalPanel(
                            condition = "input.modelCont == 'normal'",
                            # Numeric input for the Parameter mu
                            numericInput("mu", "Parameter mu in Normal" , value = 0),
                            # Numeric input for the Parameter sigma
                            numericInput("sigma", "Parameter sigma in Normal" , value = 1)
                          ),
                          
                          # Conditional panel for inputs of exponential
                          conditionalPanel(
                            condition = "input.modelCont == 'exponential'",
                            # Numeric input for the Parameter lambda
                            numericInput("lam", "Parameter lambda in exponential" , value = 1)
                          ),
                          
                          # Conditional panel for inputs of gamma
                          conditionalPanel(
                            condition = "input.modelCont == 'gamma'",
                            # Numeric input for the Parameter sigma
                            numericInput("sigma", "Parameter sigma in Gamma" , value = 1),
                            # Numeric input for the Parameter lambda
                            numericInput("lam", "Parameter lambda in Gamma" , value = 1)
                          ),
                          
                          # Conditional panel for inputs of chisquared
                          conditionalPanel(
                            condition = "input.modelCont == 'chisquared'",
                            # Numeric input for the Parameter K
                            numericInput("k", "Parameter K in chisquared" , value = 1)
                          )
                      ),
                      # Main panel for Random Variable which shows the output
                      mainPanel(
                        # Tabs in the main panel
                        tabsetPanel(type = "tabs",
                                    # Plot tab which displays the graph
                                    tabPanel("Plot", plotOutput("plot")),
                                    # Tab which displays the summary
                                    tabPanel("Summary", verbatimTextOutput("summary")),
                                    # Tab which displays the table of simulated data
                                    tabPanel("Table", tableOutput("table"))
                        )
                      )
                    )
           ),
           
           ###<<< End   <<< Ayush Varhadi(Student#10520671) <<<  
           
           ###>>> Begin >>> Kaif Siddiqui(Student#110520313) >>>                                                                                                                      ###
           
           # Tab for Prediction
           tabPanel("Prediction",
                    # Header for this tab
                    headerPanel("Prediction next value"),
                    # Sider bar which take the input
                    sidebarLayout(
                      sidebarPanel(
                        # Select for the probabilities models
                        # Default bernoulli  is selected
                        selectInput("predmodel", "Select Model",
                                    choices = c(
                                      "Uniform" = "uniform",
                                      "Normal" = "normal",
                                      "Exponential" = "exponential"
                                    ),
                                    selected = "bernoulli"
                        ),
                        
                        # Dropdown to select the input type.
                        # Default is File input
                        selectInput("predInputType", "Select Input",
                                    choices = c(
                                      "File Input" = "preFile",
                                      "Inbuild Datasets" = "preInbuild",
                                      "URL" = "preUrl",
                                      "Yahoo Finance" = "preYahoo"
                                    ),
                                    selected = "preFile"
                        ),
                        
                        # Conditional Panel to take parameter for file input
                        conditionalPanel(
                          condition = "input.predInputType == 'preFile'",
                          # Input: Select a file ----
                          fileInput("datafile", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv"))
                        ),
                        
                        # Conditional Panel to take parameter for in build dataset input
                        conditionalPanel(
                          condition = "input.predInputType == 'preInbuild'",
                          # Drop down of all the input dataset present in R
                          selectInput("preInbuildFile", "Select a Dataset",
                                      choices = ls("package:datasets")
                          )
                        ),
                        
                        # Conditional Panel to take parameter for in URL input
                        conditionalPanel(
                          condition = "input.predInputType == 'preUrl'",
                          # Text input to take data url from User
                          textInput('preUrl', 'Input URL', value="http://users.stat.ufl.edu/~winner/data/crop_circle.csv")
                        ),
                        
                        # Conditional Panel to take parameter for in Yahoo Finance input
                        conditionalPanel(
                          condition = "input.predInputType == 'preYahoo'",
                          # Text input to take Ticket Number from User
                          textInput('preYahoo', 'Enter Ticket No', value="CTSH")
                        ),
                        
                        # Drop down to select column of selected dataset
                        selectInput(inputId = "pred_columns", label = "Select a Column", choices = ""),
                        
                        # Slider to select the use of Number of simulated data for prediction
                        sliderInput("s", "Number of simulated data" ,min=1, max=1000, value = 100)
                      ),
                      
                      # Mail panel for this tab
                      mainPanel(
                        # More tabs to display more information
                        tabsetPanel(type = "tabs",
                                    # Tab that will shoW the data set in Data Table
                                    tabPanel("Data", DT::dataTableOutput('extdata')),
                                    # Tab that will show the prediction
                                    tabPanel("Prediction", verbatimTextOutput("prediction"))
                        )
                      )
                    )
           ),
           
           ###<<< End   <<< Kaif Siddiqui(Student#110520313) <<<  
           
           ###<<< Begin   <<< Sadiya Patel(Student#10520312) <<<                                                                                                        ###
           
           
           # Tab for Hypothesis Test
           tabPanel("Hypothesis Test",
                    # Header for the tab
                    headerPanel("Hypothesis testing - Mean of population"),
                    # Sidebar for the tab
                    sidebarLayout(
                      sidebarPanel(
                        # Dropdown to select the input type.
                        # Default is File input
                        selectInput("hpInputType", "Select Input",
                                    choices = c(
                                      "File Input" = "hpFile",
                                      "Inbuild Datasets" = "hpInbuild",
                                      "URL" = "hpUrl",
                                      "Yahoo Finance" = "hpYahoo"
                                    ),
                                    selected = "hpFile"
                        ),
                        
                        # Conditional Panel to take parameter for file input
                        conditionalPanel(
                          condition = "input.hpInputType == 'hpFile'",
                          # Input: Select a file ----
                          fileInput("hpDatafile", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv"))
                        ),
                        
                        # Conditional Panel to take parameter for in build input
                        conditionalPanel(
                          condition = "input.hpInputType == 'hpInbuild'",
                          # Dropdown to select the listed inbuild datasets
                          selectInput("hpInbuildFile", "Select a Dataset",
                                      choices = ls("package:datasets")
                          )
                        ),
                        
                        # Conditional Panel to take parameter for in build input
                        conditionalPanel(
                          condition = "input.hpInputType == 'hpUrl'",
                          # Text input to take url from user
                          textInput('hpUrl', 'Input URL', value="http://users.stat.ufl.edu/~winner/data/crop_circle.csv")
                        ),
                        
                        # Conditional Panel to take parameter for in Yahoo Finance input
                        conditionalPanel(
                          condition = "input.hpInputType == 'hpYahoo'",
                          # Text input to take ticket no from user
                          textInput('hpYahoo', 'Enter Ticket No', value="CTSH")
                        ),
                        
                        # Dropdown to select the column from listed columns
                        selectInput(inputId = "hp_columns", label = "Select a Column", choices = ""),
                        
                        # Slider to select Significance Level
                        sliderInput("hpAlpha", "Significance Level" ,min=0, max=1, value = 0.05, step=0.01),
                        
                        # Dropdown to select from Alternatives
                        selectInput("hpalternative", "Select Alternative",
                                    choices = c(
                                      "Lower Tail" = "less",
                                      "Upper Tail" = "greater",
                                      "Two Sided" = "two.sided"
                                    ),
                                    selected = "less"
                        ),
                        
                        # Condition for alternative is two sided
                        conditionalPanel(
                          condition = "input.hpalternative == 'two.sided'",
                          # Numeric input to take mu parameter
                          numericInput("hpMu", "Parameter mu in for Two sided" , value = 0),
                        )
                      ),
                      
                      # Main panel of the tab
                      mainPanel(
                        # More tabs for more info
                        tabsetPanel(type = "tabs",
                                    # Tab to display datatable
                                    tabPanel("Data", DT::dataTableOutput('hpextdata')),
                                    # Tab to display test result
                                    tabPanel("Result", verbatimTextOutput("testResult"))
                        )
                      )
                    )
           ),
           ###<<< End   <<< Sadiya Patel(Student#10520312) <<<

                      
           ###<<< Begin   <<< Saurabh Gupta(Student#10519384) <<<                                                                                                                     ###
           
           # Tab for GLM
           tabPanel("GLM",
                    # Header for the tab
                    headerPanel("GLM - Linear Regression"),
                    # Sidebar for the tab
                    sidebarLayout(
                      sidebarPanel(
                        # Dropdown to select the input type.
                        # Default is File input
                        selectInput("glmInputType", "Select Input",
                                    choices = c(
                                      "File Input" = "glmFile",
                                      "Inbuild Datasets" = "glmInbuild",
                                      "URL" = "glmUrl",
                                      "Yahoo Finance" = "glmYahoo"
                                    ),
                                    selected = "glmFile"
                        ),
                        
                        # Conditional Panel to take parameter for file input
                        conditionalPanel(
                          condition = "input.glmInputType == 'glmFile'",
                          # Input: Select a file ----
                          fileInput("glmDatafile", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv"))
                        ),
                        
                        # Conditional Panel to take parameter for in build input
                        conditionalPanel(
                          condition = "input.glmInputType == 'glmInbuild'",
                          # Dropdown to select the listed inbuild datasets
                          selectInput("glmInbuildFile", "Select a Dataset",
                                      choices = ls("package:datasets")
                          )
                        ),
                        
                        # Conditional Panel to take parameter for in build input
                        conditionalPanel(
                          condition = "input.glmInputType == 'glmUrl'",
                          # Text input to take url from user
                          textInput('glmUrl', 'Input URL', value="http://users.stat.ufl.edu/~winner/data/crop_circle.csv")
                        ),
                        
                        # Conditional Panel to take parameter for in Yahoo Finance input
                        conditionalPanel(
                          condition = "input.glmInputType == 'glmYahoo'",
                          # Text input to take ticket no from user
                          textInput('glmYahoo', 'Enter Ticket No', value="CTSH")
                        ),
                        
                        # Dropdown to select the column from listed columns
                        selectInput(inputId = "glm_target", label = "Select a Target Variable", choices = ""),
                        
                        # Dropdown to select the column for independent variable
                        selectInput(inputId = "glm_independent", multiple = TRUE, label = "Select the independent variables", choices = ""),
                        
                        # Slider input to take the ratio of train set
                        sliderInput('glmRatio', "Ratio for trainset", min=1, max=100, value=70),
                      ),
                      
                      # Main panel of the tab
                      mainPanel(
                        # More tabs for more info
                        tabsetPanel(type = "tabs",
                                    # Tab to display datatable
                                    tabPanel("Data", DT::dataTableOutput('glmextdata')),
                                    # Tab to display selected colums in datatable
                                    tabPanel("Selected Data", DT::dataTableOutput('glmSelectedData')),
                                    # Tab to display graph of actual and predicted data
                                    tabPanel("Test/Prediction", plotOutput('glmPlot')),
                                    # Tab to display graph of full and reduced
                                    tabPanel("RMSE", DT::dataTableOutput("glmRMSE")),
                                    # Tab to display the prediction,
                                    tabPanel("Prediction", DT::dataTableOutput('glmPrediction'))
                        )
                      )
                    )
           )
)
###<<< End   <<< Saurabh Gupta(Student#10519384) <<<  


# ))