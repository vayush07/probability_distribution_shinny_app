### Application name : CA1 B9DA101 - Statistics for Data Analytics 

### Course : MSc (Data Analytics) - Sep 2019 - Group <<A>>  

### Developed by : Ayush Varhadi (Student#10520671) / Kaif Siddiqui (Student#10520313) / Sadiya Patel (Student#10520312) / Saurabh Gupta (Student#10519384)  

### College : Dublin Business School  


# Use the shiny library
library(shiny)
# Use the pdfetch for yahoo finance
library(pdfetch)
library(MASS)

# Server part of the application
shinyServer(
  function(input, output, session) {
    
    ###>>> Begin >>> Ayush Varhadi(Student#10520671) >>>                                                                                                      
    
    # Plotting the graphical representation
    output$plot <- renderPlot({
        # Switch case for all the continuous probabilities models
        switch (input$modelCont,
                # For Uniform model
                'uniform' = {
                  # Storing a input
                  a <- input$a
                  # Storing b input
                  b <- input$b
                  # Storing s input
                  n1 <- input$s
                  # Generating the random data
                  rand.unif <- runif(n1, min = a, max = b)
                  # Plotting the hist graph
                  hist(rand.unif,
                       freq = FALSE,
                       xlab = 'x',
                       ylim = c(0, 0.7),
                       xlim = c(-3,3),
                       density = 35,
                       main = "Uniform distribution",
                       col = "blue")
                  curve(dunif(x, min = a, max = b),
                        from = -3, to = 3,
                        n = n1,
                        col = "darkred",
                        lwd = 2,
                        add = TRUE,
                        yaxt = "n",
                        ylab = 'probability')
                },
                
                # For Uniform model
                'normal' = {
                  # Generating the random data
                  x=seq(-input$i,input$i,0.01)
                  # Plotting the graph
                  plot(x,dnorm(x,input$mu,input$sigma),type='l', col='red')
                },
                
                # For Exponential model
                'exponential' = {
                  # Generating the random data
                  x=seq(0,input$i,0.01)
                  # Plotting the graph
                  plot(x,dexp(x,input$lam),type='l',col='green')
                },
                
                # For Gamma model
                'gamma' = {
                  # Generating the random data
                  x=seq(0,input$i,0.01)
                  # Plotting the graph
                  plot(x,dgamma(x,input$sigma,input$lam),type='l',col='blue')
                },
                
                # For Chi Squared model
                'chisquared' = {
                  # Generating the random data
                  x=seq(0,input$i,0.01)
                  # Plotting the graph
                  plot(x,dchisq(x,input$k),type='l',col='black')
                }
        )
    })
    
    # Tab to display the summary for each model
    output$summary <- renderPrint({
      
        # Switch case for all the continuous probabilities models
        switch (input$modelCont,
                # For Uniform model
                'uniform' = {
                  # Show the summary for random generated data
                  summary(runif(input$s,input$a, input$b))
                },
                
                # For Normal model
                'normal' = {
                  c(pnorm(input$s,input$mu, input$sigma))
                  # Show the summary for random generated data
                  summary(rnorm(input$s,input$mu, input$sigma))
                },
                
                # For Exponential model
                'exponential' = {
                  c(pexp(input$s,input$lam))
                  # Show the summary for random generated data
                  summary(rexp(input$s,input$lam))
                },
                
                # For Gamma model
                'gamma' = {
                  # Show the summary for random generated data
                  summary(rgamma(input$s,input$sigma,input$lam))
                },
                
                # For Chi Square model
                'chisquared' = {
                  # Show the summary for random generated data
                  summary(rchisq(input$s,input$k))
                }
        )
    })
    
    # Tab displayed the table of simulated data
    output$table <- renderTable({
      
        switch (input$modelCont,
                # For Uniform model
                'uniform' = {
                  # Collect the random generated data
                  c(runif(input$s,input$a, input$b))
                },
                
                # For Normal model
                'normal' = {
                  # Collect the random generated data
                  c(rnorm(input$s,input$mu, input$sigma))
                },
                
                # For Exponential model
                'exponential' = {
                  # Collect the random generated data
                  c(rexp(input$s,input$lam))
                },
                
                # For Gamma model
                'gamma' = {
                  # Collect the random generated data
                  c(rgamma(input$s,input$sigma,input$lam))
                },
                
                # For Chi Square model
                'chisquared' = {
                  # Collect the random generated data
                  c(rchisq(input$s,input$k))
                }
        )
    })
    
    ###<<< End   <<< Ayush Varhadi(Student#10520671) <<<  
    
    ###>>> Begin >>> Kaif Siddiqui(Student#110520313) >>>                                                                                                                      ###
    
    # Storing the dataset into myData
    myData <- reactive({
      # Switch case to the input type
      switch(input$predInputType,
             # For File input
             'preFile' = {
               # Storing hte input file in file1 variable
               file1 <- input$datafile
               # Return null if file1 is empty
               if (is.null(file1)) {
                 return()
               }
               # Read the CSV file and store in data
               data = read.csv(file=file1$datapath)
               data
             },
             # For Inbuild input
             'preInbuild' = {
               # get the inbuild dataset and store in data
               data = data.frame(get(input$preInbuildFile))
               data
             },
             # For URL input
             'preUrl' = {
               # Read the dataset from URL
               data = read.csv(input$preUrl)
               data
             },
             # For Yahoo Finance input
             'preYahoo' = {
               # Set from date to one year from now
               fromDate = Sys.Date() - 1*365;
               # Get output from Yahoo api
               out = pdfetch_YAHOO(input$preYahoo, fields = c("open", "high", "low", "close", "adjclose", "volume"), from = fromDate)
               # Convert it into dataframe
               stockData = data.frame(out)
               
               # Get the name of column name
               tick_open <- paste(input$preYahoo, sep = "", ".open")
               tick_high <- paste(input$preYahoo, sep = "", ".high")
               tick_low <- paste(input$preYahoo, sep = "", ".low")
               tick_volume <- paste(input$preYahoo, sep = "", ".volume")
               tick_adjclose <- paste(input$preYahoo, sep = "", ".adjclose")
               tick_close <- paste(input$preYahoo, sep = "", ".close")
               
               # Renaming Columns
               names(stockData)[names(stockData) == tick_open] <- "Open"
               names(stockData)[names(stockData) == tick_high] <- "High"
               names(stockData)[names(stockData) == tick_low] <- "Low"
               names(stockData)[names(stockData) == tick_volume] <- "Volumn"
               names(stockData)[names(stockData) == tick_adjclose] <- "Adjclose"
               names(stockData)[names(stockData) == tick_close] <- "Close"
               
               # omit the empty value and store in data variable
               data = na.omit(stockData)
               data
             }
      )
      
    })
    
    # Observe the change in myData and update the columns input
    observe({
      # Update the columns input
      updateSelectInput(session, inputId = "pred_columns", choices = colnames(myData()))
    })
    
    # Render the data with datatable
    output$extdata = DT::renderDataTable({
      # Get the data
      extdata <- myData()
      # Show the data in Datatable
      DT::datatable(extdata, options = list(lengthChange = TRUE))
    })
    
    # Tab for show the prediction 
    output$prediction <- renderPrint({
      
      # Show the column name that user have selected
      print(paste('Selected Column : ',input$pred_columns))
      # Store the data in df
      df <- myData()
      # Get the selected column from the dataset
      x <- df[,input$pred_columns]
      
      # switch case for probability models
      switch (input$predmodel,
              
              # For Uniform
              'uniform' = {
                # Display predicted value based on the random generated values
                print(paste('Predicted Value : ', mean(rnorm(input$s))))
              },
              
              # For Normal
              'normal' = {
                # Display predicted value based on the random generated values
                print(paste('Predicted Value : ', mean(rnorm(input$s, mean(x), sd(x)))))
              },
              
              # For Exponential
              'exponential' = {
                # Display predicted value based on the random generated values
                print(paste('Predicted Value : ', mean(rexp(input$s, 1/mean(x)))))
              }
      )
    })
    
    ###<<< End   <<< Kaif Siddiqui(Student#110520313) <<<  
    
    ###<<< Begin   <<< Sadiya Patel(Student#10520312) <<<                                                                                                        ###
    
    
    # Storing the dataset into hpData
    hpData <- reactive({
      # Switch case to the input type
      switch(input$hpInputType,
             # For File input
             'hpFile' = {
               # Storing hte input file in file1 variable
               file1 <- input$hpDatafile
               # Return null if file1 is empty
               if (is.null(file1)) {
                 return()
               }
               # Read the CSV file and store in data
               data = read.csv(file=file1$datapath)
               data
             },
             # For Inbuild input
             'hpInbuild' = {
               # get the inbuild dataset and store in data
               data = data.frame(get(input$hpInbuildFile))
               data
             },
             # For URL input
             'hpUrl' = {
               # Read the dataset from URL
               data = read.csv(input$hpUrl)
               data
             },
             # For Yahoo Finance input
             'hpYahoo' = {
               # Set from date to one year from now
               fromDate = Sys.Date() - 1*365;
               # Get output from Yahoo api
               out = pdfetch_YAHOO(input$hpYahoo, fields = c("open", "high", "low", "close", "adjclose", "volume"), from = fromDate)
               # Convert it into dataframe
               stockData = data.frame(out)
               
               # Get the name of column name
               tick_open <- paste(input$hpYahoo, sep = "", ".open")
               tick_high <- paste(input$hpYahoo, sep = "", ".high")
               tick_low <- paste(input$hpYahoo, sep = "", ".low")
               tick_volume <- paste(input$hpYahoo, sep = "", ".volume")
               tick_adjclose <- paste(input$preYahoo, sep = "", ".adjclose")
               tick_close <- paste(input$hpYahoo, sep = "", ".close")
               
               # Renaming Columns
               names(stockData)[names(stockData) == tick_open] <- "Open"
               names(stockData)[names(stockData) == tick_high] <- "High"
               names(stockData)[names(stockData) == tick_low] <- "Low"
               names(stockData)[names(stockData) == tick_volume] <- "Volumn"
               names(stockData)[names(stockData) == tick_adjclose] <- "Adjclose"
               names(stockData)[names(stockData) == tick_close] <- "Close"
               
               # omit the empty value and store in data variable
               data = na.omit(stockData)
               data
             }
      )
      
    })
    
    # Observe the change in myData and update the columns input
    observe({
      # Update the columns input
      updateSelectInput(session, inputId = "hp_columns", choices = colnames(hpData()))
    })
    
    # Render the data with datatable
    output$hpextdata = DT::renderDataTable({
      # Get the data
      extdata <- hpData()
      # Show the data in Datatable
      DT::datatable(extdata, options = list(lengthChange = TRUE))
    })
    
    # Tab for the output and print the result
    output$testResult <- renderPrint({
      
      # Get the data
      df <- hpData()
      # Get the selected column in X variable
      x <- df[,input$hp_columns]
      # Get the selected column in Y variable
      y <- df[,input$hp_columns_y]
      
      # Default set it to zero
      mu = 1
      if(input$hpalternative == 'two.sided'){
        mu = input$hpMu
      }
      
      # Run the test by t.test function
      test = t.test(x=x, mu=mu, alternative=input$hpalternative)
      
      # Store the result in decision according the p.value
      if(test$p.value < input$hpAlpha){
        decision='Reject H_0'
      }else{
        decision='Accept H_0'
      }
      
      # Display the result
      print(paste('Decision: ', decision))
      # If the alternative is two sided
      if(input$hpalternative == 'two.sided'){
        # Calculate the lower limit
        L=mean(x)-abs(qnorm(input$hpAlpha/2))*sd(x)/sqrt(length(x))
        # Calculate the upper limit
        U=mean(x)+abs(qnorm(input$hpAlpha/2))*sd(x)/sqrt(length(x))
        # Display the lower limit
        print(paste('Lower Limit : ', L))
        # Display the upper limit
        print(paste('Higher Limit : ', U))
      }
    })
    
    ###<<< End   <<< Sadiya Patel(Student#10520312) <<<
    
    ###<<< Begin   <<< Saurabh Gupta(Student#10519384) <<<                                                                                                                     ###
    
    # Set RMSE to 0
    glmRMSE <- 0
    
    glmValues <- reactiveValues()
    
    # Storing the dataset into glmData
    glmData <- reactive({
      # Switch case to the input type
      switch(input$glmInputType,
             # For File input
             'glmFile' = {
               # Storing hte input file in file1 variable
               file1 <- input$glmDatafile
               # Return null if file1 is empty
               if (is.null(file1)) {
                 return()
               }
               # Read the CSV file and store in data
               data = read.csv(file=file1$datapath)
               data
             },
             # For Inbuild input
             'glmInbuild' = {
               # get the inbuild dataset and store in data
               data = data.frame(get(input$glmInbuildFile))
               data
             },
             # For URL input
             'glmUrl' = {
               # Read the dataset from URL
               data = read.csv(input$glmUrl)
               data
             },
             # For Yahoo Finance input
             'glmYahoo' = {
               # Set from date to one year from now
               fromDate = Sys.Date() - 1*365;
               # Get output from Yahoo api
               out = pdfetch_YAHOO(input$glmYahoo, fields = c("open", "high", "low", "close", "adjclose", "volume"), from = fromDate)
               # Convert it into dataframe
               stockData = data.frame(out)
               
               # Get the name of column name
               tick_open <- paste(input$glmYahoo, sep = "", ".open")
               tick_high <- paste(input$glmYahoo, sep = "", ".high")
               tick_low <- paste(input$glmYahoo, sep = "", ".low")
               tick_volume <- paste(input$glmYahoo, sep = "", ".volume")
               tick_adjclose <- paste(input$preYahoo, sep = "", ".adjclose")
               tick_close <- paste(input$glmYahoo, sep = "", ".close")
               
               # Renaming Columns
               names(stockData)[names(stockData) == tick_open] <- "Open"
               names(stockData)[names(stockData) == tick_high] <- "High"
               names(stockData)[names(stockData) == tick_low] <- "Low"
               names(stockData)[names(stockData) == tick_volume] <- "Volumn"
               names(stockData)[names(stockData) == tick_adjclose] <- "Adjclose"
               names(stockData)[names(stockData) == tick_close] <- "Close"
               
               # omit the empty value and store in data variable
               data = na.omit(stockData)
               data
             }
      )
      
    })
    
    # Observe the change in myData and update the columns input
    observe({
      # Update the columns input
      updateSelectInput(session, inputId = "glm_target", choices = colnames(glmData()))
      updateSelectInput(session, inputId = "glm_independent", choices = colnames(glmData()))
    })
    
    # Render the data with datatable
    output$glmextdata = DT::renderDataTable({
      # Get the data
      extdata <- glmData()
      # Show the data in Datatable
      DT::datatable(extdata, options = list(lengthChange = TRUE))
    })
    
    # For plotting the actual and predicted value
    output$glmPlot <- renderPlot({
      # Creating the data frame of dataset
      df = na.omit(glmData())
      # using the cbind function on both target and independent variable
      tarIndData <- cbind(df[,input$glm_target], df[,input$glm_independent])
      # collecting the column names of the data frame
      colnames(tarIndData) = c(input$glm_target, input$glm_independent)
      # Assigning Y to second column
      colnames(tarIndData)[1] = 'Y'
      
      # Taking seed as 199
      set.seed(199)
      # Storing the number of rows of traIndData in n
      n = nrow(tarIndData)
      # Creating the indexes as per user input
      indexs = sample(n, n * (input$glmRatio/100))
      # Making hte train set
      trainSet = na.omit(data.frame(tarIndData[indexs,]))
      # Making hte test set
      testSet = na.omit(data.frame(tarIndData[-indexs,]))
      # Assigning the actual set
      actual = testSet$Y
      
      # Creating the data frame of test data
      pred_test = data.frame(testSet)
      
      # Calculate the GLM on train set with Gaussian family
      fullModel = glm(Y ~ ., data = trainSet, family = "gaussian")
      # Storing the full model in global variable in glmValues
      glmValues$full <- fullModel
      
      # Predicting the full model
      predFull = predict(fullModel, testSet[,input$glm_independent])
      rmseFull = sqrt(sum((predFull - actual)^2)/(nrow(testSet)))
      
      # Calculating the reduced model
      reduced.model = stepAIC(fullModel)
      # Storing the full model in global variable in glmValues
      glmValues$full <- fullModel
      # Storing the reduced model in global variable in glmValues
      glmValues$reduced <- reduced.model
      
      # Predicting the reduced model
      pred_red = predict(reduced.model, testSet[,input$glm_independent])
      # Finding the RMSE of reduced model
      rmse_red = sqrt(sum((pred_red - actual)^2)/(nrow(testSet)))
      
      # Storing the rmse in global variable in glmValues
      glmValues$rmse <- data.frame('Full' = rmseFull, 'Reduced' = rmse_red)
      
      # Dividing the page into 2 columns
      par(mfrow = c(1,2))
      # plotting the first graph
      plot(actual, type = "o", col = "red", xlab="observations", ylab=input$glm_target, main="Full")
      # plotting the lines for first graph
      lines(predFull, type = "o", col = "blue")
      # writing the legend for first graph
      legend(
        "topleft",
        lty=c(1,1),
        col=c("red","blue"),
        legend=c("Real","Predicted")
      )
      
      # plotting the second graph
      plot(actual, type = "o", col = "red", xlab="observations", ylab=input$glm_target, main="Reduced")
      # plotting the lines for Second graph
      lines(predFull, type = "o", col = "blue")
      # writing the legend for second graph
      legend(
        "topleft",
        lty=c(1,1),
        col=c("red","blue"),
        legend=c("Real","Predicted")
      )
    })
    
    # Datatable for selected columns
    output$glmSelectedData <- DT::renderDataTable({
      df <- glmData()
      tarIndData <- cbind(df[,input$glm_target],df[,input$glm_independent])
      colnames(tarIndData) = c(input$glm_target,input$glm_independent)
      
      DT::datatable(tarIndData,options = list(lengthChange = TRUE))
    })
    
    # Displaying the RMSE
    output$glmRMSE <- DT::renderDataTable({
      DT::datatable(glmValues$rmse,options=list(lengthChange=TRUE))
    })
    
    # Reactive forecast for prediction
    forecast_out <- reactive({
      Var_Count <- length(input$glm_independent)
      new_data <- as.numeric(paste(lapply(1:Var_Count,function(i){
        inputName <- paste0(input$glm_independent[i])
        input[[inputName]]
      })))
      input_data <- na.omit(data.frame(t(new_data)))
      
      for (i in 1:Var_Count)
      {
        colnames(input_data)[i] <- input$glm_independent[i]
      }
      
      new_predict_full <- predict(glmValues$full,input_data)
      new_predict_red <- predict(glmValues$reduced,input_data)
      
      pred_data_new <- na.omit(data.frame(new_predict_full,new_predict_red))
      
      colnames(pred_data_new)[1] <- paste('Full Mode -', input$glm_target)
      colnames(pred_data_new)[2] <- paste('Reduced Mode -', input$glm_target)
      
      return(pred_data_new)
    })
    
    # Rendering the prediction
    output$glmPrediction <- DT::renderDataTable({
      DT::datatable(forecast_out(),options=list(lengthChange = TRUE))
    })
  }
)

###<<< End   <<< Saurabh Gupta(Student#10519384) <<<  