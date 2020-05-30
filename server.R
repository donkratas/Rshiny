#libs
library(rpart.plot)
library(RColorBrewer)
library(ggplot2)
library(e1071)
library(caret)
library(plotly)
library(car)
library(factoextra)
library(rattle)
library(randomForest)


# Help funkctions ----
source("helpers.R")

# Main ----
shinyServer(function(input, output) {
  
  # Decision Trees ----
  output$table_for_dt <- DT::renderDataTable({
    DT::datatable(train_msc, options = list(orderClasses = TRUE))
  })
  
  output$classvar <- renderText({
    print(paste("Bandome nuspeti kintamaji: is atributed"))
  })
  output$classvara <- renderText({
    print(paste("Bandome nuspeti kintamaji: is atributed"))
  })
  
  output$depth <- renderUI({
    sliderInput("depth","Pasirinkite medzio gyli:",10,
                min = 1, max = 30, width='400px')
  })
  
  output$split <- renderUI({
    sliderInput("split","Pasirinkite padalijimu skaiciu", 1,
                min = 1, max = 50, step=1, width='400px')
  })
  
  output$number_of_folds_dt <- renderUI({
    sliderInput("number_of_folds_dt","Pasirinkite kryzmines validacijos daliu skaiciu",5,
                min = 5, max = 10, width='400px')
  })
  
  output$cv_repeats_dt <- renderUI({
    sliderInput("cv_repeats_dt","Pasirinkite kryzmines validacijos pakartojimu skaiciu",1,
                min = 1, max = 3, width='400px')
  })
  
  # Build the decision tree 
  observeEvent(input$action, {
    
    my_tree_one <- caret::train(is_attributed~.,
                                data = train_msc,
                                method = 'rpart',
                                minsplit=input$split,
                                maxdepth=input$depth,
                                trControl = trainControl(method = "repeatedcv",
                                                         number = input$number_of_folds_dt,
                                                         repeats = input$cv_repeats_dt))
    
    
    my_prediction <- predict(my_tree_one, newdata = test_msc)
    compare <- table(test_msc[["is_attributed"]], my_prediction)
    accuracy <- sum(diag(compare)) / sum(compare)
    full <- test_msc[,-6]
    full$prediction = my_prediction
    #calculating roc messure
    ROC_dt <- roc.curve(test_msc$is_attributed, my_prediction)
    
    
    
    #Rezultatams paskutiniame tab'e - confusion matrix
    output$pred_dt_rez <- renderPrint({
      names(dimnames(compare)) <- list("", "Klasifikacine lentele")
      compare
      
    })
    #Rezultatams paskutiniame tab'e - auc, and accuracy
    output$ROC_dt_rez <- renderPrint({
      print(paste('Tikslumas', round(accuracy,3)))
      print(paste('AUC ivertis', round(ROC_dt$auc,3)))
    })
    
    # Build the decision tree
    output$plot <- renderPlot({
      fancyRpartPlot(my_tree_one$finalModel)
    })
    
    # Predictions on the test set
    output$pred <- renderPrint({
      names(dimnames(compare)) <- list("", "Klasifikacine lentele") 
      compare 
    })
    
    # Accuracy on the test set
    output$accur <- renderPrint({
      print(paste('Modelio tikslumas', round(accuracy,3)))
    })
    
    output$AUC_dt <- renderPrint({
      print(paste('AUC ivertis', round(ROC_dt$auc,3)))
    })
    
    
    # Prediction results
    output$table2 <- DT::renderDataTable({
      DT::datatable(full, options = list(orderClasses = TRUE))
    })
    
  })
  
  #  -----------------------------Random forest------------
  
  output$table_for_rf <- DT::renderDataTable({
    DT::datatable(train_msc, options = list(orderClasses = TRUE))
  })
  
  output$number_of_folds <- renderUI({
    sliderInput("number_of_folds","Pasirinkite į kiek daliu dalinti duomenu aibe kryzminei validacijai",5,
                min = 5, max = 10, width='400px')
  })
  
  output$cv_repeats <- renderUI({
    sliderInput("cv_repeats","Pasirinkite kryzmines validacijos pakartojimu skaiciu",1,
                min = 1, max = 3, width='400px')
  })
  
  # output$tuneLength <- renderUI({
  #   sliderInput("tuneLength","Enter number of tuneLength:",20,
  #               min = 20, max = 30, width='400px')
  # })
  output$ntree <- renderUI({
    sliderInput("ntree","Pasirinkite medziu kieki:",10,
                min = 10, max = 2000,step=50, width='400px')
  })
  output$mtry <- renderUI({
    sliderInput("mtry","Pasirinkite kintamuju skaiciu padalijimams rasti kieki:",2,
                min = 2, max = 6,step=2, width='400px')
  })
  
  observeEvent(input$action_rf, {
    
    
    rf_model <- caret::train(is_attributed~.,
                             data = train_msc,
                             method = 'rf',
                             ntree = input$ntree,
                             trControl = trainControl(method = "repeatedcv",
                                                      number = input$number_of_folds,
                                                      repeats = input$cv_repeats),
                             tuneGrid = expand.grid(mtry = input$mtry)
    )
    
    my_prediction_rf <- predict(rf_model, newdata = test_msc)
    compare_rf <- table(test_msc[["is_attributed"]], my_prediction_rf)
    accuracy_rf <- sum(diag(compare_rf)) / sum(compare_rf)
    full1 <- test_msc
    full1$prediction = my_prediction_rf
    
    ROC_rf <- roc.curve(test_msc$is_attributed, my_prediction_rf)
    
    
    output$AUC_rf <- renderPrint({
      print(paste('AUC ivertis', round(ROC_rf$auc,3)))
    })
    
    #Rezultatams paskutiniame tab'e - confusion matrix
    output$pred_rf_rez <- renderPrint({
      names(dimnames(compare_rf)) <- list("", " Klasifikacijos lentele")
      colnames(compare_rf) <- c("0", "1")
      compare_rf
    })
    #Rezultatams paskutiniame tab'e - auc, and accuracy
    output$ROC_rf_rez <- renderPrint({
      print(paste('Tikslumas', round(accuracy_rf,3)))
      print(paste('AUC ivertis', round(ROC_rf$auc,3)))
    })
    
    # Build the rf tree
    output$plot_rf <- renderPlot({
      plot(rf_model)
    })
    
    # Predictions on the test set
    output$pred1 <- renderPrint({
      names(dimnames(compare_rf)) <- list("", "Klasifikacine lentele")
      colnames(compare_rf) <- c("0", "1")
      compare_rf
    })
    
    # Accuracy on the test set
    output$accur1 <- renderPrint({
      print(paste('Modelio tikslumas', round(accuracy_rf,3)))
    })
    
    # Prediction results
    output$table21 <- DT::renderDataTable({
      DT::datatable(full1, options = list(orderClasses = TRUE))
    })
  })
  #--------------------------------K nearest neighbours-------------------------------------------
  
  output$qqq <- DT::renderDataTable({
    DT::datatable(train_msc, options = list(orderClasses = TRUE))
  })
  
  output$number_of_folds_knn <- renderUI({
    sliderInput("number_of_folds_knn","Pasirinkite į kiek daliu dalinti duomenu aibe kryzminei validacijai",5,
                min = 5, max = 10, width='400px')
  })
  
  output$cv_repeats_knn <- renderUI({
    sliderInput("cv_repeats_knn","Pasirinkite kryzmines validacijos pakartojimu skaiciu",1,
                min = 1, max = 3, width='400px')
  })
  
  output$k_number <- renderUI({
    sliderInput("k_number","Pasirinkite kaimynu skaiciu:",1, 40, c(1, 7), width='400px')
  })
  
  observeEvent(input$action_knn, {
    
    knn_model <- caret::train(is_attributed~.,
                              data = train_msc,
                              method = 'knn',
                              tuneGrid   = expand.grid(k = c(input$k_number[1]:input$k_number[2])), #pakeist
                              trControl = trainControl(method = "repeatedcv",
                                                       number = input$number_of_folds_knn,
                                                       repeats = input$cv_repeats_knn),
                              preProc = c("center", "scale"))
    
    my_prediction_knn <- predict(knn_model, newdata = test_msc)
    compare_knn <- table(test_msc[["is_attributed"]], my_prediction_knn)
    accuracy_knn <- sum(diag(compare_knn)) / sum(compare_knn)
    full_knn <- test_msc
    full_knn$prediction = my_prediction_knn
    
    
    ROC_knn <- roc.curve(test_msc$is_attributed, my_prediction_knn)
    
    #Rezultatams paskutiniame tab'e - confusion matrix
    output$pred_knn_rez <- renderPrint({
      names(dimnames(compare_knn)) <- list("", " Klasifikacijos lentele")
      compare_knn
      
    })
    #Rezultatams paskutiniame tab'e - auc, and accuracy
    output$ROC_knn_rez <- renderPrint({
      print(paste('Tikslumas', round(accuracy_knn,3)))
      print(paste('AUC ivertis', round(ROC_knn$auc,3)))
    })
    
    output$ROC_knn <- renderPrint({
      print(paste('AUC ivertis', round(ROC_knn$auc,3)))
    })
    
    # Predictions on the test set
    output$pred_knn <- renderPrint({
      names(dimnames(compare_knn)) <- list("", "Klasifikacine lentele")
      compare_knn
    })
    
    # Accuracy on the test set
    output$accur_knn <- renderPrint({
      print(paste('Modelio tikslumas', round(accuracy_knn,3)))
    })
    
    # Prediction results
    output$table_knn <- DT::renderDataTable({
      DT::datatable(full_knn, options = list(orderClasses = TRUE))
    })
    
    output$k_neighbours <- renderPlot({
      plot(knn_model)
    })
  })
  #-------------------Support vector machines----------------------------------------------
  
  
  output$table_for_svm <- DT::renderDataTable({
    DT::datatable(train_msc, options = list(orderClasses = TRUE))
  })
  
  
  output$sigma1 <- renderUI({
    selectInput("sigma1","Pasirinkite Sigma:", c("Pasirinkite sigma" = "", c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                                                 0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9,5)), selected = 0.01)
  })
  
  output$C <- renderUI({
    selectInput("C","Pasirinkite baudos parametra C:", c("Pasirinkite baudos parametra C" = "", c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                                                     1, 1.5, 2,5,10)), selected = 0.01)
  })
  
  output$number_of_folds_svm <- renderUI({
    sliderInput("number_of_folds_svm","Pasirinkite į kiek daliu dalinti duomenu aibe kryzminei validacijai",5,
                min = 5, max = 10, width='400px')
  })
  
  output$cv_repeats_svm <- renderUI({
    sliderInput("cv_repeats_svm","Pasirinkite kryzmines validacijos pakartojimu skaiciu",1,
                min = 1, max = 3, width='400px')
  })
  
  output$test <- renderText({
    print(class(input$sigma))
    
  })
  
  
  observeEvent(input$action_svm, {
    
    
    svm_Radial_Grid <- caret::train(is_attributed~., data = train_msc, method = "svmRadial",
                                    trControl=trainControl(method = "repeatedcv",
                                                           number = input$number_of_folds_svm,
                                                           repeats = input$cv_repeats_svm),
                                    preProcess = c("center", "scale"),
                                    tuneGrid = expand.grid(sigma=as.numeric(input$sigma1), C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                                                                                 1, 1.5, 2,5,10)),
                                    tuneLength = 10)
    
    my_prediction_svm <- predict(svm_Radial_Grid, newdata = test_msc)
    compare_svm <- table(test_msc[["is_attributed"]], my_prediction_svm)
    accuracy_svm <- sum(diag(compare_svm)) / sum(compare_svm)
    full1 <- test_msc
    full1$prediction = my_prediction_svm
    
    ROC_svm <- roc.curve(test_msc$is_attributed, my_prediction_svm)
    
    
    output$ROC_svm <- renderPrint({
      print(paste('AUC ivertis', round(ROC_svm$auc,3)))
    })
    
    #rezultatams paskutiniame tab'e
    output$ROC_svm_rez <- renderPrint({
      print(paste('Tikslumas', round(accuracy_svm,3)))
      print(paste('AUC ivertis', round(ROC_svm$auc,3)))
    })
    
    # Build the rf tree
    output$plot_svm <- renderPlot({
      plot(svm_Radial_Grid)
    })
    
    # Predictions on the test set
    output$pred_svm <- renderPrint({
      names(dimnames(compare_svm)) <- list("", "Klasifikacine lentele")
      compare_svm
    })
    
    output$pred_svm_rez <- renderPrint({
      names(dimnames(compare_svm)) <- list("", " Klasifikacijos lentele")
      compare_svm
      
    })
    
    # Accuracy on the test set
    output$accur_svm <- renderPrint({
      print(paste('Modelio tikslumas', round(accuracy_svm,3)))
    })
    
    
    # Prediction results
    output$table_svm <- DT::renderDataTable({
      DT::datatable(full1, options = list(orderClasses = TRUE))
    })
  })
  
  #-------------------------------
  
  
  output$table_for_log <- DT::renderDataTable({
    DT::datatable(train_msc, options = list(orderClasses = TRUE))
  })
  
  output$number_of_folds_log <- renderUI({
    sliderInput("number_of_folds_log","Pasirinkite į kiek daliu dalinti duomenu aibe kryzminei validacijai",5,
                min = 5, max = 10, width='400px')
  })
  
  output$cv_repeats_log <- renderUI({
    sliderInput("cv_repeats_log","Pasirinkite kryzmines validacijos pakartojimu skaiciu",1,
                min = 1, max = 3, width='400px')
  })
  
  observeEvent(input$action_log, {
    
    log_model <- caret::train(is_attributed ~ip+app+channel,
                              data = train_msc,
                              method = 'glm',
                              family = binomial(),
                              trControl = trainControl(method = "repeatedcv",
                                                       number = input$number_of_folds_log,
                                                       repeats = input$cv_repeats_log))
    
    my_prediction_log <- predict(log_model, newdata = test_msc)
    compare_log <- table(test_msc[["is_attributed"]], my_prediction_log)
    accuracy_log <- sum(diag(compare_log)) / sum(compare_log)
    full_log<- test_msc
    full_log$prediction = my_prediction_log
    
    ROC_log <- roc.curve(test_msc$is_attributed, my_prediction_log)
    
    
    output$ROC_log <- renderPrint({
      print(paste('AUC ivertis', round(ROC_log$auc,3)))
    })
    
    # Predictions on the test set
    output$pred_log <- renderPrint({
      names(dimnames(compare_log)) <- list("", "Klasifikacine lentele")
      compare_log
    })
    
    #Rezultatams paskutiniame tab'e - confusion matrix
    output$pred_log_rez <- renderPrint({
      names(dimnames(compare_log)) <- list("", " Klasifikacijos lentele")
      compare_log
      
    })
    #Rezultatams paskutiniame tab'e - auc, and accuracy
    output$ROC_log_rez <- renderPrint({
      print(paste('Tikslumas', round(accuracy_log,3)))
      print(paste('AUC ivertis', round(ROC_log$auc,3)))
    })
    
    # Accuracy on the test set
    output$accur_log <- renderPrint({
      print(paste('Modelio tikslumas', round(accuracy_log,3)))
    })
    
    # Prediction results
    output$table_log <- DT::renderDataTable({
      DT::datatable(full_log, options = list(orderClasses = TRUE))
    })
    
  })
  
  
  
  
  
  
})











