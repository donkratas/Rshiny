library(keras)
library(tensorflow)
use_condaenv("tf") 

n_data <- train_msc[ ,1:6]
n_test <- test_msc[ ,c(-6)]

n_data[ ,1:5] <- apply(n_data[,1:5],2, function(x) (x - min(x))/(max(x) - min(x)))
n_test <- apply(n_test[,1:5],2, function(x) (x - min(x))/(max(x) - min(x)))
n_data <- as.matrix(n_data)
dimnames(n_data) <- NULL
n_test <- as.matrix(n_test)
dimnames(n_test) <- NULL

nn_train <- n_data[,1:5]
nn_train_y <- n_data[, 6]

n_test <- n_test[, 1:5]
nn_test_y <- as.matrix(test_msc[, 7])

nn_train_y_tr <- to_categorical(nn_train_y) #tr - transformed
nn_test_y_tr <- to_categorical(nn_test_y) 

nn_model <- keras_model_sequential()
nn_model %>% 
  layer_dense(units = 50, activation = 'relu', input_shape = c(5)) %>%
  layer_dense(units = 2, activation = 'softmax')

nn_model %>% compile(loss = 'categorical_crossentropy',
                     optimizer = 'adam',
                     metrics = 'accuracy')

history <- nn_model %>% 
  fit(nn_train,
      nn_train_y_tr,
      epoch = 100,
      batch_size = 32,
      validation_split = 0.2)

#Confusion matrix on train data
nn_cm_tr <- table(Predicted = nn_model %>% predict_classes(nn_train), Actual = nn_train_y)

#Confusion matrix on test data
nn_cm_te <- table(Predicted = nn_model %>% predict_classes(nn_test), Actual = nn_test_y)

#Accuracy on test data
nn_model %>% evaluate(nn_test, nn_test_y_tr)

#Accuracy on train data
nn_model %>% evaluate(nn_train, nn_train_y_tr)

sensitivity(nn_cm_te)
specificity(nn_cm_te)