# 1. Prepare the dataframe for classification

    # the Knee column and range class
    k <- c(Knee[[1]], Knee[[2]], Knee[[3]])
    rr <- c()
    for(i in 1:124){
        if(k[i] < 500){
            rr[i] <- 1  # short
        }else{
            rr[i] <- 2  # long
        }
    }
    data <- data.frame(Knee = k, Range = rr)
    remove(k, rr)
    
    # cycle 2~41 of each battery
    for(cycle in 2:41){
        t1 <- c(); t2 <- c(); t3 <- c()
        for(i in 1:41) t1[i] <- QD.GAM[[1]][[i]]$capacity[cycle]
        for(i in 1:43) t2[i] <- QD.GAM[[2]][[i]]$capacity[cycle]
        for(i in 1:40) t3[i] <- QD.GAM[[3]][[i]]$capacity[cycle]
        t <- c(t1, t2, t3)
        data <- cbind(data, t)
        names(data)[cycle+1] <- paste("cycle_", cycle, sep = "")
    }
    remove(list = c("cycle", "i", "t1", "t2", "t3", "t"))
    
    # remove the first column
    data <- data[, -1]
    
    # turn "range" into factor
    data$Range <- factor(data$Range, levels = c(1, 2))
    
    # remove extreme cases
    data <- data[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,17, 18, 
                    42, 43, 45, 46, 48, 51, 52, 55, 56, 84,
                    90, 91, 100, 101, 105, 111, 115, 119, 124),]
    # final check
    str(data)
    table(data$Range)

# 2. SVM

    # package for svm
    library(e1071)
    library(caret)
    
    # train and test sets
    set.seed(88293)
    indexes = createDataPartition(data$Range, p = .8, list = F)
    svm_train = data[indexes, ]
    svm_test = data[-indexes, ]
    
    # model training
    svm_model <- svm(Range ~ ., 
                     data = svm_train, 
                     method = "C-classification", 
                     kernal = "radial", 
                     gamma = 0.1, 
                     cost = 10)
    
    # evalute the accuracy
    svm_pred <- predict(svm_model, svm_test)
    svm_table <- table(svm_test$Range, svm_pred)
    svm_table
    (svm_table[1, 1] + svm_table[2, 2])/nrow(svm_test)

