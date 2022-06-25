# 1. Prepare the dataframe for classification

    # the Knee column and range column
    k <- c(Knee[[1]], Knee[[2]], Knee[[3]])
    rr <- c()
    for(i in 1:124){
        if(k[i] < 500){
            rr[i] <- 1  # short
        }else{
            rr[i] <- 2  # long
        }
    }
    data <- data.frame(Knee = k, label = rr)
    remove(k, rr)
    
    # cycle 2~301 of each battery
    for(cycle in 2:301){
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
    data$label <- factor(data$label, levels = c(1, 2))
    
    # remove extreme cases
    n1 <- c(1:10, 17, 18)
    n2 <- c(1, 2, 4, 5, 7, 10, 11, 14, 15, 43) + BatchNum[1]
    n3 <- c(6, 7, 16, 17, 21, 27, 31, 35, 40) + BatchNum[1] + BatchNum[2]
    data <- data[-c(n1, n2, n3),]
    remove(list = c("n1", "n2", "n3"))
    
    # final check
    str(data)
    table(data$label)

# 2. SVM, first try

    # package for svm
    #library(e1071)
    #library(caret)
    
    # train and test sets
    set.seed(88293)
    indexes = createDataPartition(data$label, p = .8, list = F)
    svm_train = data[indexes, ]
    svm_test = data[-indexes, ]
    remove(indexes)
    
    # model training
    svm_model <- svm(label ~ ., 
                     data = svm_train, 
                     method = "C-classification", 
                     kernal = "radical", 
                     gamma = 0.1, 
                     cost = 10)
    
    # evalute the accuracy
    svm_pred <- predict(svm_model, svm_test)
    svm_table <- table(svm_test$label, svm_pred)
    svm_table
    #library(psych)
    tr(svm_table)/nrow(svm_test)
    
# 3. SVM, with cross validation using leave-one-out strategy
    
    svm_acc <- c()
    count <- 1
    for(i in 1:93){
        # train and test sets
        svm_train = data[-i, ]
        svm_test = data[i, ]
        
        # model training
        svm_model <- svm(label ~ ., 
                         data = svm_train, 
                         method = "C-classification", 
                         kernal = "radical", 
                         gamma = 0.1, 
                         cost = 10)
        
        # evalute the accuracy
        svm_pred <- predict(svm_model, svm_test)
        svm_table <- table(svm_test$label, svm_pred)
        svm_acc[count] <- tr(svm_table)/nrow(svm_test)
        count <- count + 1
        print(count)
    }
    remove(list = c("i", "count"))
    mean(svm_acc) # 0.9032258

# 4. which number of cycles as input will reach the highest accuracy?
    
    result.2 <- c()
    count <- 1
    for(cc in 3:301){
        
        # 1. data
            
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
            data <- data.frame(Knee = k, label = rr)
            # cycle 2~cc of each battery
            for(cycle in 2:cc){
                t1 <- c(); t2 <- c(); t3 <- c()
                for(i in 1:41) t1[i] <- QD.GAM[[1]][[i]]$capacity[cycle]
                for(i in 1:43) t2[i] <- QD.GAM[[2]][[i]]$capacity[cycle]
                for(i in 1:40) t3[i] <- QD.GAM[[3]][[i]]$capacity[cycle]
                t <- c(t1, t2, t3)
                data <- cbind(data, t)
                names(data)[cycle+1] <- paste("cycle_", cycle, sep = "")
            }
            # remove the first column
            data <- data[, -1]
            # turn "range" into factor
            data$label <- factor(data$label, levels = c(1, 2))
            # remove extreme cases
            n1 <- c(1:10, 17, 18)
            n2 <- c(1, 2, 4, 5, 7, 10, 11, 14, 15, 43) + BatchNum[1]
            n3 <- c(6, 7, 16, 17, 21, 27, 31, 35, 40) + BatchNum[1] + BatchNum[2]
            data <- data[-c(n1, n2, n3),]
        
        # 2. svm with cross validation
            
            svm_acc <- c()
            for(i in 1:93){
                # train and test sets
                svm_train = data[-i, ]
                svm_test = data[i, ]
                # model training
                svm_model <- svm(label ~ ., 
                                 data = svm_train, 
                                 method = "C-classification", 
                                 kernal = "radical", 
                                 gamma = 0.1, 
                                 cost = 10)
                # evalute the accuracy
                svm_pred <- predict(svm_model, svm_test)
                svm_table <- table(svm_test$label, svm_pred)
                svm_acc[i] <- tr(svm_table)/nrow(svm_test)
                print(paste("count =", count, "i =", i))
            }
            result.2[count] <- mean(svm_acc)
            count <- count + 1
    }
    remove(list = c("cc", "count", "cycle", "i", "k",
                    "n1", "n2", "n3", "rr", "t", "t1", "t2", "t3"))
    
    plot(x = 3:301, y = result.2, pch = 20,
         xlab = "number of cycles as input",
         ylab = "accuracy",
         main = "Binary Classification Accuracy Using SVM")
    abline(h = 0.9, lty = 2, col = "grey")
    abline(v = 115, lty = 2, col = "grey")
    text("accuracy = 0.9", x = 30, y = 0.91)
    text("cc = 115", x = 130, y = 0.6)
    
    max(result.2)
