# 1. Prepare the dataframe for classification

    # the Knee column and range class
    k <- c(Knee[[1]], Knee[[2]], Knee[[3]])
    data <- data.frame(Knee = k)
    remove(k)
    
    # cycle 2~301 of each battery
    for(cycle in 2:301){
        t1 <- c(); t2 <- c(); t3 <- c()
        for(i in 1:41) t1[i] <- QD.GAM[[1]][[i]]$capacity[cycle]
        for(i in 1:43) t2[i] <- QD.GAM[[2]][[i]]$capacity[cycle]
        for(i in 1:40) t3[i] <- QD.GAM[[3]][[i]]$capacity[cycle]
        t <- c(t1, t2, t3)
        data <- cbind(data, t)
        names(data)[cycle] <- paste("cycle_", cycle, sep = "")
    }
    remove(list = c("cycle", "i", "t1", "t2", "t3", "t"))
    
    # remove extreme cases
    n1 <- c(1:10, 17, 18)
    n2 <- c(1, 2, 4, 5, 7, 10, 11, 14, 15, 43) + BatchNum[1]
    n3 <- c(6, 7, 16, 17, 21, 27, 31, 35, 40) + BatchNum[1] + BatchNum[2]
    data <- data[-c(n1, n2, n3),]
    remove(list = c("n1", "n2", "n3"))
    
    
    # divide the knees into Nc classes, and label them
    Nc <- 3 # number of class
    label <- c()
    for(i in 1:length(data$Knee)){
        label[i] <- ceiling( Nc * (data$Knee[i] - min(data$Knee)) /  (max(data$Knee) - min(data$Knee)))
        if(data$Knee[i] == min(data$Knee)){
            label[i] <- label[i] + 1
        }
    }
    remove(list = c("i", "Nc"))
    label <- as.factor(label)
    
    # add the label to the data
    data <- data.frame(label, data)
    remove(label)
    
    # remove the column knee
    data <- data[, -2]
    
    # final check
    str(data)
    table(data$label)

# 2. SVM, first try

    # package for svm
    #library(e1071)
    #library(caret)
    
    # train and test sets
    set.seed(6677112)
    indexes = createDataPartition(data$label, p = .8, list = F)
    svm_train = data[indexes, ]
    svm_test = data[-indexes, ]
    remove(indexes)
    
    # model training
    svm_model <- svm(label ~ ., 
                     data = svm_train, 
                     method = "C-classification", 
                     kernal = "radical",
                     probability = TRUE,
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
                         probability = TRUE,
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
    mean(svm_acc) # 0.7419355

# 4. which number of cycles as input will reach the highest accuracy?

    result.3 <- c()
    count <- 1
    for(cc in 3:301){
        
        # 1. data
        
            # the Knee column and range class
            k <- c(Knee[[1]], Knee[[2]], Knee[[3]])
            data <- data.frame(Knee = k)
            remove(k)
            
            # cycle 2~cc of each battery
            for(cycle in 2:cc){
                t1 <- c(); t2 <- c(); t3 <- c()
                for(i in 1:41) t1[i] <- QD.GAM[[1]][[i]]$capacity[cycle]
                for(i in 1:43) t2[i] <- QD.GAM[[2]][[i]]$capacity[cycle]
                for(i in 1:40) t3[i] <- QD.GAM[[3]][[i]]$capacity[cycle]
                t <- c(t1, t2, t3)
                data <- cbind(data, t)
                names(data)[cycle] <- paste("cycle_", cycle, sep = "")
            }
            remove(list = c("cycle", "i", "t1", "t2", "t3", "t"))
            
            # remove extreme cases
            n1 <- c(1:10, 17, 18)
            n2 <- c(1, 2, 4, 5, 7, 10, 11, 14, 15, 43) + BatchNum[1]
            n3 <- c(6, 7, 16, 17, 21, 27, 31, 35, 40) + BatchNum[1] + BatchNum[2]
            data <- data[-c(n1, n2, n3),]
            remove(list = c("n1", "n2", "n3"))
            
            
            # divide the knees into Nc classes, and label them
            Nc <- 3 # number of class
            label <- c()
            for(i in 1:length(data$Knee)){
                label[i] <- ceiling( Nc * (data$Knee[i] - min(data$Knee)) /  (max(data$Knee) - min(data$Knee)))
                if(data$Knee[i] == min(data$Knee)){
                    label[i] <- label[i] + 1
                }
            }
            remove(list = c("i", "Nc"))
            label <- as.factor(label)
            
            # add the label to the data
            data <- data.frame(label, data)
            remove(label)
            
            # remove the column knee
            data <- data[, -2]
        
        # 2. svm with cross validation
        
        svm_acc <- c()
        for(i in 1:93){
            # train and test sets
            set.seed(88293)
            svm_train = data[-i, ]
            svm_test = data[i, ]
            # model training
            svm_model <- svm(label ~ ., 
                             data = svm_train, 
                             method = "C-classification", 
                             kernal = "radical",
                             probability = TRUE,
                             gamma = 0.1, 
                             cost = 10)
            # evalute the accuracy
            svm_pred <- predict(svm_model, svm_test)
            svm_table <- table(svm_test$label, svm_pred)
            svm_acc[i] <- tr(svm_table)/nrow(svm_test)
            print(paste("count =", count, "i =", i))
        }
        result.3[count] <- mean(svm_acc)
        count <- count + 1
    }
    remove(list = c("cc", "count"))
    
    plot(x = 3:301, y = result.3, pch = 20,
         xlab = "number of cycles as input",
         ylab = "accuracy",
         main = "3-Class Classification Accuracy Using SVM")
    
    max(result.3) # 0.7634409
    
    # compare with binary classification
    plot(x = 3:301, y = result.2, pch = 20, col = "blue",
         xlab = "number of cycles as input",
         ylab = "accuracy", ylim = c(0.5, 1),
         main = "Binary v.s. 3-Class")
    points(x = 3:301, y = result.3, pch = 20, col = "red")
    legend("bottomright", 
           legend=c("binary", "3-class"),
           col=c("blue", "red"), pch=20)
