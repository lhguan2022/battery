# 1. preparation
    
    setwd("~/Documents/R/battery")
    library(tidyverse)

    
# 2. the names of the files, for data read-in

    Name1 = c(); Name2 = c(); Name3 = c()
    for(i in 0:45){
        Name1[i+1] <- paste("b1c", as.character(i), sep = "")
    }
    for(i in 0:47){
        Name2[i+1] <- paste("b2c", as.character(i), sep = "")
    }
    for(i in 0:45){
        Name3[i+1] <- paste("b3c", as.character(i), sep = "")
    }
    remove(i)
    Name1 <- Name1[-c(9, 11, 13, 14, 23)]
    Name2 <- Name2[-c(8, 9, 10, 16, 17)]
    Name3 <- Name3[-c(3, 24, 33, 38, 43, 44)]
    

# 3. create three lists of dataframes

    Bat1.QD <- list(); Bat2.QD <- list(); Bat3.QD <- list()
    for(i in 1:length(Name1)){
        f1 = paste("Data/QD/", Name1[i], ".csv", sep = "")
        Bat1.QD[[i]] <- read_csv(f1)
    }
    for(i in 1:length(Name2)){
        f1 = paste("Data/QD/", Name2[i], ".csv", sep = "")
        Bat2.QD[[i]] <- read_csv(f1)
    }
    for(i in 1:length(Name3)){
        f1 = paste("Data/QD/", Name3[i], ".csv", sep = "")
        Bat3.QD[[i]] <- read_csv(f1)
    }
    remove(i); remove(f1)


# 4. the cycle index start from 0
#    we add 1 to the cycle number
    
    for(i in 1:41){
        names(Bat1.QD[[i]])[1] <- "cycle"
        Bat1.QD[[i]]$cycle <-  Bat1.QD[[i]]$cycle + 1
    }
    for(i in 1:43){
        names(Bat2.QD[[i]])[1] <- "cycle"
        Bat2.QD[[i]]$cycle <-  Bat2.QD[[i]]$cycle + 1
    }
    for(i in 1:40){
        names(Bat3.QD[[i]])[1] <- "cycle"
        Bat3.QD[[i]]$cycle <-  Bat3.QD[[i]]$cycle + 1
    }
    remove(i)


# 5. put the data into a compact list
#    from now on, we can use index on batch :D

    QD <- list(Bat1.QD, Bat2.QD, Bat3.QD)
    remove(Bat1.QD); remove(Bat2.QD); remove(Bat3.QD)
    # for example, if we want to view the 6th QD 
    # data in bathc 2, we do:
    # View( QD[[2]][[6]] )

    
# 6. calculate the life cycle (EOL) for each battery, where
#    EOL := the first cycle such that the capacity < 0.88

    EOL1 <- c(); EOL2 <- c(); EOL3 <- c()
    for(i in 1:41){
        arr <- QD[[1]][[i]]$capacity
        arr <- arr[-1] # remove the first cycle in batch 1
        EOL1[i] <- length(arr[arr>0.88]) + 1 + 1 # add back the first cycle
    }
    for(i in 1:43){
        arr <- QD[[2]][[i]]$capacity
        EOL2[i] <- length(arr[arr>0.88]) + 1
    }
    for(i in 1:40){
        arr <- QD[[3]][[i]]$capacity
        EOL3[i] <- length(arr[arr>0.88]) + 1
    }
    remove(i); remove(arr)
    

# 7. put the EOFs into a compact list
#    from now on, we can use index on batch :D

    EOL <- list(EOL1, EOL2, EOL3)
    remove(EOL1); remove(EOL2); remove(EOL3)


# 8. In Batch 2 Number 24, the EOF should be 509, not 510
    
    EOL[[2]][24] <- EOL[[2]][24] - 1

# 9. number of cells in each batch
    
    BatchNum = c(length(Name1), length(Name2), length(Name3))

# 10. discard the datapoint that beyond the EOL

    for(batch in 1:3){
        for(num in 1:BatchNum[batch]){
            QD[[batch]][[num]] <- QD[[batch]][[num]][1:(EOL[[batch]][num]-1),]
        }
    }
    remove(batch); remove(num)



# 11. the first row of each dataframe in batch 1 
#     are all zeros, we remove them

    for(i in 1:41){
        QD[[1]][[i]] <- QD[[1]][[i]][-1,]
    }
    remove(i)




