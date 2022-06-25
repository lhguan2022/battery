# 0. Knee Detection from Satopaa, Ville, 
#    et al. (2011)

# 1. Calculate the distence from a line 
#    through points "b" and "c" and another 
#    point "a"

    dist2d <- function(a,b,c) {
        v1 <- b - c
        v2 <- a - b
        m <- cbind(v1,v2)
        d <- abs(det(m))/sqrt(sum(v1*v1))
    } 

# 2. Detect the knee points
    
    getKnee <- function(batch, number){
        data <- QD.GAM[[batch]][[number]]
        N <- nrow(data)
        b <- c(data$cycle[1], data$capacity[1])
        c <- c(data$cycle[N], data$capacity[N])
        d <- c()
        for(i in 1:N){
            a <- c(data$cycle[i], data$capacity[i])
            d[i] <- dist2d(a, b, c)
        }
        return(which.max(d))
    }

# 3. Collect the knee points in a list
    
    Knee1 <- c(); Knee2 <- c(); Knee3 <- c()
    Knee <- list(Knee1, Knee2, Knee3)
    for(batch in 1:3){
        for(number in 1:BatchNum[batch]){
            Knee[[batch]][number] <- getKnee(batch = batch, number = number)
        }
    }
    remove(list = c("Knee1", "Knee2", "Knee3", "batch", "number"))

# 4. A strong linear relationship between 
#    EOL and Knee-Point
    
    plot(EOL[[1]], Knee[[1]], pch=20, col="red", 
         xlab = "EOL", ylab = "Knee", main = "EOL v.s. Knee-Points",
         ylim = c(0, 1700), xlim = c(0, 3000))
    points(EOL[[2]], Knee[[2]], pch=20, col="blue")
    points(EOL[[3]], Knee[[3]], pch=20, col="green")
    legend("bottomright", legend=c("Batch 1", "Batch 2", "Batch 3"),
           col=c("red", "blue", "green"), pch=20)

# 5. A linear model
    
    # data frame
    d1 <- cbind(EOL[[1]], Knee[[1]])
    d2 <- cbind(EOL[[2]], Knee[[2]])
    d3 <- cbind(EOL[[3]], Knee[[3]])
    dd <- rbind(d1, d2, d3)
    dd <- data.frame(dd)
    
    # linear model
    mm <- lm(formula = X2 ~ X1, data = dd)
    summary(mm)  # the intercept is not significantly non-zero
    mm <- lm(formula = X2 ~ X1-1, data = dd)  # lm without intercept
    summary(mm)  # Adjusted R-squared: 0.9964
    
    # add the regression line to the scatter plot
    
        plot(EOL[[1]], Knee[[1]], pch=20, col="red", 
             xlab = "EOL", ylab = "Knee", main = "EOL v.s. Knee-Points",
             ylim = c(0, 1700), xlim = c(0, 3000))
        points(EOL[[2]], Knee[[2]], pch=20, col="blue")
        points(EOL[[3]], Knee[[3]], pch=20, col="green")
        legend("bottomright", legend=c("Batch 1", "Batch 2", "Batch 3"),
               col=c("red", "blue", "green"), pch=20)
        abline(a=0, b=mm$coefficients, lty=2, col="grey")
        text(x = 2000, y = 900, labels = "Knee = 0.731 * EOL")
        text(x = 2000, y = 700, labels = "R-sq = 0.9964")
        remove(list = c("d1", "d2", "d3", "dd", "mm"))
    

# 6. knee on the plots

    # all
    {
        plot(x=-1, y=-1, xlim=c(0, 2400), ylim=c(0.85, 1.1),
             xlab="cycle", ylab="capacity", main="Knee Points")
        for(batch in 1:3){
            for(number in 1:BatchNum[batch]){
                points(QD.GAM[[batch]][[number]], type="l")
                points(x = QD.GAM[[batch]][[number]]$cycle[Knee[[batch]]][number],
                       y = QD.GAM[[batch]][[number]]$capacity[Knee[[batch]]][number],
                       col = "red", pch = 20)
            }
        }
        remove(list = c("batch", "number"))
        abline(h=0.88, lty=2)
    }
    
    # without extreme cases
    {
        plot(x=-1, y=-1, xlim=c(0, 1300), ylim=c(0.85, 1.1),
             xlab="cycle", ylab="capacity", main="Knee Points, batch 3")
        n1 <- c(1:10, 17, 18)
        n2 <- c(1, 2, 4, 5, 7, 10, 11, 14, 15, 43)
        n3 <- c(6, 7, 16, 17, 21, 27, 31, 35, 40)
        for(batch in 1:3){
            for(number in 1:BatchNum[batch]){
                if(batch==1 && number %in% n1) next
                if(batch==2 && number %in% n2) next
                if(batch==3 && number %in% n3) next
                points(QD.GAM[[batch]][[number]], type="l")
                points(x = QD.GAM[[batch]][[number]]$cycle[Knee[[batch]]][number],
                       y = QD.GAM[[batch]][[number]]$capacity[Knee[[batch]]][number],
                       col = "red", pch = 20)
            }
        }
        remove(list = c("batch", "number", "n1", "n2", "n3"))
        abline(h=0.88, lty=2)
    }
    
    # batch 1
    {
        plot(x=-1, y=-1, xlim=c(0, 1300), ylim=c(0.85, 1.1),
             xlab="cycle", ylab="capacity", main="Knee Points, batch 1")
        n1 <- c(1:10, 17, 18)
        n2 <- c(1, 2, 4, 5, 7, 10, 11, 14, 15, 43)
        n3 <- c(6, 7, 16, 17, 21, 27, 31, 35, 40)
        for(batch in 1:3){
            for(number in 1:BatchNum[batch]){
                if(batch==1 && number %in% n1) next
                if(batch==2 && number %in% n2) next
                if(batch==3 && number %in% n3) next
                if(batch==1){
                    points(QD.GAM[[batch]][[number]], type="l")
                    points(x = QD.GAM[[batch]][[number]]$cycle[Knee[[batch]]][number],
                           y = QD.GAM[[batch]][[number]]$capacity[Knee[[batch]]][number],
                           col = "red", pch = 20)
                }else{
                    points(QD.GAM[[batch]][[number]], type="l", col = "grey", lty = 2)
                }
            }
        }
        remove(list = c("batch", "number", "n1", "n2", "n3"))
        abline(h=0.88, lty=2)
    }
    
    # batch 2
    {
        plot(x=-1, y=-1, xlim=c(0, 1300), ylim=c(0.85, 1.1),
             xlab="cycle", ylab="capacity", main="Knee Points, batch 2")
        n1 <- c(1:10, 17, 18)
        n2 <- c(1, 2, 4, 5, 7, 10, 11, 14, 15, 43)
        n3 <- c(6, 7, 16, 17, 21, 27, 31, 35, 40)
        for(batch in 1:3){
            for(number in 1:BatchNum[batch]){
                if(batch==1 && number %in% n1) next
                if(batch==2 && number %in% n2) next
                if(batch==3 && number %in% n3) next
                if(batch==2){
                    points(QD.GAM[[batch]][[number]], type="l")
                    points(x = QD.GAM[[batch]][[number]]$cycle[Knee[[batch]]][number],
                           y = QD.GAM[[batch]][[number]]$capacity[Knee[[batch]]][number],
                           col = "red", pch = 20)
                }else{
                    points(QD.GAM[[batch]][[number]], type="l", col = "grey", lty = 2)
                }
            }
        }
        remove(list = c("batch", "number", "n1", "n2", "n3"))
        abline(h=0.88, lty=2)
    }
    
    # batch 3
    {
        plot(x=-1, y=-1, xlim=c(0, 1300), ylim=c(0.85, 1.1),
             xlab="cycle", ylab="capacity", main="Knee Points, batch 3")
        n1 <- c(1:10, 17, 18)
        n2 <- c(1, 2, 4, 5, 7, 10, 11, 14, 15, 43)
        n3 <- c(6, 7, 16, 17, 21, 27, 31, 35, 40)
        for(batch in 1:3){
            for(number in 1:BatchNum[batch]){
                if(batch==1 && number %in% n1) next
                if(batch==2 && number %in% n2) next
                if(batch==3 && number %in% n3) next
                if(batch==3){
                    points(QD.GAM[[batch]][[number]], type="l")
                    points(x = QD.GAM[[batch]][[number]]$cycle[Knee[[batch]]][number],
                           y = QD.GAM[[batch]][[number]]$capacity[Knee[[batch]]][number],
                           col = "red", pch = 20)
                }else{
                    points(QD.GAM[[batch]][[number]], type="l", col = "grey", lty = 2)
                }
            }
        }
        remove(list = c("batch", "number", "n1", "n2", "n3"))
        abline(h=0.88, lty=2)
    }
    