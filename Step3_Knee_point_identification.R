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
        data <- QD[[batch]][[number]]
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
    abline(a=mm$coefficients[1], b=mm$coefficients[2], lty=2, col="grey")
    text(x = 2000, y = 900, labels = "Knee = 12.38 + 0.69 * EOL")
    text(x = 2000, y = 700, labels = "R-sq = 0.8003")
    remove(list = c("d1", "d2", "d3", "dd", "mm"))

# 6. knee on the plots
    
    b = 1; n = 4
    plot(x = QD.GAM[[b]][[n]]$cycle,
         y = QD.GAM[[b]][[n]]$capacity,
         pch = 20, xlab = "cycle", ylab = "capacity")
    k = Knee[[b]][n]
    points(x = k, y = QD.GAM[[b]][[n]]$capacity[k], 
          col = "red", pch = 19)
    remove(list=c("b", "n", "k"))

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
             xlab="cycle", ylab="capacity", main="Knee Points")
        for(batch in 1:3){
            for(number in 1:BatchNum[batch]){
                if(batch==1 && number==1) next
                if(batch==1 && number==2) next
                if(batch==1 && number==3) next
                if(batch==1 && number==4) next
                if(batch==1 && number==5) next
                if(batch==1 && number==6) next
                if(batch==1 && number==7) next
                if(batch==1 && number==8) next
                if(batch==1 && number==9) next
                if(batch==1 && number==10) next
                if(batch==1 && number==17) next
                if(batch==1 && number==18) next
                if(batch==2 && number==1) next
                if(batch==2 && number==2) next
                if(batch==2 && number==4) next
                if(batch==2 && number==5) next
                if(batch==2 && number==7) next
                if(batch==2 && number==10) next
                if(batch==2 && number==11) next
                if(batch==2 && number==14) next
                if(batch==2 && number==15) next
                if(batch==2 && number==43) next
                if(batch==3 && number==6) next
                if(batch==3 && number==7) next
                if(batch==3 && number==16) next
                if(batch==3 && number==17) next
                if(batch==3 && number==21) next
                if(batch==3 && number==27) next
                if(batch==3 && number==31) next
                if(batch==3 && number==35) next
                if(batch==3 && number==40) next
                points(QD.GAM[[batch]][[number]], type="l")
                points(x = QD.GAM[[batch]][[number]]$cycle[Knee[[batch]]][number],
                       y = QD.GAM[[batch]][[number]]$capacity[Knee[[batch]]][number],
                       col = "red", pch = 20)
            }
        }
        remove(list = c("batch", "number"))
        abline(h=0.88, lty=2)
    }