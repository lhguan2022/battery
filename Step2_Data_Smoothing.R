# 1. write the smoothing procedure as a function

    # input: dataframe of the original cycle v. capacity data
    # output:  dataframce of the smoothed data
    library(mgcv)
    GAM <- function(batch, number, w1=1, w2=1){
        
        # data
        x <- QD[[batch]][[number]]$cycle
        y <- QD[[batch]][[number]]$capacity
        N <- length(x)
        data <- data.frame(x, y)
        
        # do-while loop
        repeat{
            
            # the GAM fitting part
            # library(mgcv)
            gam_fit <- gam(data = data, formula = y ~ s(x, bs = "cr"))
            res <- data$y - predict(gam_fit)
            
            # terms needed in the updating part
            y1 <- c()               # new points
            yy <- data$y            # old points
            ff <- predict(gam_fit)  # gam points
            rr <- sd(res)           # RSS
            
            # algorithm: 
            # if old points fall out of the range
            # replace it by the gam points
            for(i in 1:N){
                cond1 <- yy[i] > ff[i] - w1*rr
                cond2 <- yy[i] < ff[i] + w2*rr 
                y1[i] <- ifelse(cond1&&cond2, yy[i], ff[i])
            }
            
            # update and the stopping condition
            data$y <- y1
            if(rr < 1e-6){ print("OK"); break }
        }
        
        # return the smoothed data
        return( data.frame(cycle = data$x, capacity = data$y) )
    }

    
# 2. apply the GAM() function on each dataframe
    
    QD.GAM1 <- list(); QD.GAM2 <- list(); QD.GAM3 <- list()
    QD.GAM <- list(QD.GAM1, QD.GAM2, QD.GAM3)
    for(batch in 1:3){
        for(number in 1:BatchNum[batch]){
            print(paste("Batch", batch, "Number", number, sep = " "))
            QD.GAM[[batch]][[number]] <- GAM(batch = batch, number = number)
        }
    }
    remove(list = c("QD.GAM1", "QD.GAM2", "QD.GAM3", "batch", "number"))

# 3. plots
    
    # original
    
        plot(x=-1, y=-1, xlim=c(0, 2400), ylim=c(0.85, 1.1),
             xlab="cycle", ylab="capacity", main="Original")
        for(batch in 1:3){
            for(number in 1:BatchNum[batch]){
                points(QD[[batch]][[number]], type="l", col=batch+1)
            }
        }
        remove(list = c("batch", "number"))
        abline(h=0.88, lty=2)
    
    
    # smoothed
    
        plot(x=-1, y=-1, xlim=c(0, 2400), ylim=c(0.85, 1.1),
             xlab="cycle", ylab="capacity", main="Smoothed by repeated GAM")
        for(batch in 1:3){
            for(number in 1:BatchNum[batch]){
                points(QD.GAM[[batch]][[number]], type="l", col=batch+1)
            }
        }
        remove(list = c("batch", "number"))
        abline(h=0.88, lty=2)
    
    # smoothed without extreme cases
    
        plot(x=-1, y=-1, xlim=c(0, 1200), ylim=c(0.85, 1.1),
             xlab="cycle", ylab="capacity", main="Smoothed by repeated GAM")
        n1 <- c(1:10, 17, 18)
        n2 <- c(1, 2, 4, 5, 7, 10, 11, 14, 15, 43)
        n3 <- c(6, 7, 16, 17, 21, 27, 31, 35, 40)
        for(batch in 1:3){
            for(number in 1:BatchNum[batch]){
                if(batch==1 && number %in% n1) next
                if(batch==2 && number %in% n2) next
                if(batch==3 && number %in% n3) next
                points(QD.GAM[[batch]][[number]], type="l", col=batch+1)
            }
        }
        remove(list = c("batch", "number", "n1", "n2", "n3"))
        abline(h=0.88, lty=2)
    
    




