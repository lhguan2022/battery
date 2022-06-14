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
    {
        plot(x=-1, y=-1, xlim=c(0, 1200), ylim=c(0.85, 1.1),
             xlab="cycle", ylab="capacity", main="Smoothed by repeated GAM")
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
                points(QD.GAM[[batch]][[number]], type="l", col=batch+1)
            }
        }
        remove(list = c("batch", "number"))
        abline(h=0.88, lty=2)
    }
    
    




