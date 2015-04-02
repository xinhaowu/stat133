#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.


#This function counts the steps taken when the models hit grid lock. It runs the 
#model 100 times and omit the situation when it is free flow.
lock.count <- function(r,c,p){
        
        result <- t(replicate(100, bml.sim(r,c,p))[c(1,2),])
        colnames(result) <- c("lock","count")
        result2 <- as.data.frame(result)
        result3 <- as.numeric(result[result2$lock == TRUE, ][,2])
        
       return(result3) 
}


#run this function with fixed size and shape, with different density
pl0.5 <- lock.count(10,10,0.5)
pl0.6 <- lock.count(10,10,0.6)
pl0.7 <- lock.count(10,10,0.7)
pl0.8 <- lock.count(10,10,0.8)
pl0.9 <- lock.count(10,10,0.9)

#Merging the data and calculate the mean
density.and.lock <- list(pl0.5,pl0.6,pl0.7,pl0.8,pl0.9)
mean.density.and.lock <- sapply(density.and.lock, mean)
length.density.and.lock <- sapply(density.and.lock, length)
save(density.and.lock, file = "stepndensity.rda")

plot(c(0.5,0.6,0.7,0.8,0.9),mean.density.and.lock, type = "l", lwd = 3, col = "green",
     xlab = "Density", ylab = "Steps till Gridlock",
     main = "Steps till Gridlock vs Density")

plot(c(0.5,0.6,0.7,0.8,0.9),length.density.and.lock, type = "l", xlim = c(0,1),
     ylim = c(-10,100),
     xlab = "Density", ylab = "Percentage of Gridlock",
     main = "Percentage Gridlock vs Density", lwd = 3, col = "blue")
lines(c(0.4,0.5),c(0,length.density.and.lock[1]), lwd = 3, col = "blue")
lines(c(0,0.4), c(0,0), lwd = 3, col = "blue")

#run the function with fixed density and shape but different size
lc2 <- lock.count(2,2,0.8)
lc5 <- lock.count(5,5,0.8)
lc10 <- lock.count(10,10,0.8)
lc20 <- lock.count(20,20,0.8)
lc50 <- lock.count(50,50,0.8)
lc100 <- lock.count(100,100,0.8)

#Merging the data and calculate the mean
size.and.lock <- list(size4 = lc2,size25 = lc5, size100 = lc10, size400 = lc20,
                      size2500 = lc50, size10000 = lc100)
mean.size.and.lock <- sapply(size.and.lock,mean)
length.size.and.lock <- sapply(size.and.lock, length)
save(size.and.lock, file = "stepnsize.rda")

#make a plot
plot(c(4,25,100,400,2500,10000),mean.size.and.lock,type ="l",
     xlab = "Size of the Grid", ylab = "Steps till Gridlock",
     main = "Steps till Gridlock vs Size of the Grid", lwd  = 3,col = "green")
plot(c(4,25,100,400,2500,10000),length.size.and.lock,type ="l",
     xlab = "Size of the Grid", ylab = "Percentage of Gridlock",
     main = "Percentage Gridlock vs Size of the Grid", lwd = 3, col = "blue")

#run this function with fixed density and size but different shape
rl2 <- lock.count(2,200,0.9)
rl5 <- lock.count(5,80,0.9)
rl10 <- lock.count(10,40,0.9)
rl20 <- lock.count(20,20,0.9)
rl40 <- lock.count(40,10,0.9)
rl80 <- lock.count(80,5,0.9)

#Merging the data and calculate the mean
shape.and.lock <- list(rl2,rl5,rl10,rl20,rl40,rl80)
mean.shape.and.lock <- sapply(shape.and.lock,mean)
length.shape.and.lock <- sapply(shape.and.lock,length)
save(shape.and.lock,file = "stepnshape.rda")

#Make a plot
plot(c(2/200,5/80,10/40,20/20,40/10,80/5),mean.shape.and.lock, 
     ylim = c(0,20),type = "l", lwd = 3, col = "green",
     xlab = "Row/Column Ratio", ylab = "Steps till Gridlock",
     main = "Steps till Gridlock vs Shape")

plot(c(2/200,5/80,10/40,20/20,40/10,80/5),length.shape.and.lock, 
     type = "l", lwd = 3, col = "blue",
     xlab = "Row/Column Ratio", ylab = "Percentage of Gridlock",
     main = "Percentage of Gridlock vs Shape")





