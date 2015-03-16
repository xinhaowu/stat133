#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
    
        
        vec.init <- sample(c(0:2),size=r*c,replace = TRUE,
                           prob = c(1-p, p/2, p/2))
        m <- matrix(data = vec.init,nrow = r,ncol = c)
  
   return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

shiftN <- function(x) {
        n <- length(x)
        y <- rep(0, n)
        
        up <- function(x) {
                if (x == n) return (1)
                else return(x+1)
        }
        
        down <- function(x) {
                if (x == 1) return (n)
                else return(x-1)
        }
        
        for(i in seq_len(n)) {
                if (x[i] == 0) 
                {
                        if(x[up(i)] == 1){
                                y[i] = 1
                                y[up(i)] = 0
                        }
                }
                else if (x[i] == 1)
                {
                        if(x[down(i)] != 0)
                                y[i] = 1
                        
                }
                else y[i] = x[i]
        }
        return(y)
}

shiftE <- function(x) {
        n <- length(x)
        y <- rep(0, n)
        
        up <- function(x) {
                if (x == n) return (1)
                else return(x+1)
        }
        
        down <- function(x) {
                if (x == 1) return (n)
                else return(x-1)
        }
        
        for(i in seq_len(n)) {
                if (x[i] == 0) 
                {
                        if(x[down(i)] == 2){
                                y[i] = 2
                                y[down(i)] = 0
                        }
                }
                else if (x[i] == 2)
                {
                        if(x[up(i)] != 0)
                                y[i] = 2
                        
                }
                else y[i] = x[i]
        }
        return(y)
}

bml.step <- function(m){
        
        prev.m <- m
        step.north <- apply(m,2,shiftN)
        m  <- t(apply(step.north,1,shiftE))
        grid.new <- all(prev.m == m)
        
   return(list(m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
        
        m <-bml.init(r, c, p)
        count <- 0
        grid.lock <- FALSE
        while(grid.lock == FALSE & count < 1000 )
        {grid.lock <-bml.step(m)[[2]]
        count <- count+1
        m <- bml.step(m)[[1]]
        }
return(list(grid.lock, count, r, c, p))
        
}
