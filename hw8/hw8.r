xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  
        return(unlist(tapply(y, x, sample, 10, replace = rep)))

}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  err.idx <- sample(length(err), length(err), replace = rep)
 
  return(fit+err[err.idx])
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if (degree == 1) coeff <- lm(y~x)[1]
  if (degree == 2) coeff <- lm(y~x + I(x^2))[1]
 
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  
  if (is.null(fit)) return(fitModel(data[,1],genBootY(data[,1],data[,2]),degree = degree))
  else return(fitModel(data[,1],genBootR(fit[,1],fit[,2]), degree = degree))
 
  ### Use fitModel to fit a model to this bootstrap Y 
 
}

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic
        
  fitR1 = cbind(lm(data[,2]~data[,1])$fitted, lm(data[,2]~data[,1])$residuals)
  fitR2 = cbind(lm(data[,2]~data[,1] + I(data[,1]^2))$fitted, 
                lm(data[,2]~data[,1] + I(data[,1]^2))$residuals)

  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  bootY1 = replicate(B, oneBoot(data = data))
  bootY2 = replicate(B, oneBoot(data = data, degree = 2))
  bootR1 = replicate(B, oneBoot(data = data, fit = fitR1))
  bootR2 = replicate(B, oneBoot(data = data, fit = fitR2, degree = 2))
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  mbootY1 = rbind(sapply(bootY1,"[",1), sapply(bootY1,"[",2))
  mbootY2 = rbind(sapply(bootY2,"[",1), sapply(bootY2,"[",2), sapply(bootY2,"[",3))
  mbootR1 = rbind(sapply(bootR1,"[",1), sapply(bootR1,"[",2))
  mbootR2 = rbind(sapply(bootR2,"[",1), sapply(bootR2,"[",2), sapply(bootR2,"[",3))
  
  names(mbootY1)
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  

  coeff <- list(mbootY1,mbootY2,mbootR1,mbootR2)
        
  return(coeff)
} 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data
  plot(x,y)
  ### Add lines or curves for each row in coeff
  ### Use transparency
  col = rainbow(1,alpha = 0.01)
  ### You should use mapply to construct all
  if (nrow(coeff) == 2) {
          mapply(function(x) abline(coeff[,x],col = col), 1:dim(coeff)[2])

  }
  if (nrow(coeff) == 3) {
  mapply(function(a,b,c) {curve(a +b*d + c*d^2,0,5,add = TRUE,xname = "d",col = col)
                          }, coeff[1,],coeff[2,],coeff[3,])
  }
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out
  curve(trueCoeff[1]+trueCoeff[2]*d+trueCoeff[3]*d^2, xname = "d", lwd = 3,
        add = TRUE)

}

### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}
