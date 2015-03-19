# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
        
        has_adopted <- matrix(ncol = n.days, nrow = n.doctors)
        has_adopted[,1] <- initial.doctors
        
        for(i in 1:n.days-1){
                
                doc.meet <- sample(x=n.doctors, size = 2)
                
                if(initial.doctors[doc.meet[1]] - initial.doctors[doc.meet[2]] == 1){
                        
                        initial.doctors[doc.meet[2]] <- sample(c(0,1),size = 1, prob = c(1-p,p))
                }
                else if (initial.doctors[doc.meet[1]]-initial.doctors[doc.meet[2]] == -1){
                        
                        initial.doctors[doc.meet[1]] <- sample(c(0,1),size = 1, prob = c(1-p,p))
                }
                
                
                has_adopted[,i+1] <- initial.doctors

        }
        
        return(has_adopted)
}


  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output


# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
initial.doctors <- sample(c(0,1),size  = 100, prob = c(0.9,0.1), replace = TRUE)
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

result0.5 <- sim.doctors(initial.doctors,100,1000,0.5)
doc.sum0.5 <- apply(result0.5,2,sum)
result0.4 <- sim.doctors(initial.doctors,100,1000,0.4)
doc.sum0.4 <- apply(result0.4,2,sum)
result0.3 <- sim.doctors(initial.doctors,100,1000,0.3)
doc.sum0.3 <- apply(result0.3,2,sum)
result0.2 <- sim.doctors(initial.doctors,100,1000,0.2)
doc.sum0.2 <- apply(result0.2,2,sum)
result0.6 <- sim.doctors(initial.doctors,100,1000,0.6)
doc.sum0.6 <- apply(result0.6,2,sum)

plot(seq(1:1000),doc.sum0.5, col = "red", type = "l", 
     xlab = "Days",ylab = "Percentage of Doctors Adopted", main = "Use of a New Drug")
lines(seq(1:1000), doc.sum0.4, col = "blue")
lines(seq(1:1000), doc.sum0.3, col = "green")
lines(seq(1:1000), doc.sum0.2, col = "purple")
lines(seq(1:1000), doc.sum0.6, col = "yellow")

legend(x=100,legend = c("p=0.2", "p=0.3", "p=0.4", "p=0.5", "p=0.6"),
       col = c("purple", "green", "blue", "red", "yellow"), 
       bg = "gray90", lwd = c(1, 1, 1, 1, 1), cex = 0.6)