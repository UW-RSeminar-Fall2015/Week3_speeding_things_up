#### Bookkeeping ####

options(digits = 6)

#function to calculate soil temperature at various soil depths
Get_SoilT3 <- function(t,z,D,Tave,A0) { #Campbell and Norman 1998 eq. 8.6
  t2 <- t * 3600 #convert hours to seconds
  t02 <- t0 * 3600 #convert hours to seconds
  Temp <- Tave + A0 * exp(-z/D) * sin((w*(t2-t02)) - (z/D))
  c(t,z,D,Tave,A0,Temp)
}


#Constants
tau <- 24 * 3600 #period of temperature fluctuation (1 day) in seconds
w <- (2*pi)/tau #angular frequency (Campbell and Norman 1998 eq. 8.7)
t0 <- 8 #phase shift (adjusts so that the minimum and maximum temps are at the correct time)


#Make sure the function works
Get_SoilT3(t = 12, z = 0.05, D = 0.1, Tave = 20 , A0 = 10)


#Vectors for simulations
tvec <- seq(from = 0, to = 22, by = 2) #time of day in hours 
zvec <- seq(from = 0, to = 1.0, by = 0.2) #depth in meters 
Dvec <- seq(from = 0.03, to = 0.15, by = .03) #dampening in moist organic soils 
Tavevec <- seq(from = 10, to = 30, by = 5) #average temperature at soil surface over a temperature cycle (c) 
A0vec <- seq(from = 10, to = 22, by = 2) #surface amplitude (1/2 of peak to peak variation)

LoopLength <- length(tvec)*length(zvec)*length(Dvec)*length(Tavevec)*length(A0vec) #my original loop required 8,065,596 calcs


#### Classic For Loop ####

#Create dataframe to build onto
ptm <- proc.time() #start time

SoilSim.1 <- data.frame()

for (ti in tvec) { 
  for (Di in Dvec) {   
    for (Tavei in Tavevec) { 
      for (A0i in A0vec) {
        for (zi in zvec) {
         Temper <- Get_SoilT3(t = ti, z = zi, D = Di, Tave = Tavei , A0 = A0i) #Calculate temperature
         SoilSim.1 <- rbind(SoilSim.1, Temper)
        }
      }
    }
  }
  print(c(ti, Di, Tavei, A0i, zi)) #gives loop's progress
}

colnames(SoilSim.1) <- c("t","z","D","Tave","A0", "Temperature")

proc.time() - ptm



#### Why the above loop structure is so slow in R ####
  # It is not vectorized (computer is constantly picking up and putting back elements for the calculation)
  # Has to build a new dataframe with each iteraction (R is unable to add rows to dataframe, recreates the entire thing each time)
  # Generally, preferable to use *apply family of functions when possible, but these often are non-intuitive and you cannot parallel process


#### Using foreach and doParallel ####

library(foreach)
library(doParallel)

# first register doparallel
registerDoParallel() # grabs 2 cores on my mac and 3 on PC
getDoParWorkers()

# Calculate the soil temps (puts values in a list, which is fastest)
ptm <- proc.time() #start time

SoilSim.2 <- foreach(t = tvec) %:%
  foreach(D = Dvec) %:%
  foreach(Tave = Tavevec) %:%
  foreach(A0 = A0vec) %:%
  foreach(z = zvec)  %do% { # '%dopar%' for parallel computing
    Get_SoilT3(t, z, D, Tave, A0)
  }

#Convert the list to a dataframe and rename columns
SoilSim.2_df <- data.frame(matrix(unlist(SoilSim.2), ncol=6, byrow=T),stringsAsFactors=FALSE)
colnames(SoilSim.2_df) <- c("t","z","D","Tave","A0", "Temp")

proc.time() - ptm #finish time

## Time comparison: this code on my MAC took approx 8 hours whereas the original loop (using for) took approx 115hrs to complete only 65% of the simulation 



