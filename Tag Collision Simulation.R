#### Tag Interval Collision Simulation
#### Written by: Stephen Scherrer
#### Written on: 2 Feb 2015

#### A script to simulate the number tag transmissions
#### discarded by a receiver due to interference from a
#### variable number of other transmission sources. 


#### Clearning Workspace and setting directory ---------
rm(list=ls()) # Clear workspace
setwd('/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Acoustic Network Design/Collision Simulation/')

#### Importing principle dependencies ------------------
#install.packages('plyr')
#install.packages('dplyr')
library('plyr') # functions: alply
library('dplyr') # functions: between
#### User defined parameters ---------------------------
  ## Number of Iterations - Number of times to run the simulation
    n_iterations = 100
  ## Number of Tags to test
    n_tags = 3
  ## Transmission Interval - for this simulation, this takes the form of a 
    ## normal distribution with mean 60 and 95% of detections between the 
    ## upper and lower interval bounds perscribed by Vemco, but should be 
    ## updated in subsequent versions to pull from a known interval
    ## distribution from eskie test
    transmission_distribution = rnorm(n = 1000, mean = 60, sd = 15) # possible ways to do this... sample (once I have data), runif (random uniform), rnorm (random normal)
  ## Transmission length - Length of time in seconds for which the tag is 
    ## actively transmitting, as calculated from our bathtub test
    transmission_length = 2.8 #seconds
  ## Blanking Interval - period of time the VR2W receiver unit shuts down to detection of other tags
    blanking_interval = 260 # milliseconds

#### Functions ------------------------------------------
  simulate_transmission_array = function(n_iterations, n_tags, 
                            transmission_interval, 
                            transmission_length,
                            blanking_interval){
    # Builds array of simulated transmission times for each tag. Each row is a tag
      # Frame 1 is begining time of transmission, Frame 2 is after blanking period
    n_samples = (60*60*24)/mean(transmission_distribution)
    transmission_array = array(0, c(n_tags, n_samples, 2)) # First frame of array is start of transmission, second frame is end
    transmission_array[ ,1, 1] = sample(x = transmission_distribution, 
                               size = n_tags, replace = TRUE)
    for (i in 2:n_samples){
      transmission_array[ ,i, 1] = (transmission_array[ ,i-1, 1] 
                                    + sample(x = transmission_distribution, 
                                              size = n_tags, replace = TRUE))
    }
    transmission_array[ , ,2] = (transmission_array[ , ,1] 
                                 + transmission_length + blanking_interval*.001)
    return (transmission_array)
  }

test_collisions = function(transmission_array){
  ## Takes a transmission array dimensions (n_tags, n_iterations, 2) and tests
    ## against other transmission ranges for collisions
  all_detections = uncolided_detections = length(unique(transmission_array[ , ,1]))
  start_transmissions_vec = as.vector(unlist(
    alply(.data = transmission_array[ , ,1],
          .margins = c(1,2))))
  transmission_list = as.list(alply(.data = transmission_array, 
                                    .margins = c(1,2)))
  for (i in 1:length(transmission_list)){
    # subtracting the number of tranmsissions that begin during another tranmsision interval
      # have to add one becaue a transmission will test true with itself, but does not preclude
      # interfeer with itself
    uncolided_detections = (uncolided_detections 
                            - length(start_transmissions_vec[
                              between(start_transmissions_vec, 
                                      left = transmission_list[[i]][1], 
                                      right = transmission_list[[i]][2])]) 
                            + 1) # add one becaue a transmission will test true 
    # with itself, but does not interfeer with itself
  }
  fraction_of_transmissions_returned = uncolided_detections/all_detections
  return(fraction_of_transmissions_returned)
}

run_simulation = function(n_iterations, n_tags, 
                          transmission_interval, 
                          transmission_length,
                          blanking_interval){
  recovery_rate = rep(0, n_iterations)
  for (i in 1:length(recovery_rate)){
    recovery_rate[i] = test_collisions(simulate_transmission_array(
      n_iterations, n_tags, 
      transmission_interval, 
      transmission_length,
      blanking_interval))
  }
  print(fivenum(recovery_rate))
  return(recovery_rate)
}

#### Usage ----------------------------------------
test_run = run_simulation(n_iterations = n_iterations, n_tags = n_tags, 
                  transmission_interval = transmission_interval, 
                  transmission_length = transmission_length,
                  blanking_interval = blanking_interval)
