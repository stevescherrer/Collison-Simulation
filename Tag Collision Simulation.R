#### Tag Interval Collision Simulation
#### Written by: Stephen Scherrer
#### Written on: 2 Feb 2015

#### A script to simulate the number tag transmissions
#### discarded by a receiver due to interference from a
#### variable number of other transmission sources. 

#### Script outputs the percentage of transmissions that
#### are not subject to interfeerence and are successfully
#### detected by a receiver.


#### Clearning Workspace and setting directory ---------
rm(list=ls()) # Clear workspace
setwd('/Users/stephenscherrer/Documents/Work/UH/Projects/dissertation work/Acoustic Network Design/Collison-Simulation/')

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
  ## Supression factor - a value out of 1 for which a tag has a probability of interfeering with the detction of other tags
    ## tags with a higher probability of detection have a higher supression value while tags farther from the receiver have 
    ## a lower supression value. Supression factor must be a factor of n_tags. 
    ## When determining supression values for a tag or set of tags, use a ratio from a range test equivilant to
    ##    Return rate of tag in similar condition / maximum return rate of test
    supression_factor = c(1, .97866, .74547) #, .10986, .00007, 0)

#### Functions ------------------------------------------
test_supression_factor = function(supression_factor, n_tags){
  ## A test of the supression factor. supression factor must be an integer, in which case it
    ## is constant for all tags, or a vector of a length which is a factor of the 
    ## total number of tags. 
  if(n_tags %% length(supression_factor) != 0){
    print('supression_factor must be an vector of a length which is a factor of n_tags')
  }
  for (i in 1:length(supression_factor)){
    if (supression_factor[i] >1 | supression_factor[i] < 0){
      print('Values of supression_factor must be < 1')
    }
  }
}

generate_number_of_samples = function(transmission_distribution){
  ## generate a number equivilant to the total amount of transmissions each 
    ## tag will be sampled
  n_samples = (60*60*24) / mean(transmission_distribution)
  return(n_samples)
}

simulate_transmission_array = function(n_tags, 
                            transmission_interval, 
                            transmission_length,
                            blanking_interval,
                            n_samples = generate_number_of_samples(transmission_distribution)){
    # Builds array of simulated transmission times for each tag. Each row is a tag
      # Frame 1 is begining time of transmission, Frame 2 is after blanking period
    # n_samples = (60*60*24)/mean(transmission_distribution)
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

convert_array_to_df = function(transmission_array){
  sim_tag_id = 1:length(transmission_array[ ,1,1])
  transmission_mat = matrix(0,0,3)
  for (j in 1:length(transmission_array[1, ,1])){
    transmission_mat = rbind(transmission_mat, cbind(sim_tag_id, 
                                                     transmission_array[ ,j,1], 
                                                     transmission_array[ ,j,2]))
  }
  transmission_df = as.data.frame(transmission_mat)
  colnames(transmission_df) = c('tag_no', 'start_transmission', 'end_transmission')
  return(transmission_df)
}

bind_supression_factor = function(transmission_df, supression_factor){
  transmission_df$supression_factor = rep(supression_factor, length(transmission_df$tag_no) / length(supression_factor))
  return(transmission_df)
}

simulate_collisions = function(transmission_df){
  ## takes a df of four columns (tag_no, start_transmission, end_transmision, supression_factor)
    ## and compares start of all trasmission iteratively to start-end range of all transmissions.
    ## if there is overlap, 
  collisions = as.data.frame(matrix(0,0,3))
  for (i in 1:length(transmission_df$start_transmission)){
    tag_tested  = which(between(as.vector(transmission_df$start_transmission), 
                                left  = transmission_df$start_transmission[i], 
                                right = transmission_df$end_transmission[i]))
    collisions  = rbind(collisions, cbind(transmission_df$tag_no[tag_tested], 
                                            transmission_df$tag_no[i],
                                            transmission_df$supression_factor[i]))
      }
    ## Removing tags that report colliding with themselves. This is an artifact of
      ## how between function evaluates, but not a real world possibility
    collisions = collisions[which(collisions[ ,1] != collisions[ ,2]), ]
    colnames(collisions) = c('tested_tag', 'tag_collided' ,'collided_supression')
  return(collisions)
}

supress_collisions = function(collisions_df){
  ## For modeling cases where a colliding tag's transmission was not detected by a
    ## receiver, for instance if the tag was sufficient distance away that not all
    ## transmissions would be detected. 
    ## Randomly generates a number between 0 and 1 from a uniform distribution for 
    ## each collision. If randomly generated number is greater than that detection's 
    ## supression factor, the interfeering tag is said to have been undetected and 
    ## collision is said to have not occurred, that is, it is removed from the list
    ## of collisions
  collisions_supression_test = collisions_df[(which(!(runif(n = length(collisions_df$collided_supression), min = 0, max = 1) > collisions_df$collided_supression))), ]
  return(collisions_supression_test)
}
  
generate_return_rates = function(transmission_df, collisions_supressed_df){
  ## function to determine individual return rates for each tag relative to the 
    ## number of collisions that it incurred by other transmission sources that 
    ## were above the supression threshold. 
  indv_return_rate = rep(0, length(unique(transmission_df$tag_no)))
  for (i  in 1:length(unique(transmission_df$tag_no))){
    indv_return_rate[i] = (length(which(transmission_df$tag_no == i)) - length(which(collisions_supressed_df$tested_tag == i))) / length(which(transmission_df$tag_no == i))
  }
  return (indv_return_rate)
}

run_simulation = function(n_iterations, n_tags, 
                          transmission_interval, 
                          transmission_length,
                          blanking_interval,
                          transmission_distribution){
  run_timer = proc.time()
  test_supression_factor(supression_factor, n_tags)
  n_samples = generate_number_of_samples(transmission_distribution)
  pooled_return_rates = c()
  for (i in 1:length(n_iterations)){
    itterate_array = simulate_transmission_array(n_tags, 
                                transmission_interval, 
                                transmission_length,
                                blanking_interval)
    itterate_df = convert_array_to_df(itterate_array)
    itterate_df_with_supression_factor = bind_supression_factor(itterate_df, supression_factor)
    collide_transmissions = simulate_collisions(itterate_df_with_supression_factor)
    collisions_supressed = supress_collisions(collide_transmissions)
    indv_return_rates = generate_return_rates(itterate_df_with_supression_factor, collisions_supressed)
    pooled_return_rates = rbind(indv_return_rates)
  }
  mean_return_rates = colMeans(pooled_return_rates)
  proc.time() - run_timer
  # print(run_timer)
  print(mean_return_rates)
  return(mean_return_rates)
}

#### Usage ----------------------------------------
output = run_simulation(n_iterations = n_iterations, n_tags = n_tags, 
                transmission_interval = transmission_interval, 
                transmission_length = transmission_length,
                blanking_interval = blanking_interval, 
                transmission_distribution = transmission_distribution)