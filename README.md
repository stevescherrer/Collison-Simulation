# Collison-Simulation
A script to simulate the collison of Vemco acoustic tag transmissions as a function of the number of tags present. 

This is important for understanding how saturated and enviornement can become when the number of tagged fish are in the area. 
Using this script will allow you to select a tag interval (seconds) that will minimize transmission collisions which result in failed detections.

Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

Prerequisites 
Prior to running code in this repository, you will need to download and install the following software: 
- R (https://www.r-project.org) 
- RStudio (http://rstudio.com)

The following R packages are also required and can be installed with the command (install.packages('package_name') where package name is repalced with the name of the package being installed
  -plyr
  -dplyr

Running a Local Instance
Load this script into RStudio
Prior to running this script, you will need to change the directory path on line 12 to reflect your local directory. 

After sourcing the script file, the function "run_simulation()" becomes available
This function requires the following arguments:
    n_iterations - Number of times to run the simulation
    n_tags -   Number of Tags to simulate
  transmission_distribution - for this simulation, this takes the form of a normal distribution with mean 60 and 95% of detections between the upper and lower interval bounds perscribed by Vemco, but should be updated in subsequent versions to pull from a known interval distribution from eskie test
    an example of this is transmission_distribution = rnorm(n = 1000, mean = 60, sd = 15) # possible ways to do this... sample (once I have data), runif (random uniform), rnorm (random normal)
  transmission_length - Length of time in seconds for which the tag is actively transmitting, as calculated from our bathtub test
  blanking_interval - period of time (ms) the VR2W receiver unit shuts down to detection of other tags
  
 Running the function will return a simulated recovery rate (transmissions detected / transmissions sent) for each tag tag and print a five number summary (min, 1st quantile, median, 3rd quantile, max)
