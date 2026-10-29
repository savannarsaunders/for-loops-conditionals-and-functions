## for loops
library(tidyverse)


# For loops ---------------------------------------------------------------
#1. Create a for loop that prints each integer from 1 to 25
for(Integer in 1:25) {
  print(Integer)
}

#2. Create a for loop that prints all the unique species from the ‘fish_psu_density’ dataset
fish_density <- read_csv("data/fish_psu_density.csv")

for(uni_sp in fish_density) {
  uni_sp <- unique(fish_density$SPECIES_CD)
}
print(uni_sp)

#3. Create a for loop that prints the square of each integer from 1 to 10
for(square in 1:10){
  square <- square^2
  print(square)
}

#4. Create a for loop that prints only the odd numbers from a sequence of integers from 1 to 20
for(odd in 1:20){
  odd_num <- odd[odd %% 2 !=0]
  print(odd_num)
}

#5. Create a for loop that prints integer ‘is even’ or ‘is odd’ for a sequence of integers from 1 to 10
#Because there are only 2 conditions its ok to do if and else if but if more than two then use else if
for(Integer in 1:10){
  if(Integer %% 2 ==0) {
    print(paste(Integer, "is even"))
  }
  
  else if(Integer %% 2 !=0) {
    print(paste(Integer, "is odd"))
  }
}
#Simplified version
for(Integer in 1:10){
  if(Integer %% 2 ==0) {
    print(paste(Integer, "is even"))
  }
  
  else{
    print(paste(Integer, "is odd"))
  }
}

#6. Create a for loop that inserts all the unique species from ‘fish_psu_density’ into a new vector
unique_sp <- c() # create an empty vector

for(spp in unique(fish_density$SPECIES_CD)) {
  unique_sp <- append(unique_sp, spp)
  print(unique_sp) #this will show the progress of it printing each unique species
}


# Creating functions ------------------------------------------------------
#7. Create a function that squares any given value

square_value <- function(value) {
  val <- value^2
  print(val)
}

square_value(4)

#8. Create a function that calculates the circumference and area of a circle given any radius

circ_area_radius <- function(radius) {
  circ <- 2*pi*radius
  print(paste("circumference = ", circ))
  area <- pi*(radius^2)
  print(paste("area = ", area))
}
circ_area_radius(3)

#9. Create a function that returns the number of samples per YEAR and STRAT from the ‘fish_psu_density’ dataframe

num_samples <- function() {
  samples_per_YEARSTRAT <- fish_density |> 
    select(YEAR,STRAT) |> 
    n_distinct()
  print(samples_per_YEARSTRAT)
}
num_samples()

#10. Create a function that returns a point and line plot (+- standard error) of mean density over time by REGION for any species. Use the ‘fish_psu_density’ dataset.

graph_density <- function(data, species) {
  density_time <- {{data}} |> 
    filter(SPECIES_CD == {{species}}) |> 
    group_by(YEAR) |> 
    summarize(
      mean_den = mean(density),
      SEM_ = sd(density) / sqrt(length(density))
    )
  
  ggplot(density_time, aes(YEAR, mean_den))+
    geom_point()+
    geom_line()+
    geom_errorbar(aes(ymin = mean_den - SEM_, 
                      ymax = mean_den + SEM_), width = 0.2) +
    theme_minimal()+
    ylab("Mean Density")+
    xlab("Year")+
    labs(title = paste("Mean densisty by year for", species))
}

graph_density(fish_density, "OCY CHRY")
