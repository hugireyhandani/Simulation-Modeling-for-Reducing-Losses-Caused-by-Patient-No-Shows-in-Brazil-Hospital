# DATA PREPARATION
noshows <- KaggleV2_May_2016

# Convert "Yes" and "No" values to binary
noshows$No_show_binary <- as.integer(noshows$`No-show` == "Yes")

# Calculate the no-show rate
sum(noshows$No_show_binary) / nrow(noshows)

# Find the distribution for Hospital Charge Data
library(readxl)
library(tidyverse)
library(extraDistr)
library(fitdistrplus)
library(matrixcalc)
library(Matrix)
library(MultiRNG)
library(readxl)


data <- X042103565_EmersonHospital_standardcharges
fillter_data <- data %>% filter(data$`GROSS CHARGES` >= 50 & data$`GROSS CHARGES` <= 300)
hist(fillter_data$`GROSS CHARGES`)
dist1_Gross_Charges <- fitdist(data=fillter_data$`GROSS CHARGES`,"norm")
dist2_Gross_Charges <- fitdist(data=fillter_data$`GROSS CHARGES`,"cauchy")
dist3_Gross_Charges <- fitdist(data=fillter_data$`GROSS CHARGES`,"logis")
dist4_Gross_Charges <- fitdist(data=fillter_data$`GROSS CHARGES`,"gamma")
gofstat(list(dist1_Gross_Charges,dist2_Gross_Charges,dist3_Gross_Charges,dist4_Gross_Charges))
hist(fillter_data$`GROSS CHARGES`)
B <- gofstat(dist4_Gross_Charges)
B

# Based on above, Gamma Distribution was chosen


# Set seed for reproducibility
set.seed(42)
# Generate bootstrap samples
average_cost_samples <- rgamma(10000, shape = 3.857, rate = 0.0277)
hist(average_cost_samples)

mean(average_cost_samples)
# Determine the average cost of an appointment
mean(average_cost_samples)*sum(noshows$No_show_binary)

# Replace "average_cost" with the actual value obtained from research or consultation
average_cost <- average_cost_samples





##Optimization Part
install.packages("GA")

# Load necessary packages
library(GA)


# SIMULATION MODEL
simulate_appointments <- function(interventions, noshows) {
  # Apply the interventions (e.g., reducing no-show rates)
  SMS_received_effect <- interventions[1] * 12.65
  flexible_scheduling_effect <- interventions[2] * 18.6
  noshows$Adjusted_No_show <- noshows$No_show_binary * (1 - SMS_received_effect * noshows$SMS_received - flexible_scheduling_effect)
  
  # Calculate the total cost of missed appointments
  total_no_shows <- sum(noshows$Adjusted_No_show)
  total_cost <- mean(average_cost_samples) * total_no_shows
  
  return(total_cost)
}

# OBJECTIVE FUNCTION
objective_function <- function(interventions) {
  return(simulate_appointments(interventions, noshows))
}

# Set GA parameters
n_interventions <- 2
lower_bounds <- rep(0, n_interventions)
upper_bounds <- rep(1, n_interventions)

# Run the Genetic Algorithm
result <- ga(type = "real-valued",
             fitness = objective_function,
             lower = lower_bounds,
             upper = upper_bounds,
             popSize = 50,
             maxiter = 100,
             run = 200)

# Get the best interventions
best_interventions <- result@solution

# Print the best interventions and cost
cat("Best Interventions:\n")
print(best_interventions)
cat("Total cost with the best interventions: ", result@fitnessValue, "\n")

