# Load dplyr package
library(dplyr)

# Import and assign read csv to a df
mpg <- read_csv("MechaCar_mpg.csv")

#Perform linear regression and pass all six variables and add df as parameter
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,
   data = mpg)

#Using summary() function determin p-value and r-squared value
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,
          data = mpg))

#Import and assign read csv to a df
sustable <- read.csv("Suspension_Coil.csv")

#Create summary using summarize() function
suspension_summary <- sustable %>%
  summarize(Mean = mean(PSI),
            Median = median(PSI),
            Variance = var(PSI),
            SD = sd(PSI))

#Summarize by manufacturing lot
lot_summary <- sustable %>% group_by(Manufacturing_Lot) %>%
  summarize(Mean = mean(PSI),
            Median = median(PSI),
            Variance = var(PSI),
            SD = sd(PSI))

#t-Tests on Suspension Coils
t.test(sustable$PSI, mu=1500)

#Three more t-Tests for each lot
t.test(subset(sustable, Manufacturing_Lot == "Lot1")$PSI, mu=1500)
t.test(subset(sustable, Manufacturing_Lot == "Lot2")$PSI, mu=1500)
t.test(subset(sustable, Manufacturing_Lot == "Lot3")$PSI, mu=1500)
