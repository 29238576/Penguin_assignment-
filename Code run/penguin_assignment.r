## Script name: penguin_assignment.r

## Purpose of script:
## Loads penguin data, cleans it, and plots the body mass against flipper length, 
## and saves the plot to a file
## Research Question: can body mass be used to predict flipper length in different penguin species 

#### STEP 1) Loading and saving the data
##Load the required packages
library(palmerpenguins)
library(ggplot2)
library(janitor)
library(dplyr)

##set working directory 
setwd("~/Penguins")

##preserving the raw data before editing it 
write.csv(penguins_raw, paste0("raw_data/penguins_raw.csv"))

#load the raw data
penguins_raw_data <- read.csv("raw_data/penguins_raw.csv")

### Step 2) Defining functions 
#cleaning function which cleans column, names, removes empty rows, removes columns called comments and delta
cleaning <- function(penguins_raw_data) {
  penguins_raw_data %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

### STEP 3) Cleaning the data
##Fix the column names, remove empty rows, remove columns called comments and delta using the cleaning function
penguins_clean_data <- cleaning(penguins_raw_data)

##save the cleaned data
write.csv(penguins_clean_data, "clean_data/penguins_clean_data.csv")

### STEP 4) Running a statistical test on the data
##visualize the data 
head(penguins_clean_data)

##fit a linear model to the cleaned data using flipper length as the response variable and body mass as the explanatory variable
penguins_model1 <- lm(flipper_length_mm ~ body_mass_g, penguins_clean_data) 

#summary of the model produced
summary(penguins_model1)

#Run an ANOVA for the model
anova(penguins_model1)

# R squared of 0.759 qnd P-value of less than 0.05 indicates that there is a significant relationship between the two variables
# and indicates that can use body mass to predict flipper length 

##Investigate this trend in Adelie Penguins
adelie <- filter(penguins, species =="Adelie")
adelie_model1 <- lm(flipper_length_mm ~ body_mass_g, adelie)
summary(adelie_model1)
anova(adelie_model1)
#R squared value 0.2192 and P value of less than 0.05 indicates that there is a significant relationship between the two variables
#indicates that body mass can be used to predict flipper length in Adelie penguins

#Chinstrap Penguins
chinstrap <- filter(penguins, species =="Chinstrap")
chinstrap_model1 <- lm(flipper_length_mm ~ body_mass_g, chinstrap)
summary(chinstrap_model1)
anova(chinstrap_model1)
#R2 value for regression is 0.4116 and P value of less than 0.05 indicates that there is a significant relationship between the two variables
#indicates that body mass can be used to predict flipper length in Chinstrap penguins

#Gentoo penguins
gentoo <- filter(penguins, species =="Gentoo")
gentoo_model1 <- lm(flipper_length_mm ~ body_mass_g, gentoo)
summary(gentoo_model1)
anova(gentoo_model1)
#R2 value of 0.4896 and P value of less than 0.05 indicates  that there is a significant relationship between the two variables
#indicates that body mass can be used to predict flipper length in Gentoo penguins

### STEP 5) Creating a figure for the data
##Plot the flipper length as a linear model of body mass for all the species
plot_bodymass_flipper_figure <- function(penguins_clean_data){
  penguins_clean_data %>%  
    ggplot(aes(x = body_mass_g,
               y = flipper_length_mm,
               colour = species,
               shape = species)) +
    geom_point(
      size = 3,
      alpha = 1.0) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_jitter(position = position_jitter(width = 0.4, seed = 0)) +
    theme_bw() +
    theme(title = element_text(face = "bold")) +
    scale_colour_manual(values = c("darkorange","purple","cyan4")) +
    labs(title = "A linear regression model between body mass and flipper length",
         subtitle = "Relationship between body mass and flipper length for Adelie, Chinstrap and Gentoo penguins",
         x = "Body Mass (g)",
         y = "Flipper Length (mm)",
         colour = "Penguin Species",
         shape = "Penguin Species")
}

## Save the code for the plot and plot the graph
bodymass_flipper_figure <- plot_bodymass_flipper_figure(penguins_clean_data)
bodymass_flipper_figure


### STEP 6) Saving the figure 
#save the figure as an object
ggsave(filename = "result_bodymass_flipper_figure.png",
       plot = bodymass_flipper_figure,
       height = 24, width = 24,
       units = "cm")
