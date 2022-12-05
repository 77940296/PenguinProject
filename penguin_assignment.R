## -------Penguin assignment----------------------
##
## Script name: penguin_assignment.r
##
## Purpose of script: Loads and cleans penguin data, creates a plot exploring the interactions between flipper
# length, culmen length and species (Q4)
##      
##   
## 
# Load the packages-----------------------

library(palmerpenguins)
library(ggplot2)
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(ragg)
library(svglite)

# Functions-------------
# ---- Cleaning----------------------
# Clean column names, remove empty rows, remove columns called comment and delta
cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}

# ---- Plots -------------

# Creating a linear regression model to communicate the answer of a statistical test
# Relationship between Flipper and Culmen Length

plot_lm <- function(pengiuns_clean){
  ggplot(penguins_clean, aes(x=flipper_length_mm, y=culmen_length_mm))+geom_smooth(method=lm)+
    geom_point(colour="cadetblue4")+ labs(title= "Bill length versus Flipper length for Palmer Penguins", x = "Flipper length (mm)",
                       y = "Culmen length (mm)") +
    theme_bw()}

# --- Saving the plots ------


# Saving the linear regression model plot


save_lm_svg <- function(penguins_clean, 
                              filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  lm_plot <- plot_lm(penguins_clean)
  print(lm_plot)
  dev.off()
}

# ------------------------END OF FUNCTIONS-------------------------------------------
# Loading the data and running the code---------

penguins_raw <- read.csv("data_raw/penguins_raw.csv")

# Clean the data- fixing column names, removing empty rows and columns called comment and delta

penguins_clean <- cleaning(penguins_raw)

# save the cleaned data
write.csv(penguins_clean, "data_clean/penguins_clean.csv")


# Plotting-----------------------


# Plotting the linear model

lm_plot <- plot_lm(penguins_clean)


# Running statistical analysis---------------

linear_model <- lm(flipper_length_mm~culmen_length_mm, data=penguins_clean)
plot(linear_model, which = 2)
plot(linear_model, which = 1)

summary(linear_model)
anova(linear_model)

# Saving figures --------


# Save the linear model for a report

save_lm_svg(penguins_clean, 
            "figures/fig01_vector.svg", 
            size = 20, scaling = 1)



