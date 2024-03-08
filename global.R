## objects created in global.R
## can be used in server.R

# packages ----------------------------------------------------------------

# packages that are used
library(shiny)
library(DT)
library(tidyverse)
library(explore)


# data --------------------------------------------------------------------

data <- use_data_beer()

# add description for help-tab
data_title <- "Learn data science with beer"
  
data_description <- paste(
  "This datasets contain data of 161 beer from Austria, Germany and Swizerland.")

data_variables <- tribble(
  ~variable, ~description,
  "name", "Name of the beer (Name)",
  "brand", "brand name (Marke)",
  "country", "country (Land)",
  "year", "year when data was collected (Jahr)",
  "type", "type of beer (Art des Biers)",
  "color_dark", "is the color dark (Dunkles Bier?)",
  "alcohol_vol_pct", "alcohol in volumne % (Alkoholgehalt)",
  "original_wort", "original wort (Stammwuerze)",
  "energy_kcal_100ml", "kcal in 100 ml (Energiegehalt)",
  "carb_g_100ml", "gram carbs in 100 ml (Kohlenhydrate)",
  "sugar_g_100ml", "gram sugar in 100 ml (Zuckergehalt)"
)

# prepare for exploration -------------------------------------------------

data <- data
data_title <- data_title
data_description <- data_description
data_variables <- data_variables

target_quo <- NA
target_text <- NA

# define variables for CRAN-package check
type <- NULL
variable <- NULL

tbl_guesstarget <- describe(data) %>%
  filter(type %in% c("lgl","int","dbl","num","fct","chr")) %>%
  select(variable)
guesstarget <- as.character(tbl_guesstarget[[1]])

# hello 

# check all variables if usable
for (i in names(data))  {
  if (explore::get_type(data[[i]]) == "other")  {
    data[[i]] <- "<hide>"
  }
}

