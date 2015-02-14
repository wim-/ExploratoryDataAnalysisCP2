## Exploratory Data Analysis - course project 2
## by: github.com/wim-
## This script:
##    1) loads the required packages
##    2) reads the data, cleans it up, filters required dates
##    3) creates a graph
## nota bene:
##    The script below assumes the data is contained
##    in 1 RDS file called summarySCC_PM25



#################################
## step 1: load packages       ##
#################################
if (!require("dplyr")) {
      install.packages("dplyr")
      library(dplyr)
}
if (!require("ggplot2")) {
      install.packages("ggplot2")
      library(ggplot2)
}

#################################
## step 2: read and clean data ##
#################################
my_data <- 
      readRDS("summarySCC_PM25.rds")  %>%
      tbl_df()

#################################
## step 3: create graph        ##
#################################
png(filename = "plot3.png",
    width = 480, height = 480, units = "px")

filter(my_data, fips == '24510') %>%
      group_by(year, type) %>%
      summarise(totalEmissions = sum(Emissions)) %>%
      qplot(year,
            totalEmissions,
            main = 'Plot 3: Total Baltimore emissions per type',
            data = .,
            facets = type~.,
            geom=c('point', 'smooth'),
            method='lm'
            )

dev.off()
