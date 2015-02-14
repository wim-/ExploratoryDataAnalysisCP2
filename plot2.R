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

#################################
## step 2: read and clean data ##
#################################
my_data <- 
      readRDS("summarySCC_PM25.rds")  %>%
      tbl_df()

#################################
## step 3: create graph        ##
#################################
png(filename = "plot2.png",
    width = 480, height = 480, units = "px")

graphdata <- 
      filter(my_data, fips == '24510') %>%
      group_by(year) %>%
      summarise(totalEmissions = sum(Emissions))
with(graphdata,
     plot(year,
          totalEmissions,
          main = 'Plot 2: Total Baltimore emissions'
          )
     )
lm(totalEmissions ~ year, graphdata) %>%
      abline()

dev.off()
