## Exploratory Data Analysis - course project 2
## by: github.com/wim-
## This script:
##    1) loads the required packages
##    2) reads the data, cleans it up, filters required dates
##    3) creates a graph
## nota bene:
##    The script below assumes the data is contained
##    in 2 RDS files called summarySCC_PM25 and Source_Classification_Code



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
NEI <- 
      readRDS("summarySCC_PM25.rds")  %>%
      tbl_df()
SCC <- 
      readRDS("Source_Classification_Code.rds")  %>%
      tbl_df()

my_data <- left_join(NEI, SCC)
rm(NEI, SCC)

## create df to link relevant city names to fips identifier
city <- data.frame(
      c('Baltimore City, Maryland', 'Los Angeles County, California'),
      c('24510', '06037')
      ) %>%
      tbl_df()
colnames(city) <- c('city', 'fips')

my_data <- left_join(my_data, city)
rm(city)

#################################
## step 3: create graph        ##
#################################
png(filename = "plot6.png",
    width = 480, height = 480, units = "px")

filter(my_data, grepl('Onroad|Nonroad', Data.Category)) %>%
      filter(grepl('24510|06037', fips)) %>%
      group_by(year, city) %>%
      summarise(totalEmissions = sum(Emissions)) %>%
      qplot(year,
            totalEmissions,
            main = 'Plot 6: Total motor vehicule emissions for Baltimore and LA',
            data = .,
            facets = city~.,
            geom=c('point', 'smooth'),
            method='lm'
      )

dev.off()
