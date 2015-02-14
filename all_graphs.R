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
      tbl_df() %>%
      select(-Pollutant)
SCC <- 
      readRDS("Source_Classification_Code.rds")  %>%
      tbl_df() %>%
      select(SCC, Data.Category, Short.Name)

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
## graph 1                     ##
#################################
png(filename = "plot1.png",
    width = 480, height = 480, units = "px")

graphdata <- 
      group_by(my_data, year) %>%
      summarise(totalEmissions = sum(Emissions))
with(graphdata,
     plot(year,
          totalEmissions,
          main = 'Plot 1: Total US emissions'
          )
)
lm(totalEmissions ~ year, graphdata) %>%
      abline()

dev.off()

#################################
## graph 2                     ##
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

#################################
## graph 3                     ##
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

#################################
## graph 4                     ##
#################################
png(filename = "plot4.png",
    width = 480, height = 480, units = "px")

filter(my_data, grepl('[cC]oal', Short.Name)) %>%
      group_by(year) %>%
      summarise(totalEmissions = sum(Emissions)) %>%
      qplot(year,
            totalEmissions,
            main = 'Plot 4: Total US coal emissions',
            data = .,
            geom=c('point', 'smooth'),
            method='lm'
            )

dev.off()

#################################
## graph 5                     ##
#################################
png(filename = "plot5.png",
    width = 480, height = 480, units = "px")

filter(my_data, grepl('Onroad|Nonroad', Data.Category)) %>%
      filter(fips == '24510') %>%
      group_by(year) %>%
      summarise(totalEmissions = sum(Emissions)) %>%
      qplot(year,
            totalEmissions,
            main = 'Plot 5: Total motor vehicule emissions for Baltimore',
            data = .,
            geom=c('point', 'smooth'),
            method='lm'
            )

dev.off()

#################################
## graph 6                     ##
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
