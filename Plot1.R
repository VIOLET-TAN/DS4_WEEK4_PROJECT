
#DOWNLOAD PACKAGES
library(dplyr)
library(scales)
library(data.table)
library(ggplot2)

##READING DATA FROM •	Data for Peer Assessment [29Mb]

NEI <- readRDS("summarySCC_PM25.rds")

> head(NEI)
#    fips      SCC Pollutant Emissions  type year
#4  09001 10100401  PM25-PRI    15.714 POINT 1999
#8  09001 10100404  PM25-PRI   234.178 POINT 1999
#12 09001 10100501  PM25-PRI     0.128 POINT 1999
#16 09001 10200401  PM25-PRI     2.036 POINT 1999
#20 09001 10200504  PM25-PRI     0.388 POINT 1999
#24 09001 10200602  PM25-PRI     1.490 POINT 1999
tail(NEI)
#          fips        SCC Pollutant   Emissions     type year
#75051171 56011 2282020005  PM25-PRI 0.028598300 NON-ROAD 2008
#75051181 53009 2265003020  PM25-PRI 0.003152410 NON-ROAD 2008
#75051191 41057 2260002006  PM25-PRI 0.046869500 NON-ROAD 2008
#75051201 38015 2270006005  PM25-PRI 1.012890000 NON-ROAD 2008
#75051211 46105 2265004075  PM25-PRI 0.000486488 NON-ROAD 2008
#75051221 53005 2270004076  PM25-PRI 0.001622670 NON-ROAD 2008
dim(NEI)
#[1] 6497651       6
SCC <- readRDS("Source_Classification_Code.rds")
names(SCC)
#[1] "SCC"                 "Data.Category"       "Short.Name"         
#[4] "EI.Sector"           "Option.Group"        "Option.Set"         
#[7] "SCC.Level.One"       "SCC.Level.Two"       "SCC.Level.Three"    
#[10] "SCC.Level.Four"      "Map.To"              "Last.Inventory.Year"
#[13] "Created_Date"        "Revised_Date"        "Usage.Notes"  
dim(SCC)
#[1] 11717    15

#Question 1.	Have total emissions from PM2.5 decreased in the United States 
#from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission 
#from all sources for each of the years 1999, 2002, 2005, and 2008
#My answer to the questions is yes. Here is why.
## plot1.R and plot1 are created to answer the question.
##At first calculate the yearly PM25 emission called yearly that uses dplyr 
##group_by, filter the year of 1999,  2002, 2005, and 2008, and summarize to add up the emissions 
##for each year. Then make a numeric vector called empm
##by dividing the yearly emissions by 1,000,000 that makes emission per million ton 
##then call plot with the total yearly emission as y axis and year as x axis.   
##The below variable pyear=per year, empm=emission per million ton

pyear <- NEI %>% group_by(year) %>% 
  filter(year == 1999|2002|2005|2008) %>%  
  summarize(pyear.Total = sum(Emissions)); 
#Using the function Pretty Breakpoints, compute a sequence of about n+1 equally spaced 
#by default value
empm <- pretty(pyear$pyear.Total/1000000); 
#empm <- as.numeric(pyear$pear.Total/1000000) it return and plot a numeric
yrs <- c(1999,2002,2005,2008)
plot(pyear$year, pyear$pyear.Total/1000000, type = "l", lwd = 4, axes = FALSE,
     xlab = "Year", 
     ylab = expression("Total Tons of PM"[2.5]*" Emissions"), 
     main = expression("Total Tons of PM"[2.5]*" Emissions in the US"));
axis(1, at = yrs, labels = paste(yrs));
axis(2, at = empm, labels = paste(empm, "M", sep = ""));# M stands for millon
box()

# From Plot1, it is a decline in total tons of PM2.5 emissions in the United States 
#over these years of 1999, 2002, 2005, 2008.


