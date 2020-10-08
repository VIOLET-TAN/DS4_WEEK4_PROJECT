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

##QUESTION2
#Have total emissions from PM2.5 decreased in the Baltimore City, 
#Maryland (fips == "24510".Syntax is fips(x, to = "FIPS"). Fips converts 
#U.S. state names and abbreviations to and from FIPS codes) 
#from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.
##The below variable pyear=per year, empm=emission per million ton

baltimore <- NEI %>% 
  filter(fips == "24510") %>%     #24510 is FIPS CODE of Maryland
  group_by(year) %>% 
  summarize(pyear.Total = sum(Emissions));
baltimore.empm <- pretty(baltimore$pyear.Total/1000);
plot(baltimore$year, baltimore$pyear.Total/1000, type = "l", lwd = 4, axes = FALSE,
     xlab = "Year", 
     ylab = expression("Total Tons of PM"[2.5]*" Emissions"), 
     main = expression("Total Tons of PM"[2.5]*" Emissions in Baltimore"));
axis(1, at = c(1999:2008))
axis(2, at = baltimore.empm, labels = paste(baltimore.empm, "K", sep = ""));
box();

# Therefore from 1999~2008, the total PM2.5 emission decreased during
#1999~2002, then increased during 2002~2005, and then went down again
#during 2005~2008.
