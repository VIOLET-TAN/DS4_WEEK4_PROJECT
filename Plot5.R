#DOWNLOAD PACKAGES
library(dplyr)
library(scales)
library(data.table)
library(ggplot2)

##READING DATA FROM •	Data for Peer Assessment [29Mb]

NEI <- readRDS("summarySCC_PM25.rds")

head(NEI)
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
head(SCC$EI.Sector, 3)
#[1] Fuel Comb - Electric Generation - Coal
#[2] Fuel Comb - Electric Generation - Coal
#[3] Fuel Comb - Electric Generation - Coal

##QUESTION 5
#5.	How have emissions from motor vehicle sources changed from 
#1999–2008 in Baltimore City?

#MERGE NEI and SCC
NEISCC <- merge(NEI, SCC, by="SCC")

#EXTRACT DATA RELATED TO MOTOR VEHICLES IN BALTIMORE
bm <- NEI[NEI$fips=="24510" & NEI$type=="ON-ROAD", ]

# Aggregate Splits the data into subsets, computes summary statistics for each, 
#and returns the result in a convenient form.
pyear_bm. <- aggregate(Emissions~year, bm, sum)

# How have emissions from motor vehicle sources changed from 1999-2008 
#in Baltimore City? 

png("plot5.png", width=840, height=480)

g <- ggplot(pyear_bm., aes(factor(year), Emissions))
g <- g + geom_bar(stat="identity") + xlab("Year") + 
  ylab(expression("Total Tons of PM2.5 Emissions")) + 
  ggtitle("Annual Total Emissions related to Motor Vehicles in Baltimore")
print(g)          

dev.off()        
   

 

