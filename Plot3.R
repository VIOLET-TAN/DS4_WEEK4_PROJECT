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

##QUESTION 3
#3.	Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
#which of these four sources have seen decreases in emissions 
# from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 

#Use the ggplot2 plotting system to make a plot answer this question.

##The below bariable et.baltimore=emission type in Baltimroe
##Use facet_grid() forms a matrix of panels defined by row and column 
#faceting variables. It is most useful when you have two discrete 
#variables, and all combinations of the variables exist in the data

et.baltimore <- NEI %>% filter(fips == "24510") %>% 
  group_by(type, year) %>% summarize(pyear.Total = sum(Emissions));
#et.baltimore$type <- factor(et.baltimore$type, levels = c("POINT", "NON-POINT", "ONROAD", "NONROAD")) #factor levels 
et.baltimore$type <- factor(et.baltimore$type, levels = "ON-ROAD", "NON-ROAD", "POINT", "NONPOINT")) 
ggplot(et.baltimore, aes(x = factor(year), y = pyear.Total, fill = type)) +
  geom_bar(stat = "identity") + 
  facet_grid(. ~ type) + 
  xlab("Year") + 
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression("Total Tons of PM"[2.5]*" Emissions in Baltimore by Source Type")) +
  scale_y_continuous(labels = comma) +
  guides(fill = FALSE) +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1))

#geom_point("stat = "identity"), to count the rows for each x value but skip the aggregation with y
#facet_grid(. ~ type) divides the graphic area to 1 row 4(number of types)
#columns

##conclusion: please see the plot3