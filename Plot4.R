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
> head(SCC$EI.Sector, 3)
#[1] Fuel Comb - Electric Generation - Coal
#[2] Fuel Comb - Electric Generation - Coal
#[3] Fuel Comb - Electric Generation - Coal

##QUESTION 4
#Across the United States, how have emissions from coal combustion-related 
#sources changed from 1999–2008?

#To answer this question, both data SCC and NEI will be used. First, need to subset the data concerning emission from coal combustion-related.
#Function grep(pattern, x) can extract the factor list of coal from column EI.SEctor.Then match the $SCC in NEI


# Coal combustion related sources
SCC.coal = SCC[grepl("coal", SCC$Short.Name, ignore.case=TRUE),]

# Merge two data sets
merge <- merge(x=NEI, y=SCC.coal, by='SCC')
merge.sum <- aggregate(merge[, 'Emissions'], by=list(merge$year), sum)
colnames(merge.sum) <- c('Year', 'Emissions')

# Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999-2008?

ggplot(data=merge.sum, aes(x=Year, y=Emissions/1000000)) + 
  geom_line(aes(group=1, col=Emissions)) + geom_point(aes(size=2, col=Emissions)) + 
  ggtitle(expression('Total Emissions of PM2.5 in US Related with Coal Combustion')) + 
  ylab(expression(paste('PM2.5', ' in milliontons'))) + 
  geom_text(aes(label=round(Emissions/1000000,digits=2), size=2, hjust=1.5, vjust=1.5)) + 
  theme(legend.position='none') + scale_colour_gradient(low='black', high='red')

dev.off()