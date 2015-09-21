plot4 <- function(NEI, SCC)   {
    
    # read the data
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    # subset the data 
    smallNEI <- NEI[,c("SCC", "Emissions", "year")]
    smallSCC <- SCC[,c("SCC", "Short.Name")]
    
    # join the dataframes on SCC code
    joinedDF <- merge(smallNEI, smallSCC, by="SCC")
    
    # Assumption: Existence of "Coal" in Short.Name is considered as Coal Combustion related source
    # as coal contributes to pollution when combusted. Also, charcoal is NOT considered a form of coal
    
    # EI.Sector, SCC.Level.Three and SCC.Level.Four were considered for grep, but all have cases 
    # where Short.Name is coal related but these fields do not contain the word "Coal"
    
    subjoinedDF <- joinedDF[grep("Coal", joinedDF$Short.Name),  c("Emissions", "year")]
    
    # aggregate emissions (sum) by year, re-do column names
    aggPerYear <- aggregate.data.frame(subjoinedDF$Emissions, by = list(subjoinedDF$year), FUN = sum )
    colnames(aggPerYear) <- c("year", "Emissions")
    aggPerYear$year <- as.factor(aggPerYear$year)
    
    # start plot - emissions vs year
    # axis is for correctly labeling year axis (factor) - else R converts to numeric and we dont see the years we want
    png(filename = "plot4.png")
    plot.default(aggPerYear$year,aggPerYear$Emissions,type="l",xaxt="n", main="Total Coal Combustion related PM 2.5 emissions", xlab = "Year", ylab="PM 2.5 emissions (tons)")
    axis(side=1, at=aggPerYear$year,labels=aggPerYear$year)
    dev.off()

}