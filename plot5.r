plot5 <- function(NEI, SCC)   {
    
    # read the data 
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    # subset the data, filter by county
    smallNEI <- NEI[NEI$fips == "24510",c("SCC", "Emissions", "year")]
    smallSCC <- SCC[,c("SCC", "Short.Name")]
    
    # join the dataframes on SCC code    
    joinedDF <- merge(smallNEI, smallSCC, by="SCC")
    
    # Assumptions: Motor vehicle related pollution includes "Motor Vehicles" and "Motorcycles" 
    # but not "Rocket motors" or "Motor Vehicle fires"
    # Short.Name is searched for "Motor Vehicles" (note plural, not singular) and "Motorcycle"
    # The results are combined to form single data frame
    subjoinedDF <- joinedDF[grep("Motorcycle", joinedDF$Short.Name),  c("Emissions", "year")]
    subjoinedDF <- rbind(subjoinedDF, joinedDF[grep("Motor Vehicles", joinedDF$Short.Name),  c("Emissions", "year")])

    # aggregate the emissions (sum) by year, re do column names    
    aggPerYear <- aggregate.data.frame(subjoinedDF$Emissions, by = list(subjoinedDF$year), FUN = sum )
    colnames(aggPerYear) <- c("year", "Emissions")
    aggPerYear$year <- as.factor(aggPerYear$year)
    
    # start plot - emissions vs year
    # axis is for correctly labeling year axis (factor) - else R converts to numeric and we dont see the years we want
    png(filename = "plot5.png")
    plot.default(aggPerYear$year, aggPerYear$Emissions, type = "l", xaxt="n", main="Total Motor Vehicle related PM 2.5 emissions", xlab = "Year", ylab="PM 2.5 emissions (tons)")
    axis(side=1, at=aggPerYear$year,labels=aggPerYear$year)
    dev.off()
}