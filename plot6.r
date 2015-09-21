plot6 <- function(NEI, SCC)   {
    
    # read the data 
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    # subset the data, filter by county (Baltimore and LA)
    smallNEI <- NEI[NEI$fips == "24510" | NEI$fips == "06037",c("fips", "SCC", "Emissions", "year")]
    smallSCC <- SCC[,c("SCC", "Short.Name")]
    
    # join the dataframes on SCC code    
    joinedDF <- merge(smallNEI, smallSCC, by="SCC")
    
    # Assumptions: Motor vehicle related pollution includes "Motor Vehicles" and "Motorcycles" 
    # but not "Rocket motors" or "Motor Vehicle fires"
    # Short.Name is searched for "Motor Vehicles" (note plural, not singular) and "Motorcycle"
    # The results are combined to form single data frame
    subjoinedDF <- joinedDF[grep("Motorcycle", joinedDF$Short.Name),  c("fips", "Emissions", "year", "Short.Name")]
    subjoinedDF <- rbind(subjoinedDF, joinedDF[grep("Motor Vehicles", joinedDF$Short.Name),  c("fips", "Emissions", "year", "Short.Name")])

    # aggregate the emissions (sum) by year and county, re-do column names    
    aggPerYear <- aggregate.data.frame(subjoinedDF$Emissions, by = list(subjoinedDF$year, subjoinedDF$fips), FUN = sum )
    colnames(aggPerYear) <- c("year", "fips", "Emissions")

    # plot a line plot to see if the emissions increased or decreased and a boxplot to see the absolute change
    library(gridExtra)
    library(ggplot2)
    png(filename = "plot6.png")
    p1 <- ggplot(aggPerYear, aes(year, Emissions, group=1)) + facet_grid(fips ~ . ) + geom_line() + labs(x="Year", y="Total PM 2.5 Emissions (tons)", title="Total Motor Vehicle PM 2.5 Emissions")
    p2 <- ggplot(aggPerYear, aes(fips, Emissions, color=fips)) + geom_boxplot() + labs(x="County", y="Total PM 2.5 Emissions (tons)", title=" (Boxplot) ")
    grid.arrange(p1, p2, nrow=1, ncol=2)
    dev.off()
}