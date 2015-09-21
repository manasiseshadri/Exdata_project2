plot3 <- function () {
    
    # read the data
    NEI <- readRDS("summarySCC_PM25.rds")
    
    # subset to type, emissions and year, filtered by county
    smallNEI <- NEI[NEI$fips == "24510",c("type", "Emissions", "year")]
    
    # aggregate emissions (sum) by year and type, add year column
    aggPerYear <- as.data.frame(tapply(smallNEI$Emissions, list(smallNEI$year, smallNEI$type), FUN = sum ))
    aggPerYear$year <- rownames(aggPerYear)
    
    # melt the dataframe to get rowwise types (to use facets in plot) - results in year, value (emissions), variable (type)
    library(reshape2)
    aggPerYear <- melt(aggPerYear)
    
    # plot 4x1 grid of each type, print to png
    library(ggplot2)
    png(filename = "plot3.png")
    p1 <- ggplot(aggPerYear, aes(year, value, group=1)) + facet_grid(variable ~ . ) + geom_line() + labs(x="Year", y="Total PM 2.5 Emissions (tons)", title="Total PM 2.5 Emissions Per Type in Baltimore City, MD")
    print(p1)
    dev.off()
        
}