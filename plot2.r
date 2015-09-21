plot2 <- function()  {
    
    # read the data
    NEI <- readRDS("summarySCC_PM25.rds")
    
    # subset to year and Emissions, filter by county
    smallNEI <- NEI[NEI$fips == "24510",c("Emissions", "year")]
    
    # aggregate emissions (sum) by year, add readable column names
    aggPerYear <- as.data.frame(tapply(smallNEI$Emissions, smallNEI$year, FUN = sum ))
    colnames(aggPerYear) <- c("emissions")
    aggPerYear$year <- as.factor(rownames(aggPerYear))
    
    # start plot - emissions vs year
    # axis is for correctly labeling year axis (factor) - else R converts to numeric and we dont see the years we want
    png(filename = "plot2.png")
    plot.default(aggPerYear$year,aggPerYear$emissions,type="l",xaxt="n", main="Total PM 2.5 emissions in Baltimore City, MD", xlab = "Year", ylab="PM 2.5 emissions (tons)")
    axis(side=1, at=aggPerYear$year,labels=aggPerYear$year)
    dev.off()
    
}