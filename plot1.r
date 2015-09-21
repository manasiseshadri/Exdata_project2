plot1 <- function() {
    
    # read the data
    NEI <- readRDS("summarySCC_PM25.rds")
    
    # subset to year and Emissions
    smallNEI <- NEI[,c("Emissions", "year")]
    
    # aggregate the emissions (sum them) for each year, add readable column names
    aggPerYear <- as.data.frame.array(tapply(smallNEI$Emissions, smallNEI$year, FUN = sum ))
    colnames(aggPerYear) <- c("emissions")
    aggPerYear$year <- as.factor(rownames(aggPerYear))
    
    
    # start plot - emissions vs year
    # axis is for correctly labeling year axis (factor) - else R converts to numeric and we dont see the years we want
    png(filename = "plot1.png")
    plot.default(aggPerYear$year,aggPerYear$emissions,type="l",xaxt="n", main="Total PM 2.5 emissions", xlab = "Year", ylab="PM 2.5 emissions (tons)")
    axis(side=1, at=aggPerYear$year,labels=aggPerYear$year)
    dev.off()
}