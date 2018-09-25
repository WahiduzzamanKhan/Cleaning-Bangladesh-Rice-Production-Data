# loading packages
library(tidyverse)
library(lubridate)
library(stringr)
library(readxl)

# loading and cleaning rice production data
names <- c("Year", "AreaAus", "AreaAman", "AreaBoro", "ProAus", "ProAman", "ProBoro", "YieldAus", "YieldAman", "YieldBoro")
RiceProduction <- read_excel("C:/Users/chist/Desktop/real data/rice production.xlsx", skip = 2, col_names = names)
multiplier <- function(table, i, n){
    table_final <- table
    for (i in i) {
        table_final[,i] <- table_final[,i] * n
    }
    return(table_final)
}
RiceProduction <- multiplier(RiceProduction, c(2,3,4,5,6,7), 1000)
RiceProduction <- separate(RiceProduction, Year, c("Year"), sep = "-")
RiceProduction <- RiceProduction[,-2]
RiceProduction$Year <- as.integer(RiceProduction$Year)

# loading and cleaning rice import and export data
RiceImEx <- read_csv("C:/Users/chist/Desktop/real data/WRS.csv")
RiceImEx <- spread(RiceImEx[,c(-4,-6)], Variable, Value)
RiceImEx <- multiplier(RiceImEx, c(4,6), 1000)
colnames(RiceImEx) <- c("Country", "Year", "ExportQuantity", "ExportValue", "ImportQuantity", "ImportValue")

# loading and cleaning rice rainfall data
Rain <- read_csv("C:/Users/chist/Desktop/real data/Rain.csv")
Rain <- gather(Rain, Day, RainFall, -Station, -Year, -Month)
Rain$RainFall <- str_replace_all(Rain$RainFall, "^[*]+$", "0")
Rain$RainFall <- as.numeric(Rain$RainFall)
Rain <- Rain[,c(2,5)] %>% group_by(Year) %>% mutate(Rain = sum(RainFall, na.rm = T))
Rain <- unique(Rain[,c(1,3)])

# combining all the datasets
RiceData <- tibble(Year = 1948:2018)
temp <- full_join(RiceData, RiceProduction)
temp <- full_join(temp, RiceImEx[,-1])
temp <- full_join(temp, Rain)
RiceData <- temp

# saving cleaned data
write_csv(RiceData, "C:/Users/chist/Desktop/real data/cleaned rice data.csv")
