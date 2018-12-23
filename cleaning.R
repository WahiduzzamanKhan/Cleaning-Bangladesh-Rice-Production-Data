# loading packages
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)

# loading and cleaning rice production data
RiceProduction <- read_excel("raw data/rice production.xlsx", skip = 2, col_names = F)
colnames(RiceProduction) <- c("Year", "AreaAus", "AreaAman", "AreaBoro", "ProAus", "ProAman", "ProBoro", "YieldAus", "YieldAman", "YieldBoro")
names <- c("Year", "CultivatedArea", "Production", "Yield", "RiceType")
order <- c("Year", "RiceType", "CultivatedArea", "Production", "Yield")

Aus <- RiceProduction[,c(1,2,5,8)]
Aus <- Aus %>% mutate(RiceType = "Aus") 
colnames(Aus) <- names
Aus <- Aus %>% select(order)

Aman <- RiceProduction[,c(1,3,6,9)]
Aman <- Aman %>% mutate(RiceType = "Aman") 
colnames(Aman) <- names
Aman <- Aman %>% select(order)

Boro <- RiceProduction[,c(1,4,7,10)]
Boro <- Boro %>% mutate(RiceType = "Boro") 
colnames(Boro) <- names
Boro <- Boro %>% select(order)

RiceProduction <- full_join(Aus, Aman)
RiceProduction <- full_join(RiceProduction, Boro)

RiceProduction <- RiceProduction %>% 
    mutate(CultivatedArea = CultivatedArea*1000) %>%
    mutate(Production = Production*1000)

RiceProduction <- separate(RiceProduction, Year, c("Year"), sep = "-")
RiceProduction$Year <- as.integer(RiceProduction$Year)
RiceProduction <- RiceProduction[order(RiceProduction$Year),]

# loading and cleaning rice import and export data
RiceImEx <- read_csv("raw data/WRS.csv")
RiceImEx <- spread(RiceImEx[,c(-1,-4,-6)], Variable, Value)
colnames(RiceImEx) <- c("Year", "ExportQuantity", "ExportValue", "ImportQuantity", "ImportValue")
RiceImEx$ExportValue <- RiceImEx$ExportValue * 1000
RiceImEx$ImportValue <- RiceImEx$ImportValue * 1000
RiceImEx <- RiceImEx[order(RiceImEx$Year),]

# loading and cleaning rainfall data
Rain <- read_csv("raw data/Rain.csv")
Rain <- gather(Rain, Day, RainFall, -Station, -Year, -Month)
Rain$RainFall <- str_replace_all(Rain$RainFall, "^[*]+$", "0")
Rain$RainFall <- as.numeric(Rain$RainFall)
Rain <- Rain[,c(2,5)] %>% group_by(Year) %>% mutate(Rain = sum(RainFall, na.rm = T))
Rain <- unique(Rain[,c(1,3)])
Rain <- Rain[order(Rain$Year),]

# combining all the datasets
RiceData <- full_join(RiceProduction, RiceImEx, by = "Year")
RiceData <- full_join(RiceData, Rain, by = "Year")

# saving cleaned data
write_csv(RiceData, "cleaned data/CleanedRiceData.csv")
