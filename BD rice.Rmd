---
title: "Cleaning and exploring rice production, import and export data of Bangladesh"
author: "Wahiduzzaman Khan"
output: html_notebook
---
In this project we are going to take a look at the production, import and export of rice of Bangladesh.</br>
You can download these data sets from [here](https://github.com/WahiduzzamanKhan/Cleaning-Bangladesh-Rice-Production-Data/tree/master/raw%20data). These data are collected from BRRI (Bangladesh Rice Research Institute) and WRS (World Rice Statistics).

### Loading the required packages
First we are going load all the packages that we are going to use:

```{r}
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
```

### Importing Rice Production data
As all the packages are loaded, its time to read in the data. The first data file that we are going to read is the "rice production.xlsx". Opening this excel file we can see that there are 10 columns containing information about the harvested area, production and yield of the three variants of rice (Aus, Aman, Boro) for years 1971 to 2017. Lets import this file and store it as "RiceProduction".

```{r}
RiceProduction <- read_excel("raw data/rice production.xlsx", skip = 2, col_names = F)
```

While Importing the data I skipped the first two rows because of their formatting. The way those two rows are formatted they are going to make a mess if they are imported. I also specified that the imported rows do not contain column names. So now lets take a look at the imported data.

```{r}
glimpse(RiceProduction)
```

### Cleaning Rice Production data
We can see that the is imported correctly but the names of the variables are randomly given and this kind of names make the data hard to work with. So lets give them appropriate names that explains what information they contain.

```{r}
colnames(RiceProduction) <- c("Year", "AreaAus", "AreaAman", "AreaBoro", "ProAus", "ProAman", "ProBoro", "YieldAus", "YieldAman", "YieldBoro")
glimpse(RiceProduction)
```

This already looks much better. In this dataset, the areas cultivated are expressed in 1000 hectors and produced amounts are expressed in 1000 tons. But for analysis we want them to show actual amount. So we have to multiply those columns by 1000. To do this we are going to create a function called "multiplier".

```{r}
multiplier <- function(table, i, n){
    table_final <- table
    for (i in i) {
        table_final[,i] <- table_final[,i] * n
    }
    return(table_final)
}
```

This function takes a dataset, column numbers and number as arguments and
multiplies all those columns by the number.

```{r}
RiceProduction <- multiplier(RiceProduction, c(2,3,4,5,6,7), 1000)
```

Now we have the actual values.
Lets put our concentration on the Year column now. We can see it has a range of years instead of a single year. In this analysis we are going to consider the lower bound as the year and remove the upper bound. We are also going to change Year column from character type to integers. We accomplish this by running the following code:

```{r}
RiceProduction <- separate(RiceProduction, Year, c("Year"), sep = "-")
RiceProduction$Year <- as.integer(RiceProduction$Year)
glimpse(RiceProduction)
```

We can see the Year column has a single year in each row.</br>

### Importing Rice Import and Export Data
Now we are going to import the data from the "WRS.csv" file. It contains information about importing and exporting rice. We are going to store this dataset named as "RiceImEx".

```{r}
RiceImEx <- read_csv("raw data/WRS.csv")
glimpse(RiceImEx)
```

It is visible that the column "Variable" contains values that should have their own column. So we are going to use the following code to give each of the values of "Variable" column their own column and discard the "Source" and "Unit" column.

```{r}
RiceImEx <- spread(RiceImEx[,c(-4,-6)], Variable, Value)
RiceImEx <- multiplier(RiceImEx, c(4,6), 1000)
```

We are also going to change the column names to match the naming convention we are following.

```{r}
colnames(RiceImEx) <- c("Country", "Year", "ExportQuantity", "ExportValue", "ImportQuantity", "ImportValue")
```

### Importing Rainfall Data
Now we are going to import data about rainfall from the file named "Rain.csv". This data is not about rice, but cultivation and production of rice heavily depends on rain. Thus rainfall data is very much relevant to this analysis. We are going to store this dataset named as "Rain".

```{r}
Rain <- read_csv("raw data/Rain.csv")
```

From this we get a dataset that has 34 columns. The first column named "Station" represent the weather station from where we got the data. Then we have "Year", "Month", then more 31 columns representing days of the month. The last 31 columns contains the total amount of rainfall on that day. We are going to reshape this in fashion so that, each day of a month gets a different row instead of a column.

```{r}
Rain <- gather(Rain, Day, RainFall, -Station, -Year, -Month)
```

The "Rainfall" column has "***" as value. In this case it represents a missing value. For this study we are going to replace all the missing values by "0", assuming that it did not rain that day. Also we are going to change this column from a character type to numeric type.

```{r}
Rain$RainFall <- str_replace_all(Rain$RainFall, "^[*]+$", "0")
Rain$RainFall <- as.numeric(Rain$RainFall)
```

We have our "RiceProduction" data and "RiceImEx" data expressed in a yearly fashion. So we are going to transform our "Rain" data so that the "RainFall" column represents the total amount of rainfall in a year. We are also going to drop the "Station" column.

```{r}
Rain <- Rain[,c(2,5)] %>% group_by(Year) %>% mutate(Rain = sum(RainFall, na.rm = T))
Rain <- unique(Rain[,c(1,3)])
```

### Merging All Together
Now, as all of our datasets are loaded and cleaned, we are going to combine them in a single dataset and name it "RiceData".

```{r}
RiceData <- tibble(Year = 1948:2018)
temp <- full_join(RiceData, RiceProduction)
temp <- full_join(temp, RiceImEx[,-1])
temp <- full_join(temp, Rain)
RiceData <- temp
```

### Saving Cleaned Data
We are going to save this cleaned data in CSV format.

```{r}
write_csv(RiceData, "cleaned data/cleaned rice data.csv")
```

### Conclusion
Now we have a cleaned, tidy data about the production, import, export of rice in Bangladesh, ready to perform analysis. We have also saved the cleaned dataset to use it in future if we need.