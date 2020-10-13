# 
# 
# USDA database selector https://quickstats.nass.usda.gov
# 
# berries
# https://quickstats.nass.usda.gov/results/D416E96E-3D5C-324C-9334-1D38DF88FFF1
# 
# 
# corn farming economics
# https://quickstats.nass.usda.gov/results/471133FF-98EA-306C-8C8F-4A607B48E4BB
# 



library(tidyverse)
library(magrittr)

## read the data

ag_data <- read_csv("berries.csv", col_names = TRUE)

## look at number of unique values in each column
ag_data %>% summarize_all(n_distinct) -> aa


## make a list of the columns with only one unique value
bb <- which(aa[1,]==1)

## list the 1-unique valu column names 
colnames(ag_data)[bb]

## list the 1-unique single values.  
## Consider if they should be used for labels

single_values <- ag_data[1,bb]
single_values


## remove the 1-unique columns from the dataset
ag_data %<>% select(-all_of(bb))

## Make a table of the number of unique values in each column.
aa %<>% select(-all_of(bb)) 

## State name and the State ANSI code are (sort of) redundant
## Just keep the name
## Note: There may be times when having the numeric code will be handy)

ag_data %<>% select(-4)
aa %<>% select(-4) 

## Now list the values in the columns that
aa
# A tibble: 1 x 8
# Year Period State Commodity `Data Item` Domain `Domain Category` Value
# <int>  <int> <int>     <int>       <int>  <int>             <int> <int>
#   1     5      3    18         3         106      6               269  2362



ag_data$Year %>%  unique()
## [1] 2019 2018 2017 2016 2015

ag_data$Period %>% unique()
## "MARKETING YEAR"      "YEAR"                "YEAR - AUG FORECAST"

## Year: 
## Generally refers to calendar year. 
## For Prices Received data, refers to 
##an unweighted average (by month) for the calendar year. 

## Marketing year:
## Definition varies by commodity; 
## see Agricultural Prices publications
## for definitions by commodity. 
## For Prices Received data, refers to a
## weighted average for the marketing year.



ulst <- lapply(ag_data[,(which(aa[1,]<100))], unique)

lapply(ulst, t)

ulst



########################################################################
### let's focus on: period = "Year" and Commodity = "BLUEBERRIES"

## blueberry data
ag_data_bb <- ag_data %>% filter((Commodity=="BLUEBERRIES") & (Period=="YEAR"))


################################## separate here -- 
#ag_data_bb$`Data Item`

## separate ag_data_bb$`Data Item`  into "berry", "type", "data_type"

## ag_data_bb %>% separate(`Data Item`, c("berry", "type", "data_item"), ",")
  
# head(ag_data$`Data Item` %>% unique())


ag_data_bb %<>% separate(`Data Item`, c("berry", "type", "data_item", "unit"), ",")
  
#######################################################
### 

nana <- lapply(ag_data_bb, is.na)


lapply(nana, sum)

### STRAWBERRIES

ag_data_sb <- ag_data %>% filter((Commodity=="STRAWBERRIES") & (Period=="YEAR"))

ag_data_sb %<>% separate(`Data Item`, c("berry", "type", "data_item", "unit"), ",")

nana <- lapply(ag_data_sb, is.na)

lapply(nana, sum)

### RASPBERRIES

ag_data_rb <- ag_data %>% filter((Commodity=="STRAWBERRIES") & (Period=="YEAR"))

ag_data_rb %<>% separate(`Data Item`, c("berry", "type", "data_item", "unit"), ",")

nana <- lapply(ag_data_sb, is.na)

lapply(nana, sum)
