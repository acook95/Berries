#Set up
library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(dplyr)
library(ggplot2)


## read the data
ag_data <- read_csv("berries.csv", col_names = TRUE)

## look at number of unique values in each column
ag_data %>% summarize_all(n_distinct) -> aa

## make a list of the columns with only one unique value
bb <- which(aa[1,]==1)

## list the 1-unique valu column names 
cn <- colnames(ag_data)[bb]

## remove the 1-unique columns from the dataset
ag_data %<>% select(-all_of(bb))

aa %<>% select(-all_of(bb)) 

## State name and the State ANSI code are (sort of) redundant
## Just keep the name
ag_data %<>% select(-4)
aa %<>% select(-4) 

kable(head(ag_data)) %>% kable_styling(font_size=12)

berry <- unique(ag_data$Commodity)
nberry <- length(berry)

## filter the data so that just years and raspberries are included
rberry <- ag_data %>% filter((Commodity=="RASPBERRIES") & (Period=="YEAR"))
rberry %<>% select(-c(Period, Commodity))   

## Does every Data Item begin with raspberries?
sum(str_detect(rberry$`Data Item`, "^RASPBERRIES")) == length(rberry$`Data Item`)

## Separate Data Item into two parts
rberry %<>% separate(`Data Item`, c("Type", "Production"), sep = "-")

unique(rberry$Type)

#separate type into three parts
rberry %<>% separate(Type, c("Berry", "Type", "Market"), sep = ",")

unique(rberry$Production)

#separate Production into three parts
rberry %<>% separate(Production, c("Production", "Measure", "Average"), sep = ",")

rberry[is.na(rberry)] <- ""

#moving observations from Type to Production
rberry$Market <- ifelse(rberry$Type==" PROCESSING ", paste(rberry$Type, rberry$Market, sep = ""), rberry$Market)
rberry$Type <- ifelse(rberry$Type==" PROCESSING ", NA, rberry$Type)
rberry[is.na(rberry)] <- ""

rberry$Market <- ifelse(rberry$Type==" UTILIZED ", paste(rberry$Type, rberry$Market, sep = ""), rberry$Market)
rberry$Type <- ifelse(rberry$Type==" UTILIZED ", NA, rberry$Type)
rberry[is.na(rberry)] <- ""

rberry$Market <- ifelse(rberry$Type==" FRESH MARKET ", paste(rberry$Type, rberry$Market, sep = ""), rberry$Market)
rberry$Type <- ifelse(rberry$Type==" FRESH MARKET ", NA, rberry$Type)
rberry[is.na(rberry)] <- ""

rberry$Market <- ifelse(rberry$Type==" NOT HARVESTED ", paste(rberry$Type, rberry$Market, sep = ""), rberry$Market)
rberry$Type <- ifelse(rberry$Type==" NOT HARVESTED ", NA, rberry$Type)
rberry[is.na(rberry)] <- ""

rberry$Market <- ifelse(rberry$Type==" NOT SOLD ", paste(rberry$Type, rberry$Market, sep = ""), rberry$Market)
rberry$Type <- ifelse(rberry$Type==" NOT SOLD ", NA, rberry$Type)
rberry[is.na(rberry)] <- ""

##Clean up Domain
rberry %<>% separate(Domain, c("D_left", "D_right"), sep = ", ")
rberry[is.na(rberry)] <- ""
rberry$Chemical_Type <- ifelse(rberry$D_left=="FERTILIZER", rberry$D_left, rberry$D_right)

## And now Domain Category
rberry %<>% separate(`Domain Category`, c("DC_left", "DC_right"), sep = ":")
rberry[is.na(rberry)] <- ""
rberry %<>% rename(Chemicals = DC_right)

## Remove redundant columns
rberry %<>% select(-c(D_left, D_right, DC_left, Berry, Average))

## Remove NA's from Value
rberry$Value <- ifelse(rberry$Value=="(D)", NA, rberry$Value)
rberry[is.na(rberry)] <- ""
rberry$Value <- ifelse(rberry$Value=="(NA)", NA, rberry$Value)
rberry[is.na(rberry)] <- ""


## EDA

# only looking at 4 variables, with Measure filtered to only show "MEASURED IN LB"
rberry2 <- rberry %>% select(Year, State, Measure, Chemical_Type, Value)
rberry2 %<>% filter(Measure==" MEASURED IN LB")

# average number of lbs of raspberries for each type of chemical
rberry2$Value %<>% as.numeric()
rberry2 %>% group_by(Chemical_Type) %>% summarise(mean(Value, na.rm = TRUE))

# average number of lbs of raspberries for each State
rberry2 %>% group_by(State) %>% summarize(mean(Value, na.rm = TRUE))

#average number of lbs of raspberries for each Year
rberry2 %>% group_by(Year) %>% summarize(mean(Value, na.rm = TRUE))

# histogram of Value for Measure == MEASURED in LB
hist_lbs <- hist(rberry2$Value, xlab = "Number of LBs", main = "Histogram of Value (Measured in LB)")

# histogram of Years 
hist_yrs <- hist(rberry2$Year, xlab = "Year", main = "Histogram of Years")

# scatterplot of chemical type vs. lbs
ggplot(data = rberry2, mapping = aes(Chemical_Type,(Value))) + geom_jitter(aes(color = State)) + ggtitle("Scatterplot of Chemical Type vs. LBs produced") + xlab("Chemical Type") + ylab("Value in LBs")



