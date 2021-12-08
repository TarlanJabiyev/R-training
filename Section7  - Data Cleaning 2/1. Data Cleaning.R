# Data Cleaning

library(tidyverse)
library(data.table)
library(inspectdf)
library(skimr) 

df <- fread("Future-500.csv",na.strings = "")

df %>% skim()

df$ID <- df$ID %>% as.character()

# Convert to numeric ----

# gsub() 
df$Expenses <- gsub(" Dollars","",df$Expenses)
df$Expenses <- gsub(",","",df$Expenses) %>% as.numeric()

# df$Expenses <- df$Expenses %>% 
#   gsub(" Dollars","",.) %>% 
#   gsub(",","",.) %>% 
#   as.numeric()


# parse_number()
df$Revenue <- df$Revenue %>% parse_number() 

# str_replace_all()
df$Growth <- df$Growth %>% str_replace_all("%","") %>% as.numeric()


df %>% glimpse()
df %>% summary()


# Missing Data ----
df %>% inspect_na()

df[!complete.cases(df),] %>% View()

#Filtering: using which() for missing data
df[df$Employees == 45,]
df[which(df$Employees == 45),]

#Filtering: using is.na() for missing data
df[df$Expenses == NA,]

df[is.na(df$Expenses),]

#Removing records with missing data
df[is.na(df$Industry),]
df[!is.na(df$Industry),] #opposite
df <- df[!is.na(df$Industry),]

#Repleace Missing Data: Factual Aanlysing
df[!complete.cases(df),] %>% View()

df[is.na(df$State),]
df[is.na(df$State) & df$City=="New York",] 
df[is.na(df$State) & df$City=="New York","State"] <- "NY"

df[is.na(df$State),]
df[is.na(df$State) & df$City=="San Francisco",] 
df[is.na(df$State) & df$City=="San Francisco","State"] <- "CA"

#Repleace Missing Data: Median Inputation Method (Part 1)
df[!complete.cases(df),] %>% View()

med <- median(df[df$Industry=="Retail" & df$Inception=="2012","Employees"][[1]], na.rm=TRUE)
df[is.na(df$Employees) & df$Industry=="Retail" & df$Inception=="2012","Employees"] <- med

med <- median(df[df$Industry=="Financial Services" & df$Inception=="2010","Employees"][[1]], na.rm=TRUE)
df[is.na(df$Employees) & df$Industry=="Financial Services" & df$Inception=="2010","Employees"] <- med

med <- median(df[df$Industry=="Construction" & df$Inception=="2013","Growth"][[1]], na.rm=TRUE)
df[is.na(df$Growth) & df$Industry=="Construction" & df$Inception=="2013","Growth"] <- med

med <- median(df[df$Industry=="Construction" & df$Inception=="2010","Revenue"][[1]], na.rm=TRUE)
df[is.na(df$Revenue) & df$Industry=="Construction" & df$Inception=="2010","Revenue"] <- med

med <- median(df[df$Industry=="Construction" & df$Inception=="2013","Revenue"][[1]], na.rm=TRUE)
df[is.na(df$Revenue) & df$Industry=="Construction" & df$Inception=="2013","Revenue"] <- med

med <- median(df[df$Industry=="Construction" & df$Inception=="2010","Expenses"][[1]], na.rm=TRUE)
df[is.na(df$Expenses) & df$Industry=="Construction" & df$Inception=="2010","Expenses"] <- med

med <- median(df[df$Industry=="Construction" & df$Inception=="2013","Expenses"][[1]], na.rm=TRUE)
df[is.na(df$Expenses) & df$Industry=="Construction" & df$Inception=="2013","Expenses"] <- med

df[!complete.cases(df),]


#Revenue - Expences = Profit
#Expences = Revenue - Profit
df[is.na(df$Profit),"Profit"] <- df[is.na(df$Profit),"Revenue"] - df[is.na(df$Profit),"Expenses"]
df[is.na(df$Expenses),"Expenses"] <- df[is.na(df$Expenses),"Revenue"] - df[is.na(df$Expenses),"Profit"]

df[!complete.cases(df),]


# Filling categorical variables
df$Inception %>% glimpse()
df$Inception <- df$Inception %>% as.factor()

x <- df[which(df$Industry == "Health"),"Inception"][[1]]
x <- x[!is.na(x)]
ux <- unique(x)
mode_inc <- ux[match(x, ux) %>% tabulate() %>% which.max()]
df[is.na(df$Inception) & df$Industry == "Health","Inception"] <- mode_inc

df %>% inspect_na()

Mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[match(x, ux) %>% tabulate() %>% which.max()])
}
Mode(x, na.rm = TRUE) -> df[is.na(df$Inception) & df$Industry == "Health","Inception"]


# Outliers ----
num_vars <- df %>% 
  select_if(is.numeric) %>% 
  names()
num_vars

library(graphics)
for (b in num_vars) {
  OutVals <- boxplot(df[[b]])$out
  if(length(OutVals)>0){
    print(b)
    print(OutVals)
  }
}


OutVals <- boxplot(df[["Revenue"]])$out
median <- median(df[["Revenue"]])
o3 <- ifelse(OutVals>median,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]
o1 <- ifelse(OutVals<median,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]

val <- quantile(df[["Revenue"]],0.75) + 1.5*IQR(df[["Revenue"]])
df[df[["Revenue"]] %in% o3,"Revenue"] <- val

val <- quantile(df[["Revenue"]],0.25) - 1.5*IQR(df[["Revenue"]])
df[df[["Revenue"]] %in% o1,"Revenue"] <- val


OutVals <- boxplot(df[["Employees"]])$out
median <- median(df[["Employees"]])
o3 <- ifelse(OutVals>median,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]
o1 <- ifelse(OutVals<median,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]

val <- quantile(df[["Employees"]],0.75) + 1.5*IQR(df[["Employees"]])
df[df[["Employees"]] %in% o3,"Employees"] <- val

val <- quantile(df[["Employees"]],0.25) - 1.5*IQR(df[["Employees"]])
df[df[["Employees"]] %in% o1,"Employees"] <- val


OutVals <- boxplot(df[["Profit"]])$out
median <- median(df[["Profit"]])
o3 <- ifelse(OutVals>median,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]
o1 <- ifelse(OutVals<median,OutVals,NA) %>% na.omit() %>% as.matrix() %>% .[,1]

val <- quantile(df[["Profit"]],0.75) + 1.5*IQR(df[["Profit"]])
df[df[["Profit"]] %in% o3,"Profit"] <- val

val <- quantile(df[["Profit"]],0.25) - 1.5*IQR(df[["Profit"]])
df[df[["Profit"]] %in% o1,"Profit"] <- val
