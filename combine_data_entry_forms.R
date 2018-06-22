###############################################
# Code for Decompression Census data cleaning #
###############################################

#####################
#Import libraries
library(data.table)
library(stringr)
library(dplyr)

transpose_data <- function(dat){
  headers <- c("shift", "lane", "clicker", "skipVeto", "birth", "resideNumeric", "residence_details", "gender", "attend_decom",
              "attend_burn", "nbburns")
  dat <- data.frame(t(dat[,-c(1,2)]))
  dat <- dat[, 1:length(headers)]
  names(dat) <- headers
  dat
}


#Apply over file paths to get a list of all data sets
filePaths <- list.files("~/Documents/census/Data/main_results_2017/2017_SF_Decom_Sampling_Data", full.names = TRUE)
filePaths <- filePaths[grepl(".csv", filePaths)]
dataList <- lapply(filePaths, read.csv, na.strings = c(".", " ", ""), nrows = 24, header = FALSE)
dataList <- lapply(dataList, transpose_data)


#add var to identify keypunchers and add column names 
fileNames <- list.files("~/Documents/census/Data/main_results_2017/2017_SF_Decom_Sampling_Data", full.names = FALSE)
fileNames <- fileNames[grepl(".csv", fileNames)]
fileNameSplit <- str_split(fileNames, "_")
keyPunchers <- sapply(fileNameSplit, function(x){str_to_lower(x[1])})
dateEntered <- sapply(fileNameSplit, function(x){str_extract(x[2], ".+(?=.csv)")})
# keyPunchers <- str_extract(filePaths, "(?<=_)[A-Za-z]+(?=.csv)")

dataList <- lapply(1:length(dataList), function(i, d, kp, de){
  dTemp = d[[i]]
  dTemp[,"keypuncher"] = kp[i]
  dTemp[,"dateEntered"] = de[i]
  dTemp
}, d = dataList, kp = keyPunchers, de = dateEntered)


#row bind the data sets
mergedData = Reduce(rbind, dataList)
mergedData = as.data.frame(apply(mergedData, 2, trimws), stringsAsFactors = FALSE)
rownames(mergedData) <- NULL

#decode values
#Residence
mergedData$residence = ifelse(mergedData$resideNumeric == "1", "CA - San Francisco",
                                      ifelse(mergedData$resideNumeric %in% c("2", "12"), "CA - Other City",
                                             ifelse(mergedData$resideNumeric %in% c("3", "23"), "USA_other",
                                                    ifelse(mergedData$resideNumeric %in% c("4", "3,4"), "Other", NA))))
#Move SF information to residence_details and create combined SF category. This makes it more comparable to main census results
mergedData$residence_details[mergedData$residence == "CA - San Francisco"] = "San Francisco"
mergedData$residence[mergedData$residence %in% c("CA - San Francisco", "CA - Other City")] = "California"

mergedData$resideNumeric = NULL

#Gender
mergedData$gender = ifelse(mergedData$gender == 1, "female",
                           ifelse(mergedData$gender == 2, "male", 
                                  ifelse(mergedData$gender == 3, "fluid", NA)))

#Attended decompression previously
mergedData$attend_decom = ifelse(mergedData$attend_decom == 1, "Yes",
                                ifelse(mergedData$attend_decom == 2, "No", NA))
#Attended burn previously
mergedData$attend_burn = ifelse(mergedData$attend_burn == 1, "Yes, in 2017",
                               ifelse(mergedData$attend_burn == 2, "Yes, not in 2017",
                                      ifelse(mergedData$attend_burn == 3, "No", NA)))

write.csv(mergedData, file = "2017_SF_Decom_Combined_Sampling_data.csv")
