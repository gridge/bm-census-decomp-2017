library(data.table)
library(dplyr)

decompression = fread("2017_SF_Decom_Combined_Sampling_data.csv") %>%
  select(birth, residence, gender, nbburns)
census2017 = fread("/home/aaron/Documents/census/Data/main_results_2017/Clean2017CensusFulltabMar2018.csv",
                   sep = "\t", fill = TRUE)
census2017$residence = census2017$residence
census2017$residence[census2017$residence2 == "Nevada"] = "USA_other"
census2017$residence[census2017$residence2 == "Canada"] = "Other"
census17 = select(census2017, birth, residence, gender, nbburns, weightnerds)
fwrite(decompression, "2017_SF_Decom_Subset.csv")
fwrite(decompression, "2017_Census_Subset.csv")





