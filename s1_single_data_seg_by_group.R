# seg single variable outliers, and group info is from multi variable
library(readxl)
library(dplyr) # for %>% 

rm(list=ls())
# set parameter
source_path <- 'I:\\imputation\\imp_compare\\s1_data\\psytool\\single\\IQR_psy_1.5.xlsx'
info_path <- 'I:\\imputation\\imp_compare\\s1_data\\psytool\\multi\\RData\\Out_n3.Rdata'
save_path <-  'I:\\imputation\\imp_compare\\s1_data\\psytool\\single\\IQR_psy_1.5.RData'

# load data
raw_data <- read_excel(source_path, sheet = 1, na = 'NA')
data <- as.data.frame(raw_data)
colnames(data)[1] <- 'RefID'
data$RefID <- substring(data$RefID, 5)

# set Nan to data (outlier only be marked)
for (outliers_i in grep('outlier', colnames(data))){
  data[which(data[, outliers_i]==0), outliers_i - 1] <- NA
}

# get group info
load(info_path)
group_info <- lapply(data_save_list$TotalData, colnames)

# seg single check data
single_missing_data_list <- lapply(group_info, function(x, data){
  outdata <- data[, c('RefID', x)]
  outdata[, -1] <- sapply(outdata[, -1], as.numeric)
  return(outdata)
}, data)

single_complete_data_list <- lapply(single_missing_data_list, function(x){x[complete.cases(x),]})

single_data_list <- list(TotalData=single_missing_data_list,
                         CompleteData=single_complete_data_list)
# save data
save(single_data_list, file=save_path)
