# seg data by each scale
library(readxl)
library(dplyr) # for %>% 
library(stringr)
library(mice)

# set parameter
source_path <-  'I:\\imputation\\imp_compare\\s1_data\\psytool_QC_ALL_results.xlsx'
out_path <- 'I:\\imputation\\imp_compare\\s1_data\\Out_n1.5.Rdata'

# load data
# sheet 1 is total data; 2 is cluster defination; 3-6 is outliers checked(n=1.5/2/2.5/3)
data_raw <-  read_excel(source_path, sheet = 3, na = 'NaN')  # outliers checked
group_name_raw <-  read_excel(source_path, sheet = 2)

data <-  as.data.frame(data_raw[-1, ])
info <-  as.vector(as.numeric(data_raw[1, ]))
group_name <- as.data.frame(group_name_raw)

# get group data
group_data_list <-  list()

for (i in 1:max(info, na.rm = T)){
  group_data_list[[i]] <- data[, c(2, which(info==i))]
}
names(group_data_list) <- group_name[, 1]

group_data_list_num <- lapply(group_data_list, function(x){
  x1 <- apply(x[, -1], 2, as.numeric)
  rownames(x1) <- x[, 1]
  return(x1)
  })

group_data_list_num_complete <- lapply(group_data_list_num, function(x){x[complete.cases(x),]})

# save list
data_save_list <- list(TotalData=group_data_list_num, CompleteData=group_data_list_num_complete)
save(data_save_list, file=out_path)
