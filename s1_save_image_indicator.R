# save T1 surf as .RData (save image indicator)
library(dplyr) # for %>% 
library(tools)

# set parameter
source_dir <- 'I:\\imputation\\imp_compare\\s1_data\\T1_surf\\raw'
save_path <- 'I:\\imputation\\imp_compare\\s1_data\\T1_surf\\Rdata\\T1Surf.RData'

# load data
T1Surf_list <- list()
i <- 0
for (file_name in list.files(source_dir, '.csv')){
  i <- i + 1
  file_path <- file.path(source_dir, file_name)
  T1Surf_list[[i]] <- read.csv(file_path)
  rownames(T1Surf_list[[i]]) <- T1Surf_list[[i]][, 1]
  names(T1Surf_list)[i] <- file_path_sans_ext(file_name) 
}

# save
save(T1Surf_list, file = save_path)
