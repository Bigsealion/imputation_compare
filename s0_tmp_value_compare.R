# compare imputation method by value
rm(list=ls())
library(VIM)
library(naniar)
# set source path
data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/cog_70miss/s1.4_Cognative_T1surf_Combine_Data.Rdata/simulation_boot_1_2021-02-02-08:48:27.RData'

# load data
load_name <- load(data_path)
eval(parse(text=sprintf("seg_data_list <- %s", load_name)))

missing_data <- seg_data_list$simulation_missing_data_boot_list
complete_data <- seg_data_list$complete_data_boot_list

# get missing true value
sprintf('get missing coord and true value\n') %>% cat()
missing_true_list <- list()
missing_coord_list <- list()
for (scale_name in names(missing_data)){
  sprintf('    scale: %s...\n', scale_name) %>% cat()
  for (boot_num in 1:length(missing_data[[scale_name]])){
    missing_coord <- which(is.na(missing_data[[scale_name]][[boot_num]]$simulation_data), arr.ind = T)
    
    missing_coord_list[[scale_name]][[boot_num]] <- missing_coord
    missing_true_list[[scale_name]][[boot_num]] <- complete_data[[scale_name]][[boot_num]][missing_coord]
  }
}

# EM imputation
time_em_op <- Sys.time()
sprintf('EM imputation, Now is %s\n', time_em_op) %>% cat()
EM_imp_list <- list()
for (scale_name in names(missing_data)){
  time_scale_op <- Sys.time()
  sprintf('\tscale: %s...\n', scale_name) %>% cat()
  
  boot_data_num <- length(missing_data[[scale_name]])
  for (boot_num in 1:boot_data_num){
    time_boot_op <- Sys.time()
    sprintf('\t\t%d/%d...', boot_num, boot_data_num) %>% cat()
    
    EM_imp_list[[scale_name]][[boot_num]] <- irmi(missing_data[[scale_name]][[boot_num]]$simulation_data)
    
    sprintf('boot time used: %.4f %s\n', 
            Sys.time() - time_boot_op, attr(Sys.time() - time_boot_op, 'units')) %>% cat()
  }
  sprintf('\tScale end. Now: %s, Scale time: %.4f %s, Total time: %.4f %s\n', 
          Sys.time(), 
          Sys.time() - time_scale_op, attr(Sys.time() - time_scale_op, 'units'),
          Sys.time() - time_em_op, attr(Sys.time() - time_em_op, 'units')) %>% cat()
  
}
