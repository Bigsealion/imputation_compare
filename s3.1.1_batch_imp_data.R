# s3.1: get data which wouldn't get error when run mice
# s3.1_1: only imp, not compare
# no error means wouldn't get error when run mice
# return imputed data and summary of lm model (contain CI)
cat(sprintf('============ Start R Program ==============\n'))
rm(list=ls())
source('/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/imp_compare_utils.R')
library(mice)
library(dplyr)
library(naniar)
library(pheatmap)

time_op <- Sys.time()
sprintf('============ Start R Imputation ==============\n')%>% cat()
sprintf('Open time: %s\n', time_op) %>% cat()
# get parameter by input
Args <- commandArgs(T)

source_path <- Args[1]
save_dir <- Args[2]
imp_predictor_pattern_save_dir <- Args[3]
imp_method <- Args[4] # ['CCA', mice_mean'. 'mice_pmm', 'mice_norm_pred', 'raw', 'vim_em', 'vim_em_robust']

# set parmeter

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/Cog5_ZsHarQCT1_TrueMiss_NoRep0.8/s1.4_Cog5_ZsHarQCT1_CorCombine_Data.Rdata/simulation_boot_100_2021-03-06-20:15:06.RData'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/Cog5_ZsHarQCT1_TrueMiss_NoRep0.8'
# imp_predictor_pattern_save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/Cog5_ZsHarQCT1_TrueMiss_NoRep0.8/pattern'

# imp_method <- 'vim_em'  # ['CCA', mice_mean'. 'mice_pmm', 'mice_norm_pred', 'raw', 'vim_em', 'vim_em_robust']
is_check_data <- F  # using T to check error, F to run. if T, check result will be out to imp_predictor_pattern_save_dir

choose_name <- c("Base", "RO", "UG", "GoNogo", 'nBack', 'PassBall', 'CVLT')
is_choose_name <- F # is using sub data, setting T, apply choose_name
seed <- 123456
set.seed(seed)

print(Sys.info())
sprintf('Set seed: %d\n', seed) %>% cat()
sprintf('Imp method: %s\n', imp_method) %>% cat()
sprintf('source_path: %s\n', source_path) %>% cat()
sprintf('save_dir: %s\n', save_dir) %>% cat()

# load data
{
  load(source_path)
  sprintf('data loaded!\n') %>% cat()
  missing_data <- save_list$simulation_missing_data_boot_list
  
  # del ID
  missing_data <- lapply(missing_data, function(x){
    lapply(x, function(x){
      x$simulation_data <- x$simulation_data[, -1]
      return(x)
    })
  })
  
  sprintf('delete ID!\n') %>% cat()
}

# imp data----------------------------------------------------------------
# run imputation
{
  if (is_check_data){
    sprintf('run in check mode!\n') %>% cat()
    # mkdir out dir
    if (!file.exists(imp_predictor_pattern_save_dir)){
      dir.create(imp_predictor_pattern_save_dir, recursive = T)
      sprintf('Create check out dir! %s\n', imp_predictor_pattern_save_dir) %>% cat()
    }
    
    # check is error in imputation
    imp_res <- imp_data_batch(missing_data, imp_method)
    
    # get error name
    error_name <- c()
    for(scale_name in names(imp_res)){
      if (is.numeric(imp_res[[scale_name]])){
        error_name <- c(error_name, scale_name)
      }
      else{
        imp_n <- imp_res[[scale_name]][[1]]$imp
        # save logging
        write.csv(imp_n$loggedEvents, 
                  file.path(imp_predictor_pattern_save_dir, paste(scale_name, '_LogEvents.csv', sep = '')))
        
        # draw
        # png(file.path(imp_predictor_pattern_save_dir, paste(scale_name, '_predictor.png', sep = '')),
        #     width=1200, height=1200)
        pheatmap(imp_n$predictorMatrix, cluster_rows = F, cluster_cols = F, fontsize = 20,
                 filename=file.path(imp_predictor_pattern_save_dir, paste(scale_name, '_predictor.png', sep = '')),
                 width=12, height=12, silent = T)
        # dev.off()
      }
    }
    
    print('=========ERROR name=========')
    print(error_name)
    
  }
  else{
    sprintf('run in imputation mode!\n') %>% cat()
    # mkdir out dir
    if (!file.exists(save_dir)){
      dir.create(save_dir,recursive = T)
      sprintf('Create save dir! %s\n', save_dir) %>% cat()
    }
    
    #===================run in choose data=====================
    if (!is_choose_name){choose_name <- names(missing_data)}
    sprintf('choosing psy scale:\n') %>% cat()
    sprintf('    %s\n', choose_name) %>% cat()
    missing_data_choose <- missing_data[choose_name]
    
    # imp
    imp_res <- imp_data_batch(missing_data_choose, imp_method)
    
    save_path <- file.path(save_dir, sprintf('ImpResult_Imp_Method__%s__%s', imp_method, basename(source_path)))
    save(imp_res, file = save_path)
    sprintf('imp result saved!: %s\n', save_path) %>% cat()
    
    sprintf('Now: %s, Total time: %.4f %s\n', 
            Sys.time(), Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
  }
}
