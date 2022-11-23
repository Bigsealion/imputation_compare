# s3.1: get data which wouldn't get error when run mice
# no error means wouldn't get error when run mice
# return imputed data and summary of lm model (contain CI)
# data structure:
{
  # save_list 
  #     |--> simulation_missing_data_boot_list  
  #         |--> scale 1  # structure in other scale is same to this
  #             |--> bootstrap 1  #  bootstrap sample in raw data, other is same to this
  #                 |--> simulation_data  # missing data which is simulation by true pattern
  #                     |--> col 1: RefID  # (there are some repeat ID as a result from bootstrap)
  #                     |--> col 2: T1Surf  # which is y (outcome), no missing
  #                     |--> col 3~end: content of sacle  # missing by simulation
  #                 |--> missing_pattern  # is first n missing pattern
  #                 |--> simulation_number  # number of each pattern in this simulation data
  #             |--> bootstrap ...
  #             |--> bootstrap n
  #         |--> scale 2
  #         |--> sacle ...
  #         |--> scale n
  #     |--> complete_data_boot_list
  #         |--> scale 1
  #             |--> bootstrap 1  # which structure is same to simulation_data, but no missing
  #                 |--> col 1: RefID 
  #                 |--> col 2: T1Surf 
  #                 |--> col 3~end: content of sacle  
  #             |--> bootstrap ...
  #             |--> bootstrap n
  #         |--> sacle ...
  #         |--> scale n
}

source('/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/imp_compare_utils.R')
library(mice)
library(dplyr)
library(naniar)
library(pheatmap)

time_op <- Sys.time()
sprintf('Open time: %s\n', time_op) %>% cat()
# set parmeter
source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/choose_psy_1.5.RData/simulation_boot_100_2021-01-16-19:09:47.RData'
save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/psy5_boot100_data_list'
imp_predictor_pattern_save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/test2_boot_data_list/pattern'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/choose_cog_1.5.RData/simulation_boot_100_2021-01-17-20:38:57.RData'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/cog_test_boot_data_list'
# imp_predictor_pattern_save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/cog_choose_boot_data_list/pattern'

imp_method <- 'mice_mean'  # ['CCA', mice_mean'. 'mice_pmm']
is_check_data <- F  # using T to check error, F to run. if T, check result will be out to imp_predictor_pattern_save_dir

choose_name <- c("Base", "City", "RO", "UG", "GoNogo")
is_choose_name <- F # is using sub data, setting T, apply choose_name

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
# run imputation and suammry model
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
    
    # get summary and CI
    imp_summary_list <- get_imp_summary_list(imp_res, imp_method)
    
    # save summary of imputed data
    # .RData will in tail of basename(source_path)
    save_path <- file.path(save_dir, sprintf('Summary_Imp_Method__%s__%s', imp_method, basename(source_path)))
    save(imp_summary_list, file = save_path)
    sprintf('summary saved!: %s\n', save_path) %>% cat()
    
    save_path <- file.path(save_dir, sprintf('ImpResult_Imp_Method__%s__%s', imp_method, basename(source_path)))
    save(imp_res, file = save_path)
    sprintf('imp result saved!: %s\n', save_path) %>% cat()
    
    sprintf('Now: %s, Total time: %.4f %s\n', 
            Sys.time(), Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
  }
}
