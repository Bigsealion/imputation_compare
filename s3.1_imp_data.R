# s3.1: get data which wouldn't get error when run mice
# s3.1_1: only imp, not compare
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
rm(list=ls())
debugSource('/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/imp_compare_utils.R')
library(mice)
library(dplyr)
library(naniar)
library(pheatmap)

time_op <- Sys.time()
sprintf('Open time: %s\n', time_op) %>% cat()
# set parmeter
# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/test/choose_psy_1.5.RData/simulation_boot_100_2021-01-28-17:39:26.RData'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/psy5_missing50_boot100_data_list'
# imp_predictor_pattern_save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/psy5_missing50_boot100_data_list/pattern'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/cog_50miss/s1.4_Cognative_T1surf_Combine_Data.Rdata/simulation_boot_100_2021-02-13-16:51:56.RData'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/cog_50miss_boot100'
# imp_predictor_pattern_save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/cog_50miss_boot100/pattern'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/simuY_cog_50miss/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-02-25-05:12:09.RData'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/simuY_cog_50miss_boot100'
# imp_predictor_pattern_save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/simuY_cog_50miss_boot100/pattern'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/Reho_age_40miss/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata/simulation_boot_100_2021-03-05-09:32:29.RData'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/Reho_age_40miss_boot100'
# imp_predictor_pattern_save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/Reho_age_40miss_boot100/pattern'

#source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/cog_TrueMiss/s1.4_Cognative_T1surf_Combine_Data.Rdata/simulation_boot_100_2021-03-01-14:29:52.RData'
#save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/cog_TrueMiss_boot100'
#imp_predictor_pattern_save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/cog_TrueMiss_boot100/pattern'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/cog_TrueMiss_NoRep0.8/s1.4_Cognative_T1surf_Combine_Data.Rdata/simulation_boot_100_2021-03-02-21:14:51.RData'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/cog_TrueMiss_NoRep0.8_boot100'
# imp_predictor_pattern_save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/cog_TrueMiss_NoRep0.8_boot100/pattern'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/Cog5_ZsHarQCT1_60Miss_NoRep0.8/s1.4_Cog5_ZsHarQCT1_CorCombine_Data.Rdata/simulation_boot_100_2021-03-06-20:31:25.RData'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/Cog5_ZsHarQCT1_60Miss_NoRep0.8'
# imp_predictor_pattern_save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/Cog5_ZsHarQCT1_60Miss_NoRep0.8/pattern'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_ZsHarQCT1_TrueMiss_NoRep0.8/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata/simulation_boot_100_2021-03-07-09:33:54.RData'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/CVLT_ZsHarQCT1_TrueMiss_NoRep0.8'
# imp_predictor_pattern_save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/CVLT_ZsHarQCT1_TrueMiss_NoRep0.8/pattern'

# source_path='/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_SimuY_80Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-03-10-11:29:29.RData'
# save_dir='/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_SimuY_80Miss_NoRep0.8'
# imp_predictor_pattern_save_dir='/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_SimuY_80Miss_NoRep0.8/pattern'

# source_path='/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_UnNormSimuY_80Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-03-18-15:56:11.RData'
# save_dir='/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_UnNormSimuY_80Miss_NoRep0.8'
# imp_predictor_pattern_save_dir='/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_UnNormSimuY_80Miss_NoRep0.8/pattern'

# source_path='/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_UnNormSimuY_80Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-03-18-18:09:14.RData'
# save_dir='/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/CVLT_UnNormSimuY_80Miss_NoRep0.8'
# imp_predictor_pattern_save_dir='/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/CVLT_UnNormSimuY_80Miss_NoRep0.8/pattern'

source_path='/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_TCoefSimuY_TrueMiss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-04-27-15:30:31.RData'
save_dir='/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_TrueMiss_NoRep0.8'
imp_predictor_pattern_save_dir='/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_TrueMiss_NoRep0.8/pattern'

imp_method <- 'mice_rf'  # ['CCA', mice_mean'. 'mice_pmm', 'mice_rf', mice_norm_pred', 'raw', 'vim_em', 'vim_em_robust]
is_check_data <- F  # using T to check error, F to run. if T, check result will be out to imp_predictor_pattern_save_dir

choose_name <- c("Base", "RO", "UG", "GoNogo", 'nBack', 'PassBall', 'CVLT')
is_choose_name <- F # is using sub data, setting T, apply choose_name
seed <- 123456
set.seed(seed)

print(Sys.info())
sprintf('Set seed: %d\n', seed) %>% cat()
sprintf('Imp method: %s\n', imp_method) %>% cat()


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

