# s2.1 get missing pattern which combine 1 T1surf and psytool
# using bootstrap to get n sample
rm(list = ls())
source('/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/imp_compare_utils.R')

time_op <- Sys.time()
sprintf('Start time: %s\n', time_op) %>% cat
# set parameter
# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/combine_data/choose_psy_1.5.RData'
# img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/psytool/single/choose_1.5_pattern/'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/test'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_Cognative_T1surf_Combine_Data.Rdata'
# # source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup//s1.7_SimulationOutcomeData.RData'
# img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/cog_TrueMiss_NoRep0.8/img'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/cog_TrueMiss_NoRep0.8'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/Reho_age/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata'
# img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/Reho_age_20miss_NoRep0.8/img'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/Reho_age_20miss_NoRep0.8'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_Cog5_ZsHarQCT1_CorCombine_Data.Rdata'
# img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/Cog5_ZsHarQCT1_TrueMiss_NoRep0.8/img'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/Cog5_ZsHarQCT1_TrueMiss_NoRep0.8'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata'
# img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_ZsHarQCT1_80Miss_NoRep0.8/img'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_ZsHarQCT1_80Miss_NoRep0.8'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt/s1.7_SimulationOutcomeData.RData'
# img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_SimuY_80Miss_NoRep0.8/img'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_SimuY_80Miss_NoRep0.8'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/reho/s1.7_SimulationOutcomeData.RData'
# img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_SimuY_80Miss_NoRep0.8/img'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_SimuY_80Miss_NoRep0.8'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/reho_UnNorm/s1.7_SimulationOutcomeData.RData'
# img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_UnNormSimuY_TrueMiss_NoRep0.8/img'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_UnNormSimuY_TrueMiss_NoRep0.8'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt_UnNorm/s1.7_SimulationOutcomeData.RData'
# img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_UnNormSimuY_TrueMiss_NoRep0.8/img'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_UnNormSimuY_TrueMiss_NoRep0.8'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/reho_TCoef/s1.7_SimulationOutcomeData.RData'
# img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_TCoefSimuY_80Miss_NoRep0.8/img'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_TCoefSimuY_80Miss_NoRep0.8'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt_TCoef/s1.7_SimulationOutcomeData.RData'
# img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_TCoefSimuY_80Miss_NoRep0.8/img'
# save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_TCoefSimuY_80Miss_NoRep0.8'

source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt_IntTCoef/s1.7_SimulationOutcomeData.RData'
img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_IntTCoefSimuY_TrueMiss_NoRep0.8/img'
save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_IntTCoefSimuY_TrueMiss_NoRep0.8'

is_out_pattern_img <- F
boot_num <- 100
is_replace <- F  # if T, sample is back up, F mean no repetition
sample_size <- 0.8  # 0~1 means sample_size * data_size, >1 means size number
pattern_num <- -1 # -1 means all pattern
add_missing_rate <- NULL  # 0~1, if true rate, setting to NULL

save_dir <- file.path(save_dir, basename(source_path))

# print log
sprintf('add_missing_rate: %.3f\nsample_size: %.3f\n', 
        add_missing_rate, sample_size) %>% cat

# mkdir out dir
{
  if (!file.exists(img_dir)){
    dir.create(img_dir, recursive = T)
    sprintf('Create img dir! %s\n', img_dir) %>% cat()
  }
  if (!file.exists(save_dir)){
    dir.create(save_dir, recursive = T)
    sprintf('Create save dir! %s\n', save_dir) %>% cat()
  }
}

# load data
{
  load_name <- load(source_path)
  eval(parse(text=sprintf("data_save_list <- %s", load_name)))
  
  miss_data_list <- data_save_list
  complete_data_list <- lapply(data_save_list, function(psy){
    psy[complete.cases(psy), ]
  })
}

# out patten image of each scales
if (is_out_pattern_img){
  for (name_i in names(miss_data_list)){
    out_path = file.path(img_dir, paste(name_i, '_pattern.png', sep=''))
    png(out_path, width=1200, height=1200)
    md.pattern(miss_data_list[[name_i]], rotate.names = T)
    dev.off()
    print(name_i)
  }
}

# bootstrap
{
  boot_op_time <- Sys.time()
  # miss_data_boot_list <- lapply(miss_data_list, get_bootstrap_data,
  #                               sample_number=boot_num, sample_size=sample_size, is_replace=is_replace)
  # 
  # complete_data_boot_list <- lapply(miss_data_boot_list, function(x){
  #   lapply(x, function(x){x[complete.cases(x),]})
  # })
  
  complete_data_boot_list <- lapply(complete_data_list, get_bootstrap_data, 
                                    sample_number=boot_num, sample_size=sample_size, is_replace=is_replace)
  
  sprintf('bootstrap end, time used: %.4f %s\n', 
          Sys.time() - boot_op_time, attr(Sys.time() - boot_op_time, 'units')) %>% cat()
}

# add simulation missing pattern in complete data
{
  simu_op_time <- Sys.time()
  # # 2021.01.12, using total missing pattern rather than boot missing pattern
  # simulation_missing_data_list <- map2(complete_data_boot_list, miss_data_list, function(c, m, pattern_num, add_missing_rate){
  #   return(lapply(c, function(c, m, pattern_num, add_missing_rate){
  #     sprintf('simulation end %s\n', Sys.time()) %>% cat()
  #     return(simulation_missing_pattern(c, m, pattern_num=pattern_num, add_missing_case_rate=add_missing_rate))
  #   }, m, pattern_num, add_missing_rate))
  # }, pattern_num=pattern_num, add_missing_rate=add_missing_rate)
  
  sprintf('Simulation Pattern...\n') %>% cat()
  simulation_missing_data_list <- list()
  scale_i <- 0
  scale_number <- length(complete_data_boot_list)
  
  for (scale_name in names(complete_data_boot_list)){
    scale_op_time <- Sys.time()
    scale_i = scale_i + 1
    boot_number <- length(complete_data_boot_list[[scale_name]])
    for (boot_i in 1:boot_number){
      boot_op_time <- Sys.time()
      simulation_missing_data_list[[scale_name]][[boot_i]] <-
        simulation_missing_pattern(complete_data_boot_list[[scale_name]][[boot_i]],
                                   miss_data_list[[scale_name]],
                                   pattern_num=pattern_num, 
                                   add_missing_case_rate=add_missing_rate)
      
      scale_time <- Sys.time() - scale_op_time
      boot_time <- Sys.time() - boot_op_time
      total_time <- Sys.time() - time_op
      sprintf('    Scale: %d/%d(%s %.4f %s), Boot: %d/%d(%.4f %s), total: %.4f %s\n', 
              scale_i, scale_number, scale_name,
              scale_time, attr(scale_time, 'units'),
              boot_i, boot_number,
              boot_time, attr(boot_time, 'units'),
              total_time, attr(total_time, 'units')) %>% cat()
    }
  }
  
  sprintf('Simulation end, time used: %.4f %s\n', 
          Sys.time() - simu_op_time, attr(Sys.time() - simu_op_time, 'units')) %>% cat()
}

# save data
save_list <- list(simulation_missing_data_boot_list=simulation_missing_data_list,
                  complete_data_boot_list=complete_data_boot_list, 
                  total_complete_data_list=complete_data_list)

save_path <- file.path(save_dir, sprintf('simulation_boot_%d_%s.RData', boot_num,
                                         strftime(Sys.time(),format='%Y-%m-%d-%H:%M:%S')))
save(save_list, file=save_path)

sprintf('simulation data saved!: %s\n', save_path) %>% cat()
sprintf('save end, total time used: %.4f %s\n', 
        Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
