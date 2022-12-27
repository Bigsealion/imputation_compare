# s3.1.1: get model and summary
rm(list=ls())
library(ggplot2)
library(tidyr)
source('/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/imp_compare_utils.R')

time_op <- Sys.time()

# set parameter
# imp_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/cog_80miss/s1.4_Cognative_T1surf_Combine_Data.Rdata/simulation_boot_100_2021-02-16-17:16:06.RData'
# imp_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/cog_80miss_boot100/ImpResult_Imp_Method__vim_em__simulation_boot_100_2021-02-16-17:16:06.RData'
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_Cognative_T1surf_Combine_Data.Rdata/Formula__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/simuY_cog_70miss_boot100_CorStep'

# imp_data_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/simuY_cog_70miss_boot100/'
# raw_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/simuY_cog_70miss/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-02-24-05:39:23.RData'
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/newY_model__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/simuY_cog_70miss_boot100_CorStep_2'

# imp_data_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/Reho_age_boot100/'
# raw_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/Reho_age/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata/simulation_boot_100_2021-02-28-19:10:58.RData'
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata/Formula__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/Reho_age_boot100_CorStepVar'

# imp_data_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/cog_TrueMiss_NoRep0.8_boot100/'
# raw_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/cog_TrueMiss_NoRep0.8/s1.4_Cognative_T1surf_Combine_Data.Rdata/simulation_boot_100_2021-03-02-21:14:51.RData'
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_Cognative_T1surf_Combine_Data.Rdata/Formula__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/cog_TrueMiss_NoRep0.8_boot100'

# imp_data_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/Cog5_ZsHarQCT1_20Miss_NoRep0.8/'
# raw_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/Cog5_ZsHarQCT1_20Miss_NoRep0.8/s1.4_Cog5_ZsHarQCT1_CorCombine_Data.Rdata/simulation_boot_100_2021-03-06-20:16:45.RData'
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_Cog5_ZsHarQCT1_CorCombine_Data.Rdata/Formula__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/Cog5_ZsHarQCT1_20Miss_NoRep0.8'

# imp_data_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/CVLT_ZsHarQCT1_TrueMiss_NoRep0.8/'
# raw_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_ZsHarQCT1_TrueMiss_NoRep0.8/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata/simulation_boot_100_2021-03-07-09:33:54.RData'
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata/Formula__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/CVLT_ZsHarQCT1_TrueMiss_NoRep0.8'

# imp_data_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/Reho_age_Truemiss_NoRep0.8/'
# raw_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/Reho_age_Truemiss_NoRep0.8/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata/simulation_boot_100_2021-03-08-18:30:43.RData'
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata/Formula__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/Reho_age_TrueMiss_NoRep0.8'

# imp_data_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/CVLT_SimuY_TrueMiss_NoRep0.8/'
# raw_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_SimuY_TrueMiss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-03-09-12:17:52.RData'
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt/newYFormula_model__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/CVLT_SimuY_TrueMiss_NoRep0.8'

# imp_data_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_SimuY_TrueMiss_NoRep0.8/'
# raw_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_SimuY_TrueMiss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-03-10-11:25:09.RData'
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/reho/newYFormula_model__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/ReHo_SimuY_TrueMiss_NoRep0.8'

# imp_data_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_UnNormSimuY_80Miss_NoRep0.8/'
# raw_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_UnNormSimuY_80Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-03-18-15:56:11.RData'
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/reho_UnNorm/newYFormula_model__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/ReHo_UnNormSimuY_80Miss_NoRep0.8'

# imp_data_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/CVLT_UnNormSimuY_80Miss_NoRep0.8/'
# raw_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_UnNormSimuY_80Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-03-18-18:09:14.RData'
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt_UnNorm/newYFormula_model__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/CVLT_UnNormSimuY_80Miss_NoRep0.8'

imp_data_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/CVLT_TCoefSimuY_60Miss_NoRep0.8/'
raw_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_TCoefSimuY_60Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-04-27-16:13:03.RData'
formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt_TCoef/newYFormula_model__cor_step__.RData'
out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1.1_summary_data/CVLT_TCoefSimuY_60Miss_NoRep0.8_TEST'  # TEST PATH

# imp_data_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_40Miss_NoRep0.8/'
# raw_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_TCoefSimuY_40Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-04-27-15:37:00.RData'
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/reho_TCoef/newYFormula_model__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/ReHo_TCoefSimuY_40Miss_NoRep0.8'

pattern <- 'Imp_Method__(.*?)__'
# choosed_scale <- c('Base', 'RO', 'UG', 'GoNogo', 'PassBall', 'CVLT')
remove_scale <- NULL #c('nBack')  # if not using, set to NULL

is_get_model_summary <- T  # when the first running, set T
is_compare_by_ImputedValue <- T # when the first running, set T
is_compare_by_CrossValidation <- T # when the first running, set T

is_print_imputed_value_figure <- F # if T, out ImpValue compare figure
is_print_cv_plot <- T  # if T, out ImpPredict result value figure

# get model summary===============
if(is_get_model_summary){
  sprintf('Remove scale:\n') %>% cat()
  for (remove_sc in remove_scale){
    sprintf('    %s\n', remove_sc) %>% cat
  }
  
  # get true and complete summary
  get_impdata_summary(raw_data_path, formula_path, out_dir, special_mode='true',remove_scale=remove_scale)
  get_impdata_summary(raw_data_path, formula_path, out_dir, special_mode='complete',remove_scale=remove_scale)
  
  # get imp summary
  for (file_name in dir(imp_data_dir)){
    if (str_detect(file_name, pattern)){
      imp_path <- file.path(imp_data_dir, file_name)
      sprintf('file: %s\n', file_name) %>% cat()
      get_impdata_summary(imp_path, formula_path, out_dir, special_mode='', remove_scale=remove_scale)
    }
  }
  
  # get summary end
  sprintf('Get Summary Complete. Now: %s, Total time: %.4f %s\n', 
          Sys.time(), Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
  sprintf('================ Stack =======================\n') %>% cat()
  
  # stack 
  stack_summary_in_dir(out_dir)
  sprintf('Stack Complete. Now: %s, Total time: %.4f %s\n', 
          Sys.time(), Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
}

# compare by imputed value===================================================
if(is_compare_by_ImputedValue){
  # get imputed data
  {
    # load raw data
    raw_data_load_name <- load(raw_data_path)
    eval(parse(text=sprintf("raw_data <- %s", raw_data_load_name)))
    
    # get NA coord
    miss_coord_list <- lapply(raw_data$simulation_missing_data_boot_list, function(sc){
      lapply(sc, function(boot){
        which(is.na(boot$simulation_data), arr.ind = T)
      })
    })
    
    # check na number of each var
    # miss_coord_list1 <- lapply(miss_coord_list, function(sc){
    #   sapply(sc, function(boot){
    #     length(unique(boot[, 2]))
    #   })
    # })
    
    true_data_list <- get_imputed_value(raw_data$complete_data_boot_list, miss_coord_list, is_complete=T)
    # get true (by booted complete data)
    imp_value_method_list <- list()
    imp_value_method_list$complete <- true_data_list
    
    for(file_name in dir(imp_data_dir)){
      if (str_detect(file_name, pattern)){
        method_name <- str_match(file_name, pattern)[2]
        if (!(method_name %in% c('CCA'))){
          sprintf('Get imputed value...: %s\n', method_name) %>% cat()
          imped_data_path <- file.path(imp_data_dir, file_name)
          load_imp_name <- load(imped_data_path)
          eval(parse(text=sprintf("imp_data <- %s", load_imp_name)))
          
          if (!is.null(remove_scale)){
            for (remove_sc in remove_scale){
              imp_data[[remove_sc]] <- NULL
            }
          }
          
          imp_value_method_list[[method_name]] <- get_imputed_value(imp_data, miss_coord_list, is_complete=F)
        }
      }
    }
  }
  
  # mkdir out dir
  {
    imp_value_out_dir <- file.path(out_dir, 'imputed_value')
    if (!file.exists(imp_value_out_dir)){
      dir.create(imp_value_out_dir, recursive = T)
      sprintf('Create res_box save dir! %s\n', imp_value_out_dir) %>% cat()
    }
  }
 
  # draw imp value figure 
  if(is_print_imputed_value_figure){
    sprintf('========================= Figure =====================\n') %>% cat()
    # box figure
    # only for numeric data
    {
      # convert scale to out layer
      imp_value_scale_list <- list()
      for(method_name in names(imp_value_method_list)){
        for (scale_name in names(imp_value_method_list[[method_name]])){
          imp_value_scale_list[[scale_name]][[method_name]] <- imp_value_method_list[[method_name]][[scale_name]]
        }
      }
      
      # melt 
      for (scale_name in names(imp_value_scale_list)){
        melt_imp_value_scale_list <- melt(imp_value_scale_list[[scale_name]])
        names(melt_imp_value_scale_list) <- c('value', 'var', 'boot', 'method')
        
        gg_imp_value_bias_box <- ggplot(melt_imp_value_scale_list, aes(x = var, y = value, fill=method)) +
          geom_boxplot(alpha=0.7) +
          stat_summary(fun=mean, geom="point", shape=23) +  # mean line
          scale_y_continuous(name = "imputed value")+
          scale_x_discrete(name = "variable") +
          ggtitle(sprintf("%s imputed vale", scale_name))+
          theme(legend.title = element_blank(),
                legend.position = 'right',
                axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1))
        
        ggsave(file.path(imp_value_out_dir, sprintf('%s_imputed_value.png', scale_name)),
               plot = gg_imp_value_bias_box, device = NULL, path = NULL,
               scale = 1, width = 12, height = 6, units ="in",
               dpi = 300, limitsize = T)
        sprintf('%s imputed value saved!\n', scale_name) %>% cat
      }
    }
  }
  
  # NRMSE and Cor
  {
    # calculate
    {
      method_list <- names(imp_value_method_list)
      method_list <- method_list[method_list != 'complete']
      
      nrmse_time_op <- Sys.time()
      NRMSE_list <- list()
      Cor_list <- list()
      for(meth in method_list){
        imp_value_me <- imp_value_method_list[[meth]]
        sprintf('method: %s\n', meth) %>% cat
        for (sc in names(imp_value_me)){
          sprintf('    scale: %s\n', sc) %>% cat
          # NRMSE
          NRMSE_list[[meth]][[sc]] <- map2(imp_value_me[[sc]], imp_value_method_list$complete[[sc]], function(bootx, bootc){
            map2(bootx, bootc, function(varx, varc){
              sqrt(sum((varx - varc)^2) / length(varx)) / sd(varc)
            })
          })
          
          # Cor
          Cor_list[[meth]][[sc]] <- map2(imp_value_me[[sc]], imp_value_method_list$complete[[sc]], function(bootx, bootc){
            map2(bootx, bootc, function(varx, varc){
              cor(varx, varc)
            })
          })
        }
      }
      
      NRMSE_summary_list <- sapply(NRMSE_list, function(met){
        sapply(met, function(sc){
          sapply(sc, unlist) %>% apply(1, mean)
        }) %>% mean
      })
      
      Cor_summary_list <- sapply(Cor_list, function(met){
        sapply(met, function(sc){
          sapply(sc, unlist) %>% apply(1, mean)
        }) %>% mean
      })
    }
    
    # figure
    {
      # NRMSE
      
    }
    
    # out
    imp_value_compare_outpath <- file.path(imp_value_out_dir, 'ImpValueCompare.Rdata')
    imp_value_compare_save <- list(NRMSE_raw=NRMSE_list, NRMSE_sum=NRMSE_summary_list,
                                   Cor_raw=Cor_list, Cor_sum=Cor_summary_list)
    save(imp_value_compare_save, file=imp_value_compare_outpath)
    sprintf('NRMSE saved! %s\n', imp_value_compare_outpath) %>% cat
    sprintf('Imp Value compare, Now: %s, Total time: %.4f %s\n', 
            Sys.time(), Sys.time() - nrmse_time_op, attr(Sys.time() - nrmse_time_op, 'units')) %>% cat()
  }
  
  sprintf('Get imputed value end. Now: %s, Total time: %.4f %s\n', 
          Sys.time(), Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
  sprintf('==================================================\n') %>% cat()
  
}

# compare impute method by cross validation ===============================
if(is_compare_by_CrossValidation){
  sprintf('=====================Compare by Cross Validation=======================\n') %>% cat()
  cv_time_op <- Sys.time()
  
  imp_cv_compare_boot_list <- list()
  imp_cv_res_boot_list <- list()

  # run in boot complete data
  sprintf('file: Complete\n') %>% cat()
  cv_out_list <- get_imp_cross_validation_compare_batch(
    raw_data_path, formula_path,
    is_complete_data = TRUE, remove_scale = remove_scale
  )

  imp_cv_compare_boot_list[['complete']] <- cv_out_list$ind
  imp_cv_res_boot_list[['complete']] <- cv_out_list$res
  
  # run in boot imputed data
  for (file_name in dir(imp_data_dir)){
    if (str_detect(file_name, pattern)){
      method_name <- str_match(file_name, pattern)[2]
      imp_path <- file.path(imp_data_dir, file_name)
      sprintf('file: %s\n', file_name) %>% cat()
      cv_out_list <-
        get_imp_cross_validation_compare_batch(imp_path, formula_path,
                                               is_complete_data=F, remove_scale=remove_scale)

      imp_cv_compare_boot_list[[method_name]] <- cv_out_list$ind
      imp_cv_res_boot_list[[method_name]] <- cv_out_list$res
    }
  }
  
  sprintf('Coss Validation Compare end. Now: %s, CV time used: %.4f %s\n, Total time: %.4f %s\n', 
          Sys.time(), 
          Sys.time() - cv_time_op, attr(Sys.time() - cv_time_op, 'units'),
          Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
  
  # draw figure
  {
    # mkdir
    {
      cv_out_dir <- file.path(out_dir, 'cv_res')
      if (!file.exists(cv_out_dir)){
        dir.create(cv_out_dir, recursive = T)
        sprintf('Create CV result dir: %s\n', cv_out_dir) %>% cat()
      }
      else{
        sprintf('Cross Validation Compare Result Save dir: %s\n', cv_out_dir) %>% cat
      }
    }

    # melt data
    {
      imp_cv_compare_boot_list_summary <- lapply(imp_cv_compare_boot_list, function(met){
        lapply(met, function(sc){
          cv_res <- sapply(sc, function(boot){boot})
          res_mean <- apply(cv_res, 1, function(ind){data.frame(mean=mean(ind), sd=sd(ind))})
        })
      })
    }
    
    melt_cv_res <- melt(imp_cv_compare_boot_list_summary)
    colnames(melt_cv_res) <- c('stat', 'value', 'indicator', 'scale', 'method')
    cv_res_stat <- spread(melt_cv_res, stat, value)
    
    # colnames(melt_cv_res) <- c('compare_res', 'scale', 'value', 'method')
    # melt_cv_res [, 1] <- as.character(melt_cv_res[, 1]) 
    # melt_cv_res [, 2] <- as.character(melt_cv_res[, 2]) 
    
    # draw figure
    sprintf('Saved:\n') %>% cat
    for (compare_ind in unique(cv_res_stat[, 'indicator'])){
      for (scale_name in unique(cv_res_stat[, 'scale'])){
        melt_cv_res_fig <- filter(cv_res_stat, indicator==compare_ind, scale==scale_name)
        
        gg_cv = ggplot(melt_cv_res_fig, aes(x = reorder(method, abs(mean)),y = mean,fill = method))+
          geom_bar(stat ="identity",width = 0.6, color='black', position = "dodge")+   
          geom_errorbar(aes(ymin = mean, ymax = mean+sd), 
                        position = position_dodge(width=0.6),
                        width = 0.25)+
          geom_text(aes(label = sprintf('%.4f', mean), vjust = -0.8, hjust = 0.5), size=3) +
          labs(x = "variables",y = compare_ind, title = sprintf('%s Cross Validaiton Compare', scale_name))+  
          guides(fill = guide_legend(reverse = F))+
          theme(legend.title = element_blank(),
                legend.position = 'right',
                axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
        
        ggsave(file.path(cv_out_dir, sprintf('CrossValidationCompareEB_%s_%s.png', compare_ind, scale_name)),
               plot = gg_cv, device = NULL, path = NULL,
               scale = 1, width = 12, height = 6, units = "in",
               dpi = 300, limitsize = T)
        sprintf('    %s_%s\n', compare_ind, scale_name) %>% cat
      }
    }
  }

  # save cv result -----------------------------------------
  # cv indicator
  cv_rdata_save_path <- file.path(cv_out_dir, 'CrossValidationBootResult.RData')
  save(imp_cv_compare_boot_list, file = cv_rdata_save_path)
  sprintf('Cross Validatino Compare Result Rdata Saved! %s\n', cv_rdata_save_path) %>% cat

  # CV values
  cv_values_save_path <- file.path(cv_out_dir, 'CrossValidationBootValues.RData')
  save(imp_cv_res_boot_list, file = cv_values_save_path)
  sprintf('Cross Validatino Values Rdata Saved! %s\n', cv_values_save_path) %>% cat

  sprintf('Cross Validatino Compare Complete! %s\n', Sys.time()) %>% cat
}
