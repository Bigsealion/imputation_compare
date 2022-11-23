# not using code which for Master Paper

# formula ??? for test
if(F){
  rm(list=ls())
  cvlt_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata/model__cor_step__.RData'
  load(cvlt_path)
  
  cvlt_feature <- as.character(formula_save$cvlt)[3]
  
  reho_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata/model__cor_step__.RData'
  load(reho_path)
  
  step_model$reho$coefficients %>% length
  
  reho_feature <- as.character(formula_save$reho)[3]
  
  sprintf('%s\n', reho_feature) %>% cat()
}

# 2.1  impute value compare figure
if(F){
  rm(list=ls())
  # set parameter
  source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/'
  out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/ImpValueCompare_1/'
  subdir_name <- 'imputed_value'
  
  # file_pattern <- 'Reho_age_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'ReHo_True'
  
  # file_pattern <- 'ReHo_SimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'ReHo_SimuY'
  
  # file_pattern <- 'ReHo_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'ReHo_UnNormSimuY'
  
  # file_pattern <- 'CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'CVLT_True'
  
  # file_pattern <- 'CVLT_SimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'CVLT_SimuY'
  
  file_pattern <- 'CVLT_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  save_name <- 'CVLT_UnNormSimuY'
  
  fill_level <- c('TrueMiss', '20Miss', '40Miss', '60Miss', '80Miss')
  # fill_level <- c('Truemiss', '20miss', '40miss', '60miss', '80miss')
  
  out_dir <- file.path(out_dir, save_name)
  
  # mkdir
  if (!file.exists(out_dir)){
    dir.create(out_dir,recursive = T)
    sprintf('Create out dir! %s\n', out_dir) %>% cat()
  }
  else{
    sprintf('out dir: %s\n', out_dir) %>% cat
  }
  
  # load data
  data_list <- list()
  for (file_name in dir(source_dir)){
    if (str_detect(file_name, file_pattern)){
      print(file_name)
      missing_rate <- str_match(file_name, file_pattern)[2]
      
      subdir_path <- file.path(source_dir, file_name, subdir_name)
      subfile_path <- file.path(subdir_path, 'ImpValueCompare.Rdata')
      
      data_load_name <- load(subfile_path)
      eval(parse(text=sprintf("data_list[['%s']]  <- %s",missing_rate, data_load_name)))
    }
  }
  
  # remove NA and Inf, get summary
  data_list_NRMSE <- sapply(data_list, function(mr){
    NRMSE_summary_list <- sapply(mr$NRMSE_raw, function(met){
      sapply(met, function(sc){
        sapply(sc, function(x){
          x <- unlist(x)
          x[is.infinite(x)] <- NA
          return(x)
        }) %>% apply(1, mean, na.rm=T)
      }) %>% mean(na.rm=T)
    })
  })
  
  data_list_Cor <- sapply(data_list, function(mr){
    NRMSE_summary_list <- sapply(mr$Cor_raw, function(met){
      sapply(met, function(sc){
        sapply(sc, function(x){
          x <- unlist(x)
          x[is.infinite(x)] <- NA
          return(x)
        }) %>% apply(1, mean, na.rm=T)
      }) %>% mean(na.rm=T)
    })
  })
  data_list_Cor <- data_list_Cor[complete.cases(data_list_Cor),]
  
  # figure
  {
    # NRMSE
    {
      NRMSE_melt <- melt(data_list_NRMSE)
      colnames(NRMSE_melt) <- c('method', 'MissRate', 'NRMSE')
      NRMSE_melt[, 'MissRate'] <- factor(NRMSE_melt[, 'MissRate'], level=fill_level)
      
      # ggplot bar
      gg_NRMSE = ggplot(NRMSE_melt, aes(x = method,y = NRMSE, fill = MissRate))+
        geom_bar(stat ="identity",width = 0.6,position = "dodge")+
        labs(x = "Method",y = "NRMSE", title = save_name)+  
        guides(fill = guide_legend(reverse = F))+
        theme(legend.title = element_blank(),
              legend.position = 'right',
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
      
      ggsave(file.path(out_dir, sprintf('%s_NRMSE.png', save_name)),
             plot = gg_NRMSE, device = NULL, path = NULL,
             scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             dpi = 300, limitsize = T)
    }
    
    # Cor
    {
      Cor_melt <- melt(data_list_Cor)
      colnames(Cor_melt) <- c('method', 'MissRate', 'NRMSE')
      Cor_melt[, 'MissRate'] <- factor(Cor_melt[, 'MissRate'], level=fill_level)
      
      # ggplot bar
      gg_Cor = ggplot(Cor_melt, aes(x = method,y = NRMSE, fill = MissRate))+
        geom_bar(stat ="identity",width = 0.6,position = "dodge")+
        labs(x = "Method",y = "NRMSE", title = save_name)+  
        guides(fill = guide_legend(reverse = F))+
        theme(legend.title = element_blank(),
              legend.position = 'right',
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
      
      ggsave(file.path(out_dir, sprintf('%s_Cor.png', save_name)),
             plot = gg_Cor, device = NULL, path = NULL,
             scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             dpi = 300, limitsize = T)
    }
    
    sprintf('Figure saved!\n') %>% cat
  }
  
  # csv
  {
    NRMSE_csv_save_path <- file.path(out_dir, sprintf('%s_NRMSE.csv', save_name))
    Cor_csv_save_path <- file.path(out_dir, sprintf('%s_Cor.csv', save_name))
    
    write.csv(data_list_NRMSE, file=NRMSE_csv_save_path)
    write.csv(data_list_Cor, file=Cor_csv_save_path)
    
    sprintf('csv saved!\n') %>% cat
  }
  
}

# 3.1 model PBNED
if(F){
  rm(list=ls())
  # set parameter
  img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2_compare_summary/'
  save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/ImpValueCompare_1/PBNED'
  mis_pattern <- '(True|\\d+)(M|m)iss'
  
  # file_pattern <- 'ReHo_UnNormSimuY_(True|\\d+)(M|m)iss_NoRep0.8'
  # scale_name <- 'reho'
  # save_name <- 'ReHo_UnNormSimuY'
  
  file_pattern <- 'CVLT_UnNormSimuY_(True|\\d+)(M|m)iss_NoRep0.8'
  scale_name <- 'cvlt'
  save_name <- 'CVLT_UnNormSimuY'
  
  # mkdir out dir
  if (!file.exists(save_dir)){
    dir.create(save_dir,recursive = T)
    sprintf('Create out dir! %s\n', save_dir) %>% cat()
  }
  
  
  # load data
  EucD_list <- list()
  for (mr_dir in dir(img_dir)){
    if (str_detect(mr_dir, file_pattern)){
      missing_rate <- str_match(mr_dir, mis_pattern)[1]
      res_data_name <- sprintf('res_%s', missing_rate)
      
      res_data_path <- file.path(img_dir, mr_dir, 'compare_figure_data.RData')
      res_load_name <- load(res_data_path)
      eval(parse(text=sprintf("%s <- %s",res_data_name, res_load_name)))
      
      # get info
      eval(parse(text=sprintf("EucD_list$'%s' <- %s$PBEucDis['%s',]"
                              ,missing_rate, res_data_name, scale_name)))
      
      sprintf('%s\n', missing_rate) %>% cat
    }
  }
  
  EucD_df <- as.data.frame(EucD_list)
  rownames(EucD_df) <- c('Complete', 'CCA', 'Mean', 'Pred', 'PMM', 'EM')
  colnames(EucD_df) <- c('20', '40', '60', '80', 'raw')
  
  EucD_mat <- as.matrix(EucD_df)
  
  EucD_melt <- melt(EucD_mat)
  colnames(EucD_melt) <- c('method', 'missrate', 'value')
  EucD_melt[, 2] <- factor(EucD_melt[, 2], levels = c('raw', 20,40,60,80))
  
  
  # save csv
  csv_save_path <- file.path(save_dir, sprintf('%s_PBNED.csv', save_name))
  write.csv(EucD_mat, file=csv_save_path)
  sprintf('csv saved! %s', csv_save_path) %>% cat
  
  
}

# 3.2 CI coverage rate
if(F){
  rm(list=ls())
  # set parameter
  source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2_compare_summary/'
  out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/CI_CR'
  subfile_name <- 'compare_figure_data.RData'
  
  # file_pattern <- 'CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'CVLT_True'
  
  # file_pattern <- 'CVLT_SimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'CVLT_Simu'
  
  file_pattern <- 'CVLT_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  save_name <- 'CVLT_UnNormSimu'
  
  # file_pattern <- 'Reho_age_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'ReHo_True'
  
  # file_pattern <- 'ReHo_SimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'ReHo_Simu'
  
  # file_pattern <- 'ReHo_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'ReHo_UnNormSimu'
  
  # mkdir
  if (!file.exists(out_dir)){
    dir.create(out_dir,recursive = T)
    sprintf('Create out dir! %s\n', out_dir) %>% cat()
  }
  else{
    sprintf('out dir: %s\n', out_dir) %>% cat
  }
  
  # load data
  data_list <- list()
  for (file_name in dir(source_dir)){
    if (str_detect(file_name, file_pattern)){
      print(file_name)
      missing_rate <- str_match(file_name, file_pattern)[2]
      
      subfile_path <- file.path(source_dir, file_name, subfile_name)
      
      data_load_name <- load(subfile_path)
      eval(parse(text=sprintf("data_list[['%s']]  <- %s",missing_rate, data_load_name)))
    }
  }
  
  data_list <- lapply(data_list, function(mr){mr$CR %>% as.data.frame()})
  data_list_df <- lapply(data_list, function(mr){
    sapply(mr, function(met){
      sapply(met, function(x){mean(x)}) 
    })
  })
  
  data_list_melt <- melt(data_list_df)
  colnames(data_list_melt)[4] <- 'missrate'
  data_list_save <- spread(data_list_melt, missrate, value)
  
  # csv
  {
    CI_CR_Result_csv_save_path <- file.path(out_dir, sprintf('%s_CI_CR_Result.csv', save_name))
    write.csv(data_list_save, file=CI_CR_Result_csv_save_path)
    
    sprintf('csv saved: %s\n', CI_CR_Result_csv_save_path) %>% cat
  }
  
}

# 3.3 AW bias rate
if(F){
  rm(list=ls())
  # set parameter
  source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2_compare_summary/'
  out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/CI_AW_PerBiasMean'
  subfile_name <- 'compare_result.RData'
  
  # file_pattern <- 'CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'CVLT_True'
  
  # file_pattern <- 'CVLT_SimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'CVLT_Simu'
  
  file_pattern <- 'CVLT_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  save_name <- 'CVLT_UnNormSimu'
  
  # file_pattern <- 'Reho_age_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'ReHo_True'
  
  # file_pattern <- 'ReHo_SimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'ReHo_Simu'
  
  # file_pattern <- 'ReHo_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'ReHo_UnNormSimu'
  
  # mkdir
  {
    if (!file.exists(out_dir)){
      dir.create(out_dir,recursive = T)
      sprintf('Create out dir! %s\n', out_dir) %>% cat()
    }
    else{
      sprintf('out dir: %s\n', out_dir) %>% cat
    }
  }
  
  # load data
  data_list <- list()
  for (file_name in dir(source_dir)){
    if (str_detect(file_name, file_pattern)){
      print(file_name)
      missing_rate <- str_match(file_name, file_pattern)[2]
      
      subfile_path <- file.path(source_dir, file_name, subfile_name)
      
      data_load_name <- load(subfile_path)
      eval(parse(text=sprintf("data_list[['%s']]  <- %s",missing_rate, data_load_name)))
    }
  }
  
  # AWPerBias_list <- lapply(data_list, function(mr){
  #   lapply(mr, function(met){
  #     lapply(met$compare_result_mean, function(sc){
  #       sc$AW_per_bias %>% as.data.frame() %>% t
  #     }) 
  #   })
  # })
  
  # AWPerVar_list_melt <- melt(AWPerBias_list)[, -1]
  # colnames(AWPerVar_list_melt) <- c('var','value', 'scale','method', 'missrate')
  # AWPerVar_list_melt_var <- spread(AWPerVar_list_melt, var, value)
  
  AWPerBiasMean_list <- lapply(data_list, function(mr){
    lapply(mr, function(met){
      lapply(met$compare_result_mean, function(sc){
        sc$AW_per_bias %>% mean
      }) 
    })
  })
  
  AWPerBiasMean_list_melt <- melt(AWPerBiasMean_list)
  colnames(AWPerBiasMean_list_melt) <- c('value', 'scale', 'method', 'missrate')
  AWPerBiasMean_list_melt_mr <- spread(AWPerBiasMean_list_melt, missrate, value)
  
  # csv
  {
    Result_csv_save_path <- file.path(out_dir, sprintf('%s_AW_PerBiasMean_Result.csv', save_name))
    write.csv(AWPerBiasMean_list_melt_mr, file=Result_csv_save_path)
    
    sprintf('csv saved: %s\n', Result_csv_save_path) %>% cat
  }
  
}

# 4.1 predict cv result
if(F){
  rm(list=ls())
  # set parameter
  source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/'
  out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/CVCompare'
  subdir_name <- 'cv_res'
  
  # file_pattern <- 'CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'CVLT_True'
  
  # file_pattern <- 'CVLT_SimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'CVLT_Simu'
  
  file_pattern <- 'CVLT_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  save_name <- 'CVLT_UnNormSimu'
  
  # file_pattern <- 'Reho_age_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'ReHo_True'
  
  # file_pattern <- 'ReHo_SimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'ReHo_Simu'
  
  # file_pattern <- 'ReHo_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
  # save_name <- 'ReHo_UnNormSimu'
  
  # mkdir
  if (!file.exists(out_dir)){
    dir.create(out_dir,recursive = T)
    sprintf('Create out dir! %s\n', out_dir) %>% cat()
  }
  else{
    sprintf('out dir: %s\n', out_dir) %>% cat
  }
  
  # load data
  data_list <- list()
  for (file_name in dir(source_dir)){
    if (str_detect(file_name, file_pattern)){
      print(file_name)
      missing_rate <- str_match(file_name, file_pattern)[2]
      
      subdir_path <- file.path(source_dir, file_name, subdir_name)
      subfile_path <- file.path(subdir_path, 'CrossValidationResult.RData')
      
      data_load_name <- load(subfile_path)
      eval(parse(text=sprintf("data_list[['%s']]  <- %s",missing_rate, data_load_name)))
    }
  }
  
  data_list_melt <- melt(data_list)
  names(data_list_melt)[6] <- 'missrate'
  data_list_save <- spread(data_list_melt, missrate, value)
  
  # csv
  {
    CvCompareResult_csv_save_path <- file.path(out_dir, sprintf('%s_CvResult.csv', save_name))
    write.csv(data_list_save, file=CvCompareResult_csv_save_path)
    
    sprintf('csv saved: %s\n', CvCompareResult_csv_save_path) %>% cat
  }
  
}

# 2.1.1  impute value compare figure; +std
if(T){
  rm(list=ls())
  # set parameter
  {
    source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/'
    out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/ImpValueCompare_2/'
    subdir_name <- 'imputed_value'
    
    file_pattern_list <- list()
    file_pattern_list[['CVLT_TGMV']] <- 'CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Gender']] <- 'Reho_age_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['CVLT_Y1']] <- 'CVLT_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Y2']] <- 'ReHo_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    
    missrate_level <- c('TrueMiss', '20Miss', '40Miss', '60Miss', '80Miss')
    method_level <- c('Complete', 'CCA', 'Mean', 'Pred', 'EM', 'PMM')
    
  }
  
  # mkdir
  if (!file.exists(out_dir)){
    dir.create(out_dir,recursive = T)
    sprintf('Create out dir! %s\n', out_dir) %>% cat()
  }
  else{
    sprintf('out dir: %s\n', out_dir) %>% cat
  }
  
  # run in each data pattern
  NRMSE_list_all <- list()
  PCC_list_all <- list()
  gg_list_all <- list()
  for (save_name in names(file_pattern_list)){
    sprintf('%s...\n', save_name) %>% cat
    # load data
    {
      data_list <- list()
      for (file_name in dir(source_dir)){
        if (str_detect(file_name, file_pattern_list[[save_name]])){
          print(file_name)
          missing_rate <- str_match(file_name, file_pattern_list[[save_name]])[2]
          
          subdir_path <- file.path(source_dir, file_name, subdir_name)
          subfile_path <- file.path(subdir_path, 'ImpValueCompare.Rdata')
          
          data_load_name <- load(subfile_path)
          eval(parse(text=sprintf("data_list[['%s']]  <- %s",missing_rate, data_load_name)))
        }
      }
    }
    
    # remove NA and Inf, get summary
    # NRMSE
    {
      NRMSE_list_all[[save_name]] <- list()
      for (mr in names(data_list)){
        for (met in names(data_list[[mr]][['NRMSE_raw']])){
          for (sc in names(data_list[[mr]][['NRMSE_raw']][[met]])){
            boot_mean <-sapply(data_list[[mr]][['NRMSE_raw']][[met]][[sc]], function(boot_v){
              boot_v <- unlist(boot_v)
              boot_v[is.infinite(boot_v)] <- NA
              return(boot_v)
            }) %>% apply(2, mean, na.rm=T)
            NRMSE_list_all[[save_name]][[mr]][[met]][[sc]] <- data.frame(mean=mean(boot_mean, na.rm=T), sd=sd(boot_mean, na.rm=T))
          }
        }
      }
      
      
      NRMSE_info_melt <- melt(NRMSE_list_all[[save_name]])
      colnames(NRMSE_info_melt) <- c('stat', 'value', 'scale', 'method', 'missrate')
      NRMSE_info_melt['scale'] <- save_name
      
      NRMSE_info_mr <- spread(NRMSE_info_melt, missrate, value)
      
    }
    
    # PCC
    {
      PCC_list_all[[save_name]] <- list()
      for (mr in names(data_list)){
        for (met in names(data_list[[mr]][['Cor_raw']])){
          for (sc in names(data_list[[mr]][['Cor_raw']][[met]])){
            boot_mean <-sapply(data_list[[mr]][['Cor_raw']][[met]][[sc]], function(boot_v){
              boot_v <- unlist(boot_v)
              boot_v[is.infinite(boot_v)] <- NA
              return(boot_v)
            }) %>% apply(2, mean, na.rm=T)
            PCC_list_all[[save_name]] [[mr]][[met]][[sc]] <- data.frame(mean=mean(boot_mean, na.rm=T), sd=sd(boot_mean, na.rm=T))
          }
        }
      }
      
      PCC_info_melt <- melt(PCC_list_all[[save_name]] )
      colnames(PCC_info_melt) <- c('stat', 'value', 'scale', 'method', 'missrate')
      PCC_info_melt['scale'] <- save_name
      
      PCC_info_mr <- spread(PCC_info_melt, missrate, value)
    }
    
    # figure
    {
      # NRMSE
      {
        # adjust
        {
          NRMSE_info_stat <- spread(NRMSE_info_melt, stat, value)
          
          NRMSE_info_stat[,'missrate'] <- factor(NRMSE_info_stat[,'missrate'], levels = missrate_level)
          
          NRMSE_info_stat[which(NRMSE_info_stat[,'method']=='mice_mean'), 'method'] <- 'Mean'
          NRMSE_info_stat[which(NRMSE_info_stat[,'method']=='mice_norm_pred'), 'method'] <- 'Pred'
          NRMSE_info_stat[which(NRMSE_info_stat[,'method']=='vim_em'), 'method'] <- 'EM'
          NRMSE_info_stat[which(NRMSE_info_stat[,'method']=='mice_pmm'), 'method'] <- 'PMM'
          NRMSE_info_stat[,'method'] <- factor(NRMSE_info_stat[,'method'], levels = method_level)
        }
        
        # ggplot bar
        gg_list_all[['NRMSE']][[save_name]] = ggplot(NRMSE_info_stat, aes(x = missrate,y = mean, fill = method))+
          geom_bar(stat ="identity",width = 0.6, color='black', position = "dodge")+
          geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), 
                        position = position_dodge(width=0.6),
                        width = 0.25)+
          labs(x = "Subject Miss Rate",y = "Imputation Value NRMSE", title = save_name, fill='Method')+
          guides(fill = guide_legend(reverse = F))+
          theme(legend.position = 'right',
                axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
                plot.title = element_text(hjust = 0.5))
      }
      
      # Cor
      {
        # adjust
        {
          PCC_info_stat <- spread(PCC_info_melt, stat, value)
          PCC_info_stat <- PCC_info_stat[complete.cases(PCC_info_stat), ]
          
          PCC_info_stat[,'missrate'] <- factor(PCC_info_stat[,'missrate'], levels = missrate_level)
          
          PCC_info_stat[which(PCC_info_stat[,'method']=='mice_mean'), 'method'] <- 'Mean'
          PCC_info_stat[which(PCC_info_stat[,'method']=='mice_norm_pred'), 'method'] <- 'Pred'
          PCC_info_stat[which(PCC_info_stat[,'method']=='vim_em'), 'method'] <- 'EM'
          PCC_info_stat[which(PCC_info_stat[,'method']=='mice_pmm'), 'method'] <- 'PMM'
          PCC_info_stat[,'method'] <- factor(PCC_info_stat[,'method'], levels = method_level)
        }
        
        # ggplot bar
        gg_list_all[['PCC']][[save_name]] = ggplot(PCC_info_stat, aes(x = missrate,y = mean, fill = method))+
          geom_bar(stat ="identity",width = 0.6, color='black', position = "dodge")+
          geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), 
                        position = position_dodge(width=0.6),
                        width = 0.25)+
          labs(x = "Subject Miss Rate",y = "Imputation Value PCC", title = save_name, fill='Method')+
          guides(fill = guide_legend(reverse = F))+
          theme(legend.position = 'right',
                axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
                plot.title = element_text(hjust = 0.5))
      }
    }
  }
  
  # combine figure, save
  {
    gg_NRMSE_comb <- cowplot::plot_grid(gg_list_all[['NRMSE']][[1]],
                                        gg_list_all[['NRMSE']][[3]],
                                        gg_list_all[['NRMSE']][[2]],
                                        gg_list_all[['NRMSE']][[4]],
                                        nrow=2, labels = c('(a)', '(b)', '(c)', '(d)'))
    
    gg_PCC_comb <- cowplot::plot_grid(gg_list_all[['PCC']][[1]],
                                      gg_list_all[['PCC']][[3]],
                                      gg_list_all[['PCC']][[2]],
                                      gg_list_all[['PCC']][[4]],
                                      nrow=2, labels = c('(a)', '(b)', '(c)', '(d)'))
    
    ggsave(file.path(out_dir, 'ImpValue_NRMSE_combine.png'),
           plot = gg_NRMSE_comb, device = NULL, path = NULL,
           scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
           dpi = 300, limitsize = T)
    
    ggsave(file.path(out_dir, 'ImpValue_PCC_combine.png'),
           plot = gg_PCC_comb, device = NULL, path = NULL,
           scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
           dpi = 300, limitsize = T)
    sprintf('Combine figure saved!\n') %>% cat
  }
  
  # csv
  {
    # melt data
    NRMSE_reshape_all <- melt(NRMSE_list_all) %>% spread(L2, value)
    colnames(NRMSE_reshape_all)[1:4] <- c('stat', 'scale', 'method', 'ScaleSaveName')
    
    PCC_reshape_all <- melt(PCC_list_all) %>% spread(L2, value)
    colnames(PCC_reshape_all)[1:4] <- c('stat', 'scale', 'method', 'ScaleSaveName')
    
    NRMSE_csv_save_path <- file.path(out_dir,'ALL_ImpValue_NRMSE.csv')
    PCC_csv_save_path <- file.path(out_dir,'ALL_ImpValue_PCC.csv')
    
    write.csv(NRMSE_reshape_all, file=NRMSE_csv_save_path)
    write.csv(PCC_reshape_all, file=PCC_csv_save_path)
    
    sprintf('csv saved!\n') %>% cat
  }
  
}


