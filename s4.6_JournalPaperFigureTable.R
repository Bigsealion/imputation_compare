# 2021.03.13 paper figure
# 2022.10.05 edit for Journal Figure and table (new data)
library(stringr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(cowplot)
library(data.table)
library(pheatmap)
library(patchwork)

# total set (e.g. method and color)
if(F){
  method_level <- c('Complete', 'CCA', 'Mean', 'Pred', 'EM', 'PMM')
  # this color list is deafult 6 color in ggplot2
  color_list <- c('#F8766D', '#B79F00', '#00BA38', '#00BFC4', '#619CFF', '#F564E3')
}

# 1 base info cvlt
if(F){
  rm(list=ls())
  # set parameter
  source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation'
  boot_dir_name <- 's1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata'
  file_pattern <- 'CVLT_ZsHarQCT1_(True|\\d+)(M|m)iss_NoRep0.8'
  out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/BaseInfo'
  save_name <- 'CVLT_true_baseinfo.csv'
  
  # mkdir
  if (!file.exists(out_dir)){
    dir.create(out_dir,recursive = T)
    sprintf('Create out dir! %s\n', out_dir) %>% cat()
  }
  
  data_list <- list()
  for (file_name in dir(source_dir)){
    if (str_detect(file_name, file_pattern)){
      print(file_name)
      missing_rate <- str_match(file_name, file_pattern)[1]
      
      boot_dir_path <- file.path(source_dir, file_name, boot_dir_name)
      boot_file_path <- file.path(boot_dir_path, dir(boot_dir_path)[1])
      
      data_load_name <- load(boot_file_path)
      eval(parse(text=sprintf("data_list[['%s']]  <- %s",missing_rate, data_load_name)))
    }
  }
  
  data_info_list <- lapply(data_list, function(mr){
    lapply(mr$simulation_missing_data_boot_list$cvlt, function(boot){
      sapply(boot$simulation_data[, -1], function(x){
        ms=sum(is.na(x)) / length(x)
        return(c(me=mean(x, na.rm=T), std=sd(x,na.rm=T), ms=ms))
      })
    })
  })
  
  data_info_list_summary <- lapply(data_info_list, function(mr){
    mr_s <- Reduce('+', mr) / 100
    mrs <- apply(mr_s, 2,function(x){
      c(mestd=sprintf('%.2f±%.2f', x[1], x[2]), ms=sprintf('%.2f%%', x[3]*100))
    }) %>% t
  })
  
  data_info_list_summary_bind <- Reduce(cbind, data_info_list_summary)
  
  
  # save_path
  save_path <- file.path(out_dir, save_name)
  write.csv(data_info_list_summary_bind, file=save_path)
  
  
}

# 1 base info cvlt simuY
if(F){
  rm(list=ls())
  # set parameter
  source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/'
  boot_dir_name <- 's1.7_SimulationOutcomeData.RData'
  # file_pattern <- 'CVLT_UnNormSimuY_(True|\\d+)(M|m)iss_NoRep0.8'
  file_pattern <- 'CVLT_TCoefSimuY_(True|\\d+)(M|m)iss_NoRep0.8'
  out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut3/BaseInfo'
  save_name <- 'CVLT_TCoefSimuY_true_baseinfo.csv'
  
  # mkdir
  if (!file.exists(out_dir)){
    dir.create(out_dir,recursive = T)
    sprintf('Create out dir! %s\n', out_dir) %>% cat()
  }
  
  data_list <- list()
  for (file_name in dir(source_dir)){
    if (str_detect(file_name, file_pattern)){
      print(file_name)
      missing_rate <- str_match(file_name, file_pattern)[1]
      
      boot_dir_path <- file.path(source_dir, file_name, boot_dir_name)
      boot_file_path <- file.path(boot_dir_path, dir(boot_dir_path)[1])
      
      data_load_name <- load(boot_file_path)
      eval(parse(text=sprintf("data_list[['%s']]  <- %s",missing_rate, data_load_name)))
    }
  }
  
  data_info_list <- lapply(data_list, function(mr){
    lapply(mr$simulation_missing_data_boot_list$cvlt, function(boot){
      sapply(boot$simulation_data[, -1], function(x){
        ms=sum(is.na(x)) / length(x)
        return(c(me=mean(x, na.rm=T), std=sd(x,na.rm=T), ms=ms))
      })
    })
  })
  
  data_info_list_summary <- lapply(data_info_list, function(mr){
    mr_s <- Reduce('+', mr) / 100
    mrs <- apply(mr_s, 2,function(x){
      c(mestd=sprintf('%.2f±%.2f', x[1], x[2]), ms=sprintf('%.2f%%', x[3]*100))
    }) %>% t
  })
  
  data_info_list_summary_bind <- Reduce(cbind, data_info_list_summary)
  
  
  # save_path
  save_path <- file.path(out_dir, save_name)
  write.csv(data_info_list_summary_bind, file=save_path)
  
  
}

# 1 base info reho
if(F){
  rm(list=ls())
  
  # set parameter
  source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/'
  boot_dir_name <- 's1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata'
  file_pattern <- 'Reho_age_(True|\\d+)(M|m)iss_NoRep0.8'
  out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/BaseInfo'
  save_name <- 'reho_true_BaseInfo.csv'
  
  # mkdir
  if (!file.exists(out_dir)){
    dir.create(out_dir,recursive = T)
    sprintf('Create out dir! %s\n', out_dir) %>% cat()
  }
  
  
  data_list <- list()
  for (file_name in dir(source_dir)){
    if (str_detect(file_name, file_pattern)){
      print(file_name)
      missing_rate <- str_match(file_name, file_pattern)[1]
      
      boot_dir_path <- file.path(source_dir, file_name, boot_dir_name)
      boot_file_path <- file.path(boot_dir_path, dir(boot_dir_path)[1])
      
      data_load_name <- load(boot_file_path)
      eval(parse(text=sprintf("data_list[['%s']]  <- %s",missing_rate, data_load_name)))
    }
  }
  
  data_info_list <- lapply(data_list, function(mr){
    lapply(mr$simulation_missing_data_boot_list$reho, function(boot){
      sapply(boot$simulation_data[, -1], function(x){
        ms=sum(is.na(x)) / length(x)
        return(c(me=mean(x, na.rm=T), std=sd(x,na.rm=T), ms=ms))
      })
    })
  })
  
  data_info_list_summary <- lapply(data_info_list, function(mr){
    mr_s <- Reduce('+', mr) / 100
    mrs <- apply(mr_s, 2,function(x){
      c(mestd=sprintf('%.2f±%.2f', x[1], x[2]), ms=sprintf('%.2f%%', x[3]*100))
    }) %>% t
  })
  
  data_info_list_summary_bind <- Reduce(cbind, data_info_list_summary)
  
  
  # save_path
  save_path <- file.path(out_dir, save_name)
  write.csv(data_info_list_summary_bind, file=save_path)
  sprintf('saved: %s', save_path) %>% cat
  
  
}

# 1 base info reho simuY
if(T){
  rm(list=ls())
  
  # set parameter
  source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/'
  boot_dir_name <- 's1.7_SimulationOutcomeData.RData'
  # file_pattern <- 'ReHo_UnNormSimuY_(True|\\d+)(M|m)iss_NoRep0.8'
  file_pattern <- 'ReHo_TCoefSimuY_(True|\\d+)(M|m)iss_NoRep0.8'
  out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut3/BaseInfo'
  save_name <- 'ReHo_TCoefSimuY_true_BaseInfo.csv'
  
  # mkdir
  if (!file.exists(out_dir)){
    dir.create(out_dir,recursive = T)
    sprintf('Create out dir! %s\n', out_dir) %>% cat()
  }
  
  
  data_list <- list()
  for (file_name in dir(source_dir)){
    if (str_detect(file_name, file_pattern)){
      print(file_name)
      missing_rate <- str_match(file_name, file_pattern)[1]
      
      boot_dir_path <- file.path(source_dir, file_name, boot_dir_name)
      boot_file_path <- file.path(boot_dir_path, dir(boot_dir_path)[1])
      
      data_load_name <- load(boot_file_path)
      eval(parse(text=sprintf("data_list[['%s']]  <- %s",missing_rate, data_load_name)))
    }
  }
  
  data_info_list <- lapply(data_list, function(mr){
    lapply(mr$simulation_missing_data_boot_list$reho, function(boot){
      sapply(boot$simulation_data[, -1], function(x){
        ms=sum(is.na(x)) / length(x)
        return(c(me=mean(x, na.rm=T), std=sd(x,na.rm=T), ms=ms))
      })
    })
  })
  
  data_info_list_summary <- lapply(data_info_list, function(mr){
    mr_s <- Reduce('+', mr) / 100
    mrs <- apply(mr_s, 2,function(x){
      c(mestd=sprintf('%.2f±%.2f', x[1], x[2]), ms=sprintf('%.2f%%', x[3]*100))
    }) %>% t
  })
  
  data_info_list_summary_bind <- Reduce(cbind, data_info_list_summary)
  
  # save_path
  save_path <- file.path(out_dir, save_name)
  write.csv(data_info_list_summary_bind, file=save_path)
  sprintf('saved: %s', save_path) %>% cat
  
  
}

# 1 simu coef bar (deprecated)
if(F){
  rm(list=ls())
  # set parameter
  # reho_coef_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/reho/s1.7_SimulationCoef.RData'
  # cvlt_coef_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt/s1.7_SimulationCoef.RData'
  # SimuCoefFigure_save_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/SimuCoefFigure.png'
  
  reho_coef_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/reho_UnNorm/s1.7_SimulationCoef.RData'
  cvlt_coef_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt_UnNorm/s1.7_SimulationCoef.RData'
  SimuCoefFigure_save_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/Simu_UnNormCoefFigure.png'
  
  # load
  # reho
  {
    reho_coef_load_name <- load(reho_coef_path)
    eval(parse(text=sprintf("reho_coef <- %s", reho_coef_load_name)))
    reho_coef <- reho_coef$reho
    
    reho_melt <- melt(reho_coef)
    reho_melt <- cbind(rownames(reho_melt), reho_melt)
    colnames(reho_melt) <- c('var', 'value')
    
    # figure - bar
    gg_RehoCoef = ggplot(reho_melt, aes(x = var,y = value, fill=value))+
      geom_bar(stat ="identity",width = 0.6,position = "dodge")+
      labs(x = "variables",y = "Simulation Coef", title='ReHo')+  
      guides(fill = guide_legend(reverse = F))+
      theme(legend.title = element_blank(),
            legend.position = 'none',
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
  }
  
  # cvlt
  {
    cvlt_coef_load_name <- load(cvlt_coef_path)
    eval(parse(text=sprintf("cvlt_coef <- %s", cvlt_coef_load_name)))
    cvlt_coef <- cvlt_coef$cvlt
    
    cvlt_melt <- melt(cvlt_coef)
    cvlt_melt <- cbind(rownames(cvlt_melt), cvlt_melt)
    cvlt_melt[, 1] <- substring(cvlt_melt[, 1], 6)
    colnames(cvlt_melt) <- c('var', 'value')
    
    # figure - bar
    gg_cvltCoef = ggplot(cvlt_melt, aes(x = var,y = value, fill=value))+
      geom_bar(stat ="identity",width = 0.6,position = "dodge")+
      labs(x = "variables",y = "Simulation Coef", title='CVLT')+  
      guides(fill = guide_legend(reverse = F))+
      theme(legend.title = element_blank(),
            legend.position = 'none',
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
  }
  
  gg_comb <- cowplot::plot_grid(gg_cvltCoef, gg_RehoCoef, nrow=2, labels = c('(a)', '(b)'))
  
  ggsave(SimuCoefFigure_save_path,
         plot = gg_comb, device = NULL, path = NULL,
         scale = 1, width = 10, height = 6, units =c("in", "cm", "mm"),
         dpi = 300, limitsize = T)
  sprintf('Simu Coef Figure saved! %s\n', SimuCoefFigure_save_path) %>% cat
  
}

# 1 CVLT Coef bar
if(F){
  rm(list=ls())
  # set parameter
  cvlt_coef_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata/model__cor_step__.RData'
  SimuCoefFigure_save_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut3/CVLT_CoefFigure.png'
  
  # load
  # coef
  {
    cvlt_coef_load_name <- load(cvlt_coef_path)
    eval(parse(text=sprintf("reho_coef <- %s", reho_coef_load_name)))
    reho_coef <- reho_coef$reho
    
    reho_melt <- melt(reho_coef)
    reho_melt <- cbind(rownames(reho_melt), reho_melt)
    colnames(reho_melt) <- c('var', 'value')
    
    # figure - bar
    gg_RehoCoef = ggplot(reho_melt, aes(x = var,y = value, fill=value))+
      geom_bar(stat ="identity",width = 0.6,position = "dodge")+
      labs(x = "variables",y = "Simulation Coef", title='ReHo')+  
      guides(fill = guide_legend(reverse = F))+
      theme(legend.title = element_blank(),
            legend.position = 'none',
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
  }
  
  # cvlt
  {
    cvlt_coef_load_name <- load(cvlt_coef_path)
    eval(parse(text=sprintf("cvlt_coef <- %s", cvlt_coef_load_name)))
    cvlt_coef <- cvlt_coef$cvlt
    
    cvlt_melt <- melt(cvlt_coef)
    cvlt_melt <- cbind(rownames(cvlt_melt), cvlt_melt)
    cvlt_melt[, 1] <- substring(cvlt_melt[, 1], 6)
    colnames(cvlt_melt) <- c('var', 'value')
    
    # figure - bar
    gg_cvltCoef = ggplot(cvlt_melt, aes(x = var,y = value, fill=value))+
      geom_bar(stat ="identity",width = 0.6,position = "dodge")+
      labs(x = "variables",y = "Simulation Coef", title='CVLT')+  
      guides(fill = guide_legend(reverse = F))+
      theme(legend.title = element_blank(),
            legend.position = 'none',
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            plot.title = element_text(hjust = 0.5))
  }
  
  gg_comb <- cowplot::plot_grid(gg_cvltCoef, gg_RehoCoef, nrow=2, labels = c('(a)', '(b)'))
  
  ggsave(SimuCoefFigure_save_path,
         plot = gg_comb, device = NULL, path = NULL,
         scale = 1, width = 10, height = 6, units =c("in", "cm", "mm"),
         dpi = 300, limitsize = T)
  sprintf('Simu Coef Figure saved! %s\n', SimuCoefFigure_save_path) %>% cat
  
}

# 2.1.1 impute value compare figure; +std
if(F){
  rm(list=ls())
  # set parameter
  {
    source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/'
    out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut3/CompareImg1/ImpValueCompare/'
    subdir_name <- 'imputed_value'
    
    file_pattern_list <- list()
    file_pattern_list[['CVLT_TGMV']] <- 'CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Gender']] <- 'Reho_age_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['CVLT_Y1']] <- 'CVLT_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['ReHo_Y2']] <- 'ReHo_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['CVLT_Y1']] <- 'CVLT_TCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['ReHo_Y2']] <- 'ReHo_TCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['CVLT_Y1']] <- 'CVLT_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Y2']] <- 'ReHo_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    
    missrate_level <- c('TrueMiss', '20Miss', '40Miss', '60Miss', '80Miss')
    method_level <- c('Complete', 'CCA', 'Mean', 'Pred', 'EM', 'PMM')
    color_list <- c('#F8766D', '#B79F00', '#00BA38', '#00BFC4', '#619CFF', '#F564E3')
    
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
          geom_errorbar(aes(ymin = mean, ymax = mean+sd), 
                        position = position_dodge(width=0.6),
                        width = 0.25)+
          scale_fill_manual(breaks = method_level, values = color_list) +
          labs(x = "Subject Miss Rate",y = "Imputation Value NRMSE", title = save_name, fill='Method')+
          guides(fill = guide_legend(reverse = F))+
          theme(legend.position = 'right',
                plot.title = element_text(hjust = 0.5, size = 20),
                axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size=12),
                axis.text.y = element_text(size=12),
                axis.title.x = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                legend.title = element_text(size=15),
                legend.text = element_text(size=12)
                )
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
          geom_errorbar(aes(ymin = mean, ymax = mean+sd), 
                        position = position_dodge(width=0.6),
                        width = 0.25)+
          scale_fill_manual(breaks = method_level, values = color_list) +
          labs(x = "Subject Miss Rate",y = "Imputation Value PCC", title = save_name, fill='Method')+
          guides(fill = guide_legend(reverse = F))+
          theme(legend.position = 'right',
                plot.title = element_text(hjust = 0.5, size = 20),
                axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size=12),
                axis.text.y = element_text(size=12),
                axis.title.x = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                legend.title = element_text(size=15),
                legend.text = element_text(size=12)
          )
      }
    }
  }
  
  # combine figure, save
  {
    # gg_NRMSE_comb <- cowplot::plot_grid(gg_list_all[['NRMSE']][[1]],
    #                                     gg_list_all[['NRMSE']][[3]],
    #                                     gg_list_all[['NRMSE']][[2]],
    #                                     gg_list_all[['NRMSE']][[4]],
    #                                     nrow=2, labels = c('(a)', '(b)', '(c)', '(d)'))
    # 
    # gg_PCC_comb <- cowplot::plot_grid(gg_list_all[['PCC']][[1]],
    #                                   gg_list_all[['PCC']][[3]],
    #                                   gg_list_all[['PCC']][[2]],
    #                                   gg_list_all[['PCC']][[4]],
    #                                   nrow=2, labels = c('(a)', '(b)', '(c)', '(d)'))
    
    gg_NRMSE_comb <- gg_list_all[['NRMSE']][[1]] + gg_list_all[['NRMSE']][[3]] +
      gg_list_all[['NRMSE']][[2]] + gg_list_all[['NRMSE']][[4]] +
      plot_layout(guides = 'collect') +
      plot_annotation(tag_levels = "a", tag_prefix = '(', tag_suffix = ')')
    
    gg_PCC_comb <- gg_list_all[['PCC']][[1]] + gg_list_all[['PCC']][[3]] +
      gg_list_all[['PCC']][[2]] + gg_list_all[['PCC']][[4]] +
      plot_layout(guides = 'collect') +
      plot_annotation(tag_levels = "a", tag_prefix = '(', tag_suffix = ')')
  
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

# 3.1.1 model PBNED # deprecated
if(F){
  rm(list=ls())
  # set parameter
  {
    source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2_compare_summary/'
    # save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/ModelCompare/PBNED'
    save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut3/CompareImg1/PBNED'
    mis_pattern <- '(True|\\d+)(M|m)iss'
    indicator_name <- 'percent_bias'
    
    file_pattern_list <- list()
    file_pattern_list[['CVLT_TGMV']] <- 'CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Gender']] <- 'Reho_age_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['CVLT_Y1']] <- 'CVLT_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['ReHo_Y2']] <- 'ReHo_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    
    # file_pattern_list[['CVLT_Y1']] <- 'CVLT_TCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['ReHo_Y2']] <- 'ReHo_TCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['CVLT_Y1']] <- 'CVLT_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Y2']] <- 'ReHo_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    
    missrate_level <- c('TrueMiss', '20Miss', '40Miss', '60Miss', '80Miss')
    method_level <- c('Complete', 'CCA', 'Mean', 'Pred', 'EM', 'PMM')
    color_list <- c('#F8766D', '#B79F00', '#00BA38', '#00BFC4', '#619CFF', '#F564E3')
  }

  # mkdir out dir
  if (!file.exists(save_dir)){
    dir.create(save_dir,recursive = T)
    sprintf('Create out dir! %s\n', save_dir) %>% cat()
  }
  
  # run in each pattern
  Indicator_list_all <- list()
  gg_list_all <- list()
  for (save_name in names(file_pattern_list)){
    # get data, mean and std
    for (file_name in dir(source_dir)){
      if (str_detect(file_name, file_pattern_list[[save_name]])){
        print(file_name)
        # load data
        {
          mr <- str_match(file_name, file_pattern_list[[save_name]])[2]
          res_data_path <- file.path(source_dir, file_name, 'compare_result.RData')
          load(res_data_path)  # name is compare_result_method
        }
        
        # get mean and std
        {
          for (met in names(compare_result_method)){
            for (sc in names(compare_result_method[[met]][["compare_result"]])){
              # remove intercept, so using [-1]
              boot_mean <-sapply(compare_result_method[[met]][["compare_result"]][[sc]], function(boot){
                # sqrt(sum(boot[[indicator_name]] ^2) / length(boot[[indicator_name]]))
                sqrt(sum(boot[[indicator_name]][-1] ^2) / length(boot[[indicator_name]][-1]))
              })
              Indicator_list_all[[save_name]][[mr]][[met]][[sc]] <- data.frame(mean=mean(boot_mean, na.rm=T), sd=sd(boot_mean, na.rm=T))
            }
          }
        }
      }
    }
    
    # melt, adjudt
    {
      Indicator_melt <- melt(Indicator_list_all[[save_name]])
      colnames(Indicator_melt) <- c('stat', 'value', 'scale', 'method', 'missrate')
      Indicator_melt <- Indicator_melt[-which(Indicator_melt[, 'scale'] == '__aux'), ]
      
      Indicator_melt['scale'] <- save_name
      
      Indicator_mr <- spread(Indicator_melt, missrate, value)
    }
    
    # figure
    {
      # adjust
      {
        Indicator_stat <- spread(Indicator_melt, stat, value)
        
        Indicator_stat[,'missrate'] <- factor(Indicator_stat[,'missrate'], levels = missrate_level)
        
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__mice_mean__'), 'method'] <- 'Mean'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__mice_norm_pred__'), 'method'] <- 'Pred'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__vim_em__'), 'method'] <- 'EM'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__mice_pmm__'), 'method'] <- 'PMM'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__CCA__'), 'method'] <- 'CCA'
        Indicator_stat[,'method'] <- factor(Indicator_stat[,'method'], levels = method_level)
      }
      
      # ggplot bar
      gg_list_all[[save_name]] = ggplot(Indicator_stat, aes(x = missrate,y = mean, fill = method))+
        geom_bar(stat ="identity",width = 0.6, color='black', position = "dodge")+
        geom_errorbar(aes(ymin = mean, ymax = mean+sd), 
                      position = position_dodge(width=0.6),
                      width = 0.25)+
        scale_fill_manual(breaks = method_level, values = color_list) +
        # labs(x = "Subject Miss Rate",y = "Model PBNED", title = save_name, fill='Method')+
        labs(x = "Subject Miss Rate",y = "Model PBNED", title = save_name, fill='Method')+
        coord_cartesian(ylim = c(0, 100))+
        guides(fill = guide_legend(reverse = F))+
        theme(legend.position = 'right',
              plot.title = element_text(hjust = 0.5, size = 20),
              axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size=12),
              axis.text.y = element_text(size=12),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              legend.title = element_text(size=15),
              legend.text = element_text(size=12)
              )
    }
  }
  
  # combine figure, save (by patchwork)
  {
    # gg_PBNED_comb <- cowplot::plot_grid(gg_list_all[[1]],
    #                                     gg_list_all[[3]],
    #                                     gg_list_all[[2]],
    #                                     gg_list_all[[4]],
    #                                     nrow=2, labels = c('(a)', '(b)', '(c)', '(d)'))
    
    gg_PBNED_comb <- gg_list_all[[1]] + gg_list_all[[3]] + gg_list_all[[2]] + gg_list_all[[4]] +
      plot_layout(guides = 'collect') +
      plot_annotation(tag_levels = "a", tag_prefix = '(', tag_suffix = ')')

    
    ggsave(file.path(save_dir, 'Model_PBNED_combine.png'),
           plot = gg_PBNED_comb, device = NULL, path = NULL,
           scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
           dpi = 300, limitsize = T)
  }

  # save csv
  {
    Indicator_list_all_melt <- melt(Indicator_list_all)
    Indicator_list_all_melt <- Indicator_list_all_melt[-which(Indicator_list_all_melt[, 'L4'] == '__aux'),]
    colnames(Indicator_list_all_melt) <- c('stat', 'value', 'sc', 'method', 'missrate', 'sc_save_ame')
    Indicator_list_all_mr <- spread(Indicator_list_all_melt, missrate, value)
    
    csv_save_path <- file.path(save_dir, 'Combine_Model_PBNED.csv')
    write.csv(Indicator_list_all_mr, file=csv_save_path)
    sprintf('csv saved! %s', csv_save_path) %>% cat
  }
}

# 3.1.1 model mean PB (remove intercept)
if(T){
  rm(list=ls())
  # set parameter
  {
    source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2_compare_summary/'
    # save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/ModelCompare/PBNED'
    save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut3/CompareImg1/PB_noInt111'
    mis_pattern <- '(True|\\d+)(M|m)iss'
    indicator_name <- 'percent_bias'
    
    file_pattern_list <- list()
    file_pattern_list[['CVLT_TGMV']] <- 'CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Gender']] <- 'Reho_age_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['CVLT_Y1']] <- 'CVLT_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['ReHo_Y2']] <- 'ReHo_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    
    # file_pattern_list[['CVLT_Y1']] <- 'CVLT_TCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['ReHo_Y2']] <- 'ReHo_TCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['CVLT_Y1']] <- 'CVLT_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Y2']] <- 'ReHo_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    
    missrate_level <- c('TrueMiss', '20Miss', '40Miss', '60Miss', '80Miss')
    method_level <- c('Complete', 'CCA', 'Mean', 'Pred', 'EM', 'PMM')
    color_list <- c('#F8766D', '#B79F00', '#00BA38', '#00BFC4', '#619CFF', '#F564E3')
  }
  
  # mkdir out dir
  if (!file.exists(save_dir)){
    dir.create(save_dir,recursive = T)
    sprintf('Create out dir! %s\n', save_dir) %>% cat()
  }
  
  # run in each pattern
  Indicator_list_all <- list()
  gg_list_all <- list()
  for (save_name in names(file_pattern_list)){
    # get data, mean and std
    for (file_name in dir(source_dir)){
      if (str_detect(file_name, file_pattern_list[[save_name]])){
        print(file_name)
        # load data
        {
          mr <- str_match(file_name, file_pattern_list[[save_name]])[2]
          res_data_path <- file.path(source_dir, file_name, 'compare_result.RData')
          load(res_data_path)  # name is compare_result_method
        }
        
        # get mean and std
        {
          for (met in names(compare_result_method)){
            for (sc in names(compare_result_method[[met]][["compare_result"]])){
              # remove intercept, so using [-1]
              boot_mean <-sapply(compare_result_method[[met]][["compare_result"]][[sc]], function(boot){
                mean(boot[[indicator_name]][-1])
              })
              Indicator_list_all[[save_name]][[mr]][[met]][[sc]] <- data.frame(mean=mean(boot_mean, na.rm=T), sd=sd(boot_mean, na.rm=T))
            }
          }
        }
      }
    }
    
    # melt, adjudt
    {
      Indicator_melt <- melt(Indicator_list_all[[save_name]])
      colnames(Indicator_melt) <- c('stat', 'value', 'scale', 'method', 'missrate')
      Indicator_melt <- Indicator_melt[-which(Indicator_melt[, 'scale'] == '__aux'), ]
      
      Indicator_melt['scale'] <- save_name
      
      Indicator_mr <- spread(Indicator_melt, missrate, value)
    }
    
    # figure
    {
      # adjust
      {
        Indicator_stat <- spread(Indicator_melt, stat, value)
        
        Indicator_stat[,'missrate'] <- factor(Indicator_stat[,'missrate'], levels = missrate_level)
        
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__mice_mean__'), 'method'] <- 'Mean'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__mice_norm_pred__'), 'method'] <- 'Pred'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__vim_em__'), 'method'] <- 'EM'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__mice_pmm__'), 'method'] <- 'PMM'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__CCA__'), 'method'] <- 'CCA'
        Indicator_stat[,'method'] <- factor(Indicator_stat[,'method'], levels = method_level)
      }
      
      # ggplot bar
      gg_list_all[[save_name]] = ggplot(Indicator_stat, aes(x = missrate,y = mean, fill = method))+
        geom_bar(stat ="identity",width = 0.6, color='black', position = "dodge")+
        geom_errorbar(aes(ymin = mean, ymax = mean+sd), 
                      position = position_dodge(width=0.6),
                      width = 0.25)+
        scale_fill_manual(breaks = method_level, values = color_list) +
        labs(x = "Subject Miss Rate",y = "Model PB", title = save_name, fill='Method')+
        # coord_cartesian(ylim = c(0, 100))+
        guides(fill = guide_legend(reverse = F))+
        theme(legend.position = 'right',
              plot.title = element_text(hjust = 0.5, size = 20),
              axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size=12),
              axis.text.y = element_text(size=12),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              legend.title = element_text(size=15),
              legend.text = element_text(size=12)
        )
    }
  }
  
  # combine figure, save (by patchwork)
  {
    # gg_PBNED_comb <- cowplot::plot_grid(gg_list_all[[1]],
    #                                     gg_list_all[[3]],
    #                                     gg_list_all[[2]],
    #                                     gg_list_all[[4]],
    #                                     nrow=2, labels = c('(a)', '(b)', '(c)', '(d)'))
    
    gg_PBNED_comb <- gg_list_all[[1]] + gg_list_all[[3]] + gg_list_all[[2]] + gg_list_all[[4]] +
      plot_layout(guides = 'collect') +
      plot_annotation(tag_levels = "a", tag_prefix = '(', tag_suffix = ')')
    
    # test
    # {
    #   gg_comb_test <- {gg_list_all[[1]] +
    #   {gg_list_all[[3]] +
    #   {gg_list_all[[2]] + gg_list_all[[2]] +  plot_layout(nrow = 2)
    #           } + plot_layout(nrow = 2)}
    #     }+
    #     plot_layout(guides = 'collect')
    #   }
    
    
    ggsave(file.path(save_dir, 'Model_Mean_PB_combine.png'),
           plot = gg_PBNED_comb, device = NULL, path = NULL,
           scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
           dpi = 300, limitsize = T)
  }
  
  # save csv
  {
    Indicator_list_all_melt <- melt(Indicator_list_all)
    Indicator_list_all_melt <- Indicator_list_all_melt[-which(Indicator_list_all_melt[, 'L4'] == '__aux'),]
    colnames(Indicator_list_all_melt) <- c('stat', 'value', 'sc', 'method', 'missrate', 'sc_save_ame')
    Indicator_list_all_mr <- spread(Indicator_list_all_melt, missrate, value)
    
    csv_save_path <- file.path(save_dir, 'Combine_Model_Mean_PB.csv')
    write.csv(Indicator_list_all_mr, file=csv_save_path)
    sprintf('csv saved! %s', csv_save_path) %>% cat
  }
}

# 3.2.1 model CR (remove intercept)
if(F){
  rm(list=ls())
  # set parameter
  {
    source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2_compare_summary/'
    # save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/ModelCompare/CR'
    save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut3/CompareImg1/CR'
    mis_pattern <- '(True|\\d+)(M|m)iss'
    indicator_name <- 'coverage_rate'
    
    file_pattern_list <- list()
    file_pattern_list[['CVLT_TGMV']] <- 'CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Gender']] <- 'Reho_age_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['CVLT_Y1']] <- 'CVLT_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['ReHo_Y2']] <- 'ReHo_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['CVLT_Y1']] <- 'CVLT_TCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['ReHo_Y2']] <- 'ReHo_TCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['CVLT_Y1']] <- 'CVLT_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Y2']] <- 'ReHo_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    
    missrate_level <- c('TrueMiss', '20Miss', '40Miss', '60Miss', '80Miss')
    method_level <- c('Complete', 'CCA', 'Mean', 'Pred', 'EM', 'PMM')
    color_list <- c('#F8766D', '#B79F00', '#00BA38', '#00BFC4', '#619CFF', '#F564E3')
  }
  
  # mkdir out dir
  if (!file.exists(save_dir)){
    dir.create(save_dir,recursive = T)
    sprintf('Create out dir! %s\n', save_dir) %>% cat()
  }
  
  # run in each pattern
  Indicator_list_all <- list()
  gg_list_all <- list()
  for (save_name in names(file_pattern_list)){
    # get data, mean and std
    for (file_name in dir(source_dir)){
      if (str_detect(file_name, file_pattern_list[[save_name]])){
        print(file_name)
        # load data
        {
          mr <- str_match(file_name, file_pattern_list[[save_name]])[2]
          res_data_path <- file.path(source_dir, file_name, 'compare_result.RData')
          load(res_data_path)  # name is compare_result_method
        }
        
        # get mean and std
        {
          for (met in names(compare_result_method)){
            for (sc in names(compare_result_method[[met]][["compare_result"]])){
              boot_mean <-sapply(compare_result_method[[met]][["compare_result"]][[sc]], function(boot){
                # mean(boot[[indicator_name]])
                
                # remove Intercept
                mean(boot[[indicator_name]][-1])
              })
              Indicator_list_all[[save_name]][[mr]][[met]][[sc]] <- data.frame(mean=mean(boot_mean, na.rm=T), sd=sd(boot_mean, na.rm=T))
            }
          }
        }
      }
    }
    
    # melt, adjudt
    {
      Indicator_melt <- melt(Indicator_list_all[[save_name]])
      colnames(Indicator_melt) <- c('stat', 'value', 'scale', 'method', 'missrate')
      Indicator_melt <- Indicator_melt[-which(Indicator_melt[, 'scale'] == '__aux'), ]
      
      Indicator_melt['scale'] <- save_name
      
      Indicator_mr <- spread(Indicator_melt, missrate, value)
    }
    
    # figure
    {
      # adjust
      {
        Indicator_stat <- spread(Indicator_melt, stat, value)
        
        Indicator_stat[,'missrate'] <- factor(Indicator_stat[,'missrate'], levels = missrate_level)
        
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__mice_mean__'), 'method'] <- 'Mean'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__mice_norm_pred__'), 'method'] <- 'Pred'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__vim_em__'), 'method'] <- 'EM'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__mice_pmm__'), 'method'] <- 'PMM'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__CCA__'), 'method'] <- 'CCA'
        Indicator_stat[,'method'] <- factor(Indicator_stat[,'method'], levels = method_level)
      }
      
      # ggplot bar
      gg_list_all[[save_name]] = ggplot(Indicator_stat, aes(x = missrate,y = mean, fill = method))+
        geom_bar(stat ="identity",width = 0.6, color='black', position = "dodge")+
        geom_errorbar(aes(ymin = mean, ymax = mean+sd), 
                      position = position_dodge(width=0.6),
                      width = 0.25)+
        geom_hline(yintercept = 0.95, linetype="dashed", size=0.5)+
        scale_fill_manual(breaks = method_level, values = color_list) +
        scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 0.95, 1)) +
        labs(x = "Subject Miss Rate",y = "Model CR", title = save_name, fill='Method')+
        guides(fill = guide_legend(reverse = F))+
        theme(legend.position = 'right',
              plot.title = element_text(hjust = 0.5, size = 20),
              axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size=12),
              axis.text.y = element_text(size=12),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              legend.title = element_text(size=15),
              legend.text = element_text(size=12)
        )
    }
  }
  
  # combine figure, save
  {
    # gg_PBNED_comb <- cowplot::plot_grid(gg_list_all[[1]],
    #                                     gg_list_all[[3]],
    #                                     gg_list_all[[2]],
    #                                     gg_list_all[[4]],
    #                                     nrow=2, labels = c('(a)', '(b)', '(c)', '(d)'))
    
    gg_CR_comb <- gg_list_all[[1]] + gg_list_all[[3]] + gg_list_all[[2]] + gg_list_all[[4]] +
      plot_layout(guides = 'collect') +
      plot_annotation(tag_levels = "a", tag_prefix = '(', tag_suffix = ')')
    
    ggsave(file.path(save_dir, 'Model_CR_combine.png'),
           plot = gg_CR_comb, device = NULL, path = NULL,
           scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
           dpi = 300, limitsize = T)
  }
  
  # save csv
  {
    Indicator_list_all_melt <- melt(Indicator_list_all)
    Indicator_list_all_melt <- Indicator_list_all_melt[-which(Indicator_list_all_melt[, 'L4'] == '__aux'),]
    colnames(Indicator_list_all_melt) <- c('stat', 'value', 'sc', 'method', 'missrate', 'sc_save_ame')
    Indicator_list_all_mr <- spread(Indicator_list_all_melt, missrate, value)
    
    csv_save_path <- file.path(save_dir, 'Combine_Model_CR.csv')
    write.csv(Indicator_list_all_mr, file=csv_save_path)
    sprintf('csv saved! %s', csv_save_path) %>% cat
  }
}

# 3.3.1 model AW
if(F){
  rm(list=ls())
  # set parameter
  {
    source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2_compare_summary/'
    # save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/ModelCompare/AW'
    save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut3/CompareImg1/AW'
    mis_pattern <- '(True|\\d+)(M|m)iss'
    indicator_name <- 'AW_per_bias'
    
    file_pattern_list <- list()
    file_pattern_list[['CVLT_TGMV']] <- 'CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Gender']] <- 'Reho_age_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['CVLT_Y1']] <- 'CVLT_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['ReHo_Y2']] <- 'ReHo_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['CVLT_Y1']] <- 'CVLT_TCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['ReHo_Y2']] <- 'ReHo_TCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['CVLT_Y1']] <- 'CVLT_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Y2']] <- 'ReHo_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    
    missrate_level <- c('TrueMiss', '20Miss', '40Miss', '60Miss', '80Miss')
    method_level <- c('Complete', 'CCA', 'Mean', 'Pred', 'EM', 'PMM')
    color_list <- c('#F8766D', '#B79F00', '#00BA38', '#00BFC4', '#619CFF', '#F564E3')
  }
  
  # mkdir out dir
  if (!file.exists(save_dir)){
    dir.create(save_dir,recursive = T)
    sprintf('Create out dir! %s\n', save_dir) %>% cat()
  }
  
  # run in each pattern
  Indicator_list_all <- list()
  gg_list_all <- list()
  for (save_name in names(file_pattern_list)){
    # get data, mean and std
    for (file_name in dir(source_dir)){
      if (str_detect(file_name, file_pattern_list[[save_name]])){
        print(file_name)
        # load data
        {
          mr <- str_match(file_name, file_pattern_list[[save_name]])[2]
          res_data_path <- file.path(source_dir, file_name, 'compare_result.RData')
          load(res_data_path)  # name is compare_result_method
        }
        
        # get mean and std
        {
          for (met in names(compare_result_method)){
            for (sc in names(compare_result_method[[met]][["compare_result"]])){
              boot_mean <-sapply(compare_result_method[[met]][["compare_result"]][[sc]], function(boot){
                # mean(boot[[indicator_name]])
                
                # remove Intercept
                mean(boot[[indicator_name]][-1])
              })
              Indicator_list_all[[save_name]][[mr]][[met]][[sc]] <- data.frame(mean=mean(boot_mean, na.rm=T), sd=sd(boot_mean, na.rm=T))
            }
          }
        }
      }
    }
    
    # melt, adjudt
    {
      Indicator_melt <- melt(Indicator_list_all[[save_name]])
      colnames(Indicator_melt) <- c('stat', 'value', 'scale', 'method', 'missrate')
      Indicator_melt <- Indicator_melt[-which(Indicator_melt[, 'scale'] == '__aux'), ]
      
      Indicator_melt['scale'] <- save_name
      
      Indicator_mr <- spread(Indicator_melt, missrate, value)
    }
    
    # figure
    {
      # adjust
      {
        Indicator_stat <- spread(Indicator_melt, stat, value)
        
        Indicator_stat[,'missrate'] <- factor(Indicator_stat[,'missrate'], levels = missrate_level)
        
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__mice_mean__'), 'method'] <- 'Mean'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__mice_norm_pred__'), 'method'] <- 'Pred'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__vim_em__'), 'method'] <- 'EM'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__mice_pmm__'), 'method'] <- 'PMM'
        Indicator_stat[which(Indicator_stat[,'method']=='Imp_Method__CCA__'), 'method'] <- 'CCA'
        Indicator_stat[,'method'] <- factor(Indicator_stat[,'method'], levels = method_level)
      }
      
      # ggplot bar
      gg_list_all[[save_name]] = ggplot(Indicator_stat, aes(x = missrate,y = mean, fill = method))+
        geom_bar(stat ="identity",width = 0.6, color='black', position = "dodge")+
        geom_errorbar(aes(ymin = mean, ymax = mean+sd), 
                      position = position_dodge(width=0.6),
                      width = 0.25)+
        scale_fill_manual(breaks = method_level, values = color_list) +
        labs(x = "Subject Miss Rate",y = "Model Percent AW", title = save_name, fill='Method')+
        guides(fill = guide_legend(reverse = F))+
        theme(legend.position = 'right',
              plot.title = element_text(hjust = 0.5, size = 20),
              axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size=12),
              axis.text.y = element_text(size=12),
              axis.title.x = element_text(size = 15),
              axis.title.y = element_text(size = 15),
              legend.title = element_text(size=15),
              legend.text = element_text(size=12)
        )
    }
  }
  
  # combine figure, save
  {
    # gg_PBNED_comb <- cowplot::plot_grid(gg_list_all[[1]],
    #                                     gg_list_all[[3]],
    #                                     gg_list_all[[2]],
    #                                     gg_list_all[[4]],
    #                                     nrow=2, labels = c('(a)', '(b)', '(c)', '(d)'))
    
    gg_AW_comb <- gg_list_all[[1]] + gg_list_all[[3]] + gg_list_all[[2]] + gg_list_all[[4]] +
      plot_layout(guides = 'collect') +
      plot_annotation(tag_levels = "a", tag_prefix = '(', tag_suffix = ')')
    
    ggsave(file.path(save_dir, 'Model_AW_combine.png'),
           plot = gg_AW_comb, device = NULL, path = NULL,
           scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
           dpi = 300, limitsize = T)
  }
  
  # save csv
  {
    Indicator_list_all_melt <- melt(Indicator_list_all)
    Indicator_list_all_melt <- Indicator_list_all_melt[-which(Indicator_list_all_melt[, 'L4'] == '__aux'),]
    colnames(Indicator_list_all_melt) <- c('stat', 'value', 'sc', 'method', 'missrate', 'sc_save_ame')
    Indicator_list_all_mr <- spread(Indicator_list_all_melt, missrate, value)
    
    csv_save_path <- file.path(save_dir, 'Combine_Model_AW.csv')
    write.csv(Indicator_list_all_mr, file=csv_save_path)
    sprintf('csv saved! %s', csv_save_path) %>% cat
  }
  
  
  
}

# 4.1.1 Model Predict CV Result
if(F){
  rm(list=ls())
  # set parameter
  {
    source_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/'
    # out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut/ModelPredictCompare/'
    out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/PaperOut3/CompareImg1/ModelPredictCompare/'
    subdir_name <- 'cv_res'
    
    file_pattern_list <- list()
    file_pattern_list[['CVLT_TGMV']] <- 'CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Gender']] <- 'Reho_age_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['CVLT_Y1']] <- 'CVLT_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['ReHo_Y2']] <- 'ReHo_UnNormSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    
    # file_pattern_list[['CVLT_Y1']] <- 'CVLT_TCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    # file_pattern_list[['ReHo_Y2']] <- 'ReHo_TCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['CVLT_Y1']] <- 'CVLT_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    file_pattern_list[['ReHo_Y2']] <- 'ReHo_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8'
    
    missrate_level <- c('TrueMiss', '20Miss', '40Miss', '60Miss', '80Miss')
    method_level <- c('Complete', 'CCA', 'Mean', 'Pred', 'EM', 'PMM')
    color_list <- c('#F8766D', '#B79F00', '#00BA38', '#00BFC4', '#619CFF', '#F564E3')
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
  Ind_list_all <- list()

  for (save_name in names(file_pattern_list)){
    sprintf('%s...\n', save_name) %>% cat
    # load data
    data_list <- list()
    for (file_name in dir(source_dir)){
      if (str_detect(file_name, file_pattern_list[[save_name]])){
        print(file_name)
        missing_rate <- str_match(file_name, file_pattern_list[[save_name]])[2]
        
        subdir_path <- file.path(source_dir, file_name, subdir_name)
        subfile_path <- file.path(subdir_path, 'CrossValidationBootResult.RData')
        
        load(subfile_path)  # name is imp_cv_compare_boot_list
        
        # get summary
        Ind_list_all[[save_name]][[missing_rate]] <- lapply(imp_cv_compare_boot_list, function(met){
          lapply(met, function(sc){
            cv_res <- sapply(sc, function(boot){boot})
            res_mean <- apply(cv_res, 1, function(ind){data.frame(mean=mean(ind), sd=sd(ind))})
          })
        })
      }
    }
  }
  
  # melt
  {
    Ind_list_stat <- melt(Ind_list_all) %>% spread(variable, value)
    colnames(Ind_list_stat)[1:5] <- c('Indicator', 'scale', 'method', 'missrate', 'scale_save_name')
    Ind_list_stat[which(Ind_list_stat[,'method']=='mice_mean'), 'method'] <- 'Mean'
    Ind_list_stat[which(Ind_list_stat[,'method']=='mice_norm_pred'), 'method'] <- 'Pred'
    Ind_list_stat[which(Ind_list_stat[,'method']=='vim_em'), 'method'] <- 'EM'
    Ind_list_stat[which(Ind_list_stat[,'method']=='mice_pmm'), 'method'] <- 'PMM'
    Ind_list_stat[which(Ind_list_stat[,'method']=='complete'), 'method'] <- 'Complete'
    
    Ind_list_stat[which(Ind_list_stat[,'Indicator']=='cor'), 'Indicator'] <- 'PCC'
    
    Ind_list_stat[,'method'] <- factor(Ind_list_stat[,'method'], levels = method_level)
    Ind_list_stat[,'missrate'] <- factor(Ind_list_stat[,'missrate'], levels = missrate_level)
  }
  
  # get figure and save csv
  {
    gg_list_all <- list()
    gg_combine_list <- list()
    for (compare_ind in unique(Ind_list_stat[, 'Indicator'])){
      for (scale_name in unique(Ind_list_stat[, 'scale_save_name'])){
        Ind_list_stat_choosed <- filter(Ind_list_stat, Indicator==compare_ind, scale_save_name==scale_name)
        
        gg_list_all[[compare_ind]][[scale_name]] = ggplot(Ind_list_stat_choosed, aes(x = missrate, y = mean,fill = method))+
          geom_bar(stat ="identity",width = 0.6, color='black', position = "dodge")+   
          geom_errorbar(aes(ymin = mean, ymax = mean+sd), 
                        position = position_dodge(width=0.6),
                        width = 0.25)+
          scale_fill_manual(breaks = method_level, values = color_list) +
          labs(x = "Subject Miss Rate",y = sprintf('Predict %s', compare_ind), title = scale_name, fill='Method')+
          guides(fill = guide_legend(reverse = F))+
          theme(legend.position = 'right',
                plot.title = element_text(hjust = 0.5, size = 20),
                axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size=12),
                axis.text.y = element_text(size=12),
                axis.title.x = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                legend.title = element_text(size=15),
                legend.text = element_text(size=12)
          )
      }
      
      # combine
      # gg_combine_list[[compare_ind]] <- cowplot::plot_grid(gg_list_all[[compare_ind]][[1]],
      #                                                      gg_list_all[[compare_ind]][[2]],
      #                                                      gg_list_all[[compare_ind]][[3]],
      #                                                      gg_list_all[[compare_ind]][[4]],
      #                                                      nrow=2, labels = c('(a)', '(b)', '(c)', '(d)'))
      
      gg_combine_list[[compare_ind]] <- gg_list_all[[compare_ind]][[1]] + gg_list_all[[compare_ind]][[2]] +
        gg_list_all[[compare_ind]][[3]] + gg_list_all[[compare_ind]][[4]] +
        plot_layout(guides = 'collect') +
        plot_annotation(tag_levels = "a", tag_prefix = '(', tag_suffix = ')')
      
      # save figure
      ggsave(file.path(out_dir, sprintf('Predict_%s_combine.png', compare_ind)),
             plot = gg_combine_list[[compare_ind]], device = NULL, path = NULL,
             scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             dpi = 300, limitsize = T)
      sprintf('%s Error Bar saved!\n', compare_ind) %>% cat
      
      # save csv
      {
        Ind_list_stat_saved <- filter(Ind_list_stat, Indicator==compare_ind)
        Ind_list_stat_saved_mr <- melt(Ind_list_stat_saved) %>% spread(missrate, value)
        colnames(Ind_list_stat_saved_mr)[5] <- 'stat'
        
        Ind_csv_save_path <- file.path(out_dir, sprintf('Predict_%s_combine.csv', compare_ind))
        write.csv(Ind_list_stat_saved_mr, file=Ind_csv_save_path)
        sprintf('%s CSV saved!\n', compare_ind) %>% cat
      }
    }
  }
}




