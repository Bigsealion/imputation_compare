# s2.2: check simulation boot data pattern
rm(list = ls())
library(ggplot2)
library(reshape2)
source('/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/imp_compare_utils.R')

time_op <- Sys.time()
# set parameter
# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/test/choose_psy_1.5.RData/simulation_boot_100_2021-01-28-17:39:26.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2.1_simulation_check/test/choose_psy_1.5.RData'
# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/cog_70miss/s1.4_Cognative_T1surf_Combine_Data.Rdata/simulation_boot_1_2021-02-02-08:48:27.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2.1_simulation_check/s1.4_Cognative_T1surf_Combine_Data_70miss.Rdata'
# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/cog_TrueMiss/s1.4_Cognative_T1surf_Combine_Data.Rdata/simulation_boot_100_2021-03-01-14:29:52.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2.1_simulation_check/s1.7_SimulationOutcome_Cog_TrueMiss'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/CVLT_SimuY_TrueMiss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-03-09-12:17:52.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2.1_simulation_check/CVLT_SimuY_TrueMiss_NoRep0.8'

source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/Reho_age_Truemiss_NoRep0.8/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata/simulation_boot_100_2021-03-08-18:30:43.RData'
out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2.1_simulation_check/ReHo_Gender_TrueMiss_NoRep0.8'

# load data
load_name <- load(source_path)
eval(parse(text=sprintf("boot_data_list <- %s", load_name)))
simu_data <- boot_data_list$simulation_missing_data_boot_list
comp_data <- boot_data_list$complete_data_boot_list
true_data <- boot_data_list$total_complete_data_list

# mkdir out dir
if (!file.exists(out_dir)){
  dir.create(out_dir, recursive = T)
  sprintf('Create save dir! %s\n', out_dir) %>% cat()
}

# out
{
  # path
  {
    save_path_list <- list()
    save_path_list$missing_rate <- file.path(out_dir, 'missing_rate')
    save_path_list$missing_view <- file.path(out_dir, 'missing_view')
    save_path_list$all_missing_rate <- file.path(out_dir, 'scale_missing_element.png')
  }
  
  # get missing rate of each variable
  {
    if (!file.exists(save_path_list$missing_rate)){
      dir.create(save_path_list$missing_rate, recursive = T)
      sprintf('Create var missing rate save dir! %s\n', save_path_list$missing_rate) %>% cat()
    }
    
    var_missing_rate <- lapply(simu_data, function(psy){
      sapply(psy, function(boot){
        mv <- miss_var_summary(boot$simulation_data[, -c(1,2)]) %>% as.data.frame()
        mv <- mv[order(mv[, 1]), ]
        missing_rate <- mv[, 3]
        names(missing_rate) <- mv[, 1]
        return(missing_rate)
      }) %>% t() %>% colMeans()
    })
    
    # draw
    for (scale_names in names(var_missing_rate)){
      mv <- var_missing_rate[[scale_names]]
      melt_mv <- melt(mv)
      melt_mv <- cbind(rownames(melt_mv), melt_mv)
      colnames(melt_mv) <- c('names', 'value')
      
      gg_mv = ggplot(melt_mv, aes(x = names,y = value, fill=value))+
        geom_bar(stat ="identity",width = 0.6,position = "dodge")+     
        labs(x = "variables",y = "missing rate (%)", title = scale_names)+  
        guides(fill = guide_legend(reverse = F))+
        theme(legend.title = element_blank(),
              legend.position = 'right',
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
      
      ggsave(file.path(save_path_list$missing_rate, sprintf('%s_VarMissingRate.png', scale_names)),
             plot = gg_mv, device = NULL, path = NULL,
             scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             dpi = 300, limitsize = T)
    }
    
    sprintf('missing rate saved! %s\n', save_path_list$missing_rate) %>% cat()
  }
  
  # vis miss
  {
    if (!file.exists(save_path_list$missing_view)){
      dir.create(save_path_list$missing_view, recursive = T)
      sprintf('Create var missing view save dir! %s\n', save_path_list$missing_view) %>% cat()
    }
    
    sprintf('missing view\n') %>% cat()
    for (scale_name in names(simu_data)){
      sprintf('  %s...\n', scale_name) %>% cat()
      gg_vis <- vis_miss(simu_data[[scale_name]][[1]]$simulation_data, cluster=T) +
        theme(axis.text.x = element_text(angle = 90))
      
      ggsave(file.path(save_path_list$missing_view, sprintf('%s_missing_view_cluster.png', scale_name)),
             plot = gg_vis, device = NULL, path = NULL,
             scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             dpi = 300, limitsize = T)
      
      # no cluster
      gg_vis <- vis_miss(simu_data[[scale_name]][[1]]$simulation_data) +
        theme(axis.text.x = element_text(angle = 90))
      
      ggsave(file.path(save_path_list$missing_view, sprintf('%s_missing_view.png', scale_name)),
             plot = gg_vis, device = NULL, path = NULL,
             scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             dpi = 300, limitsize = T)
    }
    sprintf('missing view saved! %s\n', save_path_list$missing_view) %>% cat()
  }
  
  # total missing rate
  {
    missing_rate_list <- sapply(simu_data, function(scale){
      data <- scale[[1]]$simulation_data
      mr <- length(which_na(data)) / (nrow(data) * ncol(data))
      mc <- (nrow(data) - length(which(complete.cases(data)))) / nrow(data)
      return(c(missing_element=mr, missing_case=mc))
    })
    
    melt_mr <- melt(missing_rate_list)
    colnames(melt_mr) <- c('class', 'scale', 'value')
    
    gg_mr = ggplot(melt_mr, aes(x = scale,y = value, fill=class))+
      geom_bar(stat ="identity",width = 0.6,position = "dodge")+     
      geom_text(aes(label = sprintf('%.2f%%', value*100), vjust = -0.8, hjust = 0.5),
                position=position_dodge(width = 0.5)) +
      labs(x = "scale name",y = "missing rate", title = 'scale element missing rate')+  
      guides(fill = guide_legend(reverse = F))+
      theme(legend.title = element_blank(),
            legend.position = 'right',
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    ggsave(save_path_list$all_missing_rate,
           plot = gg_mr, device = NULL, path = NULL,
           scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
           dpi = 300, limitsize = T)
    sprintf('all missing rate saved: %s\n', save_path_list$all_missing_rate) %>% cat()
  }
}

