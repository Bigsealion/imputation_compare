# 2022.12.23
# s4.5.1: compare simulation and sample results (but not with true model)
rm(list=ls())
source('/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/imp_compare_utils.R')
library(ggplot2)
library(reshape2)

# time
time_op <- Sys.time()

# set parameter ================================================================================================
summary_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1.1_summary_data/CVLT_ZsHarQCT1_TrueMiss_NoRep0.8'
out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2.1_compare_summary_Complete/CVLT_ZsHarQCT1_TrueMiss_NoRep0.8'

# summary_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1.1_summary_data/CVLT_IntTCoefSimuY_80Miss_NoRep0.8'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2.1_compare_summary_Complete/CVLT_IntTCoefSimuY_80Miss_NoRep0.8'

# summary_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1.1_summary_data/HarRehoGender_80Miss_NoRep0.8'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2.1_compare_summary_Complete/HarRehoGender_80Miss_NoRep0.8'

# summary_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1.1_summary_data/HarRehoGenderSimuY_TrueMiss_NoRep0.8'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2.1_compare_summary_Complete/HarRehoGenderSimuY_TrueMiss_NoRep0.8'

# scale_choose <- c('Base', 'RO', 'UG', 'GoNogo', 'nBack', 'PassBall', 'CVLT')
# scale_choose <- c('RO', 'PassBall')
# scale_choose <- c('reho')
# remove_method <- c('CCA')
remove_method <- c()

is_choose_scale_auto <- T
is_get_summary_data_auto <- T  # will reset simu_path and true_path
is_remove_method <- T  # will remove summary with method in vector 'remove_method'
pattern <- 'Imp_Method__(.*?)__'

# other methods will compare with ref method, "Compelte" is complete sample results
# also can set as "Imp_Method__mice_pmm__", details should to view the "pattern"
ref_method <- "Complete" 

# get simu_path and true_path auto ========================================
if (is_get_summary_data_auto){
  simu_path <- file.path(summary_dir, 'Summary_Stack.RData')
  
  true_path <- ''
  for (file_name in dir(summary_dir)){
    if (str_detect(file_name, 'Summary_TRUE_')){
      true_path <- file.path(summary_dir, file_name)
    }
  }
  
  if (!(file_test('-f', simu_path) & file_test('-f', true_path))){
    print('ERROR: lack file!')
    return(-1)}
  
  sprintf('Auto get file in summary_dir: %s\n', summary_dir) %>% cat()
}

# mkdir out dir ============================================================
if (!file.exists(out_dir)){
  dir.create(out_dir, recursive = T)
  sprintf('Create out dir! %s\n', out_dir) %>% cat()
}

# load data ================================================================
{
  simu_data_name <- load(simu_path)
  eval(parse(text=sprintf("simu_summary_method_list <- %s", simu_data_name)))
  
  true_data_name <- load(true_path)
  eval(parse(text=sprintf("true_summary_list <- %s", true_data_name)))
  
  sprintf('data loaded! %s\n', Sys.time()) %>% cat()
  
  # if only one scale, code will error, add
  if (length(true_summary_list$info) == 1){
    sprintf('__aux is meaningless\n') %>% cat()
    only_name <- names(true_summary_list$info)
    
    # add in simu_summary_method_list
    for (method_name in names(simu_summary_method_list)){
      simu_summary_method_list[[method_name]]$summary[['__aux']] <- simu_summary_method_list[[method_name]]$summary[[only_name]]
      simu_summary_method_list[[method_name]]$info[['__aux']] <- simu_summary_method_list[[method_name]]$info[[only_name]]
    }
    true_summary_list$summary[['__aux']] <- true_summary_list$summary[[only_name]]
    true_summary_list$info[['__aux']] <- true_summary_list$info[[only_name]]
  }
}

# remove method ============================================================
if (is_remove_method){
  for(imp_method in names(simu_summary_method_list)){
    if (str_detect(imp_method, pattern)){
      method_name <- str_match(imp_method, pattern)[2]
      if ( method_name %in% remove_method){
        simu_summary_method_list[[imp_method]] <- NULL
        sprintf('Remove imp method: %s\n', method_name) %>% cat()
      }
    }
  }
}

# choose scale =============================================================
{
  if (is_choose_scale_auto){
    sprintf('Choose scale auto!\n') %>% cat
    scale_info_all <- lapply(simu_summary_method_list, function(x){x$info})
    all_scale_names <- lapply(scale_info_all, names) %>% unlist %>% unique
    
    scale_info_all <- sapply(scale_info_all, function(scale, all_scale_names){
      diff_names <- setdiff(all_scale_names, names(scale))
      if (length(diff_names)){scale[diff_names] <- T}
      scale <- scale[order(names(scale))]
      return(scale)
    }, all_scale_names)
    
    scale_good <- apply(scale_info_all, 1, any)
    scale_choose <- names(scale_good)[!scale_good]
    sprintf('scale choosed:\n') %>% cat
    for (scale_choosed_name in scale_choose){
      sprintf('    %s\n', scale_choosed_name) %>% cat
    }
  }
  
  for (scale_names in names(simu_summary_method_list[[1]]$summary)){
    if (!scale_names %in% scale_choose){
      for (simu_method in names(simu_summary_method_list)){
        simu_summary_method_list[[simu_method]]$summary[[scale_names]] <- NULL
        simu_summary_method_list$info <- simu_summary_method_list$info[-which(names(simu_summary_method_list$info) == scale_names)]
      }
      true_summary_list$summary[[scale_names]] <- NULL
      true_summary_list$info <- true_summary_list$info[-which(names(true_summary_list$info) == scale_names)]
      
      sprintf('remove %s in scale\n', scale_names) %>% cat()
    }
  }
}

# checking whether reference method in names(simu_summary_method_list) =====
{
  method_names <- names(simu_summary_method_list)
  if (ref_method %in% method_names){
    method_names <- method_names[-which(method_names == ref_method)]
  }
  else{
    cat(sprintf("Error: ref_method '%s' not in input list!\n", ref_method))
  }
}

# compare simulation with sample resutls ====================================
compare_result_method <- list()
for (method_name in method_names){
  sprintf('method: %s...\n', method_name) %>% cat()
  
  compare_result_method[[method_name]] <- get_compare_result_by_complete_batch(
    simu_summary_method_list[[method_name]], simu_summary_method_list[[ref_method]])
  
  sprintf('Now: %s, Total time: %.4f %s\n', 
          Sys.time(), Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
  
}

# save  ------------
{
  # save_path
  {
    save_path_list <- list()
    save_path_list$compare_result <- file.path(out_dir, 'compare_result.RData')
    save_path_list$bias_dir <- file.path(out_dir, 'bias')
    save_path_list$pre_bias_dir <- file.path(out_dir, 'pre_bias')
    save_path_list$PB_EucDistance_dir <- file.path(out_dir, 'PB_EucDistance')
    save_path_list$pre_bias_diffcomp_Cut10_dir <- file.path(out_dir, 'pre_bias_diffcomp_Cut100')
    save_path_list$pre_bias_diffcomp_DiffAbsMeanPmm_dir <- file.path(out_dir, 'pre_bias_diffcomp_DiffAbsMeanPmm')
    save_path_list$CI_CR <- file.path(out_dir, 'CI_coverage_rate')
    save_path_list$CI_CR_95diff <- file.path(out_dir, 'CI_coverage_rate_95diff')
    save_path_list$CI_AW <- file.path(out_dir, 'CI_AW')
    save_path_list$CI_AW_PB <- file.path(out_dir, 'CI_AW_PB')
    save_path_list$res_pre <- file.path(out_dir, 'ResidualPrecent')
    save_path_list$res_box <- file.path(out_dir, 'ResidualBox')
    save_path_list$compare_figure_data <- file.path(out_dir, 'compare_figure_data.RData')
  }
  
  # save RData
  {
    save(compare_result_method, file=save_path_list$compare_result)
    sprintf('compare result.RData saved!: %s\n', save_path_list$compare_result) %>% cat()
  }
  
  # out percent bias bin
  {
    # mkdir
    if (!file.exists(save_path_list$bias_dir)){
      dir.create(save_path_list$bias_dir, recursive = T)
      sprintf('Create bias save dir! %s\n', save_path_list$bias_dir) %>% cat()
    }
    
    # get bias of each psytool scale
    bias_matrix <- sapply(compare_result_method, function(method){
      lapply(method$compare_result_mean, function(psy){
        psy$raw_bias
      })
    })
    
    sprintf('bias bar save dir: %s\n',save_path_list$bias_dir) %>% cat()
    for (psy in rownames(bias_matrix)){
      sprintf('%s\n', psy) %>% cat()
      psy_bias <- bias_matrix[psy, ] %>% as.data.frame()
      psy_bias <- cbind(names=rownames(psy_bias), psy_bias)
      melt_psy_bias <- melt(psy_bias, variable.name='method', value.name='bias')
      
      gg_bias = ggplot(melt_psy_bias, aes(x = names,y = bias,fill = method))+
        geom_bar(stat ="identity",width = 0.6,position = "dodge")+     
        labs(x = "variables",y = "bias", title = psy)+  
        guides(fill = guide_legend(reverse = F))+
        theme(legend.title = element_blank(),
              legend.position = 'right',
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
      
      ggsave(file.path(save_path_list$bias_dir, sprintf('%s_Bias.png', psy)),
             plot = gg_bias, device = NULL, path = NULL,
            #  scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             scale = 1, width = 12, height = 6, units ="in",
             dpi = 300, limitsize = T)
    }
  }
  
  # out percent percent bias bin
  {
    # mkdir
    if (!file.exists(save_path_list$pre_bias_dir)){
      dir.create(save_path_list$pre_bias_dir, recursive = T)
      sprintf('Create percent bias save dir! %s\n', save_path_list$pre_bias_dir) %>% cat()
    }
    
    # get percent bias of each psytool scale
    percent_bias_matrix1 <- sapply(compare_result_method, function(method){
      lapply(method$compare_result_mean, function(psy){
        psy$percent_bias
      })
    })
    
    sprintf('percent bias bar save dir: %s\n',save_path_list$pre_bias_dir) %>% cat()
    for (psy in rownames(percent_bias_matrix1)){
      sprintf('%s\n', psy) %>% cat()
      psy_pb <- percent_bias_matrix1[psy, ] %>% as.data.frame()
      psy_pb <- cbind(names=rownames(psy_pb), psy_pb)
      melt_psy_pb <- melt(psy_pb, variable.name='method', value.name='percent_bias')
      
      gg_pb = ggplot(melt_psy_pb, aes(x = names,y = percent_bias,fill = method))+
        geom_bar(stat ="identity",width = 0.6,position = "dodge")+     
        # scale_fill_manual(values = c("red","blue"))+
        labs(x = "variables",y = "precent bias", title = psy)+  
        geom_hline(yintercept = 100, linetype="dashed", size=0.1)+
        # geom_text(aes(label = melt_psy_pb$percent_bias),position=position_dodge(width = 0.5),size = 5,vjust = -0.25)+ 
        guides(fill = guide_legend(reverse = F))+
        theme(legend.title = element_blank(),
              legend.position = 'right',
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
      
      ggsave(file.path(save_path_list$pre_bias_dir, sprintf('%s_PercentBias.png', psy)),
             plot = gg_pb, device = NULL, path = NULL,
            #  scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             scale = 1, width = 12, height = 6, units ="in",
             dpi = 300, limitsize = T)
    }
  }
  
  # out percent percent bias, diff of complete, Cut100 bin
  {
    # mkdir
    if (!file.exists(save_path_list$pre_bias_diffcomp_Cut10_dir)){
      dir.create(save_path_list$pre_bias_diffcomp_Cut10_dir, recursive = T)
      sprintf('Create percent bias save dir! %s\n', save_path_list$pre_bias_diffcomp_Cut10_dir) %>% cat()
    }
    
    # get percent bias of each psytool scale
    percent_bias_matrix2 <- sapply(compare_result_method, function(method){
      lapply(method$compare_result_mean, function(psy){
        psy$percent_bias
      })
    })
    
    if (F){
            sprintf('Diff Complete Cut10 percent bias bar save dir: %s\n',save_path_list$pre_bias_diffcomp_Cut10_dir) %>% cat()
      for (psy in rownames(percent_bias_matrix2)){
        sprintf('%s\n', psy) %>% cat()
        psy_pb <- percent_bias_matrix2[psy, ] %>% as.data.frame()
        psy_pb_diffcomp <- subset(psy_pb, select=-Complete) - psy_pb[, 'Complete']
        
        psy_pb_diffcomp <- cbind(names=rownames(psy_pb), psy_pb_diffcomp)
        melt_psy_pb_diffcomp <- melt(psy_pb_diffcomp, variable.name='method', value.name='percent_bias')
        melt_psy_pb_diffcomp[which(melt_psy_pb_diffcomp[,3] > 100),3] <- 100 
        melt_psy_pb_diffcomp[which(melt_psy_pb_diffcomp[,3] < -100),3] <- -100
        
        gg_pbdiff = ggplot(melt_psy_pb_diffcomp, aes(x = names,y = percent_bias,fill = method))+
          geom_bar(stat ="identity",width = 0.6,position = "dodge")+     
          # scale_fill_manual(values = c("red","blue"))+
          labs(x = "variables",y = "precent bias (diff of Complete) Cut100", title = psy)+  
          # geom_text(aes(label = melt_psy_pb$percent_bias),position=position_dodge(width = 0.5),size = 5,vjust = -0.25)+ 
          guides(fill = guide_legend(reverse = F))+
          theme(legend.title = element_blank(),
                legend.position = 'right',
                axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
        
        ggsave(file.path(save_path_list$pre_bias_diffcomp_Cut10_dir, sprintf('%s_PercentBiasDiffCompleteCut10.png', psy)),
              plot = gg_pbdiff, device = NULL, path = NULL,
              #  scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
              scale = 1, width = 12, height = 6, units ="in",
              dpi = 300, limitsize = T)
      }
    }
  }
  
  # out distance of PB 
  {
    # mkdir
    if (!file.exists(save_path_list$PB_EucDistance_dir)){
      dir.create(save_path_list$PB_EucDistance_dir, recursive = T)
      sprintf('Create percent bias save dir! %s\n', save_path_list$PB_EucDistance_dir) %>% cat()
    }
    
    sprintf('Norm Distance of PB:\n') %>% cat
    PB_NormEucDistance <- apply(percent_bias_matrix1, 2, function(met){
      sapply(met, function(sc){
        sqrt(sum(sc^2)/ length(sc))
      })
    })
    
    for (scale_name in rownames(PB_NormEucDistance)){
      sprintf('    %s\n', scale_name) %>% cat()
      melt_PB_EucDis <- melt(PB_NormEucDistance[scale_name, ])
      melt_PB_EucDis <- cbind( rownames(melt_PB_EucDis), melt_PB_EucDis)
      colnames(melt_PB_EucDis) <- c('method', 'value')
      
      gg_PBEucDis = ggplot(melt_PB_EucDis, aes(x = reorder(method, value),y = value,fill = method))+
        geom_bar(stat ="identity",width = 0.6,position = "dodge")+     
        labs(x = "method",y = "PB_EucDistance", title = sprintf('%s', scale_name))+  
        geom_text(aes(label = sprintf('%.2f', value)), vjust = -0.25)+ 
        guides(fill = guide_legend(reverse = F))+
        theme(legend.title = element_blank(),
              legend.position = 'right',
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
      
      ggsave(file.path(save_path_list$PB_EucDistance_dir, sprintf('PB_EucDistance_%s.png', scale_name)),
             plot = gg_PBEucDis, device = NULL, path = NULL,
            #  scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             scale = 1, width = 12, height = 6, units ="in",
             dpi = 300, limitsize = T)
    }
  }
  
  # out percent percent bias, diff of complete, then diff mean and pmm, Cut100, bin
  {
    # mkdir
    if (!file.exists(save_path_list$pre_bias_diffcomp_DiffAbsMeanPmm_dir)){
      dir.create(save_path_list$pre_bias_diffcomp_DiffAbsMeanPmm_dir, recursive = T)
      sprintf('Create pre_bias_diffcomp_DiffAbsMeanPmm dir! %s\n', save_path_list$pre_bias_diffcomp_DiffAbsMeanPmm_dir) %>% cat()
    }
    
    # get percent bias of each psytool scale
    percent_bias_matrix3 <- sapply(compare_result_method, function(method){
      lapply(method$compare_result_mean, function(psy){
        psy$percent_bias
      })
    })
    
    if (F) {
      sprintf("pre_bias_diffcomp_DiffAbsMeanPmm dir: %s\n", save_path_list$pre_bias_diffcomp_DiffAbsMeanPmm_dir) %>% cat()
      for (psy in rownames(percent_bias_matrix3)) {
        sprintf("%s\n", psy) %>% cat()
        psy_pb <- percent_bias_matrix3[psy, ] %>% as.data.frame()
        psy_pb_diffcomp <- subset(psy_pb, select = -Complete) - psy_pb[, "Complete"]
        psy_pb_diffcomp_DiffAbsMeanPmm <- abs(psy_pb_diffcomp[, "Imp_Method__mice_mean__"]) - abs(psy_pb_diffcomp[, "Imp_Method__mice_pmm__"])
        psy_pb_diffcomp_DiffAbsMeanPmm[which(psy_pb_diffcomp_DiffAbsMeanPmm > 100)] <- 100
        psy_pb_diffcomp_DiffAbsMeanPmm[which(psy_pb_diffcomp_DiffAbsMeanPmm < -100)] <- -100
        psy_pb_diffcomp_DiffAbsMeanPmm <- psy_pb_diffcomp_DiffAbsMeanPmm %>% as.data.frame()

        psy_pb_diffcomp_DiffAbsMeanPmm <- cbind(names = rownames(psy_pb), diff = psy_pb_diffcomp_DiffAbsMeanPmm)
        colnames(psy_pb_diffcomp_DiffAbsMeanPmm)[2] <- "diff"

        gg_pbdiff2 = ggplot(psy_pb_diffcomp_DiffAbsMeanPmm, aes(x = names, y = diff, fill = names)) +
          geom_bar(stat = "identity", width = 0.8) +
          labs(x = "variables", y = "precent bias (diff of Complete) (mean - pmm) Cut100", title = psy) +
          guides(fill = guide_legend(reverse = F)) +
          theme(
            legend.title = element_blank(),
            legend.position = "none",
            axis.text.x = element_text(angle = 90)
          )

        ggsave(file.path(save_path_list$pre_bias_diffcomp_DiffAbsMeanPmm_dir, sprintf("%s_PercentBiasDiffCompleteDiffAbsMeanPmmCut10.png", psy)),
          plot = gg_pbdiff2, device = NULL, path = NULL,
          #  scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
          scale = 1, width = 12, height = 6, units = "in",
          dpi = 300, limitsize = T
        )
      }
    }

  }
  
  # out percent CI coverage rate bin
  {
    # mkdir
    if (!file.exists(save_path_list$CI_CR)){
      dir.create(save_path_list$CI_CR, recursive = T)
      sprintf('Create CI_CR save dir! %s\n', save_path_list$CI_CR) %>% cat()
    }
    
    # get percent bias of each psytool scale
    CI_CR_matrix <- sapply(compare_result_method, function(method){
      lapply(method$compare_result_mean, function(psy){
        psy$coverage_rate
      })
    })
    
    sprintf('CI CR bar save dir: %s\n',save_path_list$CI_CR) %>% cat()
    for (psy in rownames(CI_CR_matrix)){
      sprintf('%s\n', psy) %>% cat()
      psy_CICR <- CI_CR_matrix[psy, ] %>% as.data.frame()
      psy_CICR <- cbind(names=rownames(psy_CICR), psy_CICR)
      melt_psy_CICR <- melt(psy_CICR, variable.name='method', value.name='CI_CR')
      
      gg_CICR = ggplot(melt_psy_CICR, aes(x = names,y = CI_CR,fill = method))+
        geom_bar(stat ="identity",width = 0.6,position = "dodge")+
        labs(x = "variables",y = "95CI Coverage Rate", title = psy)+  
        guides(fill = guide_legend(reverse = F))+
        geom_hline(yintercept = 0.95, linetype="dashed", size=0.5)+
        theme(legend.title = element_blank(),
              legend.position = 'right',
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
      
      ggsave(file.path(save_path_list$CI_CR, sprintf('%s_CI_CoverageRate.png', psy)),
             plot = gg_CICR, device = NULL, path = NULL,
            #  scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             scale = 1, width = 12, height = 6, units ="in",
             dpi = 300, limitsize = T)
    }
  }
  
  # out percent CI coverage rate bin 95diff
  {
    # mkdir
    if (!file.exists(save_path_list$CI_CR_95diff)){
      dir.create(save_path_list$CI_CR_95diff, recursive = T)
      sprintf('Create CI_CR_95diff save dir! %s\n', save_path_list$CI_CR_95diff) %>% cat()
    }
    
    # get percent bias of each psytool scale
    CI_CR_matrix <- sapply(compare_result_method, function(method){
      lapply(method$compare_result_mean, function(psy){
        psy$coverage_rate
      })
    })
    
    sprintf('CI CR bar save dir: %s\n',save_path_list$CI_CR) %>% cat()
    for (psy in rownames(CI_CR_matrix)){
      sprintf('%s\n', psy) %>% cat()
      psy_CICR <- CI_CR_matrix[psy, ] %>% as.data.frame()
      psy_CICR <- cbind(names=rownames(psy_CICR), psy_CICR)
      melt_psy_CICR_diff <- melt(psy_CICR, variable.name='method', value.name='CI_CR')
      melt_psy_CICR_diff[, 3] <- melt_psy_CICR_diff[, 3] - 0.95
      
      gg_CICR_95diff = ggplot(melt_psy_CICR_diff, aes(x = names,y = CI_CR,fill = method))+
        geom_bar(stat ="identity",width = 0.6,position = "dodge")+
        labs(x = "variables",y = "95CI Coverage Rate", title = psy)+  
        guides(fill = guide_legend(reverse = F))+
        theme(legend.title = element_blank(),
              legend.position = 'right',
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
      
      ggsave(file.path(save_path_list$CI_CR_95diff, sprintf('%s_CI_CoverageRate_95diff.png', psy)),
             plot = gg_CICR_95diff, device = NULL, path = NULL,
            #  scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             scale = 1, width = 12, height = 6, units ="in",
             dpi = 300, limitsize = T)
    }
  }
  
  # out AW
  {
    # mkdir
    if (!file.exists(save_path_list$CI_AW)){
      dir.create(save_path_list$CI_AW, recursive = T)
      sprintf('Create bias save dir! %s\n', save_path_list$CI_AW) %>% cat()
    }
    
    # get bias of each psytool scale
    AW_matrix <- sapply(compare_result_method, function(method){
      lapply(method$compare_result_mean, function(psy){
        psy$average_width
      })
    })
    
    sprintf('AW bar save dir: %s\n',save_path_list$bias_dir) %>% cat()
    for (psy in rownames(AW_matrix)){
      sprintf('%s\n', psy) %>% cat()
      psy_AW <- AW_matrix[psy, ] %>% as.data.frame()
      psy_AW <- cbind(names=rownames(psy_AW), psy_AW)
      melt_psy_AW <- melt(psy_AW, variable.name='method', value.name='AW')
      
      gg_AW = ggplot(melt_psy_AW, aes(x = names,y = AW,fill = method))+
        geom_bar(stat ="identity",width = 0.6,position = "dodge")+     
        labs(x = "variables",y = "AW", title = psy)+  
        guides(fill = guide_legend(reverse = F))+
        theme(legend.title = element_blank(),
              legend.position = 'right',
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
      
      ggsave(file.path(save_path_list$CI_AW, sprintf('%s_AW.png', psy)),
             plot = gg_AW, device = NULL, path = NULL,
            #  scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             scale = 1, width = 12, height = 6, units ="in",
             dpi = 300, limitsize = T)
    }
  }
  
  # out AW per bias
  {
    # mkdir
    if (!file.exists(save_path_list$CI_AW_PB)){
      dir.create(save_path_list$CI_AW_PB, recursive = T)
      sprintf('Create bias save dir! %s\n', save_path_list$CI_AW_PB) %>% cat()
    }
    
    # get bias of each psytool scale
    AWPB_matrix <- sapply(compare_result_method, function(method){
      lapply(method$compare_result_mean, function(psy){
        psy$AW_per_bias
      })
    })
    
    sprintf('AW_PB bar save dir: %s\n',save_path_list$bias_dir) %>% cat()
    for (psy in rownames(AWPB_matrix)){
      sprintf('%s\n', psy) %>% cat()
      psy_AWPB <- AWPB_matrix[psy, ] %>% as.data.frame()
      psy_AWPB <- cbind(names=rownames(psy_AWPB), psy_AWPB)
      melt_psy_AWPB <- melt(psy_AWPB, variable.name='method', value.name='AW')
      
      gg_AWPB = ggplot(melt_psy_AWPB, aes(x = names,y = AW,fill = method))+
        geom_bar(stat ="identity",width = 0.6,position = "dodge")+     
        labs(x = "variables",y = "AW per bias", title = psy)+  
        guides(fill = guide_legend(reverse = F))+
        theme(legend.title = element_blank(),
              legend.position = 'right',
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
      
      ggsave(file.path(save_path_list$CI_AW_PB, sprintf('%s_AW_PB.png', psy)),
             plot = gg_AWPB, device = NULL, path = NULL,
            #  scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             scale = 1, width = 12, height = 6, units ="in",
             dpi = 300, limitsize = T)
    }
  }
  
  # out Residual Precent  
  # maybe something is wrong, stopping this evaluation
  if(F){
    # mkdir
    if (!file.exists(save_path_list$res_pre)){
      dir.create(save_path_list$res_pre, recursive = T)
      sprintf('Create res_pre save dir! %s\n', save_path_list$res_pre) %>% cat()
    }
    
    # get residual percent of each psytool scale
    ResPre_matrix <- sapply(compare_result_method, function(method){
      sapply(method$compare_result_mean, function(psy){
        psy$res_out[['mean_pre_res']]
      })
    })
    
    ResCV_matrix <- sapply(compare_result_method, function(method){
      lapply(method$compare_result_mean, function(psy){
        psy$res_out[['cv_res']]
      })
    })
    
    ResMean_matrix <- sapply(compare_result_method, function(method){
      sapply(method$compare_result_mean, function(psy){
        psy$res_out[['mean_res']]
      })
    })
    

  }
  
  # out residual Box figure
  # maybe this figure is meaningless
  {
    # mkdir
    if (!file.exists(save_path_list$res_box)){
      dir.create(save_path_list$res_box, recursive = T)
      sprintf('Create res_box save dir! %s\n', save_path_list$res_box) %>% cat()
    }
    
    # get y bias
    # get y bias of each psytool scale
    y_bias_list <- lapply(simu_summary_method_list, function(method){
      lapply(method$summary, function(sc){
        res_list <- lapply(sc, function(boot){boot$res$res_value})
        Reduce(function(x,y){c(x,y)}, res_list)
      })
    })
    
    y_bias_scale_list <- list()
    for(method_name in names(y_bias_list)){
      for (scale_name in names(y_bias_list[[method_name]])){
        y_bias_scale_list[[scale_name]][[method_name]] <- y_bias_list[[method_name]][[scale_name]]
      }
    }
    
    # draw
    for (scale_name in names(y_bias_scale_list)){
      melt_scale_y_bias <- melt(y_bias_scale_list[[scale_name]])
      names(melt_scale_y_bias) <- c('value', 'method')
      
      gg_y_bias_box <- ggplot(melt_scale_y_bias, aes(x = method, y = value, fill=method)) +
        geom_boxplot(alpha=0.7) +
        scale_y_continuous(name = "y res")+
        scale_x_discrete(name = "scale") +
        ggtitle(sprintf("%s y_bias", scale_name))+
        theme(legend.title = element_blank(),
              legend.position = 'right',
              axis.text.x = element_text(angle = -20, hjust = 0, vjust = 1))
      
      ggsave(file.path(save_path_list$res_box, sprintf('%s_y_Bias.png', scale_name)),
             plot = gg_y_bias_box, device = NULL, path = NULL,
            #  scale = 1, width = 12, height = 6, units =c("in", "cm", "mm"),
             scale = 1, width = 12, height = 6, units ="in",
             dpi = 300, limitsize = T)
    }
    
    # draw in one figure will effect by unit of y (like 1 and 100, can't view 1)
    if(F){
      melt_y_bias <- melt(y_bias_list)
      names(melt_y_bias) <- c('value', 'scale', 'method')
      
      # draw
      gg_y_bias_box <- ggplot(melt_y_bias, aes(x = scale, y = value, fill = method)) +
        geom_boxplot(alpha=0.7) +
        scale_y_continuous(name = "y res")+
        scale_x_discrete(name = "scale") +
        ggtitle("y bias") +
        theme_bw()
    }
  }
  
  # save compare list RData
  {
    # save compare_out_list
    compare_out_list <- list()
    compare_out_list$AW_PB <- AWPB_matrix
    compare_out_list$RB <- bias_matrix
    compare_out_list$PB <- percent_bias_matrix1
    compare_out_list$CR <- CI_CR_matrix
    compare_out_list$PBEucDis <- PB_NormEucDistance
    
    save(compare_out_list, file=save_path_list$compare_figure_data)
    sprintf('compare_figure_data.RData saved: %s\n', save_path_list$compare_figure_data) %>% cat
  }
  
}

sprintf('Now: %s, Total time: %.4f %s\n', 
        Sys.time(), Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
