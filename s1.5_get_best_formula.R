# s1.5: get best formula of the data
rm(list=ls())
library(pheatmap)
source('/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/imp_compare_utils.R')

time_op <- Sys.time()
# set parameter
# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/combine_data/choose_psy_1.5.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/'
# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_Cognative_T1surf_Combine_Data.Rdata'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/Reho_age/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula'

# source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_Cog5_ZsHarQCT1_CorCombine_Data.Rdata'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula'

#source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata'
#out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula'

#source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/Reho_age/s1.4_zRehoAAL116Center_Gender_CorCombine_Data.Rdata'
#out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula'

source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_x1.2_CovCVLT_ZsHarQCT1_CorCombine_Data.Rdata'
out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula'


method <- 'cor_step'  # ['step', 'cor_step', 'all']
cor_limit <- 0.9  # only using in step_cor

# mkdir out dir
save_dir <- file.path(out_dir, basename(source_path))
if (!file.exists(save_dir)){
  dir.create(save_dir, recursive = T)
  sprintf('Create out dir! %s\n', save_dir) %>% cat()
}

# load parameter
load_name <- load(source_path)
eval(parse(text=sprintf("seg_data_list <- %s", load_name)))

# get formula
{
  selected_var_list <- list()
  
  if (method == 'step'){
    step_model <- lapply(seg_data_list, function(psy){
      psy <- psy[complete.cases(psy) , -1]  # remove RefID
      
      var <- colnames(psy)
      fx <- paste(var[-1], collapse = '+')
      fyx <- paste(var[1], fx, sep='~') %>% formula()
      
      model <- lm(fyx, psy)
      model.step <- step(model)
    
      return(model.step)
    })
    
    formula_save <- lapply(step_model, function(scale){scale$call$formula})
    selected_var_list <- lapply(step_model, function(scale){
      select_name <- names(scale$coefficients)
      select_name <- subset(select_name, select_name != '(Intercept)')
      })
    
    model_summary <- lapply(step_model, summary)
  }
  else if (method == 'cor_step'){
    seg_data_list_lowcor <- lapply(seg_data_list, function(scale, cor_limit){
      # col 1 is ID, 2 is y
      remove_var_all <- cor_select(scale[, -c(1,2)], cor_limit = cor_limit)
      scale <- scale[, !names(scale) %in% remove_var_all, drop=F]
      return(scale)
    }, cor_limit)
    
    step_model <- lapply(seg_data_list_lowcor, function(psy){
      psy <- psy[complete.cases(psy) , -1]  # remove RefID
      
      var <- colnames(psy)
      fx <- paste(var[-1], collapse = '+')
      fyx <- paste(var[1], fx, sep='~') %>% formula()
      
      model <- lm(fyx, psy)
      model.step <- step(model)
      
      return(model.step)
    })
    
    formula_save <- lapply(step_model, function(scale){scale$call$formula})
    selected_var_list <- lapply(step_model, function(scale){
      select_name <- names(scale$coefficients)
      select_name <- subset(select_name, select_name != '(Intercept)')
    })
    
    model_summary <- lapply(step_model, summary)
  }
  else if (method == 'all'){
    step_model <- lapply(seg_data_list, function(psy){
      psy <- psy[complete.cases(psy) , -1]  # remove RefID
      
      var <- colnames(psy)
      fx <- paste(var[-1], collapse = '+')
      # fyx <- paste(var[1], fx, sep='~') %>% formula()
      fyx <- paste(var[1], fx, sep='~')
      
      # model <- lm(fyx, psy)
      
      eval(parse(text=sprintf("model <- lm(%s, psy)", fyx)))
      
      return(model)
    })
    
    formula_save <- lapply(step_model, function(scale){scale$call$formula})
    selected_var_list <- lapply(step_model, function(scale){
      select_name <- names(scale$coefficients)
      select_name <- subset(select_name, select_name != '(Intercept)')
    })
    
    model_summary <- lapply(step_model, summary)
  }
  else{
    sprintf('ERROR: Unsupport method: %s\n', method) %>% cat()
  }

  sprintf('Complete get formula. Now: %s, Total time: %.4f %s\n', 
          Sys.time(), Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
  
}

# get cor matrix of selected variable
{
  sprintf('Getting Cor Matrix...\n') %>% cat()
  cor_list <- list()
  for (scale_name in names(seg_data_list)){
    sprintf('    %s\n', scale_name) %>% cat
    selected_var <- selected_var_list[[scale_name]]
    cor_list[[scale_name]] <- seg_data_list[[scale_name]][selected_var] %>% cor(use = 'complete.obs')
  }
}

# save formula
save_path <- file.path(save_dir, sprintf('Formula__%s__.RData', method))
save(formula_save, file=save_path)
sprintf('Formula saved: %s\n', save_path) %>% cat

# save model
model_save_path <- file.path(save_dir, sprintf('model__%s__.RData', method))
save(step_model, file=model_save_path)
sprintf('model saved: %s\n', model_save_path) %>% cat

# save cor heatmap
{
  cor_out_dir <- file.path(save_dir, sprintf('Cor__%s__', method))
  if (!file.exists(cor_out_dir)){
    dir.create(cor_out_dir,recursive = T)
    sprintf('Create out dir! %s\n', cor_out_dir) %>% cat()
  }
  
  for(scale_name in names(cor_list)){
    pheat_out_path <- file.path(cor_out_dir, sprintf('%s.png', scale_name))
    png(pheat_out_path, width=1200, height=1200)
    pheatmap(cor_list[[scale_name]], cluster_rows = F, cluster_cols = F, display_numbers = T)
    dev.off()
  }
  sprintf('Cor heatmap saved! %s\n', cor_out_dir) %>% cat()
}

# for check------------------------------------------------
if(T){
  if (!file.exists(file.path(save_dir, 'ModelFit_True_Cor'))){
    dir.create(file.path(save_dir, 'ModelFit_True_Cor'), recursive = T)
    sprintf('Create out dir! %s\n', file.path(save_dir, 'ModelFit_True_Cor')) %>% cat()
  }
  
  for (scale_names in names(step_model)){
    y <- step_model[[scale_names]]$residuals + step_model[[scale_names]]$fitted.values
    fity <- step_model[[scale_names]]$fitted.values
    cor_model <- cor(y, fity)
    
    img_out_path <- file.path(save_dir, 'ModelFit_True_Cor', sprintf('%s_Method__%s__ModelFit_True_Cor.png', scale_names,method ))
    png(img_out_path, width=1200, height=1200)
    plot(y, fity)
    title(sprintf('%s, model predit cor=%.4f', scale_names, cor_model))
    dev.off()
  }
  sprintf('ModelFit_True_Cor saved!: %s\n', save_dir) %>% cat()
}
