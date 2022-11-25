# 2021.02.23 simulation y
rm(list=ls())
library(dplyr)
library(mice)
library(dplyr)

# set parameter
set.seed(12345)
# complete_data_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/s1.7_ImpCompleteData_s1.4_Cognative_T1surf_Combine_Data.Rdata'
# raw_data_path <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata"
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata/model__cor_step__.RData'
# load_coef_path <- ''
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt_UnNorm/'

# complete_data_path <- ''
# raw_data_path <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/Reho_age/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata"
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata/model__cor_step__.RData'
# load_coef_path <- ''
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/reho_UnNorm/'

# complete_data_path <- ''
# raw_data_path <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata"
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata/model__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt_IntTCoef/'

# complete_data_path <- ''
# raw_data_path <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/Reho_age/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata"
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_zRehoAAL116_Gender_CorCombine_Data.Rdata/model__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/reho_IntTCoef/'

# complete_data_path <- ''
# raw_data_path <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_x1.2_CovCVLT_ZsHarQCT1_CorCombine_Data.Rdata"
# formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_x1.2_CovCVLT_ZsHarQCT1_CorCombine_Data.Rdata/model__cor_step__.RData'
# out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt_cov/'

complete_data_path <- ''
raw_data_path <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/Reho_age/s1.4_HarRehoAAL116_Gender.RData"
formula_path <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_HarRehoAAL116_Gender.RData/model__cor_step__.RData"
out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/HarReho_gender/'

is_imp_raw_data <- T  # if complete_data_path, setting F
is_standard_data <- F
is_balance_coef <- T # balance simu coef by col mean
is_load_coef <- T  # if load coef from formula_path, setting T, else will create new coef
is_add_intercept <- T  # if using this, intercept of raw model will be add (in is_load_coef)

# create out dir
if (!file.exists(out_dir)){
  dir.create(out_dir,recursive = T)
  sprintf('Create out dir! %s\n', out_dir) %>% cat()
}

# load data and formula
# data col1 is ID; col2 is T1 y, which will be replace by new simulation value
data_load_name <- load(raw_data_path)
eval(parse(text=sprintf("seg_data_list <- %s", data_load_name)))

if(!is_imp_raw_data){
  complete_data_load_name <- load(complete_data_path)
  eval(parse(text=sprintf("complete_data <- %s", complete_data_load_name)))
}

formula_load_name <- load(formula_path)
eval(parse(text=sprintf("model_list <- %s", formula_load_name)))

# if (is_load_coef){
#   # coef_load_name <- load(load_coef_path)
#   # eval(parse(text=sprintf("simu_coef_list <- %s", coef_load_name)))
#   simu_coef_list <- lapply(model_list, function(sc){sc$coefficients[-1]})
# }

# get selected variable
selected_var_list <- lapply(model_list, function(sc){
  select_name <- names(sc$coefficients)
  select_name <- subset(select_name, select_name != '(Intercept)')
})

# col standard data
{
  if(is_standard_data){
    sprintf('Standard data...\n') %>% cat
    standard_data <- lapply(seg_data_list, function(sc){
      # col 1 is ID, col 2 is T1
      sc[, -1] <- scale(sc[, -1])
      return(sc)
    })
  }
  else{
    standard_data <- seg_data_list
  }
}

# imp raw data
if (is_imp_raw_data){
  imp_raw_data <- lapply(standard_data, function(sc){
    mice(sc[, -c(1,2)], m=1, seed = 0)
  })
  
  complete_data <- lapply(imp_raw_data, complete)
  
  for (scale_name in names(seg_data_list)){
    complete_data[[scale_name]] <- cbind(seg_data_list[[scale_name]][, c(1,2)], complete_data[[scale_name]])
  }
  
}

# simulation coef
# True coef is 10*rnorm(0, 1) (only be buliding once)
# simulation y will add 1 * rnorm(0, 1) to each coef
{
  if (is_load_coef){
    sprintf('Getting Coef by True Model!\n') %>% cat()
    simu_coef_list <- lapply(model_list, function(sc){sc$coefficients})

  }
  else{
    sprintf('Building random Coef!\n') %>% cat()
    simu_coef_list <- list()
    for (scale_name in names(standard_data)){
      simu_coef_list[[scale_name]] <- 10*rnorm(length(selected_var_list[[scale_name]]), mean = 0, sd = 1)
      names(simu_coef_list[[scale_name]]) <- selected_var_list[[scale_name]]
      
      if(is_balance_coef){
        # if mean > 1, balance coef by (coef / mean)
        colmean <- complete_data[[scale_name]][, selected_var_list[[scale_name]]] %>% colMeans(na.rm=T)
        balance_coef <- colmean[which(colmean > 1)]
        simu_coef_list[[scale_name]][names(balance_coef)] <- 
          simu_coef_list[[scale_name]][names(balance_coef)] / balance_coef
      }
      
      # add Intercept
      if(is_add_intercept){
       intercept_i <- norm(1) 
      }
      else{
        intercept_i <- 0
      }
      sprintf('    add Intercept\n') %>% cat
      simu_coef_list[[scale_name]] <- c(intercept_i, simu_coef_list[[scale_name]])
      names(simu_coef_list[[scale_name]])[1] <- '(Intercept)'
    }
  }
  
}

# simulation y
simulation_y_data <- standard_data
for (scale_name in names(standard_data)){
  # col 2 is y
  simu_coef <- simu_coef_list[[scale_name]][-1]
  simu_intercept <- simu_coef_list[[scale_name]][1]
  
  # var_num <- length(simu_coef)
  # coef_matrix <- matrix(simu_coef, length(simu_coef), nrow(complete_data[[scale_name]])) %>% t
  # colnames(coef_matrix) <- names(simu_coef)
  x_df <- complete_data[[scale_name]][, names(simu_coef)]
  
  # matrix multiply
  # simulation_y <- as.matrix(x_df) %*% simu_coef + rnorm(nrow(x_df), 0, 1)*0.15 + simu_intercept # not var_num # cvlt
  simulation_y <- as.matrix(x_df) %*% simu_coef + rnorm(nrow(x_df), 0, 1)*0.25 + simu_intercept # not var_num # reho
  
  simulation_y_data[[scale_name]][, 2] <- simulation_y %>% as.numeric() # must change to num, else format of y is matrix
  names(simulation_y_data[[scale_name]])[2] <- 'simulation_y'
  
  # test the R2
  {
    dd1 <- cbind(simulation_y, x_df)
    mm1 <- lm(simulation_y~., dd1)
    ss1 <- summary(mm1)
    sprintf('Simulation Model R2=%.4f\n', ss1$r.squared) %>% cat
  }
}

# new formula (name of y is simulation_y)
new_model_formula <- lapply(selected_var_list, function(sc){
  x <- paste(sc, collapse = '+')
  fyx <- sprintf('simulation_y~%s', x) %>% as.formula()
})

# save--------
{
  # save imp complete data (for raw data)
  if(is_imp_raw_data){
    data_save_path <- file.path(out_dir,sprintf('s1.7_ImpCompleteData_%s', basename(raw_data_path)))
    save(complete_data, file = data_save_path)
    sprintf('Imp data saved! %s\n', data_save_path) %>% cat
  }
  
  # save coef
  if (!is_load_coef){
    coef_save_path <- file.path(out_dir, 's1.7_SimulationCoef.RData')
    save(simu_coef_list, file = coef_save_path)
    sprintf('Coef saved! %s\n', coef_save_path) %>% cat
  }
  
  # save data with simulation y
  {
    simulation_y_save_path <- file.path(out_dir, 's1.7_SimulationOutcomeData.RData')
    save(simulation_y_data, file = simulation_y_save_path)
    sprintf('Simulation Y data saved! %s\n', simulation_y_save_path) %>% cat
  }
  
  # save new formula
  {
    new_formula_save_path <- file.path(out_dir, sprintf('newYFormula_%s', basename(formula_path)))
    save(new_model_formula, file = new_formula_save_path)
    sprintf('New Formula saved! %s\n', new_formula_save_path) %>% cat
  }
}


# test data
if(F){
  d1 <- simulation_y_data[[1]][, -1]
  m1=lm(simulation_y~., d1)
  t1 <- m1$residuals+m1$fitted.values
  f1 <- m1$fitted.values
  cor(t1, f1)
  plot(t1, f1)
  
}

