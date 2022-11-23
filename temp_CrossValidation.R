# set parameter
source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/cog_TrueMiss_NoRep0.8_boot100/ImpResult_Imp_Method__mice_pmm__simulation_boot_100_2021-03-02-21:14:51.RData'
formula_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_Cognative_T1surf_Combine_Data.Rdata/Formula__cor_step__.RData'

# def function
get_imp_cv_result <- function(data, folder_info, model_formula){
  # using model is lm()
  # data: 1col is y, other col is x
  
  # cross validation
  true_y <- vector()
  pred_y <- vector()
  for (i in unique(folder_info)){
    train_data <- data[folder_info != i, ]
    test_data <- data[folder_info == i, ]
    
    model <- lm(model_formula, train_data)
    
    true_y <- c(true_y, test_data[, 1])
    pred_y <- c(pred_y, predict(model, test_data))
  }
  
  return(list(true_y=true_y, pred_y=pred_y))
}

get_imp_cross_validation_compare <- function(imp_data, folder_info, model_formula, is_mice=F){
  if(is_mice){  # MI, data saved as mice format
    m <- imp_data$m
    res_mat <- matrix(nrow=m, ncol=2, dimnames = list(c(), c('cor', 'RMSE')))
    for (i in 1:m){
      complete_data <- complete(imp_data, i)
      res <- get_imp_cv_result(complete_data, folder_info, model_formula)
      res_cor <- cor(res$true_y, res$pred_y)
      res_rmse <- sqrt(mean((res$true_y - res$pred_y)^2))
      res_mat[i, ] <- c(res_cor, res_rmse)
    }
    res_vct <- colMeans(res_mat)
  }
  else{  # SI
    res <- get_cv_result_imp(imp_data, folder_info, model_formula)
    res_cor <- cor(res$true_y, res$pred_y)
    res_rmse <- sqrt(mean((res$true_y - res$pred_y)^2))
    res_vct <- c(cor=res_cor, RMSE=res_rmse)
  }
  
  return(res_vct)
}


# load data
data_load_name <- load(source_path)
eval(parse(text=sprintf("imp_res <- %s", data_load_name)))
formula_load_name <- load(formula_path)
eval(parse(text=sprintf("formula_save <- %s", formula_load_name)))

# is mice
is_mice <- F
if (class(imp_res[[1]][[1]]$imp) == 'mids'){is_mice <- T}

# run cv, get result
time_op <- Sys.time()
imp_cv_compare <- list()
for (scale_name in names(imp_res)){
  print(scale_name)
  print(Sys.time() - time_op)
  boot_number <- length(imp_res[[scale_name]])
  comp_data <- complete(imp_res[[scale_name]][[1]]$imp)
  # set folder info
  folder_info <- c(rep(c(1:10), floor(nrow(comp_data) / 10)), seq(0, nrow(comp_data) %% 10, 1)[-1])
  
  for (boot_i in 1:boot_number){
    sprintf('%s - %d\n', scale_name, boot_i) %>% cat
    
    imp_cv_compare[[scale_name]][[boot_i]] <- get_imp_cross_validation_compare(
      imp_res[[scale_name]][[boot_i]]$imp, folder_info, formula_save[[scale_name]], is_mice = is_mice)
  }
}
print(Sys.time() - time_op)

imp_cv_compare_mean <- sapply(imp_cv_compare, function(sc){rowMeans(as.data.frame(sc))})




