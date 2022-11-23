# s3 imp data by different method
source('D:\\software\\git\\liyifan_R_program\\R_program\\imputation_compare\\imp_compare_utils.R')
library(mice)
library(dplyr)
library(naniar)
# set parmeter
source_path <- 'I:\\imputation\\imp_compare\\s2_simulation\\psy_1.5_pattern.RData\\simu_boot_data_list.RData'
save_path <- 'I:\\imputation\\imp_compare\\s2_simulation\\psy_1.5_pattern.RData\\simu_noerror_boot_data_list.RData'
imp_method <- 'mean'
# load data
{
  load(source_path)
  # missing_data <- save_list$simulation_missing_data_boot_list
  
  s1 <- save_list
  s1$simulation_missing_data_boot_list$WellBeingIndex <- NULL
  s1$simulation_missing_data_boot_list$SatisficationWithLife <- NULL
  s1$simulation_missing_data_boot_list$MultidimensionalScaleofPerceivedSocialSupport <- NULL
  for (ename in error_name ){
    s1$simulation_missing_data_boot_list[[ename]] <- NULL
  }
  
  s2 <- save_list
  s2$simulation_missing_data_boot_list <- s2$simulation_missing_data_boot_list['EyeVision']
  
  missing_data <- s1$simulation_missing_data_boot_list
}

# imp data
imp_boot_list <- lapply(missing_data, function(x, method){
  lapply(x, function(x, method){
    imp_data_by_method(x$simulation_data,method=method)
  }, method)
}, imp_method)

imp_res <- list()
for (scale_name in names(missing_data)){
  print(scale_name)
  imp1 <- tryCatch({
    a=lapply(missing_data[[scale_name]], function(x, method){
      imp_data_by_method(x$simulation_data,method=method)
    }, imp_method)
  },
  error=function(e){
    sprintf('ERROR\n') %>% cat()
    print(e)
    return(-1)
  }
  )
  imp_res[[scale_name]] <- imp1
}

error_name <- c()
for(scale_name in names(imp_res)){
  if (is.numeric(imp_res[[scale_name]])){
    error_name <- c(error_name, scale_name)
  }
}

data <- missing_data$EyeVision[[1]]$simulation_data
imp_data_by_method <- function(data, method='mice_pmm'){
  if(method == 'mice_pmm'){
    imp <- mice(data, m=5, method = 'pmm')
  }
  else if(method=='mean'){
    imp <- mice(data, m=1, method = 'mean', printFlag = F)
  }
  else{
    sprintf('ERROR: Unsupport method: %s', method) %>% cat()
    return(1)
  }
  
  # return
  return(list(imp=imp, method=method))
}

#=========================imp summary==============================
# run night
{
  source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/data/simu_noerror_boot_data_list.RData'
  save_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/data/imp_pmm1.RData'
  imp_method <- 'mean'
  load(source_path)
  missing_data <- s1$simulation_missing_data_boot_list
  
  imp_res <- list()
  for (scale_name in names(missing_data)){
    print(scale_name)
    imp1 <- tryCatch({
      a=lapply(missing_data[[scale_name]], function(x, method){
        imp_data_by_method(x$simulation_data,method=method)
      }, imp_method)
    },
    error=function(e){
      sprintf('ERROR\n') %>% cat()
      print(e)
      return(-1)
    }
    )
    imp_res[[scale_name]] <- imp1
  }
  
  # aftet mice, NA in data
  l1 <- lapply(imp_res, function(x1){
    lapply(x1, function(x2){
      lapply(complete(x2$imp, 'all'), function(x3){
        length(which_na(x3))
      })
    })
  })
  unl1 <- lapply(l1, function(x){sum(unlist(x))})
  print(unl1)  # after mice, NA in data
  
  # using with
  fit_all <- lapply(imp_res, function(boot_data){
    # get formula
    var <- colnames(boot_data[[1]]$imp$data)
    fx <- paste(var[-length(var)], collapse = '+')
    fyx <- paste(var[length(var)], fx, sep='~') %>% formula()
    
    fit_bootdata <- lapply(boot_data, function(data, fyx){
      imp <- data$imp
      return(with(imp, lm(formula(format(fyx)))))
    }, fyx)
    return(fit_bootdata)
  })
  
  # pool
  pool_all <- lapply(fit_all, function(fit_boot){
    pool_boot <- lapply(fit_boot, function(fit){
      return(summary(pool(fit)))
    })
    return(pool_boot)
  })
  
  # mean boot, named 'pool_mean'
  {
    if(F){
      # some variable not in summary can't using '+'
      pool_mean <- lapply(pool_all, function(pool_boot){
        pool_boot_num <- lapply(pool_boot, function(pl){
          rownames(pl) <- pl[, 1]
          pl <- pl[, -1]
          return(pl)
        })
        # pool_boot_mean <- Reduce('+', pool_boot_num) / length(pool_boot_num)
      })
    }
    
    # using 1 element
    pool_mean <- lapply(pool_all, function(pool_boot){pool_boot[[1]]})
  }
  
  # save pool_mean
  imp_save_list <- list(imp_data=imp_res,
                        pool_mean=pool_mean,
                        method=imp_method)
  save(imp_save_list, file = save_path)
}


#===================complete data lm model=====================
# save path
complete_summary_save_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/data/complete_summary.RData'
# get data
no_error_name <- names(s1$simulation_missing_data_boot_list)
names(no_error_name) <- no_error_name
complete_data <- lapply(no_error_name, function(x, data){data[[x]]}, s1$complete_data_boot_list)

# build model
model_all <- lapply(complete_data, function(boot_data){
  # get formula
  var <- colnames(boot_data[[1]])
  fx <- paste(var[-length(var)], collapse = '+')
  fyx <- paste(var[length(var)], fx, sep='~') %>% formula()
  
  fit_bootdata <- lapply(boot_data, function(data, fyx){
    return(lm(fyx, data))
  }, fyx)
  return(fit_bootdata)
})

# pool
pool_all <- lapply(model_all, function(fit_boot){
  pool_boot <- lapply(fit_boot, function(fit){
    return(summary(fit))
  })
  return(pool_boot)
})

# mean boot
pool_mean <- lapply(pool_all, function(pool_boot){pool_boot[[1]]})

# save
complete_summary_list <- list(data=complete_data,
                              pool_mean=pool_mean)
save(complete_summary_list, file=complete_summary_save_path)
