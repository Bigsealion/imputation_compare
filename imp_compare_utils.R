# there are utils functions, which using in imputation compare
library(mice)
library(purrr)
library(naniar)
library(dplyr)
library(stringr)
library(VIM)
library(reshape2)
# =================== s1 data preprocess===================
cor_select <- function(data, cor_limit=0.9){
  # only input var, not contain y
  
  # if no var names, setting
  if(is.null(colnames(data))){
    colnames(data) <- paste('var', c(1:ncol(data)), sep = '')
  }
  
  # get cor
  cormat <- cor(data, use = 'complete.obs') %>% abs()
  diag(cormat) <- 0
  tri_cormat <- cormat
  tri_cormat[lower.tri(cormat, diag = T)] <- 0
  # pheatmap(cormat, cluster_rows = F, cluster_cols = F, display_numbers = T)
  
  # remove var with greater cor
  max_cor <- max(tri_cormat)
  remove_var_all <- c()
  while(max_cor >= cor_limit){
    if(max_cor < cor_limit){break}
    max_coord <- which(tri_cormat == max_cor, arr.ind = T)[1, ]
    
    sum_cor <- c(sum(cormat[, max_coord[1]]), sum(cormat[, max_coord[2]]))
    names(sum_cor) <- c(rownames(tri_cormat)[max_coord[1]], colnames(tri_cormat)[max_coord[2]])
    
    remove_var <- names(sum_cor)[which.max(sum_cor)]
    remove_var_all <- c(remove_var_all, remove_var)
    
    tri_cormat[remove_var, ] <- 0
    tri_cormat[, remove_var] <- 0
    cormat[, remove_var] <- 0
    cormat[remove_var, ] <- 0
    
    max_cor <- max(tri_cormat)
  }
  print(remove_var_all)
  return(remove_var_all)
}

# ================ s2 get missing pattern ==================
# bootstrap for matrix/datafarm
get_bootstrap_data <- function(data, sample_number=10, sample_size=1, is_replace=T){
  if ((sample_size <= 1) & (sample_size >0)){
    sample_size = round(nrow(data) * sample_size)}
  
  sample_row_list <- map(1:sample_number, function(x){
    sample(1:nrow(data), sample_size, replace = is_replace)}
  )
  
  sample_data_list <- lapply(sample_row_list, function(sample_row, data){
    data[sample_row, , drop = FALSE]}, data)
  
  return(sample_data_list)
}

# def function get pattern regression model, for simulation_missing_pattern
get_pattern_reg_model <- function(missing_pattern, missing_data){
  # get pattern label
  shadow_matrix <- !is.na.data.frame(missing_data)
  pattern_row <- apply(shadow_matrix, 1, function(x, pattern){all(x==pattern)}, missing_pattern)
  
  # get regression model
  reg_data <- cbind(pattern_row, 
                    missing_data[, names(missing_pattern)[missing_pattern==1], drop = FALSE])
  
  # f_xname <- colnames(reg_data)[!str_detect(colnames(reg_data), 'T1Surf')]
  # f_xname <- f_xname[-which(f_xname == 'pattern_row')] %>% paste(collapse = '+')
  # f <- formula(paste('pattern_row~', f_xname, sep = '')) 
  # 
  # model <- lm(f, reg_data)
  model <- lm(pattern_row~., reg_data)
  
  return(model)
}


# add simulation missing pattern in complete data
simulation_missing_pattern <- function(complete_data, missing_data, pattern_num=10, add_missing_case_rate=NULL,
                                       is_del_all_missing=T, is_print_info=F){
  # complete_data: data with no missing, which will be setting patten by simulation
  #                of missing data, must have same variables with missing_data
  # missing_data: data with missing, which will be used to get missing pattern
  # pattern_num: is the max number of patterns that we want to use, -1 means using all pattern
  # add_missing_case_rate: input number which 0<n<1. if is NULL, adding missing case rate by true rate
  #                   if is number, adding missing by this special percent 
  # is_del_all_missing: if True, not using pattern which all variable is missing
  # is_print_info: if False, not print any info to screen
  
  # del T1 and subjID in data
  {
    # rownames(complete_data)  <- complete_data[, 1]
    remove_name <- colnames(complete_data)[1:2]
    
    clog <- select(complete_data, all_of(remove_name))
    complete_data <- select(complete_data, -all_of(remove_name))
    
    # rownames(missing_data) <- missing_data[, 1]
    missing_data <- select(missing_data, -all_of(remove_name))
  }
  
  # get pattern, order by case number, pt means 'pattern'
  {
    pt <- md.pattern(missing_data, plot=F)
    pt <- pt[order(as.numeric(rownames(pt)), decreasing=T), ]
    pt <- pt[-dim(pt)[1], -dim(pt)[2], drop = FALSE]
    pt <- pt[, colnames(complete_data)]
    
    # Only the pattern with the first n 'pattern_num' samples is used
    {
      if (pattern_num < 0){
        if(is_print_info){
          sprintf('using all pattern, number: %d\n', dim(pt)[1]) %>% cat()
        }
      }
      else if(dim(pt)[1] > pattern_num){
        pt <- pt[1: pattern_num, , drop = FALSE]
        if(is_print_info){
          sprintf('Using %d missing pattern\n', pattern_num) %>% cat()
        }
      }
      else{
        if(is_print_info){
          sprintf('only have %d missing pattern...\n', dim(pt)[1]) %>% cat()
        }
      }
      
    }
    
    # del pattern that all variable is missing
    if (is_del_all_missing){
      pt <- pt[apply(pt, 1, function(x){any(as.logical(x))}), , drop = FALSE]
    }
    # del pattern that no missing
    pt <- pt[apply(pt, 1, function(x){!all(as.logical(x))}), , drop = FALSE]
    
    if (is_empty(pt)){
      if(is_print_info){
        print('There is No missing patten fitted criteria!')
      }
      return(1)
    }
    
    # get add pattern pattern number
    add_pattern_num <- round(as.numeric(rownames(pt)) / nrow(missing_data) * nrow(complete_data))
    if (is.numeric(add_missing_case_rate)){
      change_add_number_rate <- nrow(complete_data) * add_missing_case_rate / sum(add_pattern_num)
      add_pattern_num <- round(add_pattern_num * change_add_number_rate)
    }
    
    # rename patterns as [pt1, pt2...]
    rownames(pt) <- paste('pat', c(1:dim(pt)[1]), sep='')
    names(add_pattern_num) <- rownames(pt)
  }
  
  # constructure regression model of each pattern
  {
    # get each pattern model
    pattern_reg_model_list <- apply(pt, 1, get_pattern_reg_model, missing_data)
    
    # get each pattern prob
    pattern_prob <- sapply(pattern_reg_model_list, function(x, complete_data){
      predict(x, newdata=complete_data)
    },complete_data)
  }
  
  # apply missing pattern to complete data
  {
    # get pattern adding order, it means we will add missing pattern in a sequential
    # e.g.: pat1, pat2, pat3, pat1, pat2, pat3, pat1, pat2, (if pat3 is used up)pat1...
    # in each adding, the most prob of this pattern will be choosed, and deleted in adding list
    # this make sure that two patterns not be set to a same case
    pattern_add_order <- add_pattern_num - c(add_pattern_num[-1], 0)
    pattern_add <- c()
    for(i in length(pattern_add_order): 1){
      pattern_add <- c(pattern_add, rep(names(pattern_add_order)[1: i], pattern_add_order[i]))
    }
    
    # reorder prob, try to speed up running a little
    pattern_prob <- pattern_prob[order(pattern_prob[, 1], decreasing = T), ,drop = FALSE]
    data_order <- rownames(pattern_prob)  # is str, not number  
    
    # add missing pattern
    for (pat_name in pattern_add){
      max_index <- which.max(pattern_prob[, pat_name])
      # data_order[max_index] is a str, is row index, not row number
      complete_data[data_order[max_index], which(pt[pat_name,]==0)] <- NA
      
      # del used case in prob data
      pattern_prob <- pattern_prob[-max_index, ,drop = FALSE]
      data_order <- data_order[-max_index]
    }
  }
  
  # cbind T1data and subID
  {
    complete_data <- cbind(clog, complete_data)
  }
  
  # return data, this 'complete_data' had be adding missing
  return_list <- list(simulation_data=complete_data,
                      missing_pattern=pt,
                      simulation_number=add_pattern_num)
  return(return_list)
}

# [deprecated] add simulation missing pattern in complete data
simulation_missing_pattern_back_up <- function(complete_data, missing_data, pattern_num=10,
                                       is_del_all_missing=T, is_print_info=F){
  # complete_data: data with no missing, which will be setting patten by simulation
  #                of missing data, must have same variables with missing_data
  # missing_data: data with missing, which will be used to get missing pattern
  # pattern_num: is the max number of patterns that we want to use
  # is_del_all_missing: if True, not using pattern which all variable is missing
  # is_print_info: if False, not print any info to screen
  
  # del T1 and subjID in data
  {
    # rownames(complete_data)  <- complete_data[, 1]
    clog <- select(complete_data, contains(c('RefID', 'T1Surf')))
    complete_data <- select(complete_data, -contains(c('T1Surf', 'RefID')))
    
    # rownames(missing_data) <- missing_data[, 1]
    missing_data <- select(missing_data, -contains(c('T1Surf', 'RefID')))
  }
  
  # get pattern, order by case number, pt means 'pattern'
  {
    pt <- md.pattern(missing_data, plot=F)
    pt <- pt[order(as.numeric(rownames(pt)), decreasing=T), ]
    pt <- pt[-dim(pt)[1], -dim(pt)[2], drop = FALSE]
    pt <- pt[, colnames(complete_data)]
    
    # Only the pattern with the first n 'pattern_num' samples is used
    if (dim(pt)[1] > pattern_num){
      pt <- pt[1: pattern_num, , drop = FALSE]
      if(is_print_info){
        sprintf('Using %d missing pattern\n', pattern_num) %>% cat()
      }
      
    }else{
      if(is_print_info){
        sprintf('only have %d missing pattern...\n', dim(pt)[1]) %>% cat()
      }
    }
    
    # del pattern that all variable is missing
    if (is_del_all_missing){
      pt <- pt[apply(pt, 1, function(x){any(as.logical(x))}), , drop = FALSE]
    }
    # del pattern that no missing
    pt <- pt[apply(pt, 1, function(x){!all(as.logical(x))}), , drop = FALSE]
    
    if (is_empty(pt)){
      if(is_print_info){
        print('There is No missing patten fitted criteria!')
      }
      return(1)
    }
    
    # get add pattern pattern number
    add_pattern_num <- ceiling(as.numeric(rownames(pt)) / dim(missing_data)[1] * dim(complete_data)[1])
    
    # rename patterns as [pt1, pt2...]
    rownames(pt) <- paste('pat', c(1:dim(pt)[1]), sep='')
    names(add_pattern_num) <- rownames(pt)
  }
  
  # constructure regression model of each pattern
  {
    # get each pattern model
    pattern_reg_model_list <- apply(pt, 1, get_pattern_reg_model, missing_data)
    
    # get each pattern prob
    pattern_prob <- sapply(pattern_reg_model_list, function(x, complete_data){
      predict(x, newdata=complete_data)
    },complete_data)
  }
  
  # apply missing pattern to complete data
  {
    # get pattern adding order, it means we will add missing pattern in a sequential
    # e.g.: pat1, pat2, pat3, pat1, pat2, pat3, pat1, pat2, (if pat3 is used up)pat1...
    # in each adding, the most prob of this pattern will be choosed, and deleted in adding list
    # this make sure that two patterns not be set to a same case
    pattern_add_order <- add_pattern_num - c(add_pattern_num[-1], 0)
    pattern_add <- c()
    for(i in length(pattern_add_order): 1){
      pattern_add <- c(pattern_add, rep(names(pattern_add_order)[1: i], pattern_add_order[i]))
    }
    
    # reorder prob, try to speed up running a little
    pattern_prob <- pattern_prob[order(pattern_prob[, 1], decreasing = T), ,drop = FALSE]
    data_order <- rownames(pattern_prob)  # is str, not number  
    
    # add missing pattern
    for (pat_name in pattern_add){
      max_index <- which.max(pattern_prob[, pat_name])
      # data_order[max_index] is a str, is row index, not row number
      complete_data[data_order[max_index], which(pt[pat_name,]==0)] <- NA
      
      # del used case in prob data
      pattern_prob <- pattern_prob[-max_index, ,drop = FALSE]
      data_order <- data_order[-max_index]
    }
  }
  
  # cind T1data and subID
  {
    complete_data <- cbind(clog, complete_data)
  }
  
  # return data, this 'complete_data' had be adding missing
  return_list <- list(simulation_data=complete_data,
                      missing_pattern=pt,
                      simulation_number=add_pattern_num)
  return(return_list)
}

# Little test
mcar_test_copy <- function(data) {
  
  # test_if_dataframe(data)
  
  # norm::prelim.norm needs to work with a data.matrix
  data <- data.matrix(data)
  
  # Number of variables in data
  n_var <- ncol(data)
  
  # Number of rows
  n <- nrow(data)
  var_names <- colnames(data)
  
  # Calculate pattern of missingness for each row
  r <- 1 * is.na(data)
  mdp <- (r %*% (2^((1:n_var - 1)))) + 1
  
  # Add pattern as column to original data
  x_miss_pattern <- data.frame(cbind(data, mdp))
  colnames(x_miss_pattern) <- c(var_names, "miss_pattern")
  
  # Number of unique missing data patterns
  n_miss_pattern <- length(unique(x_miss_pattern$miss_pattern))
  
  # Recode miss_pattern variable to go from 1 through n_miss_pattern
  x_miss_pattern <- x_miss_pattern %>%
    dplyr::mutate(miss_pattern = dplyr::dense_rank(.data$miss_pattern))
  
  # Maximum likelihood estimation from {norm}
  # Adapted from Eric Stemmler
  # https://stats-bayes.com/post/2020/08/14/r-function-for-little-s-test-for-data-missing-completely-at-random/
  s <- norm::prelim.norm(data)
  ll <- norm::em.norm(s, showits = FALSE)
  fit <- norm::getparam.norm(s = s, theta = ll)
  grand_mean <- fit$mu
  grand_cov <- fit$sigma
  colnames(grand_cov) <- rownames(grand_cov) <- colnames(data)
  
  little_calculations <- x_miss_pattern %>%
    # For each of the types of missing patterns...
    dplyr::group_by(.data$miss_pattern) %>%
    tidyr::nest() %>%
    # kj terms for degrees of freedom
    dplyr::mutate(
      kj = purrr::map_dbl(.data$data,
                          ~colSums(as.matrix(1 * !is.na(colSums(.x)))))
    ) %>%
    # Calculate column averages
    dplyr::mutate(
      mu = purrr::map(.data$data, ~colMeans(.x) - grand_mean),
      mu = purrr::map(.data$mu, ~.x[!is.na(.x)])
    ) %>%
    # Binary 0/1 indicating if column should be kept
    dplyr::mutate(
      keep = purrr::map(.data$data, ~1 * !is.na(colSums(.x))),
      keep = purrr::map(.data$keep, ~.x[which(.x[1:n_var] != 0)])
    ) %>%
    # Drop rows and columns from global covariance matrix so that the matrix
    # only contains rows and columns that exist in current missing pattern
    dplyr::mutate(
      sigma = purrr::map(.data$keep,
                         ~grand_cov[which(rownames(grand_cov) %in% names(.x)),
                                    which(colnames(grand_cov) %in% names(.x))])
    ) %>%
    # Finally calculate Little's statistic using the number of rows in missing
    # pattern, average, and covariance
    dplyr::mutate(
      d2 = purrr::pmap_dbl(
        list(.data$data, .data$mu, .data$sigma, .data$kj),
        ~ifelse(..4 == 0,
                0,  # If the pattern is all NA, use 0
                nrow(..1) * (t(..2) %*% solve(..3) %*% ..2))
      )) %>%
    dplyr::ungroup()
  
  # Main calculations
  d2 <- sum(little_calculations$d2)  # Little's d2
  df <- sum(little_calculations$kj) - n_var  # Degrees of freedom
  p_value <- 1 - stats::pchisq(d2, df)  # p-value
  
  # Return everything as a glance-like tibble
  tibble::tibble(statistic = d2, df = df, p.value = p_value,
                 missing.patterns = n_miss_pattern)
}
# ======================= s3 imp data ===========================
imp_data_by_method <- function(data, method='mice_pmm'){
  if(method == 'mice_pmm'){
    # adjust cl.type as 'PSOCK' if in windows
    imp <- mice(data, m=5, method='pmm', maxit = 10, 
                printFlag = F, seed=123456)
  }
  else if(method=='mice_def'){
    imp <- mice(data, m=5, maxit = 10, seed=123456)
  }
  else if(method=='mice_mean'){
    imp <- mice(data, m=1, maxit = 1, method = 'mean', printFlag = F,
                n.core = 8, cl.type = 'FORK')
  }
  else if(method=='mice_norm_pred'){
    imp <- mice(data, m=1, maxit = 20, method = 'norm.predict', printFlag = F)
  }
  else if(method=='mice_rf'){
    imp <- mice(data, m=5, maxit = 10, method = 'rf', printFlag = F,
                n.core = 8, cl.type = 'FORK')
  }
  else if (method == 'CCA'){
    imp <- data[complete.cases(data), ]
  }
  else if (method == 'vim_em'){
    imp <- irmi(data)
  }
  else if (method == 'vim_em_robust'){
    imp <- irmi(data, robust = T)
  }
  else{
    sprintf('ERROR: Unsupport method: %s\n', method) %>% cat()
    return(-1)
  }
  
  # return
  # sprintf('        data imp! %s\n', Sys.time()) %>% cat()
  return(list(imp=imp, method=method))
}

# batch run imp_data_by_method; set a try catch
imp_data_batch <- function(missing_data, method){
  # set method to data
  time_op <- Sys.time()
  
  # get imp result
  imp_res <- list()
  scale_loop_i <- 0
  psy_num <- length(missing_data)
  sprintf('Running imputaion, method: %s...\n', method) %>% cat()
  for (scale_name in names(missing_data)){
    scale_time_op <- Sys.time()
    scale_loop_i = scale_loop_i + 1
    
    # run in each boot
    imp_scale_result <- list()
    missing_scale <- missing_data[[scale_name]]
    boot_number <- length(missing_scale)
    for (boot_i in 1: boot_number){
      boot_time_op <- Sys.time()
      imp_scale_result[[boot_i]] <- tryCatch({
        imp_data_by_method(missing_scale[[boot_i]]$simulation_data, method=method)
      },
      error=function(e){
        sprintf('    ERROR: ') %>% cat()
        print(e)
        return(-1)
      }
      )
      
      scale_time <- Sys.time() - scale_time_op
      boot_time <- Sys.time() - boot_time_op
      total_time <- Sys.time() - time_op
      sprintf('    Scale: %d/%d(%s %.4f %s), Boot: %d/%d(%.4f %s), Total: %.4f %s, Now: %s\n', 
              scale_loop_i, psy_num, scale_name,
              scale_time, attr(scale_time, 'units'),
              boot_i, boot_number,
              boot_time, attr(boot_time, 'units'),
              total_time, attr(total_time, 'units'),
              Sys.time()) %>% cat()
    }
    
    imp_res[[scale_name]] <- imp_scale_result
    # sprintf('    used: %.4f %s, total impute time: %.4f %s\n', 
    #         Sys.time() - psy_time_op, attr(Sys.time() - psy_time_op, 'units'),
    #         Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
  }
  
  return(imp_res)
}

# ======================= s4 compare method ===========================
# s4.1 get imp summary
get_imp_summary_list_data <- function(data, method, formuls=NULL){
  # method in ['mice_pmm', 'mice_mean', 'CCA', 'complete', 'vim_em'] 
  # get summary coefficients and CI
  if (method %in% c('mice_pmm', 'mice_mean', 'mice_norm_pred', 'mice_def')){
    # get model using with
    # get formula
    var <- colnames(data$data)
    fx <- paste(var[-1], collapse = '+')
    fyx <- paste(var[1], fx, sep='~') %>% formula()
    if (!is.null(formuls)){fyx <- formuls}
    
    # fit by with
    fit <- with(data, lm(formula(format(fyx))))
    
    # get summary, by pool
    # residuals is same in any m
    res_value <- sapply(fit$analyses, function(m){m$residuals}) %>% apply(1, mean)
    fit_value <- sapply(fit$analyses, function(m){m$fitted.values}) %>% apply(1, mean)
    pre_res <- res_value / (res_value + fit_value)
    
    mean_res <- res_value %>% mean()
    mean_pre_res <- sapply(fit$analyses, function(m){m$residuals / (m$residuals + m$fitted.values)}) %>% mean()
    sd_res <- sapply(fit$analyses, function(m){sd(m$residuals)}) %>% mean()
    cv_res <- sapply(fit$analyses, function(m){sd(m$residuals) / mean(m$residuals)}) %>% mean()
    stat_res <- c(mean_res=mean_res, mean_pre_res=mean_pre_res, sd_res=sd_res, cv_res=cv_res)
    
    res <- list(res_value=res_value, fit_value=fit_value, pre_res=pre_res,
                stat_res=stat_res)
    
    # summary is different in m=1 or others
    if (length(fit$analyses) == 1){
      summary_list <- list(summary=summary(pool(fit))[['coefficients']], conf=confint(pool(fit)), res=res)
    }
    else{
      all_summary <- summary(pool(fit), 'all', conf.int = T)
      conf <- all_summary[, c("2.5 %", "97.5 %")] %>% as.matrix()
      rownames(conf) <- all_summary[, 'term']
      
      summary_out <- all_summary[, -1]
      rownames(summary_out) <- all_summary[, 'term']
      colnames(summary_out)[which(colnames(summary_out)=='estimate')] <- 'Estimate'
      
      # return
      summary_list <- list(summary=summary_out, conf=conf, res=res)
    }
    
    # check is any aliased in model. if True, this scale is have aliased
    # deprecated 
    imp_summary_info <- F
  }
  else if (method %in% c('CCA', 'complete', 'vim_em')){  # complete data analysis, complete is true data
    # get formula
    var <- colnames(data)
    fx <- paste(var[-1], collapse = '+')
    fyx <- paste(var[1], fx, sep='~') %>% formula()
    if (!is.null(formuls)){fyx <- formuls}
    
    # get model
    fit <- lm(fyx, data)
    
    # residuals
    mean_res <- mean(fit$residuals)
    mean_pre_res <- mean(fit$residuals / (fit$residuals + fit$fitted.values))
    sd_res <- sd(fit$residuals)
    cv_res <- sd_res / mean_res
    pre_res=fit$residuals / (fit$residuals + fit$fitted.values)
    
    stat_res <- c(mean_res=mean_res, mean_pre_res=mean_pre_res, sd_res=sd_res, cv_res=cv_res)
    res <- list(res_value=fit$residuals, fit_value=fit$fitted.values, pre_res=pre_res,
                stat_res=stat_res)
    
    # get summary and CI
    summary_list <- list(summary=summary(fit)[['coefficients']], conf=confint(fit), res=res)
    
    # check is any aliased in model. if True, this scale is have aliased
    imp_summary_info <- summary(fit)$aliased %>% any()
  }
  else{
    sprintf('ERROR: Unsupport method: %s\n', method) %>% cat()
    return(1)
  }
  
  # return
  return_list <- list(summary=summary_list, info=imp_summary_info)
  return(return_list)
} 

# s4.1 get imp data summary batch
get_impdata_summary <- function(imp_data_path, formula_path, out_dir, special_mode=1,
                                remove_scale=NULL){
  # special model in ['complete', 'true', '']
  
  # create out dir
  if (!file.exists(out_dir)){
    dir.create(out_dir,recursive = T)
    sprintf('Create out dir! %s\n', out_dir) %>% cat()
  }
  
  # load data and formula====================================
  {
    load_imp_name <- load(imp_data_path)
    eval(parse(text=sprintf("imp_res <- %s", load_imp_name)))
    
    if (!is.null(remove_scale)){
      for (remove_sc in remove_scale){
        imp_res[[remove_sc]] <- NULL
        }
    }
    
    if (special_mode == 'true'){
      # remove ID
      imp_res <- lapply(imp_res$total_complete_data_list, function(scale){list(list(imp=scale[, -1], method='complete'))})
    }
    else if (special_mode == 'complete'){
      # remove ID
      imp_res <- lapply(imp_res$complete_data_boot_list, function(scale){
        lapply(scale, function(boot){
          list(imp=boot[, -1], method='complete')
        })
      })
    } 
    sprintf('special mode: %s\n', special_mode) %>% cat()
    
    load_formula_name <- load(formula_path)
    eval(parse(text=sprintf("model_formula_list <- %s", load_formula_name)))
    sprintf('Data loaded! Now: %s, Total time: %.4f %s\n', 
            Sys.time(), Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
  }
  
  
  # get summary=========================================
  {
    # check names of data and formula
    if (all(names(imp_res) != names(model_formula_list))){
      print('ERROR: Names between data and formula not matched!\n') %>% cat()
    }
    
    # run
    summary_res <- list()
    for (scale_name in names(imp_res)){
      time_scale_op <- Sys.time()
      sprintf('%s...', scale_name) %>% cat()
      
      imp_error_log <- sapply(imp_res[[scale_name]], is.numeric)
      # if (!all(imp_error_log)){
      if (!any(imp_error_log)){  # remove scale which can't be imputed
        # if (any(imp_error_log)){
        #   imp_scale_res <- list()
        #   boot_ok_i <- 1
        #   for (i in 1:length(imp_res[[scale_name]])){
        #     if (!is.numeric(imp_res[[scale_name]][[i]])){
        #       imp_scale_res[[boot_ok_i]] <- imp_res[[scale_name]][[i]]
        #       boot_ok_i = boot_ok_i + 1
        #     }
        #   }
        #   sprintf('Raw boot number is %d, only using %d\n', 
        #           length(imp_res[[scale_name]]), boot_ok_i) %>% cat
        # }
        # else{
        #   imp_scale_res <- imp_res[[scale_name]]
        # }
        
        tmp_summary <- lapply(imp_res[[scale_name]], function(boot, formulas){
          get_imp_summary_list_data(boot$imp, boot$method, formulas)
        }, model_formula_list[[scale_name]])
        
        summary_res$summary[[scale_name]] <- lapply(tmp_summary, function(boot){boot$summary})
        summary_res$info[[scale_name]] <- sapply(tmp_summary, function(boot){boot$info}) %>% any
        
        sprintf('\tComplete! Now: %s, Scale time used: %.4f %s, Total time: %.4f %s\n', 
                Sys.time(), 
                Sys.time() - time_scale_op, attr(Sys.time() - time_scale_op, 'units'),
                Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
      }
      else{
        sprintf('\n    ERROR!\n') %>% cat()
      }
      
    }
    
    # save 
    if (special_mode == 'true'){
      save_name <- sprintf('Summary_TRUE_%s_%s.RData', strsplit(basename(imp_data_path), '\\.')[[1]][1],
                           strsplit(basename(formula_path), '\\.')[[1]][1])
    }
    else if (special_mode == 'complete'){
      save_name <- sprintf('Summary_Complete_%s_%s.RData', strsplit(basename(imp_data_path), '\\.')[[1]][1],
                           strsplit(basename(formula_path), '\\.')[[1]][1])
    }
    else{
      save_name <- sprintf('Summary_%s_%s.RData', strsplit(basename(imp_data_path), '\\.')[[1]][1],
                           strsplit(basename(formula_path), '\\.')[[1]][1])
    }
    
    save_path <- file.path(out_dir, save_name)
    save(summary_res, file=save_path)
    sprintf('summary saved:%s\nNow: %s, Scale time used: %.4f %s, Total time: %.4f %s\n', 
            save_path, Sys.time(), 
            Sys.time() - time_scale_op, attr(Sys.time() - time_scale_op, 'units'),
            Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
  }
  sprintf('==============================================\n') %>% cat
}

# s4.1 get mean mice imputed result
get_mean_mice_result <- function(imp){
  ll <- list()
  for (i in 1:imp$m){
    ll[[i]] <- complete(imp, i)
  }
  res <- Reduce(function(x,y){x+y}, ll) / imp$m
  return(res)
}

# s4.1 get imputed value
get_imputed_value <- function(imputed_data, miss_coord_list, is_complete=F){
  # check is imp
  if (is_complete == F){
    is_mice <- class(imputed_data[[1]][[1]]$imp) == 'mids'
    
    imputed_data <- lapply(imputed_data, function(sc){
      lapply(sc, function(boot){
        if(is_mice){
          get_mean_mice_result(boot$imp)
        }
        else(boot$imp)
      })
    })
  }
  
  # run
  true_value_list <- list()
  for (scale_name in names(imputed_data)){
    complete_scale <- imputed_data[[scale_name]]
    var_name <- complete_scale[[1]] %>% names()
    for (boot_i in 1:length(complete_scale)){
      miss_coord <- miss_coord_list[[scale_name]][[boot_i]]
      if(!is_complete){
        miss_coord[,2] <- miss_coord[,2] - 1
      }
      boot_list <- list()
      for (var_col_i in unique(miss_coord[,2])){
        var_miss_coord <- miss_coord[which(miss_coord[,2] == var_col_i), 1]
        boot_list[[var_name[var_col_i]]] <- 
          complete_scale[[boot_i]][var_miss_coord, var_col_i]
      }
      true_value_list[[scale_name]][[boot_i]] <- boot_list
    }
  }
  
  return(true_value_list)
}

# s4.1 using cross validation get compare result----------
# get cv result by a complate data
get_imp_cv_result <- function(data, folder_info, model_formula){
  # using model is lm()
  # data: 1 col is y, other col is x
  
  # cross validation
  true_y <- data[, 1]
  pred_y <- vector(length = nrow(data))

  for (i in unique(folder_info)){
    test_index <- (folder_info == i)  # get test index
    model <- lm(model_formula, data[!test_index, ])  # train model
    pred_y[test_index] <- predict(model, data[test_index, ])  # predict
  }
  
  return(list(true_y=true_y, pred_y=pred_y))
}

# get cv result by imp data
get_imp_cross_validation_compare <- function(imp_data, folder_info, model_formula, is_mice=F){
  if(is_mice){  # MI, data saved as mice format
    m <- imp_data$m
    res_list <- list()
    for (i in 1:m){
      complete_data <- complete(imp_data, i)
      res_list[[i]] <- get_imp_cv_result(complete_data, folder_info, model_formula)[["pred_y"]]
    }
    res <- list(
      true_y = complete_data[, 1],
      pred_y = Reduce(function(y1, y2){y1 + y2}, res_list) / m
    )  # average predicted value of MI

    res_cor <- cor(res$true_y, res$pred_y)
    res_rmse <- sqrt(mean((res$true_y - res$pred_y)^2)) # Reserved for code compatibility, DO NOT use
    res_mae <- mean(abs(res$true_y - res$pred_y)) # Use MAE instead of RMSE to get correct results

    res_vct <- c(cor=res_cor, RMSE=res_rmse, MAE=res_mae)
  }
  else{  # SI
    res <- get_imp_cv_result(imp_data, folder_info, model_formula)
    res_cor <- cor(res$true_y, res$pred_y)
    res_rmse <- sqrt(mean((res$true_y - res$pred_y)^2)) # Reserved for code compatibility, do not use
    res_mae <- mean(abs(res$true_y - res$pred_y)) # Use MAE instead of RMSE to get correct results
    res_vct <- c(cor=res_cor, RMSE=res_rmse, MAE=res_mae)
  }
  
  # output indicators and CV true&predict values
  return(list(ind = res_vct, res = res))  
}

# batch
get_imp_cross_validation_compare_batch <- function(source_path, formula_path, is_complete_data=F, remove_scale=NULL){
  # load data
  data_load_name <- load(source_path)
  eval(parse(text=sprintf("imp_res <- %s", data_load_name)))
  formula_load_name <- load(formula_path)
  eval(parse(text=sprintf("formula_save <- %s", formula_load_name)))
  
  # complete data
  if(is_complete_data){
    imp_res <- lapply(imp_res$complete_data_boot_list, function(sc){
      lapply(sc, function(boot){
        list(imp=boot[, -1])  # remove ID col
      })
    })
  }
  
  # remove_scale
  if (!is.null(remove_scale)){
    for (remove_sc in remove_scale){
      imp_res[[remove_sc]] <- NULL
    }
  }
  
  # is mice
  is_mice <- (class(imp_res[[1]][[1]]$imp) == 'mids')
  
  # run cv, get result
  time_op <- Sys.time()
  imp_cv_compare <- list()
  imp_cv_value <- list()
  for (scale_name in names(imp_res)){
    print(scale_name)
    print(Sys.time() - time_op)
    boot_number <- length(imp_res[[scale_name]])
    comp_data <- complete(imp_res[[scale_name]][[1]]$imp)
    # set folder info
    folder_info <- c(rep(c(1:10), floor(nrow(comp_data) / 10)), seq(0, nrow(comp_data) %% 10, 1)[-1])
    sprintf('%s: ', scale_name) %>% cat
    for (boot_i in 1:boot_number){
      sprintf('%d ', boot_i) %>% cat
      
       cv_out <- get_imp_cross_validation_compare(
        imp_res[[scale_name]][[boot_i]]$imp, folder_info, formula_save[[scale_name]], is_mice = is_mice)

      imp_cv_compare[[scale_name]][[boot_i]] <- cv_out$ind
      imp_cv_value[[scale_name]][[boot_i]] <- cv_out$res
    }
    sprintf('\nNow: %s\n', Sys.time()) %>% cat
  }
  print(Sys.time() - time_op)
  
  # indicators and CV true&predict values, adding in 2022.12.27 
  return(list(ind = imp_cv_compare, res = imp_cv_value))
}


# s4.4 stack, load summary to same name ------------------------
get_summary_file <- function(summary_file_path){
  summary_name <- load(summary_file_path)
  eval(parse(text=sprintf("summary_list <- %s", summary_name)))
  return(summary_list)
}

# s4.4 stack in dir
stack_summary_in_dir <- function(source_dir){
  pattern1 <- 'Imp_Method__.*?__'
  pattern2 <- 'Complete'
  
  # load data
  data_name <- dir(source_dir)
  
  # seg group by method
  summary_group_list <- list()
  for (file_name in dir(source_dir)){
    if (str_detect(file_name, pattern1)){
      group_name <- str_match(file_name, pattern1)[1]
    }
    else if (str_detect(file_name, pattern2)){
      group_name <- str_match(file_name, pattern2)[1]
    }
    else{
      group_name <- NULL
      sprintf('%s not matched!\n', file_name) %>% cat()
    }
    
    if (length(group_name)){
      if (group_name %in% names(summary_group_list)){
        summary_group_list[[group_name]] <- c(summary_group_list[[group_name]], file.path(source_dir, file_name))
      }
      else{
        summary_group_list[[group_name]] <- file.path(source_dir, file_name)
      }
    }
  }
  
  # stack summary
  summary_filedata_stack_list <- lapply(summary_group_list, function(method_group){
    lapply(method_group, function(summary_path){
      sprintf('data load! %s\n', Sys.time()) %>% cat()
      get_summary_file(summary_path)
    })
  })
  sprintf('all summary loaded!\n') %>% cat()
  
  summary_stack_list <- lapply(summary_filedata_stack_list, function(method_group){
    sprintf('stack one, %s\n', Sys.time()) %>% cat()
    reduce(method_group, function(data1, data2){
      data <- list()
      data$summary <- mapply(function(s1, s2){c(s1, s2)}, data1$summary, data2$summary,SIMPLIFY = F)
      data$info <- mapply(function(i1, i2){i1 & i2}, data1$info, data2$info)
      return(data)
    })
  })
  
  # save stack list
  save_path <- file.path(source_dir, 'Summary_Stack.RData')
  save(summary_stack_list, file = save_path)
  sprintf('Stack summary saved: %s', save_path) %>% cat()
  
}

# s4.5 compare imputed data model and complete data model -----------------------
# simu_summary_list$summary$PittsburghSleepQualityIndex[[1]] -> imp_summary # for test
# true_summary_list$summary$PittsburghSleepQualityIndex[[1]] -> true_summary # for test
get_compare_result <- function(imp_summary, true_summary){
  coef_i <- imp_summary$summary
  coef_t <- true_summary$summary
  
  conf_i <- imp_summary$conf
  conf_t <- true_summary$conf
  
  res_i <- imp_summary$res$stat_res
  res_t <- true_summary$res$stat_res
  # if (any(is.na(res_i))){print(res_i)}
  
  var_i <- rownames(coef_i)
  var_t <- rownames(coef_t)
  
  if (identical(sort(var_i), sort(var_t))){
    raw_bias <- coef_i[, 'Estimate'] - coef_t[, 'Estimate']
    percent_bias <- 100 * abs(raw_bias / coef_t[, 'Estimate'])
    coverage_rate <- apply(cbind(conf_i, coef_t[, 'Estimate']), 1, function(ci_coef){
      (ci_coef[3] > ci_coef['2.5 %']) & (ci_coef[3] < ci_coef['97.5 %'])})
    average_width <- apply(conf_i, 1, function(ci){ci[2] - ci[1]})
    
    average_width_t <- apply(conf_t, 1, function(ci){ci[2] - ci[1]})
    
    AW_per_bias <- (average_width - average_width_t) / average_width_t
    
    rmse <- sqrt(raw_bias^2)
    res_out <- res_i - res_t

  }
  else{print('var is different in imp model and true model!')}
  
  # return
  return_list <- list(raw_bias=raw_bias, percent_bias=percent_bias,
                      coverage_rate=coverage_rate, average_width=average_width,
                      AW_per_bias=AW_per_bias,
                      rmse=rmse,
                      res_out=res_out)
  return(return_list)
} 

# compare simulation and true
# get name which no error and intersect of simu and true
get_compare_result_batch <- function(simu_summary_list, true_summary_list){
  # check name
  check_simu_PsyName <- names(simu_summary_list$info)[unlist(simu_summary_list$info)==F]
  check_true_PsyName <- names(true_summary_list$info)[unlist(true_summary_list$info)==F]
  inter_PsyName <- intersect(check_simu_PsyName, check_true_PsyName)
  sprintf('Running Psytool Name:\n') %>% cat()
  sprintf('    %s\n', inter_PsyName) %>% cat()
  
  simu_summary_list <- lapply(simu_summary_list, function(x, name){x[name]}, inter_PsyName)
  true_summary_list <- lapply(true_summary_list, function(x, name){x[name]}, inter_PsyName)
  
  # compare
  compare_result <- map2(simu_summary_list$summary, true_summary_list$summary,
                         function(simu_summary_boot, true_summary){
                           lapply(simu_summary_boot, get_compare_result, true_summary[[1]])
                         })
  
  compare_result_mean <- lapply(compare_result, function(psy){
    add_psy <- reduce(psy, function(boot1, boot2){mapply('+', boot1, boot2, SIMPLIFY = F)})
    add_psy <- lapply(add_psy, function(indicator, boot_num){indicator / boot_num}, length(psy))
  })
  
  return(list(compare_result=compare_result, compare_result_mean=compare_result_mean))
}


# compare simulation and complete sample results (not with true data)
# "Complete" must in names of simu_summary_list
get_compare_result_by_complete_batch <- function(simu_summary_list, ref_summary_list){
  # check name
  check_simu_PsyName <- names(simu_summary_list$info)[unlist(simu_summary_list$info)==F]
  check_ref_PsyName <- names(ref_summary_list$info)[unlist(ref_summary_list$info)==F]
  inter_PsyName <- intersect(check_simu_PsyName, check_ref_PsyName)
  sprintf('Running Psytool Name:\n') %>% cat()
  sprintf('    %s\n', inter_PsyName) %>% cat()
  
  simu_summary_list <- lapply(simu_summary_list, function(x, name){x[name]}, inter_PsyName)
  ref_summary_list <- lapply(ref_summary_list, function(x, name){x[name]}, inter_PsyName)
  
  # compare
  compare_result <- map2(simu_summary_list$summary, ref_summary_list$summary,
                         function(simu_summary_boot, ref_summary_boot){
                           map2(simu_summary_boot, ref_summary_boot, get_compare_result)
                         })
  
  compare_result_mean <- lapply(compare_result, function(psy){
    add_psy <- reduce(psy, function(boot1, boot2){mapply('+', boot1, boot2, SIMPLIFY = F)})
    add_psy <- lapply(add_psy, function(indicator, boot_num){indicator / boot_num}, length(psy))
  })
  
  return(list(compare_result=compare_result, compare_result_mean=compare_result_mean))
}
