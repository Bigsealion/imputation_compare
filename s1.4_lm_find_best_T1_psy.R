# s1.4 test of lm in psy and T1surf, get the best T1surf for each psytool scale
# sig_list_all: main result, look this
# summary_sig: summary information
# t1_sig_summary: look each T1
# psy_sig_sort: final result, look this
library(stringr)
library(purrr)
library(naniar)

time_op <- Sys.time()
# set parameter
psy_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/cog_new_1.5.RData'
# psy_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/psytool/single/adjust_choose_psy_1.5/choose_psy_1.5.RData'
T1surf_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/T1_surf/Rdata/T1Surf.RData'
out_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/combine_data/'
sort_first_n <- 10  # only for looking, not effect out data
is_adjust_data <- F

# mkdir out dir
save_path <- file.path(out_dir, basename(psy_path))
if (!file.exists(out_dir)){
  dir.create(out_dir)
  sprintf('Create out dir! %s\n', out_dir) %>% cat()
}

# load data
load(T1surf_path)  # name is T1Surf_list
names(T1Surf_list)

psy_load_name <- load(psy_path)

if (is_adjust_data){
  eval(parse(text=sprintf("psy_rawdata <- %s", psy_load_name)))
  single_data_list <- list()
  single_data_list$TotalData <- psy_rawdata
  single_data_list$CompleteData <- lapply(psy_rawdata, function(psy){
    # del col which all is NA
    na_col <- which(apply(psy, 2, function(x){all(is.na(x))}))
    if (!is_empty(na_col)){psy <- psy[, -na_col]}
    psy <- psy[complete.cases(psy), ]
    return(psy)
  })
}

names(single_data_list)
complete_psy <- single_data_list$CompleteData
names(complete_psy)
psy_size <- lapply(complete_psy, dim)

# match data
sig_list_all <- list()
cor_list <- list()
T1RefID <- T1Surf_list$T1_surf_aseg.volume_7359ID$RefID
time_model_op <- Sys.time()
for (psy_name in names(complete_psy)){
  sprintf('==================%s===================\n', psy_name) %>% cat()
  sig_list_2 <- list()
  
  # psy data
  psy_1 <- complete_psy[[psy_name]] 
  psy_var_num <- ncol(psy_1) - 1
  
  for (T1name in names(T1Surf_list) ){
    sprintf('%s, Now: %s, Total time: %.4f %s\n', T1name,
            Sys.time(), Sys.time() - time_model_op, attr(Sys.time() - time_model_op, 'units')) %>% cat()
    
    # load data
    {
      # T1 data
      # lapply(T1Surf_list, function(x){unique(x$RefID) %>% length}) %>% unlist
      
      T1Surf_1 <- T1Surf_list[[T1name]]
      colnames(T1Surf_1)[-c(1,2)] <- paste('T1Surf', colnames(T1Surf_1)[-c(1,2)], sep='_')
      
      # match
      refid_inter <- intersect(psy_1$RefID, T1Surf_1$RefID)
      match_psy <- subset(psy_1, RefID %in% refid_inter)
      match_psy <- match_psy[order(match_psy$RefID), ]
      
      match_T1Surf <- subset(T1Surf_1, RefID %in% refid_inter)
      match_T1Surf <- match_T1Surf[order(match_T1Surf$RefID), ]
      match_T1Surf <- match_T1Surf[, -ncol(match_T1Surf)]
    }
    
    #lm
    {
      sig_list <- lapply(match_T1Surf[, -c(1,2)], function(x, psy){
        match_data1 <- cbind(x, psy[, -1])
        colnames(match_data1)[1] <- 'T1_y'
        model <- lm(T1_y~., data=match_data1)
        
        # model <- step(model, trace = 0)  # step
        
        model_cor <- cor(model$residuals + model$fitted.values, model$fitted.values)
        
        sum1 <- summary(model)
        p_value <- sum1$coefficients
        colnames(p_value)[4] <- 'pvalue'
        sig <- subset(as.data.frame(p_value), pvalue < 0.05)  #only save coef which significant
        return(list(sig=sig, cor=model_cor))
      }, match_psy)
      
    }
    sig_list_2[[T1name]] <- lapply(sig_list, function(x){x$sig})
    cor_list[[T1name]] <- lapply(sig_list, function(x){x$cor})
  }
  
  sig_list_2_nrow <- sapply(sig_list_2, function(x, psy_var_num){
    out <- sapply(x, function(x){nrow(x)})
    outv <- c(mean(out)-1, max(out)-1, psy_var_num,
              (mean(out)-1)/psy_var_num, (max(out)-1)/psy_var_num)
    names(outv) <- c('mean', 'max', 'psy_var', 'mean_rate', 'max_rate')
    return(outv)
  }, psy_var_num)
  
  # get max T1 scale
  sig_number <- sapply(names(sig_list_2), function(t1name, sig_list_2, psy_var_num){
    t1_sig <- sapply(sig_list_2[[t1name]], function(x, psy_var_num){
      pinfo <- c(nrow(x), nrow(x)/psy_var_num, mean(x$pvalue))
      names(pinfo) <- c('number', 'rate', 'mean_p')

      return(pinfo)
      }, psy_var_num) %>% t()
    rownames(t1_sig) <- paste(t1name, rownames(t1_sig), sep = '__')
    return(t1_sig)
  }, sig_list_2, psy_var_num) %>% reduce(rbind)
  
  # sort by number and mean_p (col2 rate is equal to number in sorting)
  sig_sort <- sig_number[order(-sig_number[, 1], sig_number[, 3]),]
  
  sig_list_all[[psy_name]] <- list(pvalue=sig_list_2,
                                 nrow1=sig_list_2_nrow,
                                 sig_num=sig_number,
                                 sig_num_sort=sig_sort,
                                 cor_list=cor_list)
}

summary_sig <- lapply(sig_list_all, function(x){x$nrow1}) %>% as.data.frame() %>% t()  # look this
good1 <-  map(rownames(summary_sig), function(x, str1){
  str_detect(x, str1)}, 'T1_surf_rh.aparc.annot.area_7359ID')
g1 <- summary_sig[unlist(good1), ]

good_list <- list()
for (t1_name in names(T1Surf_list)){
  good_n <- map(rownames(summary_sig), function(x, str1){
    str_detect(x, str1)}, t1_name)
  good_list[[t1_name]] <- summary_sig[unlist(good_n), ]
}
t1_sig_summary <- t(sapply(good_list, colMeans))  # look this, and also have other need to view

# view the best result
{
  psy_sig_sort <- lapply(sig_list_all, function(psy, n){
    psy$sig_num_sort[1:n, ]
  }, sort_first_n)
  
  print('best T1Surf to each psytool scale:')
  for (i in names(psy_sig_sort)){
    cat(sprintf('%s:\n    %s\n    num:%d, rate:%.2f, mean_p:%.6f\n',
                i,rownames(psy_sig_sort[[i]])[1],
                psy_sig_sort[[i]][1,1], psy_sig_sort[[i]][1,2], psy_sig_sort[[i]][1,3]))
  }
  
  # get the best match
  best_one_match_list <- list()
  for(i in names(psy_sig_sort)){
    best_one_match_list[[i]] <- rownames(psy_sig_sort[[i]])[1]
  }
}

# view max cor
{
  max_cor <- lapply(sig_list_all, function(scale){
    scale$cor_list %>% unlist
  })
  
  sprintf('max cor:\n') %>% cat()
  for (scale_name in names(max_cor)){
    max_cor_name <- names(max_cor[[scale_name]])[which.max(max_cor[[scale_name]])]
    max_cor_value <- max(max_cor[[scale_name]], na.rm = T)
    sprintf('%s\n  %s: %.6f\n', scale_name, max_cor_name, max_cor_value) %>% cat()
  }
}

# combine best T1surf and psytool
{
  match_data <- list()
  for (psy_name in names(best_one_match_list)){
    T1info <- as.character(unlist(strsplit(best_one_match_list[[psy_name]], split = "__")))
    T1info[2] <- sub('T1Surf_', '', T1info[2])  # del 'T1Surf_' in name
    
    # get data, del NA in T1surf
    T1surf_y <- T1Surf_list[[T1info[1]]][T1info[2]]
    T1surf_y <- T1surf_y[-which_na(T1surf_y),, drop = FALSE]
    psytool_x <- single_data_list$TotalData[[psy_name]] 
    
    # match
    id_inter <- intersect(psytool_x$RefID, rownames(T1surf_y))
    match_psy_x <- subset(psytool_x, RefID %in% id_inter)
    match_psy_x <- match_psy_x[order(match_psy_x$RefID), ]
    
    match_T1Surf_y <- subset(T1surf_y, rownames(T1surf_y) %in% id_inter)
    match_T1Surf_y <- match_T1Surf_y[order(rownames(match_T1Surf_y)), , drop = FALSE]
    
    comb_psy <- cbind(match_psy_x[, 1, drop = FALSE], match_T1Surf_y, match_psy_x[, -1] )
    
    match_data[[psy_name]] <- comb_psy
  }
  
  # save_combine data
  save(match_data, file = save_path)
  sprintf('combine data saved!: %s\n', save_path) %>% cat()
}

print(Sys.time()-time_op)
