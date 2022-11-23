# s0.5 test of lm in psy and T1surf
library(stringr)
# set parameter
psy_path <- 'I:\\imputation\\imp_compare\\s1_data\\psytool\\single\\IQR_psy_1.5.RData'
T1surf_path <- 'I:\\imputation\\imp_compare\\s1_data\\T1_surf\\Rdata\\T1Surf.RData'

# load data
load(T1surf_path)  # name is T1Surf_list
names(T1Surf_list)

load(psy_path)
names(single_data_list)
complete_psy <- single_data_list$CompleteData
names(complete_psy)
psy_size <- lapply(complete_psy, dim)

# match data
sig_list_all <- list()
T1RefID <- T1Surf_list$T1_surf_aseg.volume_7359ID$RefID
for (psy_name in names(complete_psy)){
  sprintf('==================%s===================\n', psy_name) %>% cat()
  sig_list_2 <- list()
  
  # psy data
  psy_1 <- complete_psy[[psy_name]] 
  psy_var_num <- ncol(psy_1) - 1
  
  for (T1name in names(T1Surf_list) ){
    print(T1name)
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
        sum1 <- summary(model)
        p_value <- sum1$coefficients
        colnames(p_value)[4] <- 'pvalue'
        sig <- subset(as.data.frame(p_value), pvalue < 0.05)
        return(sig)
      }, match_psy)
      
    }
    
    sig_list_2[[T1name]] <- sig_list
  }
  
  sig_list_2_nrow <- sapply(sig_list_2, function(x, psy_var_num){
    out <- sapply(x, function(x){nrow(x)})
    outv <- c(mean(out)-1, max(out)-1, psy_var_num,
              (mean(out)-1)/psy_var_num, (max(out)-1)/psy_var_num)
    names(outv) <- c('mean', 'max', 'psy_var', 'mean_rate', 'max_rate')
    return(outv)
  }, psy_var_num)
  
  sig_list_all[[psy_name]] <- list(pvalue=sig_list_2,
                                 nrow1=sig_list_2_nrow)
}

summary_sig <- lapply(sig_list_all, function(x){x$nrow1}) %>% as.data.frame() %>% t()
good1 <-  map(rownames(summary_sig), function(x, str1){
  str_detect(x, str1)}, 'T1_surf_rh.aparc.annot.area_7359ID')
g1 <- summary_sig[unlist(good1), ]

good_list <- list()
for (t1_name in names(T1Surf_list)){
  good_n <- map(rownames(summary_sig), function(x, str1){
    str_detect(x, str1)}, t1_name)
  good_list[[t1_name]] <- summary_sig[unlist(good_n), ]
}
t1_sig_summary <- t(sapply(good_list, colMeans))  # look this and also have other need to view
