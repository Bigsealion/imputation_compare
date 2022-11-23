# s2 get missing value pattern
# add T1 Surf 
# using bootstrap to get n sample
source('/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/imp_compare_utils.R')

# set parameter
source_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/psytool/single/IQR_psy_1.5.RData'
T1surf_path <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/T1_surf/Rdata/T1Surf.RData'
img_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/psytool/single/1.5_pattern/'
save_dir <- '/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/psy_1.5_pattern.RData'
is_out_pattern_img <- F
boot_num = 10

# mkdir out dir
if (!file.exists(img_dir)){
  dir.create(img_dir)
  sprintf('Create img dir! %s\n', img_dir) %>% cat()
}
if (!file.exists(save_dir)){
  dir.create(save_dir)
  sprintf('Create save dir! %s\n', save_dir) %>% cat()
}

# load data
{
  # psytool data
  load_name <- load(source_path)
  eval(parse(text=sprintf("data_save_list <- %s", load_name)))
  
  miss_data_list <- lapply(data_save_list$TotalData, as.data.frame)
  
  # T1 surf data
  load(T1surf_path)
  T1Surf_1 <- T1Surf_list$T1_surf_rh.aparc.annot.area_7359ID
  T1Surf_1[, 1] <- as.character(T1Surf_1[, 1])
  T1Surf_1 <- T1Surf_1[, -ncol(T1Surf_1)]
  T1Surf_1 <- T1Surf_1[complete.cases(T1Surf_1), ]
  colnames(T1Surf_1)[-c(1,2)] <- paste('T1Surf', colnames(T1Surf_1)[-c(1,2)], sep='_')
  
  # match data
  match_data <- lapply(miss_data_list, function(psy_1, T1Surf_1){
    refid_inter <- intersect(psy_1$RefID, T1Surf_1$RefID)
    
    match_psy <- subset(psy_1, RefID %in% refid_inter)
    match_psy <- match_psy[order(match_psy$RefID), ]
    
    match_T1Surf <- subset(T1Surf_1, RefID %in% refid_inter)
    match_T1Surf <- match_T1Surf[order(match_T1Surf$RefID), ]
    
    # del MRIid and na, save subID
    match_data <- cbind(match_T1Surf[, -c(2,ncol(match_T1Surf))], match_psy[, -1])
    return(match_data)
  }, T1Surf_1)
}

# out patten image of each scales
if (is_out_pattern_img){
  for (name_i in names(miss_data_list)){
    out_path = file.path(img_dir, paste(name_i, '_pattern.png'))
    png(out_path, width=1200, height=1200)
    md.pattern(miss_data_list[[name_i]])
    dev.off()
    print(name_i)
  }
}

# bootstrap
{
  boot_op_time <- Sys.time()
  miss_data_boot_list <- lapply(match_data, get_bootstrap_data, sample_number=boot_num)
  
  complete_data_boot_list <- lapply(miss_data_boot_list, function(x){
    lapply(x, function(x){x[complete.cases(x),]})
  })
  
  sprintf('bootstrap end, time used: %.4f %s\n', 
          Sys.time() - boot_op_time, attr(Sys.time() - boot_op_time, 'units')) %>% cat()
}

# add simulation missing pattern in complete data
{
  simu_op_time <- Sys.time()
  simulation_missing_data_list <- map2(complete_data_boot_list, miss_data_boot_list, function(c, m){
    return(map2(c, m, function(c, m){
      print('end 1')
      return(simulation_missing_pattern(c, m))
    }))
  })
  sprintf('simulation end, time used: %.4f %s\n', 
          Sys.time() - simu_op_time, attr(Sys.time() - simu_op_time, 'units')) %>% cat()
}

# save data
save_list <- list(simulation_missing_data_boot_list=simulation_missing_data_list,
                  complete_data_boot_list=complete_data_boot_list)

save(save_list, file=file.path(save_dir, 'simu_boot_data_list.RData'))

