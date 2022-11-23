# s2.1 get missing pattern which combine 1 T1surf and psytool
# using bootstrap to get n sample
rm(list = ls())
source('/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/imp_compare_utils.R')

time_op <- Sys.time()
sprintf('Start time: %s\n', time_op) %>% cat
# set parameter by input
Args <- commandArgs(T)

source_path <- Args[1]
save_dir <- Args[2]
is_out_pattern_img <- Args[3] %>% as.logical()
boot_num <- Args[4] %>% as.numeric()
is_replace <- Args[5] %>% as.logical() # if T, sample is back up, F mean no repetition
sample_size <- Args[6] %>% as.numeric()  # 0~1 means sample_size * data_size, >1 means size number
pattern_num <- Args[7] %>% as.numeric() # -1 means all pattern
add_missing_rate <- Args[8] %>% as.numeric()  # 0~1, if true rate, setting to NULL or 0

img_dir <- file.path(save_dir, 'img')
save_dir <- file.path(save_dir, basename(source_path))

if (add_missing_rate == 0){
  add_missing_rate=NULL
}

# print log
if (is.null(add_missing_rate)){
  sprintf('add_missing_rate: TRUE\nsample_size: %.3f\n', 
          sample_size) %>% cat
}else{
  sprintf('add_missing_rate: %.3f\nsample_size: %.3f\n', 
        add_missing_rate, sample_size) %>% cat
}


# mkdir out dir
{
  if (!file.exists(img_dir)){
    dir.create(img_dir, recursive = T)
    sprintf('Create img dir! %s\n', img_dir) %>% cat()
  }
  if (!file.exists(save_dir)){
    dir.create(save_dir, recursive = T)
    sprintf('Create save dir! %s\n', save_dir) %>% cat()
  }
}

# load data
{
  load_name <- load(source_path)
  eval(parse(text=sprintf("data_save_list <- %s", load_name)))
  
  miss_data_list <- data_save_list
  complete_data_list <- lapply(data_save_list, function(psy){
    psy[complete.cases(psy), ]
  })
}

# out patten image of each scales
if (is_out_pattern_img){
  for (name_i in names(miss_data_list)){
    out_path = file.path(img_dir, paste(name_i, '_pattern.png', sep=''))
    png(out_path, width=1200, height=1200)
    md.pattern(miss_data_list[[name_i]], rotate.names = T)
    dev.off()
    print(name_i)
  }
}

# bootstrap
{
  boot_op_time <- Sys.time()
  # miss_data_boot_list <- lapply(miss_data_list, get_bootstrap_data,
  #                               sample_number=boot_num, sample_size=sample_size, is_replace=is_replace)
  # 
  # complete_data_boot_list <- lapply(miss_data_boot_list, function(x){
  #   lapply(x, function(x){x[complete.cases(x),]})
  # })
  
  complete_data_boot_list <- lapply(complete_data_list, get_bootstrap_data, 
                                    sample_number=boot_num, sample_size=sample_size, is_replace=is_replace)
  
  sprintf('bootstrap end, time used: %.4f %s\n', 
          Sys.time() - boot_op_time, attr(Sys.time() - boot_op_time, 'units')) %>% cat()
}

# add simulation missing pattern in complete data
{
  simu_op_time <- Sys.time()
  # # 2021.01.12, using total missing pattern rather than boot missing pattern
  # simulation_missing_data_list <- map2(complete_data_boot_list, miss_data_list, function(c, m, pattern_num, add_missing_rate){
  #   return(lapply(c, function(c, m, pattern_num, add_missing_rate){
  #     sprintf('simulation end %s\n', Sys.time()) %>% cat()
  #     return(simulation_missing_pattern(c, m, pattern_num=pattern_num, add_missing_case_rate=add_missing_rate))
  #   }, m, pattern_num, add_missing_rate))
  # }, pattern_num=pattern_num, add_missing_rate=add_missing_rate)
  
  sprintf('Simulation Pattern...\n') %>% cat()
  simulation_missing_data_list <- list()
  scale_i <- 0
  scale_number <- length(complete_data_boot_list)
  
  for (scale_name in names(complete_data_boot_list)){
    scale_op_time <- Sys.time()
    scale_i = scale_i + 1
    boot_number <- length(complete_data_boot_list[[scale_name]])
    for (boot_i in 1:boot_number){
      boot_op_time <- Sys.time()
      simulation_missing_data_list[[scale_name]][[boot_i]] <-
        simulation_missing_pattern(complete_data_boot_list[[scale_name]][[boot_i]],
                                   miss_data_list[[scale_name]],
                                   pattern_num=pattern_num, 
                                   add_missing_case_rate=add_missing_rate)
      
      scale_time <- Sys.time() - scale_op_time
      boot_time <- Sys.time() - boot_op_time
      total_time <- Sys.time() - time_op
      sprintf('    Scale: %d/%d(%s %.4f %s), Boot: %d/%d(%.4f %s), total: %.4f %s\n', 
              scale_i, scale_number, scale_name,
              scale_time, attr(scale_time, 'units'),
              boot_i, boot_number,
              boot_time, attr(boot_time, 'units'),
              total_time, attr(total_time, 'units')) %>% cat()
    }
  }
  
  sprintf('Simulation end, time used: %.4f %s\n', 
          Sys.time() - simu_op_time, attr(Sys.time() - simu_op_time, 'units')) %>% cat()
}

# save data
save_list <- list(simulation_missing_data_boot_list=simulation_missing_data_list,
                  complete_data_boot_list=complete_data_boot_list, 
                  total_complete_data_list=complete_data_list)

save_path <- file.path(save_dir, sprintf('simulation_boot_%d_%s.RData', boot_num,
                                         strftime(Sys.time(),format='%Y-%m-%d-%H:%M:%S')))
save(save_list, file=save_path)

sprintf('simulation data saved!: %s\n', save_path) %>% cat()
sprintf('save end, total time used: %.4f %s\n', 
        Sys.time() - time_op, attr(Sys.time() - time_op, 'units')) %>% cat()
