# test batch run
library(dplyr)
Args <- commandArgs(T)
out_dir <- Args[1]
data_path <- Args[2]

if (is.na(out_dir)){out_dir <- 'NoInput'}
if (is.na(data_path)){data_path <- 'NoInput'}

sprintf('time1: %s, arg1: %s, arg2: %s\n', Sys.time(), out_dir, data_path) %>% cat
Sys.sleep(2)
sprintf('time2: %s, good end\n', Sys.time()) %>% cat