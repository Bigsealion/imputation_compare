# get parameter
source_path=$1
save_dir=$2
pattern_dir=$3
method=$4
log_path=$5

# set environment
Anaconda_path=/gpfs/lab/liangmeng/members/liyifan/anaconda3/bin
script_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/s3.1.1_batch_imp_data.R 
export PATH=${Anaconda_path}:$PATH
. activate R1

# run Rscript
Rscript $script_path ${source_path} ${save_dir} ${pattern_dir} ${method} &>> ${log_path}



