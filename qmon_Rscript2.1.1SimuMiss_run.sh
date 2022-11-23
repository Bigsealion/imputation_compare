# get parameter
source_path=$1
save_dir=$2
is_out_pattern_img=$3
boot_num=$4
is_replace=$5 # if T, sample is back up, F mean no repetition
sample_size=$6  # 0~1 means sample_size * data_size, >1 means size number
pattern_num=$7 # -1 means all pattern
add_missing_rate=$8  # 0~1, if true rate, setting to NULL
log_path=$9

echo "source_path:${source_path}"
echo "save_dir:${save_dir}"
echo "is_out_pattern_img:${is_out_pattern_img}"
echo "boot_num:${boot_num}"
echo "is_replace:${is_replace}"
echo "sample_size:${sample_size}"
echo "pattern_num:${pattern_num}"
echo "add_missing_rate:${add_missing_rate}"
echo "log_path:${log_path}"

# set environment
Anaconda_path=/gpfs/lab/liangmeng/members/liyifan/anaconda3/bin
script_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/s2.1.1_batch_set_missing_to_simulation_data.R
export PATH=${Anaconda_path}:$PATH
. activate R1

# run Rscript
Rscript $script_path ${source_path} ${save_dir} ${is_out_pattern_img} ${boot_num} ${is_replace} ${sample_size} ${pattern_num} ${add_missing_rate}  &>> ${log_path}
