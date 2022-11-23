# get parameter
imp_data_dir=$1
raw_data_path=$2
formula_path=$3
out_dir=$4
is_get_model_summary=$5
is_compare_by_ImputedValue=$6  
is_compare_by_CrossValidation=$7 
is_print_imputed_value_figure=$8 
log_path=$9

# set environment
Anaconda_path=/gpfs/lab/liangmeng/members/liyifan/anaconda3/bin
script_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/s4.1.1_get_impdata_model_summary.R
export PATH=${Anaconda_path}:$PATH
. activate R1

# run Rscript
Rscript $script_path ${imp_data_dir} ${raw_data_path} ${formula_path} ${out_dir} ${is_get_model_summary} ${is_compare_by_ImputedValue} ${is_compare_by_CrossValidation} ${is_print_imputed_value_figure}  &>> ${log_path}
