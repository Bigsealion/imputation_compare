#!/bin/bash
# ========= NOT RUN THIS SCRIPT DIRECTLY =================
# this script name must same as datasetname! (to be identified by qmon_batch_main.sh)
# For impute the missing data, this script will be Called By bash script: qmon_batch_main.sh
# this script need be changed
# input opt is -m, in ['CCA', mice_mean'. 'mice_pmm', 'mice_norm_pred', 'raw', 'vim_em']
time_op=$(date "+%Y-%m-%d-%H-%M-%S")
echo "======================NEW TASK========================="

# get input method =====================
method_list=('CCA' 'mice_mean' 'mice_pmm' 'mice_norm_pred' 'vim_em')
method=$1
qsub_name=$2
dataset_name=$3
run_index=$4

script_dir=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm
log_dir=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/qsub_run_logging/
error_dir=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/qsub_run_error/

mkdir -p $log_dir
mkdir -p $error_dir

#echo "method list: ${method_list[*]}"
#echo "used method: ${method}"

# set parameter by manual ======================
# "" will highlight some general modified strings in VSCode 
data_file_name="s1.4_HarRehoAAL116_Gender.RData"  # change this, and follow RData name

source_p[1]="/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${dataset_name}_TrueMiss_NoRep0.8/${data_file_name}/simulation_boot_100_2022-11-24-21:50:08.RData"
save_dir[1]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${dataset_name}_TrueMiss_NoRep0.8
pt_s_dir[1]=${save_dir1}/pattern
log_path[1]=$log_dir/s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_TrueMiss_NoRep0.8_boot100.txt
err_path[1]=$error_dir/ERROR_s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_TrueMiss_NoRep0.8_boot100.txt

source_p[2]="/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${dataset_name}_20Miss_NoRep0.8/${data_file_name}/simulation_boot_100_2022-11-24-21:51:08.RData"
save_dir[2]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${dataset_name}_20Miss_NoRep0.8
pt_s_dir[2]=${save_dir2}/pattern
log_path[2]=$log_dir/s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_20Miss_NoRep0.8_boot100.txt
err_path[2]=$error_dir/ERROR_s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_20Miss_NoRep0.8_boot100.txt

source_p[3]="/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${dataset_name}_40Miss_NoRep0.8/${data_file_name}/simulation_boot_100_2022-11-24-21:44:00.RData"
save_dir[3]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${dataset_name}_40Miss_NoRep0.8
pt_s_dir[3]=${save_dir3}/pattern
log_path[3]=$log_dir/s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_40Miss_NoRep0.8_boot100.txt
err_path[3]=$error_dir/ERROR_s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_40Miss_NoRep0.8_boot100.txt

source_p[4]="/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${dataset_name}_60Miss_NoRep0.8/${data_file_name}/simulation_boot_100_2022-11-24-21:54:34.RData"
save_dir[4]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${dataset_name}_60Miss_NoRep0.8
pt_s_dir[4]=${save_dir4}/pattern
log_path[4]=$log_dir/s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_60Miss_NoRep0.8_boot100.txt
err_path[4]=$error_dir/ERROR_s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_60Miss_NoRep0.8_boot100.txt

source_p[5]="/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${dataset_name}_80Miss_NoRep0.8/${data_file_name}/simulation_boot_100_2022-11-24-21:46:37.RData"
save_dir[5]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${dataset_name}_80Miss_NoRep0.8
pt_s_dir[5]=${save_dir5}/pattern
log_path[5]=$log_dir/s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_80Miss_NoRep0.8_boot100.txt
err_path[5]=$error_dir/ERROR_s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_80Miss_NoRep0.8_boot100.txt


# run imputation batch =======================
echo
echo "Starting imputation: "`date`
echo "method: ${method}"
echo "qsub name: ${qsub_name}"

# get run index
if [ "$run_index" == "" ]; then run_index=`echo ${!source_p[*]}`; fi

# up to qsub
for i in ${run_index[*]};do
  #qsub -q ${qsub_name} -o ${log_path[$i]} -e ${err_path[$i]} -N ${method}_${i}_${dataset_name} $script_dir/qmon_Rscript_run.sh ${source_p[$i]} ${save_dir[$i]} ${pt_s_dir[$i]} ${method} ${log_path[$i]}
  if [ ${qsub_name} == "" ]; then
    qsub -e ${err_path[$i]} -N ${method}_${i}_${dataset_name} $script_dir/qmon_Rscript3.1.1Imp_run.sh ${source_p[$i]} ${save_dir[$i]} ${pt_s_dir[$i]} ${method} ${log_path[$i]}
  else
    qsub -q ${qsub_name} -e ${err_path[$i]} -N ${method}_${i}_${dataset_name} $script_dir/qmon_Rscript3.1.1Imp_run.sh ${source_p[$i]} ${save_dir[$i]} ${pt_s_dir[$i]} ${method} ${log_path[$i]}
  fi
  #sleep 1
done


