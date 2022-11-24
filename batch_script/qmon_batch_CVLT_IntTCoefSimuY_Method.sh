#!/bin/bash
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
source_p[1]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${dataset_name}_TrueMiss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-05-09-16:10:51.RData
save_dir[1]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${dataset_name}_TrueMiss_NoRep0.8
pt_s_dir[1]=${save_dir1}/pattern
log_path[1]=$log_dir/s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_TrueMiss_NoRep0.8_boot100.txt
err_path[1]=$error_dir/ERROR_s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_TrueMiss_NoRep0.8_boot100.txt

source_p[2]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${dataset_name}_20Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-05-09-18:07:07.RData
save_dir[2]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${dataset_name}_20Miss_NoRep0.8
pt_s_dir[2]=${save_dir2}/pattern
log_path[2]=$log_dir/s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_20Miss_NoRep0.8_boot100.txt
err_path[2]=$error_dir/ERROR_s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_20Miss_NoRep0.8_boot100.txt

source_p[3]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${dataset_name}_40Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-05-09-18:26:54.RData
save_dir[3]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${dataset_name}_40Miss_NoRep0.8
pt_s_dir[3]=${save_dir3}/pattern
log_path[3]=$log_dir/s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_40Miss_NoRep0.8_boot100.txt
err_path[3]=$error_dir/ERROR_s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_40Miss_NoRep0.8_boot100.txt

source_p[4]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${dataset_name}_60Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-05-09-18:43:23.RData
save_dir[4]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${dataset_name}_60Miss_NoRep0.8
pt_s_dir[4]=${save_dir4}/pattern
log_path[4]=$log_dir/s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_60Miss_NoRep0.8_boot100.txt
err_path[4]=$error_dir/ERROR_s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_60Miss_NoRep0.8_boot100.txt

source_p[5]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${dataset_name}_80Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-05-09-18:51:40.RData
save_dir[5]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${dataset_name}_80Miss_NoRep0.8
pt_s_dir[5]=${save_dir5}/pattern
log_path[5]=$log_dir/s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_80Miss_NoRep0.8_boot100.txt
err_path[5]=$error_dir/ERROR_s3.1_Imp_${method}_${time_op}_${qsub_name}_${dataset_name}_80Miss_NoRep0.8_boot100.txt


# run imputation batch =======================
echo
echo "Starting imputation: "`date`
echo "method: ${method}"

# get run index
if [ "$run_index" == "" ]; then run_index=`echo ${!source_p[*]}`; fi

# up to qsub
for i in ${run_index[*]};do
  #qsub -q ${qsub_name} -o ${log_path[$i]} -e ${err_path[$i]} -N ${method}_${i}_${dataset_name} $script_dir/qmon_Rscript_run.sh ${source_p[$i]} ${save_dir[$i]} ${pt_s_dir[$i]} ${method} ${log_path[$i]}
  qsub -q ${qsub_name} -e ${err_path[$i]} -N ${method}_${i}_${dataset_name} $script_dir/qmon_Rscript3.1.1Imp_run.sh ${source_p[$i]} ${save_dir[$i]} ${pt_s_dir[$i]} ${method} ${log_path[$i]}
  #sleep 1
done


