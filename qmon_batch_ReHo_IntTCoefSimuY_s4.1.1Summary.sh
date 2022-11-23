time_op=$(date "+%Y-%m-%d-%H-%M-%S")
qsub_list=(all.q clc cluster2 cluster3 cluster5 gpu long.q short.q verylong.q veryshort.q whole)
while getopts "q:i:h" arg; do
  case $arg in
    q)
      qsub_name=$OPTARG;;
    i)
      run_index=$OPTARG;;
    h)
      echo "-q is qsub node name:"
      echo "    "${qsub_list[*]}
      echo "-i is index of run"
      echo "    which is start from 1, and determines which part of the dataset to run"
      echo "    deafult is all of the dataset (if no input of this)"
      echo "    e.g.: \"1 2 3\""
      echo "-h is help"
      exit 0
      ;;
    ?)
      echo "Unsupport input parameter!"
      exit 1
      ;;
  esac
done

script_dir=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm
log_dir=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/qsub_run_logging/
error_dir=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/qsub_run_error/

# set parameter
datasetname=ReHo_IntTCoefSimuY
#formula_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/cog_newgroup/cvlt_TCoef/newYFormula_model__cor_step__.RData
formula_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/reho_IntTCoef/newYFormula_model__cor_step__.RData

sour_dir[1]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${datasetname}_TrueMiss_NoRep0.8
raw_path[1]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${datasetname}_TrueMiss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-05-09-17:18:13.RData
save_dir[1]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/${datasetname}_TrueMiss_NoRep0.8
log_path[1]=$log_dir/s4.1_Summarys_${time_op}_${qsub_name}_${datasetname}_TrueMiss_NoRep0.8_boot100.txt
err_path[1]=$error_dir/ERROR_s4.1_Summarys_${time_op}_${qsub_name}_${datasetname}_TrueMiss_NoRep0.8_boot100.txt

sour_dir[2]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${datasetname}_20Miss_NoRep0.8
raw_path[2]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${datasetname}_20Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-05-09-17:18:27.RData
save_dir[2]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/${datasetname}_20Miss_NoRep0.8
log_path[2]=$log_dir/s4.1_Summarys_${time_op}_${qsub_name}_${datasetname}_20Miss_NoRep0.8_boot100.txt
err_path[2]=$error_dir/ERROR_s4.1_Summarys_${time_op}_${qsub_name}_${datasetname}_20Miss_NoRep0.8_boot100.txt

sour_dir[3]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${datasetname}_40Miss_NoRep0.8
raw_path[3]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${datasetname}_40Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-05-09-17:18:56.RData
save_dir[3]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/${datasetname}_40Miss_NoRep0.8
log_path[3]=$log_dir/s4.1_Summarys_${time_op}_${qsub_name}_${datasetname}_40Miss_NoRep0.8_boot100.txt
err_path[3]=$error_dir/ERROR_s4.1_Summarys_${time_op}_${qsub_name}_${datasetname}_40Miss_NoRep0.8_boot100.txt

sour_dir[4]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${datasetname}_60Miss_NoRep0.8
raw_path[4]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${datasetname}_60Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-05-09-17:19:21.RData
save_dir[4]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/${datasetname}_60Miss_NoRep0.8
log_path[4]=$log_dir/s4.1_Summarys_${time_op}_${qsub_name}_${datasetname}_60Miss_NoRep0.8_boot100.txt
err_path[4]=$error_dir/ERROR_s4.1_Summarys_${time_op}_${qsub_name}_${datasetname}_60Miss_NoRep0.8_boot100.txt

sour_dir[5]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/${datasetname}_80Miss_NoRep0.8
raw_path[5]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${datasetname}_80Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-05-09-17:19:37.RData
save_dir[5]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1_summary_data/${datasetname}_80Miss_NoRep0.8
log_path[5]=$log_dir/s4.1_Summarys_${time_op}_${qsub_name}_${datasetname}_80Miss_NoRep0.8_boot100.txt
err_path[5]=$error_dir/ERROR_s4.1_Summarys_${time_op}_${qsub_name}_${datasetname}_80Miss_NoRep0.8_boot100.txt

# other parameter
is_get_model_summary=T
is_compare_by_ImputedValue=T
is_compare_by_CrossValidation=T
is_print_imputed_value_figure=T


if [ "$run_index" == "" ]; then run_index=`echo ${!save_dir[*]}`; fi
for i in ${run_index[*]};do
  qsub -q ${qsub_name} -e ${err_path[$i]} -N SimuMiss_${i}_${datasetname} $script_dir/qmon_Rscript4.1.1Summary_run.sh ${sour_dir[$i]} ${raw_path[$i]} ${formula_path} ${save_dir[$i]} ${is_get_model_summary} ${is_compare_by_ImputedValue} ${is_compare_by_CrossValidation} ${is_print_imputed_value_figure} ${log_path[$i]}
done