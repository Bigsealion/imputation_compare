time_op=$(date "+%Y-%m-%d-%H-%M-%S")
qsub_list=(all.q clc cluster2 cluster3 cluster5 cluster40 gpu long.q short.q verylong.q veryshort.q whole fmri sbm mise)
while getopts "q:i:h" arg; do
  case $arg in
    q)
      qsub_name=$OPTARG;;
    i)
      run_index=$OPTARG;;
    h)
      echo "-q is qsub node name:"
      echo "    ${qsub_list[*]}"
      echo "    default is to assign nodes automatically (if no input of this)"
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
datasetname="HarRehoAgeSimuY"
#source_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_x1.2_CovCVLT_ZsHarQCT1_CorCombine_Data.Rdata
source_path="/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.7_simulation_y/HarReho_age/s1.7_SimulationOutcomeData.RData"

save_dir[1]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${datasetname}_TrueMiss_NoRep0.8
add_misr[1]=0  # 0~1, if true rate, setting to NULL
log_path[1]=$log_dir/s2.1_SimuMiss_${time_op}_${qsub_name}_TrueMiss_NoRep0.8_boot100.txt
err_path[1]=$error_dir/ERROR_s2.1_SimuMiss_${time_op}_${qsub_name}_TrueMiss_NoRep0.8_boot100.txt

save_dir[2]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${datasetname}_20Miss_NoRep0.8
add_misr[2]=0.2  # 0~1, if true rate, setting to NULL
log_path[2]=$log_dir/s2.1_SimuMiss_${time_op}_${qsub_name}_20Miss_NoRep0.8_boot100.txt
err_path[2]=$error_dir/ERROR_s2.1_SimuMiss_${time_op}_${qsub_name}_20Miss_NoRep0.8_boot100.txt

save_dir[3]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${datasetname}_40Miss_NoRep0.8
add_misr[3]=0.4  # 0~1, if true rate, setting to NULL
log_path[3]=$log_dir/s2.1_SimuMiss_${time_op}_${qsub_name}_40Miss_NoRep0.8_boot100.txt
err_path[3]=$error_dir/ERROR_s2.1_SimuMiss_${time_op}_${qsub_name}_40Miss_NoRep0.8_boot100.txt

save_dir[4]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${datasetname}_60Miss_NoRep0.8
add_misr[4]=0.6  # 0~1, if true rate, setting to NULL
log_path[4]=$log_dir/s2.1_SimuMiss_${time_op}_${qsub_name}_60Miss_NoRep0.8_boot100.txt
err_path[4]=$error_dir/ERROR_s2.1_SimuMiss_${time_op}_${qsub_name}_60Miss_NoRep0.8_boot100.txt

save_dir[5]=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/${datasetname}_80Miss_NoRep0.8
add_misr[5]=0.8  # 0~1, if true rate, setting to NULL
log_path[5]=$log_dir/s2.1_SimuMiss_${time_op}_${qsub_name}_80Miss_NoRep0.8_boot100.txt
err_path[5]=$error_dir/ERROR_s2.1_SimuMiss_${time_op}_${qsub_name}_80Miss_NoRep0.8_boot100.txt

# other parameter
is_out_pattern_img=F
boot_num=100
is_replace=F # if T, sample is back up, F mean no repetition
sample_size=0.8  # 0~1 means sample_size * data_size, >1 means size number
pattern_num=-1 # -1 means all pattern


if [ "$run_index" == "" ]; then run_index=`echo ${!save_dir[*]}`; fi
for i in ${run_index[*]};do
  if [ "$qsub_name" == "" ]; then
      qsub -e ${err_path[$i]} -N SimuMiss_${i}_${datasetname} $script_dir/qmon_Rscript2.1.1SimuMiss_run.sh ${source_path} ${save_dir[$i]} ${is_out_pattern_img} ${boot_num} ${is_replace} ${sample_size} ${pattern_num} ${add_misr[$i]} ${log_path[$i]}
  else
    qsub -q ${qsub_name} -e ${err_path[$i]} -N SimuMiss_${i}_${datasetname} $script_dir/qmon_Rscript2.1.1SimuMiss_run.sh ${source_path} ${save_dir[$i]} ${is_out_pattern_img} ${boot_num} ${is_replace} ${sample_size} ${pattern_num} ${add_misr[$i]} ${log_path[$i]}
  fi
done