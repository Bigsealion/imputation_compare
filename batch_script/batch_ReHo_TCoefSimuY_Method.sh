#!/bin/bash
# input opt is -m, in ['CCA', mice_mean'. 'mice_pmm', 'mice_norm_pred', 'raw', 'vim_em']
time_op=$(date "+%Y-%m-%d-%H-%M-%S")

# get input method =====================
method_list=('CCA' 'mice_mean' 'mice_pmm' 'mice_norm_pred' 'vim_em')
while getopts "m:h" arg
do
  case $arg in
    m)
      method=$OPTARG;;
    h)
      echo "-m can input ['CCA', mice_mean'. 'mice_pmm', 'mice_norm_pred', 'raw', 'vim_em']"
      exit 0
      ;;
    ?)
      echo "Unsupport input parameter!"
      exit 1
      ;;
  esac
done

# check input 
method_check=f
for method_i in ${method_list[*]}
do
  [ "${method_i}" == "${method}" ] && method_check=t && break
done

if [ ${method_check} == 'f' ];then
  echo "ERROR: method must in [${method_list[*]}]!"
  exit 1
else
  echo "method is ${method}"
fi

# set parameter by manual ======================
source_p1=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_TCoefSimuY_TrueMiss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-04-27-15:30:31.RData
save_dir1=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_TrueMiss_NoRep0.8
pt_s_dir1=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_TrueMiss_NoRep0.8/pattern
log_path1=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_Imp_${time_op}_ReHo_TCoefSimuY_TrueMiss_NoRep0.8_boot100_${method}.txt

source_p2=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_TCoefSimuY_20Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-04-27-15:33:39.RData
save_dir2=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_20Miss_NoRep0.8
pt_s_dir2=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_20Miss_NoRep0.8/pattern
log_path2=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_Imp_${time_op}_ReHo_TCoefSimuY_20Miss_NoRep0.8_boot100_${method}.txt

source_p3=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_TCoefSimuY_40Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-04-27-15:37:00.RData
save_dir3=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_40Miss_NoRep0.8
pt_s_dir3=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_40Miss_NoRep0.8/pattern
log_path3=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_Imp_${time_op}_ReHo_TCoefSimuY_40Miss_NoRep0.8_boot100_${method}.txt

source_p4=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_TCoefSimuY_60Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-04-27-15:38:53.RData
save_dir4=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_60Miss_NoRep0.8
pt_s_dir4=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_60Miss_NoRep0.8/pattern
log_path4=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_Imp_${time_op}_ReHo_TCoefSimuY_60Miss_NoRep0.8_boot100_${method}.txt

source_p5=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation/ReHo_TCoefSimuY_80Miss_NoRep0.8/s1.7_SimulationOutcomeData.RData/simulation_boot_100_2021-04-27-15:40:46.RData
save_dir5=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_80Miss_NoRep0.8
pt_s_dir5=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s3_impdata/ReHo_TCoefSimuY_80Miss_NoRep0.8/pattern
log_path5=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_Imp_${time_op}_ReHo_TCoefSimuY_80Miss_NoRep0.8_boot100_${method}.txt

#total_log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/batch_s3.1_Imp_20210307_CVLT_ZsHarQCT1_MultMiss_NoRep0.8_boot100_${method}.txt

# run imputation batch =======================
date
(echo method: ${method});

(
echo
echo ${source_p1}
echo ${save_dir1}
echo ${pt_s_dir1}
echo ${log_path1}
nohup Rscript ./s3.1.1_batch_imp_data.R ${source_p1} ${save_dir1} ${pt_s_dir1} ${method} &>${log_path1}
);

(
echo
echo ${source_p2}
echo ${save_dir2}
echo ${pt_s_dir2}
echo ${log_path2}
nohup Rscript ./s3.1.1_batch_imp_data.R ${source_p2} ${save_dir2} ${pt_s_dir2} ${method} &>${log_path2}
);

(
echo
echo ${source_p3}
echo ${save_dir3}
echo ${pt_s_dir3}
echo ${log_path3}
nohup Rscript ./s3.1.1_batch_imp_data.R ${source_p3} ${save_dir3} ${pt_s_dir3} ${method} &>${log_path3}
);

(
echo
echo ${source_p4}
echo ${save_dir4}
echo ${pt_s_dir4}
echo ${log_path4}
nohup Rscript ./s3.1.1_batch_imp_data.R ${source_p4} ${save_dir4} ${pt_s_dir4} ${method} &>${log_path4}
);

(
echo
echo ${source_p5}
echo ${save_dir5}
echo ${pt_s_dir5}
echo ${log_path5}
nohup Rscript ./s3.1.1_batch_imp_data.R ${source_p5} ${save_dir5} ${pt_s_dir5} ${method} &>${log_path5}
);

date
