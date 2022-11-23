#!/bin/bash
# -h is help; -m is method; -q is qsub node name; -d is dataset name

function CheckPara()  # check parameter
{
  #input
  opt_name=$1
  x=$2
  x_list=$3
  
  # function
  x_check=f
  if [ "${x_list[*]}" != "" ]; then
    for x_i in ${x_list[*]}; do
      [ "${x_i}" == "${x}" ] && x_check=t && break
    done
  else
    x_check=t
  fi
  
  if [ ${x_check} == 'f' ];then
    echo "ERROR: -${opt_name} input is ${x}"
    echo "    it must in [${x_list[*]}]!"
    exit 1
  else
    echo "-${opt_name} is ${x}"
  fi
}


# parameter
script_dir=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm
qsub_list=(all.q clc cluster1 cluster2 cluster3 cluster40 cluster5 fmri gpu long.q mise sbm short.q verylong.q veryshort.q whole)
method_list=(CCA mice_pmm mice_norm_pred mice_mean mice_rf vim_em)

default_method_list=(CCA mice_pmm mice_norm_pred mice_mean vim_em)  # default applying method
#total_log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/batch_s3.1_Imp_20210307_CVLT_ZsHarQCT1_MultMiss_NoRep0.8_boot100_${method}.txt

# input by parameter
while getopts "q:m:d:i:h" arg; do
  case $arg in
    d)
      dataset_name=$OPTARG;;
    m)
      method=$OPTARG;;
    q)
      qsub_name=$OPTARG;;
    i)
      run_index=$OPTARG;;
    h)
      echo "-q is qsub node name:"
      echo "    "${qsub_list[*]}
      echo "    default is to assign nodes automatically (if no input of this)"
      echo "-m is method of imputation:"
      echo "        "${method_list[*]}
      echo "    default (if no input of this) will applying following method list:"
      echo "        ${default_method_list[*]}"
      echo "-d is dataset name"
      echo "    e.g.: CVLT_TCoefSimuY"
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

# check input 
if [[ ${method} == "" ]]; then
  echo "without input -m, using default method list:"
  echo "    ${default_method_list[*]}"
else
  CheckPara m "${method}" "${method_list[*]}"
fi

if [[ ${qsub_name} == "" ]]; then
  echo "without input -q, assign node automatically!"
  qsub_name="auto_qsub_node"
else 
  CheckPara q "${qsub_name}" "${qsub_list[*]}"
fi

CheckPara d "${dataset_name}" ""

# run in qsub script
# para: method, qsub_name, dataset_name
if [[ ${method} == "" ]]; then
  for method_i in ${default_method_list[*]}; do
    sh ${script_dir}/qmon_batch_${dataset_name}_Method.sh ${method_i} ${qsub_name} ${dataset_name} "${run_index[*]}"
  done
else
  sh ${script_dir}/qmon_batch_${dataset_name}_Method.sh ${method} ${qsub_name} ${dataset_name} "${run_index[*]}"
fi

