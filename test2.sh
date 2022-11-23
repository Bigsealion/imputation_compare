a1=$1
a2=$2
a3=$3
a4=$4
a5=$5
a6=$6
a7=$7
a8=$8
a9=$9
a10=$10  # ERROR this is 1
a11=${11}  # yes, this is 11


# test 1
method_list=('CCA' 'mice_mean' 'mice_pmm' 'mice_norm_pred' 'vim_em')
while getopts "m:hi:" arg
do
  case $arg in
    m)
      method=$OPTARG
      ;;
    h)
      echo "-m can input: [${method_list[*]}]"
      exit 0
      ;;
    i)
      run_index=$OPTARG
      echo ${run_index[*]}
      for j in ${run_index[*]}; do echo "j is: ${j}"; done
      ;;
    ?)
      echo "Unsupport input parameter!"
      exit 1
      ;;
  esac
done

echo "a1: ${a1}"
echo "a2: ${a2}"
echo "a3: ${a3}"
echo "a4: ${a4}"
echo "a5: ${a5}"
echo "a6: ${a6}"
echo "a7: ${a7}"
echo "a8: ${a8}"
echo "a9: ${a9}"
echo "a10: ${a10}"
echo "a11: ${a11}"
echo "method: ${method}"
