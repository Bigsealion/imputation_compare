# get input parameter by keyboard
method_list=('CCA' 'mice_mean' 'mice_pmm' 'mice_norm_pred' 'vim_em')
while getopts "m:h" arg
do
  case $arg in
    m)
      echo "a's arg:$OPTARG"
      method=$OPTARG
      ;;
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
  echo "ERROR: -m input is ${method}"
  echo "*method must in [${method_list[*]}]!"
  exit 1
else
  echo "method is ${method}"
fi

# Call .sh script
echo "Call test2.sh"
. ./test2.sh -m ${method}