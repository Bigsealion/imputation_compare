check_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/qsub_run_logging/
check_str="No such file or directory"
echo "checked path: ${check_path}"
echo "check str: '${check_str}'"
echo
grep -rl "${check_str}" ${check_path}
