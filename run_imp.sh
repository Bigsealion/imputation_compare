t1=$(date "+%Y-%m-%d-%H-%M-%S")
para1=$1
#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_imp_data_20210225_simuY_cog_50missing_MiceMean.txt
#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_imp_data_20210305_Reho_age_40miss_MicePmm.txt
#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_imp_data_20210305_cog_TrueMiss_NoRep0.8_boot100_MiceMean.txt
#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_Imp_20210306_Cog5_ZsHarQCT1_60Miss_NoRep0.8_boot100_VimEm.txt
#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_Imp_20210308_CVLT_ZsHarQCT1_TrueMiss_NoRep0.8_boot100_MicePmm.txt
#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_imp_data_20210308_Reho_age_80miss_NoRep0.8_VimEm.txt
#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_imp_data_20210308_ReHo_SimuY_80Miss_NoRep0.8_VimEm.txt
#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s3.1_imp_data_20210319_CVLT_UnNormSimuY_80Miss_NoRep0.8_VimEm.txt
#nohup Rscript ./s3.1_imp_data.R &>${log_path} &

#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s2.1_20210306_SetMiss_CVLT_ZsHarQCT1_80Miss_NoRep0.8_boot100.txt
#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s2.1_20210308_SetMiss_ReHo_Sex_80Miss_NoRep0.8_boot100.txt
#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s2.1_20210310_SetMiss_ReHo_SimuY_80Miss_NoRep0.8_boot100.txt
#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s2.1_20210318_SetMiss_CVLT_UnNormSimuY_TrueMiss_NoRep0.8_boot100.txt
log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s2.1_${t1}_SetMiss_CVLT_TCoefSimuY_${para1}Miss_NoRep0.8_boot100.txt
#log_path=/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/programm/run_logging/s2.1_${t1}_SetMiss_ReHo_TCoefSimuY_80Miss_NoRep0.8_boot100.txt
nohup Rscript ./s2.1_set_missing_to_simulation_data.R &>${log_path} &
