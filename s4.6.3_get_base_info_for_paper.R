# 2023.01.07 checking base info
library(tidyverse)
library(logging)

# checking 1, data base info
# feature number, subject number, number of subject with missing,
# value missing rate, subject missing rate
{
    # set parameters ----------------------------------------------------
    {
        data_path_list <- list(
            "CVLT" = "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata",
            "ReHo" = "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/Reho_age/s1.4_QC2_HarRehoAAL116_gender.RData"
        )
    }

    # load data, get info -----------------------------------------------
    for (data_path_i in names(data_path_list)) {
        loginfo("=================== %s ===================", data_path_i)

        load_name <- load(data_path_list[[data_path_i]])
        new_name <- "load_data"
        if (load_name != new_name) {
            eval(parse(text = sprintf("%s <- %s", new_name, load_name)))
            rm(list = c(load_name))
        }

        data <- load_data[[1]]
        loginfo("Subjects number: %d", nrow(data))
        loginfo("variables number: %d", ncol(data) - 2) # col1 is RefID, col 2 is Y
        loginfo("Subject with missing: %d", sum(!complete.cases(data)))
        loginfo("Subject missing rate: %.4f%%", 100 * sum(!complete.cases(data)) / nrow(data))
        loginfo("Value missing rate: %.4f%%", 100 * sum(is.na(data[, -c(1, 2)])) / ((ncol(data) - 2) * nrow(data)))
    }
}
