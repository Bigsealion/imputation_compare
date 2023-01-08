# 2023.01.07 checking base info
library(tidyverse)
library(logging)

# info 1, data base info ============================================
# feature number, subject number, number of subject with missing,
# value missing rate, subject missing rate
if (F) {
    # set parameters ----------------------------------------------------
    {
        data_path_list <- list(
            "CVLT" = "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata",
            "ReHo" = "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/Reho_age/s1.4_QC2_HarRehoAAL116_gender.RData"
        )

        out_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.6.1_JournalOutComplete/All_CVLT_ReHo_TrueSimuY/"
        save_name <- "BaseInfo_%s_Original.csv"
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

        # get some info --------------------------------------
        data <- load_data[[1]]
        loginfo("Subjects number: %d", nrow(data))
        loginfo("variables number: %d", ncol(data) - 2) # col1 is RefID, col 2 is Y
        loginfo("Subject with missing: %d", sum(!complete.cases(data)))
        loginfo("Subject missing rate: %.4f%%", 100 * sum(!complete.cases(data)) / nrow(data))
        loginfo("Value missing rate: %.4f%%", 100 * sum(is.na(data[, -c(1, 2)])) / ((ncol(data) - 2) * nrow(data)))

        # mean + sd, mr in variable --------------------------
        stat_1_level <- apply(data[, -1], 2, function(x){
            avg <- mean(x, na.rm = TRUE)
            std <- sd(x, na.rm = TRUE)
            mr <- sum(is.na(x)) / length(x)
            return(c(sprintf("%.2f±%.2f", avg, std), sprintf("%.2f%%", 100 * mr)))
        }) %>% t %>% as.data.frame()
        colnames(stat_1_level) <- c("meansd", "mr")

        out_path <- file.path(out_dir, sprintf(save_name, data_path_i))
        write.csv(stat_1_level, file = out_path, row.names = TRUE)
        loginfo("Out 1 level stat: %s", out_path)

    }
}

# info 2, cvlt mean, sd, missing rate
# 1 base info cvlt
if (F) {
    rm(list = ls())
    # set parameter --------------------------------------------------------
    {
        source_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s2_simulation"

        # c[1] is boot_dir_name, and c[2] is file_pattern
        dataset_list <- list(
            "CVLT_TGMV" = c("s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata", "CVLT_ZsHarQCT1_(True|\\d+)(M|m)iss_NoRep0.8"),
            "CVLT_Y1" = c("s1.7_SimulationOutcomeData.RData", "CVLT_TCoefSimuY_(True|\\d+)(M|m)iss_NoRep0.8"),
            "ReHo_Gender" = c("s1.4_HarRehoAAL116_Gender.RData", "HarRehoGender_(True|\\d+)(M|m)iss_NoRep0.8"),
            "ReHo_Y2" = c("s1.7_SimulationOutcomeData.RData", "HarRehoGenderSimuY_(True|\\d+)(M|m)iss_NoRep0.8"),
        )

        out_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.6.1_JournalOutComplete/All_CVLT_ReHo_TrueSimuY/"
        save_name <- "BaseInfo_%s.csv"
    }

    # mkdir -------------------------------------------------------
    if (!file.exists(out_dir)) {
        dir.create(out_dir, recursive = T)
        sprintf("Create out dir! %s\n", out_dir) %>% cat()
    }

    # load data and save ----------------------------------------------
    for (ds_i in names(dataset_list)) {
        data_list <- list()
        boot_dir_name <- dataset_list[[ds_i]][1]
        file_pattern <- dataset_list[[ds_i]][2]
        cat(sprintf("%s...\n", ds_i))

        for (file_name in dir(source_dir)) {
            if (str_detect(file_name, file_pattern)) {
                print(file_name)
                missing_rate <- str_match(file_name, file_pattern)[1]

                boot_dir_path <- file.path(source_dir, file_name, boot_dir_name)
                boot_file_path <- file.path(boot_dir_path, dir(boot_dir_path)[1])

                data_load_name <- load(boot_file_path)
                eval(parse(text = sprintf("data_list[['%s']]  <- %s", missing_rate, data_load_name)))
            }
        }

        data_info_list <- lapply(data_list, function(mr) {
            lapply(mr$simulation_missing_data_boot_list[[1]], function(boot) {
                sapply(boot$simulation_data[, -1], function(x) {
                    ms <- sum(is.na(x)) / length(x)
                    return(c(me = mean(x, na.rm = T), std = sd(x, na.rm = T), ms = ms))
                })
            })
        })

        data_info_list_summary <- lapply(data_info_list, function(mr) {
            mr_s <- Reduce("+", mr) / 100
            mrs <- apply(mr_s, 2, function(x) {
                c(mestd = sprintf("%.2f±%.2f", x[1], x[2]), ms = sprintf("%.2f%%", x[3] * 100))
            }) %>% t()
        })

        data_info_list_summary_bind <- Reduce(cbind, data_info_list_summary)

        # save_path
        save_path <- file.path(out_dir, sprintf(save_name, ds_i))
        write.csv(data_info_list_summary_bind, file = save_path, row.names = TRUE)
        cat(sprintf("Out: %s\n", save_path))
    }
}

# info 3, get model coefficents, cvlt, reho
if (T) {
    rm(list = ls())
    # set parameters
    {
        formual_path_list <- list(
            "CVLT" = "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_CVLT_ZsHarQCT1_CorCombine_Data.Rdata/model__cor_step__.RData",
            "ReHo" = "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1.5_formula/s1.4_HarRehoAAL116_Gender.RData/model__cor_step__.RData"
        )

        out_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.6.1_JournalOutComplete/All_CVLT_ReHo_TrueSimuY/"
        save_name <- "ModelCoefficents_%s.csv"
    }

    # load, and out coef csv
    for (ds_i in names(formual_path_list)) {
        loginfo("%s...", ds_i)
        coef_path <- formual_path_list[[ds_i]]
        # load
        coef_load_name <- load(coef_path)
        eval(parse(text = sprintf("load_coef <- %s", coef_load_name)))
        ds_coef <- load_coef[[1]]$coefficients %>% as.data.frame()
        colnames(ds_coef) <- c("coefficients")

        # out coef
        out_path <- file.path(out_dir, sprintf(save_name, ds_i))
        write.csv(ds_coef, file = out_path, row.names = TRUE)
        loginfo("Coef out: %s", out_path)
    }
}
