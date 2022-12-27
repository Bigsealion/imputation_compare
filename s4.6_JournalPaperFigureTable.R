# 2022.11.26 Journal Figure and table (new data)
# Fig shape is 2*2, 2*3, 2*2
rm(list = ls())
library(stringr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(cowplot)
library(data.table)
library(pheatmap)
library(patchwork)

# set parameters ========================================================
# file list ----------------------------------------------------------
file_pattern_list <- list()
# file_pattern_list[["CVLT_TGMV"]] <- "CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8"  # -----> Modify!
# file_pattern_list[["ReHo_Gender"]] <- "HarRehoGender_((True|\\d+)(M|m)iss)_NoRep0.8"

file_pattern_list[["CVLT_Y1"]] <- "CVLT_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8"
file_pattern_list[["ReHo_Y2"]] <- "HarRehoGenderSimuY_((True|\\d+)(M|m)iss)_NoRep0.8"

# file_pattern_list[["CVLT_TGMV"]] <- "CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8"
# file_pattern_list[["ReHo_Age"]] <- "HarRehoAge_((True|\\d+)(M|m)iss)_NoRep0.8"

# file_pattern_list[["CVLT_Y1"]] <- "CVLT_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8"
# file_pattern_list[["ReHo_Y2"]] <- "HarRehoAgeSimuY_((True|\\d+)(M|m)iss)_NoRep0.8"

# out dir ------------------------------------------------------------
tar_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.6.1_JournalOutComplete"

# out_dir <- file.path(tar_dir, "CVLTTGMV_ReHoGender_3x2")  # -------------------------------> Modify!
out_dir <- file.path(tar_dir, "CVLTTGMVY1_ReHoGenderY2_3x2")
# out_dir <- file.path(tar_dir, "CVLTTGMV_ReHoAge")
# out_dir <- file.path(tar_dir, "CVLTTGMVY1_ReHoAgeY2")

fig_out_dir <- file.path(out_dir, "Fig")
data_out_dir <- file.path(out_dir, "Data")

# others --------------------------------------------------------------
missrate_level <- c("TrueMiss", "20Miss", "40Miss", "60Miss", "80Miss")
method_level <- c("Complete", "CCA", "Mean", "Pred", "EM", "PMM")
color_list <- c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#2745a2", "#F564E3")

# mkdir out dir =========================================================
for (out_dir_i in c(fig_out_dir, data_out_dir)) {
  if (!file.exists(out_dir_i)) {
    dir.create(out_dir_i, recursive = TRUE)
    cat(sprintf("Create out dir! %s\n", out_dir_i))
  }
}

# figure 1, impute value, NRMSE and PCC, line chart ====================
# mr is missing_rate, met is method, sc is scale
if (T) {
  cat(sprintf("=================== Fig1 impute value compare ====================\n"))
  # set parameters
  {
    source_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1.1_summary_data/"
    subdir_name <- "imputed_value"

    fig_out_path <- file.path(fig_out_dir, "ImpValueCompare_Data.pdf")
    NRMSE_csv_save_path <- file.path(data_out_dir, "ALL_ImpValue_NRMSE.csv")
    PCC_csv_save_path <- file.path(data_out_dir, "ALL_ImpValue_PCC.csv")
  }

  # run in each data pattern
  NRMSE_list_all <- list()
  PCC_list_all <- list()
  gg_list_all <- list()
  for (save_name in names(file_pattern_list)) {
    sprintf("%s...\n", save_name) %>% cat()
    # load data
    {
      data_list <- list()
      for (file_name in dir(source_dir)) {
        if (str_detect(file_name, file_pattern_list[[save_name]])) {
          print(file_name)
          missing_rate <- str_match(file_name, file_pattern_list[[save_name]])[2]

          subdir_path <- file.path(source_dir, file_name, subdir_name)
          subfile_path <- file.path(subdir_path, "ImpValueCompare.Rdata")

          data_load_name <- load(subfile_path)
          eval(parse(text = sprintf("data_list[['%s']]  <- %s", missing_rate, data_load_name)))
        }
      }
    }

    # remove NA and Inf, get summary
    # NRMSE
    {
      NRMSE_list_all[[save_name]] <- list()
      for (mr in names(data_list)) {
        for (met in names(data_list[[mr]][["NRMSE_raw"]])) {
          for (sc in names(data_list[[mr]][["NRMSE_raw"]][[met]])) {
            boot_mean <- sapply(data_list[[mr]][["NRMSE_raw"]][[met]][[sc]], function(boot_v) {
              boot_v <- unlist(boot_v)
              boot_v[is.infinite(boot_v)] <- NA
              return(boot_v)
            }) %>% apply(2, mean, na.rm = TRUE)
            NRMSE_list_all[[save_name]][[mr]][[met]][[sc]] <- data.frame(mean = mean(boot_mean, na.rm = T), sd = sd(boot_mean, na.rm = T))
          }
        }
      }


      NRMSE_info_melt <- reshape2::melt(NRMSE_list_all[[save_name]])
      colnames(NRMSE_info_melt) <- c("stat", "value", "scale", "method", "missrate")
      NRMSE_info_melt["scale"] <- save_name

      NRMSE_info_mr <- spread(NRMSE_info_melt, missrate, value)
    }

    # PCC
    {
      PCC_list_all[[save_name]] <- list()
      for (mr in names(data_list)) {
        for (met in names(data_list[[mr]][["Cor_raw"]])) {
          for (sc in names(data_list[[mr]][["Cor_raw"]][[met]])) {
            boot_mean <- sapply(data_list[[mr]][["Cor_raw"]][[met]][[sc]], function(boot_v) {
              boot_v <- unlist(boot_v)
              boot_v[is.infinite(boot_v)] <- NA
              return(boot_v)
            }) %>% apply(2, mean, na.rm = T)
            PCC_list_all[[save_name]][[mr]][[met]][[sc]] <- data.frame(mean = mean(boot_mean, na.rm = T), sd = sd(boot_mean, na.rm = T))
          }
        }
      }

      PCC_info_melt <- reshape2::melt(PCC_list_all[[save_name]])
      colnames(PCC_info_melt) <- c("stat", "value", "scale", "method", "missrate")
      PCC_info_melt["scale"] <- save_name

      PCC_info_mr <- spread(PCC_info_melt, missrate, value)
    }

    # figure
    {
      # NRMSE
      {
        # adjust
        {
          NRMSE_info_stat <- spread(NRMSE_info_melt, stat, value)

          NRMSE_info_stat[, "missrate"] <- factor(NRMSE_info_stat[, "missrate"], levels = missrate_level)

          NRMSE_info_stat[which(NRMSE_info_stat[, "method"] == "mice_mean"), "method"] <- "Mean"
          NRMSE_info_stat[which(NRMSE_info_stat[, "method"] == "mice_norm_pred"), "method"] <- "Pred"
          NRMSE_info_stat[which(NRMSE_info_stat[, "method"] == "vim_em"), "method"] <- "EM"
          NRMSE_info_stat[which(NRMSE_info_stat[, "method"] == "mice_pmm"), "method"] <- "PMM"
          NRMSE_info_stat[, "method"] <- factor(NRMSE_info_stat[, "method"], levels = method_level)
        }

        # ggplot line
        gg_list_all[["NRMSE"]][[save_name]] <-
          ggplot(NRMSE_info_stat, aes(x = missrate, y = mean, fill = method)) +
          geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
            size = 1,
            width = 0.05,
            linewidth = 0.5
          ) +
          geom_line(aes(group = method, color = method), linewidth = 0.6) +
          geom_point(aes(group = method), shape = 21, size = 3) +
          scale_fill_manual(breaks = method_level, values = color_list) +
          scale_color_manual(breaks = method_level, values = color_list) +
          labs(x = "Subject Miss Rate", y = "Imputation Value NRMSE", title = save_name, fill = "Method") +
          guides(fill = guide_legend(reverse = FALSE)) +
          theme_classic() +
          theme(
            legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 12)
          )
      }

      # PCC
      {
        # adjust
        {
          PCC_info_stat <- spread(PCC_info_melt, stat, value)
          PCC_info_stat <- PCC_info_stat[complete.cases(PCC_info_stat), ]

          PCC_info_stat[, "missrate"] <- factor(PCC_info_stat[, "missrate"], levels = missrate_level)

          PCC_info_stat[which(PCC_info_stat[, "method"] == "mice_mean"), "method"] <- "Mean"
          PCC_info_stat[which(PCC_info_stat[, "method"] == "mice_norm_pred"), "method"] <- "Pred"
          PCC_info_stat[which(PCC_info_stat[, "method"] == "vim_em"), "method"] <- "EM"
          PCC_info_stat[which(PCC_info_stat[, "method"] == "mice_pmm"), "method"] <- "PMM"
          PCC_info_stat[, "method"] <- factor(PCC_info_stat[, "method"], levels = method_level)
        }

        # ggplot line
        gg_list_all[["PCC"]][[save_name]] <-
          ggplot(PCC_info_stat, aes(x = missrate, y = mean, fill = method)) +
          geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
            size = 1,
            width = 0.05,
            linewidth = 0.5
          ) +
          geom_line(aes(group = method, color = method), linewidth = 0.6) +
          geom_point(aes(group = method), shape = 21, size = 3) +
          scale_fill_manual(breaks = method_level, values = color_list) +
          scale_color_manual(breaks = method_level, values = color_list) +
          labs(x = "Subject Miss Rate", y = "Imputation Value PCC", title = save_name, fill = "Method") +
          guides(fill = guide_legend(reverse = FALSE)) +
          theme_classic() +
          theme(
            legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 12)
          )
      }
    }
  }

  # combine figure, save
  {
    # gg_comb <- (gg_list_all[["NRMSE"]][[1]] + gg_list_all[["PCC"]][[1]]) /
    #   (gg_list_all[["NRMSE"]][[2]] + gg_list_all[["PCC"]][[2]]) +
    #   plot_layout(guides = "collect") +
    #   plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

    gg_comb <- (gg_list_all[["NRMSE"]][[1]] + gg_list_all[["NRMSE"]][[2]]) /
      (gg_list_all[["PCC"]][[1]] + gg_list_all[["PCC"]][[2]]) +
      plot_layout(guides = "collect") +
      plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

    ggsave(fig_out_path,
      plot = gg_comb, device = NULL, path = NULL,
      scale = 1, width = 12, height = 8, units = "in",
      dpi = 300, limitsize = TRUE
    )
    sprintf("Combine figure saved: %s\n", fig_out_path) %>% cat()
  }

  # csv
  {
    # melt data
    NRMSE_reshape_all <- reshape2::melt(NRMSE_list_all) %>% spread(L2, value)
    colnames(NRMSE_reshape_all)[1:4] <- c("stat", "scale", "method", "scale_save_name")

    PCC_reshape_all <- reshape2::melt(PCC_list_all) %>% spread(L2, value)
    colnames(PCC_reshape_all)[1:4] <- c("stat", "scale", "method", "scale_save_name")

    write.csv(NRMSE_reshape_all, file = NRMSE_csv_save_path)
    write.csv(PCC_reshape_all, file = PCC_csv_save_path)

    sprintf("csv saved: %s\n", NRMSE_csv_save_path) %>% cat()
    sprintf("csv saved: %s\n", PCC_csv_save_path) %>% cat()
  }
}

# figure 2, model PB, CR and AW, line chart ============================
if (T) {
  cat(sprintf("=================== Fig2 impute model compare ====================\n"))
  # set parameter
  {
    # source_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2_compare_summary/"
    source_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2.1_compare_summary_Complete/"
    mis_pattern <- "(True|\\d+)(M|m)iss"
    indicator_name_list <- c("percent_bias", "coverage_rate", "AW_per_bias")
    out_name_list <- list(
      "percent_bias" = "PB",
      "coverage_rate" = "CR",
      "AW_per_bias" = "AW"
    )

    fig_out_path <- file.path(fig_out_dir, "ImpModelCompare_Data.pdf")
  }

  # run in each pattern
  Indicator_list_all <- list()
  gg_list_all <- list()

  for (indicator_name in indicator_name_list) {
    for (save_name in names(file_pattern_list)) {
      # get data, mean and std
      for (file_name in dir(source_dir)) {
        if (str_detect(file_name, file_pattern_list[[save_name]])) {
          cat(sprintf("%s: %s\n", save_name, file_name))
          # load data
          {
            mr <- str_match(file_name, file_pattern_list[[save_name]])[2]
            res_data_path <- file.path(source_dir, file_name, "compare_result.RData")
            load(res_data_path) # name is compare_result_method
          }

          # get mean and std
          {
            for (met in names(compare_result_method)) {
              for (sc in names(compare_result_method[[met]][["compare_result"]])) {
                # remove intercept, so using [-1]
                boot_mean <- sapply(compare_result_method[[met]][["compare_result"]][[sc]], function(boot) {
                  mean(boot[[indicator_name]][-1])
                })
                Indicator_list_all[[save_name]][[mr]][[met]][[sc]] <-
                  data.frame(mean = mean(boot_mean, na.rm = T), sd = sd(boot_mean, na.rm = T))
              }
            }
          }
        }
      }

      # melt, adjudt
      {
        for (ind in indicator_name_list) {
          Indicator_melt <- reshape2::melt(Indicator_list_all[[save_name]])
          colnames(Indicator_melt) <- c("stat", "value", "scale", "method", "missrate")
          Indicator_melt <- Indicator_melt[-which(Indicator_melt[, "scale"] == "__aux"), ]

          Indicator_melt["scale"] <- save_name

          Indicator_mr <- spread(Indicator_melt, missrate, value)
        }
      }

      # figure
      {
        # adjust
        {
          Indicator_stat <- spread(Indicator_melt, stat, value)

          Indicator_stat[, "missrate"] <- factor(Indicator_stat[, "missrate"], levels = missrate_level)

          Indicator_stat[which(Indicator_stat[, "method"] == "Imp_Method__mice_mean__"), "method"] <- "Mean"
          Indicator_stat[which(Indicator_stat[, "method"] == "Imp_Method__mice_norm_pred__"), "method"] <- "Pred"
          Indicator_stat[which(Indicator_stat[, "method"] == "Imp_Method__vim_em__"), "method"] <- "EM"
          Indicator_stat[which(Indicator_stat[, "method"] == "Imp_Method__mice_pmm__"), "method"] <- "PMM"
          Indicator_stat[which(Indicator_stat[, "method"] == "Imp_Method__CCA__"), "method"] <- "CCA"
          Indicator_stat[, "method"] <- factor(Indicator_stat[, "method"], levels = method_level)
        }

        # ggplot bar
        gg_list_all[[indicator_name]][[save_name]] <-
          ggplot(Indicator_stat, aes(x = missrate, y = mean, fill = method)) +
          geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
            size = 1,
            width = 0.05,
            linewidth = 0.5
          ) +
          geom_line(aes(group = method, color = method), linewidth = 0.6) +
          geom_point(aes(group = method), shape = 21, size = 3) +
          scale_fill_manual(breaks = method_level, values = color_list) +
          scale_color_manual(breaks = method_level, values = color_list) +
          labs(
            x = "Subject Miss Rate",
            y = sprintf("Model %s", out_name_list[[indicator_name]]),
            title = save_name,
            fill = "Method"
          ) +
          guides(fill = guide_legend(reverse = FALSE)) +
          theme_classic() +
          theme(
            legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 12)
          )

        if (indicator_name == "coverage_rate"){
          # add 95% CR hline in figure
          gg_list_all[[indicator_name]][[save_name]]  <-
            gg_list_all[[indicator_name]][[save_name]] +
            geom_hline(yintercept = 0.95, linetype = "dashed", linewidth = 0.5)
        }
      }
    }

    # save csv
    {
      Indicator_list_all_melt <- reshape2::melt(Indicator_list_all)
      Indicator_list_all_melt <- Indicator_list_all_melt[-which(Indicator_list_all_melt[, "L4"] == "__aux"), ]
      colnames(Indicator_list_all_melt) <- c("stat", "value", "scale", "method", "missrate", "scale_save_name")
      Indicator_list_all_mr <- spread(Indicator_list_all_melt, missrate, value)

      csv_save_path <- file.path(data_out_dir, sprintf("Combine_Model_Mean_%s.csv", indicator_name))
      write.csv(Indicator_list_all_mr, file = csv_save_path)
      sprintf("csv saved! %s\n", csv_save_path) %>% cat()
    }
  }


  # combine figure, save (by patchwork)
  {
    # gg_PBNED_comb <- (gg_list_all[[1]][[1]] + gg_list_all[[2]][[1]] + gg_list_all[[3]][[1]]) /
    #   (gg_list_all[[1]][[2]] + gg_list_all[[2]][[2]] + gg_list_all[[3]][[2]]) +
    #   plot_layout(guides = "collect") +
    #   plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

    gg_PBNED_comb <- (gg_list_all[[1]][[1]] + gg_list_all[[1]][[2]]) /
      (gg_list_all[[2]][[1]] + gg_list_all[[2]][[2]]) /
      (gg_list_all[[3]][[1]] + gg_list_all[[3]][[2]]) +
      plot_layout(guides = "collect") +
      plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

    ggsave(fig_out_path,
      plot = gg_PBNED_comb, device = NULL, path = NULL,
      scale = 1, width = 12, height = 12, units = "in",
      dpi = 300, limitsize = T
    )
    cat(sprintf("fig out: %s\n", fig_out_path))
  }
}

# fig 3, predict MAE, PCC, line chart ==================================
if (T) {
  cat(sprintf("=================== Fig3 model predict compare ===================="))
  # set parameter
  {
    source_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1.1_summary_data/"

    fig_out_path <- file.path(fig_out_dir, "PredictValueCompare_Data.pdf")
    subdir_name <- "cv_res"
  }

  # run in each data pattern
  Ind_list_all <- list()
  for (save_name in names(file_pattern_list)) {
    sprintf("%s...\n", save_name) %>% cat()
    # load data
    data_list <- list()
    for (file_name in dir(source_dir)) {
      if (str_detect(file_name, file_pattern_list[[save_name]])) {
        print(file_name)
        missing_rate <- str_match(file_name, file_pattern_list[[save_name]])[2]

        subdir_path <- file.path(source_dir, file_name, subdir_name)
        subfile_path <- file.path(subdir_path, "CrossValidationBootResult.RData")

        load(subfile_path) # name is imp_cv_compare_boot_list

        # get summary
        Ind_list_all[[save_name]][[missing_rate]] <- lapply(imp_cv_compare_boot_list, function(met) {
          lapply(met, function(sc) {
            cv_res <- sapply(sc, function(boot) {
              boot
            })
            res_mean <- apply(cv_res, 1, function(ind) {
              data.frame(mean = mean(ind), sd = sd(ind))
            })
          })
        })
      }
    }
  }

  # melt
  {
    Ind_list_stat <- reshape2::melt(Ind_list_all) %>% spread(variable, value)
    colnames(Ind_list_stat)[1:5] <- c("Indicator", "scale", "method", "missrate", "scale_save_name")
    Ind_list_stat[which(Ind_list_stat[, "method"] == "mice_mean"), "method"] <- "Mean"
    Ind_list_stat[which(Ind_list_stat[, "method"] == "mice_norm_pred"), "method"] <- "Pred"
    Ind_list_stat[which(Ind_list_stat[, "method"] == "vim_em"), "method"] <- "EM"
    Ind_list_stat[which(Ind_list_stat[, "method"] == "mice_pmm"), "method"] <- "PMM"
    Ind_list_stat[which(Ind_list_stat[, "method"] == "complete"), "method"] <- "Complete"

    Ind_list_stat[which(Ind_list_stat[, "Indicator"] == "cor"), "Indicator"] <- "PCC"

    Ind_list_stat[, "method"] <- factor(Ind_list_stat[, "method"], levels = method_level)
    Ind_list_stat[, "missrate"] <- factor(Ind_list_stat[, "missrate"], levels = missrate_level)
  }

  # get figure and save csv
  {
    gg_list_all <- list()
    gg_combine_list <- list()
    for (compare_ind in unique(Ind_list_stat[, "Indicator"])) {
      for (scale_name in unique(Ind_list_stat[, "scale_save_name"])) {
        Ind_list_stat_choosed <- filter(Ind_list_stat, Indicator == compare_ind, scale_save_name == scale_name)

        gg_list_all[[compare_ind]][[scale_name]] <-
          ggplot(Ind_list_stat_choosed, aes(x = missrate, y = mean, fill = method)) +
          geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
            size = 1,
            width = 0.05,
            linewidth = 0.5
          ) +
          geom_line(aes(group = method, color = method), linewidth = 0.6) +
          geom_point(aes(group = method), shape = 21, size = 3) +
          scale_fill_manual(breaks = method_level, values = color_list) +
          scale_color_manual(breaks = method_level, values = color_list) +
          labs(x = "Subject Miss Rate", y = sprintf("Predict %s", compare_ind), title = scale_name, fill = "Method") +
          guides(fill = guide_legend(reverse = F)) +
          theme_classic() +
          theme(
            legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 20),
            axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 12)
          )
      }

      # save csv
      {
        Ind_list_stat_saved <- filter(Ind_list_stat, Indicator == compare_ind)
        Ind_list_stat_saved_mr <- reshape2::melt(Ind_list_stat_saved) %>% spread(missrate, value)
        colnames(Ind_list_stat_saved_mr)[5] <- "stat"

        Ind_csv_save_path <- file.path(data_out_dir, sprintf("Predict_%s_combine.csv", compare_ind))
        write.csv(Ind_list_stat_saved_mr, file = Ind_csv_save_path)
        sprintf("CSV saved: %s\n", Ind_csv_save_path) %>% cat()
      }
    }

    # gg_combine <- (gg_list_all[["RMSE"]][[1]] + gg_list_all[["PCC"]][[1]]) /
    #   (gg_list_all[["RMSE"]][[2]] + gg_list_all[["PCC"]][[2]]) +
    #   plot_layout(guides = "collect") +
    #   plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

    gg_combine <- (gg_list_all[["MAE"]][[1]] + gg_list_all[["MAE"]][[2]]) /
      (gg_list_all[["PCC"]][[1]] + gg_list_all[["PCC"]][[2]]) +
      plot_layout(guides = "collect") +
      plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

    # save figure
    ggsave(fig_out_path,
      plot = gg_combine, device = NULL, path = NULL,
      scale = 1, width = 12, height = 8, units = "in",
      dpi = 300, limitsize = TRUE
    )
    sprintf("fig saved: %s\n", fig_out_path) %>% cat()
  }
}
