# 2022.12.03 stat
# Ind_list_stat is not storing same levels in Fig1, 2, 3
# =====================================================================
# ==========> MUST RUN IN ggplot < 3.3.3 (my conda env R1) <===========
# =====================================================================
# (Fig 1 only plot in ggplot v < 3.3.3)
library(stringr)
library(dplyr)
library(logging)
library(tidyverse)
library(car)
library(ggplot2)
library(ggsci)
library(patchwork)

# def some functions =============================================
replace_col_by_list <- function(data, replace_col, replace_list) {
  # convert load names to output names
  for (raw_i in names(replace_list)) {
    data[which(data[, replace_col] == raw_i), replace_col] <- replace_list[raw_i]
  }
  return(data)
}

get_pair_test_result_df <- function(data, stat_method = "w") {
  # stat_method must in ["w", "t"], "w" is wilcox.test, and "t" is t.test
  # using sort to make the rank of method names is same as special order
  method_comb <- combn(sort(unique(data$method), decreasing = TRUE), 2)

  t_test_col_names <- c(
    "method1", "method2", "met1_mean", "met2_mean", "stat", "p",
    "CI1", "CI2", "mean_diff", "sig_lv", "stat_met"
  )
  test_res <- matrix(
    nrow = ncol(method_comb), ncol = length(t_test_col_names),
    dimnames = list(c(), t_test_col_names)
  ) %>% as.data.frame()

  for (comb_i in seq_len(ncol(method_comb))) {
    paired_data <- dplyr::filter(data, method %in% method_comb[, comb_i])

    if (stat_method == "t") {
      is_var_eq <- leveneTest(value ~ method, data = paired_data)["Pr(>F)"] > 0.05
      t_res <- t.test(value ~ method,
        data = paired_data,
        alternative = "two.sided", paired = TRUE, var.equal = is_var_eq
      )

      test_res[comb_i, "method1"] <- method_comb[1, comb_i] %>% as.character()
      test_res[comb_i, "method2"] <- method_comb[2, comb_i] %>% as.character()
      test_res[comb_i, "met1_mean"] <- mean(dplyr::filter(paired_data, method == method_comb[1, comb_i])[, "value"])
      test_res[comb_i, "met2_mean"] <- mean(dplyr::filter(paired_data, method == method_comb[2, comb_i])[, "value"])
      test_res[comb_i, "stat"] <- t_res$statistic
      test_res[comb_i, "p"] <- t_res$p.value
      test_res[comb_i, "CI1"] <- t_res$conf.int[[1]]
      test_res[comb_i, "CI2"] <- t_res$conf.int[[2]]
      test_res[comb_i, "mean_diff"] <- t_res$estimate
      test_res[comb_i, "sig_lv"] <- (t_res$p.value < 0.05)
      test_res[comb_i, "sign"] <- ifelse(t_res$statistic > 0, "+", "-")
      test_res[comb_i, "stat_met"] <- "pair_t_test"
    } else if (stat_method == "w") {
      test_out <- wilcox.test(value ~ method, data = paired_data, paired = TRUE)

      test_res[comb_i, "method1"] <- method_comb[1, comb_i] %>% as.character()
      test_res[comb_i, "method2"] <- method_comb[2, comb_i] %>% as.character()
      test_res[comb_i, "met1_mean"] <- mean(dplyr::filter(paired_data, method == method_comb[1, comb_i])[, "value"])
      test_res[comb_i, "met2_mean"] <- mean(dplyr::filter(paired_data, method == method_comb[2, comb_i])[, "value"])
      test_res[comb_i, "stat"] <- test_out$statistic
      test_res[comb_i, "p"] <- test_out$p.value
      test_res[comb_i, "CI1"] <- ""
      test_res[comb_i, "CI2"] <- ""
      test_res[comb_i, "mean_diff"] <- test_res[comb_i, "met1_mean"] - test_res[comb_i, "met2_mean"]
      test_res[comb_i, "sig_lv"] <- (test_out$p.value < 0.05)
      if (is.na(test_out$p.value)) {
        test_res[comb_i, "sign"] <- "E"
      } else {
        test_res[comb_i, "sign"] <- ifelse(test_res[comb_i, "mean_diff"] > 0, "+", "-")
      }
      test_res[comb_i, "stat_met"] <- "pair_wilcox"
    } else {
      stop(sprintf("Unsupport stat method: %s", stat_method))
    }
  }

  return(test_res)
}

get_anova_test_result_df <- function(data) {
  # must to filter indicator, save_name or others before input data
  anova_col_names <- c("method", "f", "p", "sig_lv")
  used_methods <- data$method %>%
    unique() %>%
    sort()

  anova_res <- matrix(
    nrow = length(used_methods), ncol = length(anova_col_names),
    dimnames = list(c(), anova_col_names)
  ) %>% as.data.frame()

  met_ind <- 1
  for (met_i in used_methods) {
    loginfo(sprintf("One-way ANOAV across missing rate, %s", met_i))
    test_data <- dplyr::filter(data, method == met_i)

    # one-way anova
    aov_out <- aov(value ~ missrate, data = test_data)
    aov_out_summary <- summary(aov_out)

    anova_res[met_ind, "method"] <- met_i
    anova_res[met_ind, "f"] <- aov_out_summary[[1]][1, "F value"]
    anova_res[met_ind, "p"] <- aov_out_summary[[1]][1, "Pr(>F)"]
    anova_res[met_ind, "sig_lv"] <- (aov_out_summary[[1]][1, "Pr(>F)"] < 0.05)

    met_ind <- met_ind + 1
  }

  return(anova_res)
}

# set parameters =================================================
{
  file_pattern_list <- list()
  file_pattern_list[["CVLT_TGMV"]] <- "CVLT_ZsHarQCT1_((True|\\d+)(M|m)iss)_NoRep0.8" # -----> Modify!
  file_pattern_list[["ReHo_Gender"]] <- "HarRehoGender_((True|\\d+)(M|m)iss)_NoRep0.8"

  # file_pattern_list[["CVLT_Y1"]] <- "CVLT_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8"
  # file_pattern_list[["ReHo_Y2"]] <- "HarRehoGenderSimuY_((True|\\d+)(M|m)iss)_NoRep0.8"

  tar_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.6.1_JournalOutComplete"

  # out_dir <- file.path(tar_dir, "CVLTTGMV_ReHoGender_3x2")  # --------------------------------> Modify!!!
  # out_dir <- file.path(tar_dir, "CVLTTGMVY1_ReHoGenderY2_3x2")
  out_dir <- file.path(tar_dir, "CVLTTGMV_ReHoGender_3x2_wilcox")
  # out_dir <- file.path(tar_dir, "CVLTTGMVY1_ReHoGenderY2_3x2_wilcox")

  fig_out_dir <- file.path(out_dir, "Fig")
  data_out_dir <- file.path(out_dir, "Data")

  logging_out_path <- file.path(out_dir, "logging.txt")

  # others --------------------------------------------------------------
  missrate_level <- c("TrueMiss", "20Miss", "40Miss", "60Miss", "80Miss")
  method_level <- c("Complete", "CCA", "Mean", "Pred", "EM", "MI") # MI is PMM
  color_list <- c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#2745a2", "#F564E3")

  # Indicator*method: value(4+3=7), model(3*5=15), pred(2*6=12), sum=7+15+12=34
  # stat times = indicator * method * missrate * dataset = 34 * 5 * 4 = 680
  # t test logp thre = -log(0.05 / 680, 10) = 4.133539
  t_test_logp_thre <- -log(0.05 / 680, 10)

  stat_method1 <- "w" # ["t", "w"]
  stat_fig_lim <- c(0, 20)

  stat_save_name_list <- list(
    "t" = "Ttest",
    "w" = "Wilcox"
  )
  stat_save_name_1 <- stat_save_name_list[[stat_method1]]
}

# mkdir out dir ==================================================
for (out_dir_i in c(out_dir, fig_out_dir, data_out_dir)) {
  if (!file.exists(out_dir_i)) {
    dir.create(out_dir_i, recursive = TRUE)
    sprintf("Create out dir! %s\n", out_dir_i) %>% cat()
  }
}

# logging setting ================================================
basicConfig()
addHandler(writeToFile, file = logging_out_path, level = "DEBUG")
loginfo("=============== RUN START ===============")

# figure 1, impute value, NRMSE and PCC, -log(p) heatmap ====================
# mr is missing_rate, met is method, sc is scale
if (T) {
  loginfo(sprintf("=================== Fig1 Stat impute value compare ====================\n"))
  # set parameters
  {
    source_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1.1_summary_data"
    subdir_name <- "imputed_value"
    ind_names <- c("NRMSE_raw", "Cor_raw")
    out_ind_names <- c("NRMSE", "PCC")

    fig_ttest_out_path <- sprintf(file.path(fig_out_dir, "ImpValueCompare_%s.pdf"), stat_save_name_1)
    ttest_csv_out_path <- sprintf(file.path(data_out_dir, "ImpValueCompare_Pair%s_Method.csv"), stat_save_name_1)
    anova_csv_out_path <- file.path(data_out_dir, "ImpValueCompare_AnovaMissingRate.csv")

    method_name_fig1_list <- list(
      "complete" = "Complete",
      "mice_mean" = "Mean",
      "mice_norm_pred" = "Pred",
      "mice_pmm" = "MI",
      "vim_em" = "EM"
    )

    indicator_name_fig1_list <- list(
      "Cor_raw" = "PCC",
      "NRMSE_raw" = "NRMSE"
    )
  }

  # run in each data pattern =========================================
  # load data, stat by t test and anova, ggplot to get figure
  ind_list_all <- list()
  t_test_all_list <- list()
  anova_all_list <- list()
  for (save_name in names(file_pattern_list)) {
    # debug for dplyr. if filter colnames is same as varible name, filter will not be applied
    save_name_x <- save_name
    loginfo(sprintf("%s...\n", save_name))
    ind_list_all[[save_name]] <- list()
    # load data ==========================================================
    {
      data_list <- list()
      for (file_name in dir(source_dir)) {
        if (str_detect(file_name, file_pattern_list[[save_name]])) {
          loginfo(sprintf("load file: %s", file_name))
          missing_rate <- str_match(file_name, file_pattern_list[[save_name]])[2]

          subdir_path <- file.path(source_dir, file_name, subdir_name)
          subfile_path <- file.path(subdir_path, "ImpValueCompare.Rdata")

          data_load_name <- load(subfile_path)
          eval(parse(text = sprintf("data_list[['%s']]  <- %s", missing_rate, data_load_name)))
        }
      }
    }

    # get boot results ====================================================
    for (ind_i in ind_names) {
      # get boot list
      ind_list_all[[save_name]][[ind_i]] <- list()
      for (mr in names(data_list)) {
        for (met in names(data_list[[mr]][[ind_i]])) {
          for (sc in names(data_list[[mr]][[ind_i]][[met]])) {
            boot_mean <- sapply(data_list[[mr]][[ind_i]][[met]][[sc]], function(boot_v) {
              boot_v <- unlist(boot_v)
              boot_v[is.infinite(boot_v)] <- NA
              return(boot_v)
            }) %>% apply(2, mean, na.rm = TRUE)

            ind_list_all[[save_name]][[ind_i]][[mr]][[met]][[sc]] <- boot_mean
          }
        }
      }

      # melt
      {
        Ind_list_stat <- reshape2::melt(ind_list_all)
        colnames(Ind_list_stat) <- c("value", "scale", "method", "missrate", "indicator", "save_name")

        Ind_list_stat <- replace_col_by_list(Ind_list_stat, "method", method_name_fig1_list)
        Ind_list_stat <- replace_col_by_list(Ind_list_stat, "indicator", indicator_name_fig1_list)

        Ind_list_stat[, "method"] <- factor(Ind_list_stat[, "method"], levels = method_level)
        Ind_list_stat[, "missrate"] <- factor(Ind_list_stat[, "missrate"], levels = missrate_level)
      }

      # remove mean in PCC
      Ind_list_stat <- dplyr::filter(Ind_list_stat, !(indicator == "PCC" & method == "Mean"))

      # checking NA
      if (any(is.na(Ind_list_stat$value))) {
        logwarn(sprintf("NA in %s! number: %d", ind_i, length(which(is.na(Ind_list_stat$value)))))
      } else if (any(is.infinite(Ind_list_stat$value))) {
        logwarn(sprintf("Inf in %s! number: %d", ind_i, length(which(is.infinite(Ind_list_stat$value)))))
      }
    }

    # Statistical test =====================================================
    {
      # pair t test in each missing rate ------------------------------------
      {
        stat_list <- list()
        for (ind_i in out_ind_names) {
          # pair t test across method in each missing rate
          stat_list[[ind_i]] <- list()
          for (mr_i in missrate_level) {
            loginfo(sprintf("Pair t test, %s, %s", ind_i, mr_i))
            test_data <- dplyr::filter(Ind_list_stat, missrate == mr_i, indicator == ind_i, save_name == save_name_x)
            stat_list[[ind_i]][[mr_i]] <- get_pair_test_result_df(test_data, stat_method = stat_method1)
          }
        }

        # get melt df, and generate derived variables
        stat_list_melt <- lapply(stat_list, dplyr::bind_rows, .id = "missrate") %>%
          dplyr::bind_rows(., .id = "indicator")
        stat_list_melt <- mutate(stat_list_melt, logp = abs(-log(p, 10)))  # -logp may get -0.00, so using abs for plot

        t_test_all_list[[save_name]] <- stat_list_melt # for save csv
      }

      # One-Way ANOVA across missing rates for each method -------------------------
      {
        # get ANOVA list
        mr_anova_stat_list <- list()
        for (ind_i in out_ind_names) {
          anova_data <- dplyr::filter(Ind_list_stat, indicator == ind_i, save_name == save_name_x)
          mr_anova_stat_list[[ind_i]] <- get_anova_test_result_df(anova_data)
        }

        # get melt df, and generate derived variables
        anova_list_melt <- dplyr::bind_rows(mr_anova_stat_list, .id = "indicator")
        anova_list_melt <- mutate(anova_list_melt, logp = -log(p, 10))

        anova_all_list[[save_name]] <- anova_list_melt
      }
    }
  }

  # csv, save =========================================================
  {
    # list to df
    ttest_df <- dplyr::bind_rows(t_test_all_list, .id = "save_name")
    anova_df <- dplyr::bind_rows(anova_all_list, .id = "save_name")

    # add sig
    ttest_df["BofSig"] <- ifelse(ttest_df["logp"] < t_test_logp_thre, "", "*")

    # melt data
    write.csv(ttest_df, file = ttest_csv_out_path, row.names = FALSE) # t test
    write.csv(anova_df, file = anova_csv_out_path, row.names = FALSE) # one-way ANOVA

    loginfo(sprintf("csv saved: %s\n", ttest_csv_out_path))
    loginfo(sprintf("csv saved: %s\n", anova_csv_out_path))
  }

  # ggplot ttest heatmap, combine and save =========================
  {
    # get base image ----------------------------------------
    gg_t_summary_list <- list() # t test
    for (save_name_i in names(file_pattern_list)) {
      # t test ---------------------------------------------

      fig_data <- dplyr::filter(ttest_df, save_name == save_name_i)
      fig_data[is.na(fig_data[, "logp"]), "logp"] <- 0

      # if average(method1) > average(method2), set to "+", else "-", and "E" means "equal"
      fig_data[is.na(fig_data[, "sign"]), "sign"] <- "E"

      # convert to factor to control image order
      fig_data[, "method1"] <- factor(fig_data[, "method1"], levels = method_level)
      fig_data[, "method2"] <- factor(fig_data[, "method2"], levels = method_level)
      fig_data[, "missrate"] <- factor(fig_data[, "missrate"], levels = missrate_level)

      # only run in ggplot v < 3.3.3 (my conda env R1)
      # space in facet_grid and aspect.ratio in theme together only running in ggplot2 < 3.3.3
      gg_t_summary_list[[save_name_i]] <-
        ggplot(fig_data, aes(x = method1, y = method2, fill = logp)) +
        geom_tile(aes(fill = ifelse(logp > stat_fig_lim[2], stat_fig_lim[2], logp)),
          width = 1, height = 1, size = 2, color = "white"
        ) +
        facet_grid(indicator ~ missrate, scales = "free", space = "free") + 
        geom_text(aes(
          x = method1, y = method2, label = sprintf("%s%s\n%.2f", BofSig, sign, logp)
        )) +
        scale_fill_material("orange", reverse = FALSE, limits = stat_fig_lim) +
        labs(x = "", y = "", fill = "-log p", title = save_name_i) +
        theme_bw(base_size = 18) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5),
          aspect.ratio = 1
        )
    }

    # combine image, and save -------------------------------
    # Please adjust output size!
    {
      gg_comb_t_test <-
        (gg_t_summary_list[[1]] / gg_t_summary_list[[2]]) +
        plot_layout(guides = "collect") +
        plot_annotation(
          tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
        )

      ggsave(fig_ttest_out_path,
        plot = gg_comb_t_test, device = NULL, path = NULL,
        scale = 1, width = 20, height = 16, units = "in",
        dpi = 300, limitsize = TRUE
      )
      loginfo(sprintf("Out: %s", fig_ttest_out_path))
    }
  }
}

# figure 2, imputed model, PB, CR, AW, -log(p) heatmap ======================
if (T) {
  loginfo(sprintf("=================== Fig2 Stat imputed model compare ====================\n"))

  # set parameter ===================================================
  {
    source_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.2.1_compare_summary_Complete/"
    mis_pattern <- "(True|\\d+)(M|m)iss"
    indicator_name_list <- c("percent_bias", "coverage_rate", "AW_per_bias")
    ind_levels <- c("PB", "CR", "AW")
    out_name_list <- list(
      "percent_bias" = "PB",
      "coverage_rate" = "CR",
      "AW_per_bias" = "AW"
    )
    method_name_list <- list(
      "Imp_Method__CCA__" = "CCA",
      "Imp_Method__mice_mean__" = "Mean",
      "Imp_Method__mice_norm_pred__" = "Pred",
      "Imp_Method__mice_pmm__" = "MI",
      "Imp_Method__vim_em__" = "EM"
    )

    fig_ttest_out_path <- sprintf(file.path(fig_out_dir, "ImpModelCompare_%s.pdf"), stat_save_name_1)
    ttest_csv_out_path <- sprintf(file.path(data_out_dir, "ImpModelCompare_Pair%s_Method.csv"), stat_save_name_1)
    anova_csv_out_path <- file.path(data_out_dir, "ImpModelCompare_AnovaMissingRate.csv")
  }

  # run in each pattern
  t_test_all_list <- list()
  anova_all_list <- list()
  gg_comb_list_all <- list()

  for (indicator_name in indicator_name_list) {
    ind_i <- out_name_list[[indicator_name]]
    ind_all_list <- list()
    for (save_name in names(file_pattern_list)) {
      save_name_x <- save_name
      # get data, boot indicators ---------------------
      for (file_name in dir(source_dir)) {
        if (str_detect(file_name, file_pattern_list[[save_name]])) {
          loginfo(sprintf("Load data: %s, %s\n", save_name, file_name))
          # load data
          {
            mr <- str_match(file_name, file_pattern_list[[save_name]])[2]
            res_data_path <- file.path(source_dir, file_name, "compare_result.RData")
            load(res_data_path) # name is compare_result_method
          }

          # get average variables indicators
          for (met in names(compare_result_method)) {
            for (sc in names(compare_result_method[[met]][["compare_result"]])) {
              # "__aux" is meaningless, do not need to log this
              if (sc != "__aux") {
                # remove intercept, so using [-1]
                ind_all_list[[save_name]][[mr]][[met]][[sc]] <-
                  sapply(compare_result_method[[met]][["compare_result"]][[sc]], function(boot) {
                    mean(boot[[indicator_name]][-1])
                  })
              }
            }
          }
        }
      }

      # melt, adjudt ------------------------------------
      {
        Ind_list_stat <- reshape2::melt(ind_all_list)
        colnames(Ind_list_stat) <- c("value", "scale", "method", "missrate", "save_name")
        Ind_list_stat <- dplyr::filter(Ind_list_stat, scale != "__aux")

        Ind_list_stat <- replace_col_by_list(Ind_list_stat, "method", method_name_list)

        Ind_list_stat[, "method"] <- factor(Ind_list_stat[, "method"], levels = method_level)
        Ind_list_stat[, "missrate"] <- factor(Ind_list_stat[, "missrate"], levels = missrate_level)
      }

      # statistical =============================================
      {
        # pair t test in each missing rate ------------------------------------
        {
          stat_list <- list()
          for (mr_i in missrate_level) {
            loginfo(sprintf("Pair t test, %s, %s", ind_i, mr_i))
            test_data <- dplyr::filter(Ind_list_stat, missrate == mr_i, save_name == save_name_x)

            stat_list[[mr_i]] <- get_pair_test_result_df(test_data, stat_method = stat_method1)
          }

          # get melt df, and generate derived variables
          stat_list_melt <- dplyr::bind_rows(stat_list, .id = "missrate")
          # -logp may get -0.00, so using abs for plot. 
          # Because when p = 1, -log(p, 10) will output -0
          stat_list_melt <- mutate(stat_list_melt, logp = abs(-log(p, 10)))

          t_test_all_list[[ind_i]][[save_name]] <- stat_list_melt # for save csv
        }

        # One-Way ANOVA across missing rates for each method -------------------------
        {
          anova_data <- dplyr::filter(Ind_list_stat, save_name == save_name_x)
          anova_res <- get_anova_test_result_df(anova_data)
          anova_res <- mutate(anova_res, logp = -log(p, 10))

          anova_all_list[[ind_i]][[save_name]] <- anova_res
        }
      }
    }
  }

  # stacking stat data, and save to csv
  {
    # pair t test
    ttest_df <- lapply(t_test_all_list, dplyr::bind_rows, .id = "save_name") %>%
      dplyr::bind_rows(., .id = "indicator")

    # anova
    anova_df <- lapply(anova_all_list, dplyr::bind_rows, .id = "save_name") %>%
      dplyr::bind_rows(., .id = "indicator")

    # add sig
    ttest_df["BofSig"] <- ifelse(ttest_df["logp"] < t_test_logp_thre, "", "*")

    # output to csv
    write.csv(ttest_df, file = ttest_csv_out_path, row.names = FALSE) # t test
    write.csv(anova_df, file = anova_csv_out_path, row.names = FALSE) # one-way ANOVA

    loginfo(sprintf("csv saved: %s\n", ttest_csv_out_path))
    loginfo(sprintf("csv saved: %s\n", anova_csv_out_path))
  }

  # ggplot test heatmap, combine and save =========================
  {
    # get base image ----------------------------------------
    gg_t_summary_list <- list() # t test
    for (save_name_i in names(file_pattern_list)) {
      # t test ---------------------------------------------
      fig_data <- dplyr::filter(ttest_df, save_name == save_name_i)
      fig_data[is.na(fig_data[, "logp"]), "logp"] <- 0

      # if average(method1) > average(method2), set to "+", else "-", and "E" means "equal"
      fig_data[is.na(fig_data[, "sign"]), "sign"] <- "E"
      fig_data[is.na(fig_data[, "BofSig"]), "BofSig"] <- ""

      fig_data[, "method1"] <- factor(fig_data[, "method1"], levels = method_level)
      fig_data[, "method2"] <- factor(fig_data[, "method2"], levels = method_level)
      fig_data[, "missrate"] <- factor(fig_data[, "missrate"], levels = missrate_level)
      fig_data[, "indicator"] <- factor(fig_data[, "indicator"], levels = ind_levels)

      gg_t_summary_list[[save_name_i]] <-
        ggplot(fig_data, aes(x = method1, y = method2, fill = logp)) +
        geom_tile(aes(fill = ifelse(logp > stat_fig_lim[2], stat_fig_lim[2], logp)),
          width = 1, height = 1, size = 2, color = "white"
        ) +
        facet_grid(indicator ~ missrate) +
        coord_equal() + # get square rather than rectangular cells
        geom_text(aes(
          x = method1, y = method2,
          label = ifelse(is.na(p), sprintf("%s", sign), sprintf("%s%s\n%.2f", BofSig, sign, logp)), 
        )) +
        scale_fill_material("orange", reverse = FALSE, limits = stat_fig_lim) +
        labs(x = "", y = "", fill = "-log p", title = save_name_i) +
        theme_bw(base_size = 18) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5)
        )
    }

    # combine image, and save -------------------------------
    {
      gg_comb_t_test <-
        (gg_t_summary_list[[1]] / gg_t_summary_list[[2]]) +
        plot_layout(guides = "collect") +
        plot_annotation(
          tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
        )

      ggsave(fig_ttest_out_path,
        plot = gg_comb_t_test, device = NULL, path = NULL,
        scale = 1, width = 20, height = 22, units = "in",
        dpi = 300, limitsize = TRUE
      )
      loginfo(sprintf("Out: %s", fig_ttest_out_path))
    }
  }
}

# fig 3, predict MAE, PCC, -log(p) heatmap ==================================
if (T) {
  loginfo(sprintf("=================== Fig3 Stat model predict compare ===================="))
  # set parameter
  {
    source_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1.1_summary_data/"
    subdir_name <- "cv_res"

    fig_ttest_out_path <- sprintf(file.path(fig_out_dir, "ImpPredictCompare_%s.pdf"), stat_save_name_1)
    ttest_csv_out_path <- sprintf(file.path(data_out_dir, "ImpPredictCompare_Pair%s_Method.csv"), stat_save_name_1)
    anova_csv_out_path <- file.path(data_out_dir, "ImpPredictCompare_AnovaMissingRate.csv")

    out_ind_names <- c("MAE", "PCC")
    # out_ind_names <- c("MAE", "PCC", "RMSE")

    method_name_fig3_list <- list(
      "complete" = "Complete",
      "mice_mean" = "Mean",
      "mice_norm_pred" = "Pred",
      "mice_pmm" = "MI",
      "vim_em" = "EM"
    )
  }

  # load data in a list ==========================================
  Ind_list_all <- list()
  for (save_name in names(file_pattern_list)) {
    loginfo(sprintf("%s...\n", save_name))
    # load data
    data_list <- list()
    for (file_name in dir(source_dir)) {
      if (str_detect(file_name, file_pattern_list[[save_name]])) {
        loginfo(sprintf("Load data: %s, %s\n", save_name, file_name))
        missing_rate <- str_match(file_name, file_pattern_list[[save_name]])[2]

        subdir_path <- file.path(source_dir, file_name, subdir_name)
        subfile_path <- file.path(subdir_path, "CrossValidationBootResult.RData")

        load(subfile_path) # name is imp_cv_compare_boot_list

        # get summary
        Ind_list_all[[save_name]][[missing_rate]] <-
          lapply(imp_cv_compare_boot_list, function(met) {
            lapply(met, function(sc) {
              cv_res <- sapply(sc, function(boot) {
                boot
              }) %>%
                t() %>%
                as.data.frame()
            })
          })
      }
    }
  }

  # melt as df ===================================================
  {
    Ind_list_stat <- reshape2::melt(Ind_list_all)
    colnames(Ind_list_stat) <- c("indicator", "value", "scale", "method", "missrate", "save_name")
    Ind_list_stat <- replace_col_by_list(Ind_list_stat, "method", method_name_fig3_list)

    Ind_list_stat[, "indicator"] <- as.character(Ind_list_stat[, "indicator"])
    Ind_list_stat[which(Ind_list_stat[, "indicator"] == "cor"), "indicator"] <- "PCC"

    Ind_list_stat[, "method"] <- factor(Ind_list_stat[, "method"], levels = method_level)
    Ind_list_stat[, "missrate"] <- factor(Ind_list_stat[, "missrate"], levels = missrate_level)
  }

  # get stat results =============================================
  {
    t_test_all_list <- list()
    anova_all_list <- list()
    for (save_name_i in names(file_pattern_list)) {
      for (ind_i in out_ind_names) {
        # pair t test in each missing rate ------------------------------------
        {
          stat_list <- list() # temp save
          for (mr_i in missrate_level) {
            loginfo(sprintf("Pair t test, %s, %s, %s", save_name_i, ind_i, mr_i))
            test_data <- dplyr::filter(
              Ind_list_stat, missrate == mr_i,
              save_name == save_name_i, indicator == ind_i
            )

            stat_list[[mr_i]] <- get_pair_test_result_df(test_data, stat_method = stat_method1)
          }

          # get melt df, and generate derived variables
          stat_list_melt <- dplyr::bind_rows(stat_list, .id = "missrate")
          stat_list_melt <- mutate(stat_list_melt, logp = -log(p, 10))

          t_test_all_list[[save_name_i]][[ind_i]] <- stat_list_melt # for save csv
        }

        # One-Way ANOVA across missing rates for each method -------------------------
        {
          loginfo(sprintf("One way ANOVA, %s, %s", save_name_i, ind_i))

          anova_data <- dplyr::filter(Ind_list_stat, save_name == save_name_i, indicator == ind_i)
          anova_res <- get_anova_test_result_df(anova_data)
          anova_res <- mutate(anova_res, logp = -log(p, 10))

          anova_all_list[[save_name_i]][[ind_i]] <- anova_res
        }
      }
    }
  }

  # melt stat results as df, and save to csv ======================
  {
    # pair t test
    ttest_df <- lapply(t_test_all_list, dplyr::bind_rows, .id = "indicator") %>%
      dplyr::bind_rows(., .id = "save_name")
    # add sig
    ttest_df["BofSig"] <- ifelse(ttest_df["logp"] < t_test_logp_thre, "", "*")

    # anova
    anova_df <- lapply(anova_all_list, dplyr::bind_rows, .id = "indicator") %>%
      dplyr::bind_rows(., .id = "save_name")

    # output to csv
    write.csv(ttest_df, file = ttest_csv_out_path, row.names = FALSE) # t test
    write.csv(anova_df, file = anova_csv_out_path, row.names = FALSE) # one-way ANOVA

    loginfo(sprintf("csv saved: %s\n", ttest_csv_out_path))
    loginfo(sprintf("csv saved: %s\n", anova_csv_out_path))
  }

  # ggplot ttest heatmap, combine and save =========================
  {
    # get base image ----------------------------------------
    gg_t_summary_list <- list() # t test
    for (save_name_i in names(file_pattern_list)) {
      # t test ---------------------------------------------

      fig_data <- dplyr::filter(ttest_df, save_name == save_name_i)
      fig_data[is.na(fig_data[, "logp"]), "logp"] <- 0

      # if average(method1) > average(method2), set to "+", else "-", and "E" means "equal"
      fig_data[is.na(fig_data[, "sign"]), "sign"] <- "E"
      fig_data[is.na(fig_data[, "BofSig"]), "BofSig"] <- ""

      fig_data[, "method1"] <- factor(fig_data[, "method1"], levels = method_level)
      fig_data[, "method2"] <- factor(fig_data[, "method2"], levels = method_level)
      fig_data[, "missrate"] <- factor(fig_data[, "missrate"], levels = missrate_level)

      gg_t_summary_list[[save_name_i]] <-
        ggplot(fig_data, aes(x = method1, y = method2, fill = logp)) +
        geom_tile(aes(fill = ifelse(logp > stat_fig_lim[2], stat_fig_lim[2], logp)),
          width = 1, height = 1, size = 2, color = "white"
        ) +
        facet_grid(indicator ~ missrate) +
        coord_equal() + # get square rather than rectangular cells
        geom_text(aes(
          x = method1, y = method2, label = sprintf("%s%s\n%.2f", BofSig, sign, logp)
        )) +
        scale_fill_material("orange", reverse = FALSE, limits = stat_fig_lim) +
        labs(x = "", y = "", fill = "-log p", title = save_name_i) +
        theme_bw(base_size = 18) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5)
        )
    }

    # combine image, and save -------------------------------
    {
      gg_comb_t_test <-
        (gg_t_summary_list[[1]] / gg_t_summary_list[[2]]) +
        plot_layout(guides = "collect") +
        plot_annotation(
          tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
        )

      ggsave(fig_ttest_out_path,
        plot = gg_comb_t_test, device = NULL, path = NULL,
        scale = 1, width = 20, height = 16, units = "in",
        dpi = 300, limitsize = TRUE
      )
      loginfo(sprintf("Out: %s", fig_ttest_out_path))
    }
  }
}

# fig 4, output Anova across missing rate by csv files =====================================
# the .csv data is output by figure 1-3
# so, must run fig 1-3 at least once before run fig 4
if (F) {
  # set parameter --------------------------------------------
  {
    source_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.6.1_JournalOutComplete"
    subdir_vec <- c("CVLTTGMVY1_ReHoGenderY2_3x2", "CVLTTGMV_ReHoGender_3x2")
    file_name_vec <- list(
      "Value" = "ImpValueCompare_AnovaMissingRate.csv",
      "Model" = "ImpModelCompare_AnovaMissingRate.csv",
      "Predict" = "ImpPredictCompare_AnovaMissingRate.csv"
    )

    ind_anova_levels <- c("NRMSE", "PCCV", "PB", "CR", "AW", "MAE", "PCCP")
    savename_anova_levels <- c("CVLT_TGMV", "ReHo_Gender", "CVLT_Y1", "ReHo_Y2")

    out_dir_name <- "All_CVLT_ReHo_TrueSimuY"
    fig_anova_out_path <- file.path(tar_dir, out_dir_name, "CVLT_ReHo_TrueSimuY_AONVA_MissingRate.pdf")
  }

  # load data --------------------------------------------
  {
    data_list <- list()
    data_n <- 1
    for (subdir_i in subdir_vec) {
      for (file_i in names(file_name_vec)) {
        # load
        csv_path <- file.path(source_dir, subdir_i, "Data", file_name_vec[file_i])
        loginfo("Load csv: %s", csv_path)
        data_loaded <- read.csv(csv_path)

        # PCCV in Value compare, and PCCP in predict compare
        if (file_i == "Value") {
          data_loaded[data_loaded[, "indicator"] == "PCC", "indicator"] <- "PCCV"
        } else if (file_i == "Predict") {
          data_loaded[data_loaded[, "indicator"] == "PCC", "indicator"] <- "PCCP"
        }

        # storing to list
        data_list[[data_n]] <- data_loaded
        data_n <- data_n + 1
      }
    }

    data_df <- dplyr::bind_rows(data_list)
  }

  # get figure and save ----------------------------------
  {
    # convert data
    data_df[, "indicator"] <- factor(data_df[, "indicator"], levels = ind_anova_levels)
    data_df[, "method"] <- factor(data_df[, "method"], levels = rev(method_level)) # for better figure
    data_df[, "save_name"] <- factor(data_df[, "save_name"], levels = savename_anova_levels)

    data_df[is.infinite(data_df[, "logp"]), "logp"] <- 320 # The maximum precision of the data type is 10^-320

    # get threshold, 3.434569, nrow = 136
    p_thre <- -log(0.05 / nrow(data_df), 10)
    data_df["sig"] <- ifelse(data_df["logp"] > p_thre, "*", "")
    loginfo(sprintf("Anova Sig, -logp threshold: %.4f", p_thre))

    # plot
    loginfo("Ploting ANOVA of method across missing rate...")
    gg_anova <-
      ggplot(data_df, aes(x = indicator, y = method, fill = logp)) +
      geom_tile(aes(fill = ifelse(logp > 100, 100, logp)),
        width = 1, height = 1, size = 2, color = "white"
      ) +
      facet_wrap("save_name") +
      coord_equal() + # get square rather than rectangular cells
      geom_text(aes(
        x = indicator, y = method, label = sprintf("%s\n%.2f", sig, logp)
      )) +
      scale_fill_material("orange", reverse = FALSE, limits = c(0, 100)) +
      labs(x = "", y = "", fill = "-log p") +
      theme_bw(base_size = 18) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )

    ggsave(fig_anova_out_path,
      plot = gg_anova, device = NULL, path = NULL,
      scale = 1, width = 12, height = 10, units = "in",
      dpi = 300, limitsize = TRUE
    )
    loginfo(sprintf("Out: %s", fig_anova_out_path))
  }
}
