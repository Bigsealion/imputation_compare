# 2022.12.03 stat
library(stringr)
library(dplyr)
library(logging)
library(tidyverse)
library(car)
library(ggplot2)
library(ggsci)
library(patchwork)

# set parameters =================================================
file_pattern_list <- list()
file_pattern_list[["CVLT_Y1"]] <- "CVLT_IntTCoefSimuY_((True|\\d+)(M|m)iss)_NoRep0.8"
file_pattern_list[["ReHo_Y2"]] <- "HarRehoGenderSimuY_((True|\\d+)(M|m)iss)_NoRep0.8"

tar_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.6.1_JournalOutComplete"

out_dir <- file.path(tar_dir, "CVLTTGMVY1_ReHoGenderY2_3x2")

fig_out_dir <- file.path(out_dir, "Fig")
data_out_dir <- file.path(out_dir, "Data")

logging_out_path <- file.path(out_dir, "logging.txt")

# others --------------------------------------------------------------
missrate_level <- c("TrueMiss", "20Miss", "40Miss", "60Miss", "80Miss")
method_level <- c("Complete", "CCA", "Mean", "Pred", "EM", "MI") # MI is PMM
color_list <- c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#2745a2", "#F564E3")

# mkdir out dir ==================================================
if (!file.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
  sprintf("Create out dir! %s\n", out_dir) %>% cat()
}

# logging setting ================================================
basicConfig()
addHandler(writeToFile, file = logging_out_path, level = "DEBUG")
loginfo("=============== RUN START ===============")

# figure 1, impute value, NRMSE and PCC, line chart ====================
# mr is missing_rate, met is method, sc is scale
if (T) {
  loginfo(sprintf("=================== Fig1 Stat impute value compare ====================\n"))
  # set parameters
  {
    source_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.1.1_summary_data"
    subdir_name <- "imputed_value"
    ind_names <- c("NRMSE_raw", "Cor_raw")
    out_ind_names <- c("NRMSE", "PCC")

    fig_out_path <- file.path(fig_out_dir, "ImpValueCompare_PairTtest_%s.pdf")
  }

  # run in each data pattern =========================================
  ind_list_all <- list()

  for (save_name in names(file_pattern_list)) {
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
        Ind_list_stat[which(Ind_list_stat[, "method"] == "mice_mean"), "method"] <- "Mean"
        Ind_list_stat[which(Ind_list_stat[, "method"] == "mice_norm_pred"), "method"] <- "Pred"
        Ind_list_stat[which(Ind_list_stat[, "method"] == "vim_em"), "method"] <- "EM"
        Ind_list_stat[which(Ind_list_stat[, "method"] == "mice_pmm"), "method"] <- "MI"
        Ind_list_stat[which(Ind_list_stat[, "method"] == "complete"), "method"] <- "Complete"

        Ind_list_stat[, "method"] <- factor(Ind_list_stat[, "method"], levels = method_level)
        Ind_list_stat[, "missrate"] <- factor(Ind_list_stat[, "missrate"], levels = missrate_level)

        Ind_list_stat[which(Ind_list_stat[, "indicator"] == "Cor_raw"), "indicator"] <- "PCC"
        Ind_list_stat[which(Ind_list_stat[, "indicator"] == "NRMSE_raw"), "indicator"] <- "NRMSE"
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
            test_data <- dplyr::filter(Ind_list_stat, missrate == mr_i, indicator == ind_i)

            # using sort to make the rank of method names is same as special order
            method_comb <- combn(sort(unique(test_data$method), decreasing = TRUE), 2)

            t_test_col_names <- c("method1", "method2", "t", "p", "sig_lv")
            t_test_res <- matrix(
              nrow = ncol(method_comb), ncol = length(t_test_col_names),
              dimnames = list(c(), t_test_col_names)
            ) %>% as.data.frame()

            for (comb_i in seq_len(ncol(method_comb))) {
              paired_data <- dplyr::filter(test_data, method %in% method_comb[, comb_i])

              is_var_eq <- leveneTest(value ~ method, data = paired_data)["Pr(>F)"] > 0.05
              t_res <- t.test(value ~ method,
                data = paired_data,
                alternative = "two.sided", paired = TRUE, var.equal = is_var_eq
              )

              t_test_res[comb_i, "method1"] <- method_comb[1, comb_i] %>% as.character()
              t_test_res[comb_i, "method2"] <- method_comb[2, comb_i] %>% as.character()
              t_test_res[comb_i, "t"] <- t_res$statistic
              t_test_res[comb_i, "p"] <- t_res$p.value
              t_test_res[comb_i, "sig_lv"] <- (t_res$p.value < 0.05)
            }

            stat_list[[ind_i]][[mr_i]] <- t_test_res
          }
        }

        # get melt df, and generate derived variables
        stat_list_melt <- lapply(stat_list, dplyr::bind_rows, .id = "missrate") %>%
          dplyr::bind_rows(., .id = "indicator")
        stat_list_melt <- mutate(stat_list_melt, logp = -log(p, 10))
      }

      # One-Way ANOVA across missing rates for each method -------------------------
      {
        mr_anova_stat_list <- list()

        for (ind_i in out_ind_names) {
          anova_col_names <- c("method", "f", "p", "sig_lv")
          used_methods <- dplyr::filter(Ind_list_stat, indicator == ind_i)$method %>%
            unique() %>%
            sort()

          anova_res <- matrix(
            nrow = length(used_methods), ncol = length(anova_col_names),
            dimnames = list(c(), anova_col_names)
          ) %>% as.data.frame()

          met_ind <- 1
          for (met_i in used_methods) {
            loginfo(sprintf("One-way ANOAV across missing rate, %s, %s", ind_i, met_i))
            test_data <- dplyr::filter(Ind_list_stat, method == met_i, indicator == ind_i)

            # one-way anova
            aov_out <- aov(value ~ missrate, data = test_data)
            aov_out_summary <- summary(aov_out)

            anova_res[met_ind, "method"] <- met_i
            anova_res[met_ind, "f"] <- aov_out_summary[[1]][1, "F value"]
            anova_res[met_ind, "p"] <- aov_out_summary[[1]][1, "Pr(>F)"]
            anova_res[met_ind, "sig_lv"] <- (aov_out_summary[[1]][1, "Pr(>F)"] < 0.05)

            met_ind <- met_ind + 1
          }

          mr_anova_stat_list[[ind_i]] <- anova_res
        }

        # get melt df, and generate derived variables
        anova_list_melt <- dplyr::bind_rows(mr_anova_stat_list, .id = "indicator")
        anova_list_melt <- mutate(anova_list_melt, logp = -log(p, 10))
      }
    }

    # figure ===============================================================
    # t test heatmap -------------------------------------------------------
    gg_t_list_all <- list()
    for (ind_i in out_ind_names) {
      gg_t_list_all[[ind_i]] <- list()
      for (mr_i in missrate_level) {
        fig_data <- dplyr::filter(stat_list_melt, indicator == ind_i, missrate == mr_i)

        fig_data[, "method1"] <- factor(fig_data[, "method1"], levels = method_level)
        fig_data[, "method2"] <- factor(fig_data[, "method2"], levels = method_level)
        fig_data[, "missrate"] <- factor(fig_data[, "missrate"], levels = missrate_level)

        gg_t_list_all[[ind_i]][[mr_i]] <-
          ggplot(fig_data, aes(x = method1, y = method2, fill = logp)) +
          geom_tile(aes(fill = ifelse(logp > 100, 100, logp)),
                    width = 1, height = 1, linewidth = 2, color = "white") +
          coord_equal() + # get square rather than rectangular cells
          geom_text(aes(
            x = method1, y = method2, label = sprintf("%.2f", logp)
          )) +
          scale_fill_material("orange", reverse = FALSE, limits = c(0, 100)) +
          labs(x = mr_i, y = "", fill = "-log p") +
          # ggtitle(sprintf("%s, %s", ind_i, mr_i)) +
          # theme_classic(base_size = 18)
          theme_classic()
      }
    }

    # anova heatmap --------------------------------------------------------
    gg_anova_list_all <- list()
    for (ind_i in out_ind_names) {
      fig_data <- dplyr::filter(anova_list_melt, indicator == ind_i)
      fig_data[, "method"] <- factor(fig_data[, "method"], levels = method_level)

      gg_anova_list_all[[ind_i]] <-
        ggplot(fig_data, aes(x = indicator, y = method, fill = logp)) +
        geom_tile(aes(fill = ifelse(logp > 100, 100, logp)),
                  width = 1, height = 1, linewidth = 2, color = "white") +
        coord_equal() + # get square rather than rectangular cells
        geom_text(aes(
          x = indicator, y = method, label = sprintf("%.2f", logp)
        )) +
        scale_fill_material("orange", reverse = FALSE, limits = c(0, 100)) +
        labs(x = "", y = "", fill = "-log p") +
        theme_classic() +
        theme(
            axis.ticks = element_blank(),
            axis.line = element_blank()
          )


    }
  }

  # combine figure, save ==============================================
  gg_comb_list_all <- list()
  for (ind_i in out_ind_names) {
    gg_comb_list_all[[ind_i]] <-
      ((gg_t_list_all[[ind_i]][[1]] | gg_t_list_all[[ind_i]][[2]] |
        gg_t_list_all[[ind_i]][[3]] | gg_t_list_all[[ind_i]][[4]] |
        gg_t_list_all[[ind_i]][[5]]) +
        plot_layout(guides = "collect") +
        plot_annotation(
          tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
          title = sprintf("%s", ind_i),
          subtitle = sprintf("Pair t-test -log(p) value")
        )) |
      (gg_anova_list_all[[ind_i]])

    ind_out_path <- sprintf(fig_out_path, ind_i)
    ggsave(ind_out_path,
      plot = gg_comb_list_all[[ind_i]], device = NULL, path = NULL,
      scale = 1, width = 18, height = 4, units = "in",
      dpi = 300, limitsize = TRUE
    )
    loginfo(sprintf("%s, pair t test figure saved: %s\n", ind_i, ind_out_path))
  }



  # # csv
  # {
  #   # melt data
  #   NRMSE_reshape_all <- reshape2::melt(NRMSE_list_all) %>% spread(L2, value)
  #   colnames(NRMSE_reshape_all)[1:4] <- c("stat", "scale", "method", "scale_save_name")

  #   PCC_reshape_all <- reshape2::melt(PCC_list_all) %>% spread(L2, value)
  #   colnames(PCC_reshape_all)[1:4] <- c("stat", "scale", "method", "scale_save_name")

  #   write.csv(NRMSE_reshape_all, file = NRMSE_csv_save_path)
  #   write.csv(PCC_reshape_all, file = PCC_csv_save_path)

  #   sprintf("csv saved: %s\n", NRMSE_csv_save_path) %>% cat()
  #   sprintf("csv saved: %s\n", PCC_csv_save_path) %>% cat()
  # }
}
