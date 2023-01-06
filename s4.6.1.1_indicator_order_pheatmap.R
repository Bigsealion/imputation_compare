# 2023.01.03 get indicator pheatmap
# z-indicator or rank-indicator
rm(list = ls())
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggsci)
library(logging)

replace_col_by_list <- function(data, replace_col, replace_list) {
  # convert load names to output names
  for (raw_i in names(replace_list)) {
    data[which(data[, replace_col] == raw_i), replace_col] <- replace_list[raw_i]
  }
  return(data)
}

# set parameters =================================================
{
  raw_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.6.1_JournalOutComplete/"


  source_dir_vec <- c("CVLTTGMV_ReHoGender_3x2", "CVLTTGMVY1_ReHoGenderY2_3x2")

  out_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.6.1_JournalOutComplete/All_CVLT_ReHo_TrueSimuY/"

  fig_rank_out_path <- file.path(out_dir, "Indicator_Rank.pdf")
  fig_z_out_path <- file.path(out_dir, "Indicator_zscore.pdf")

  table_rank_out_path <- file.path(out_dir, "Indicator_Rank_summary.csv")
  table_z_out_path <- file.path(out_dir, "Indicator_Ztrans_summary.csv")

  # others --------------------------------------------------------------
  used_method_level <- c("CCA", "Mean", "Pred", "EM", "MI")
  used_ind_level <- c("NRMSE", "PB", "CR", "AW", "MAE")
  used_mr_level_list <- list(
    "TrueMiss" = "TrueMiss",
    "X20Miss" = "20Miss",
    "X40Miss" = "40Miss",
    "X60Miss" = "60Miss",
    "X80Miss" = "80Miss"
  )
  scale_level <- c("CVLT_TGMV", "ReHo_Gender", "CVLT_Y1", "ReHo_Y2")
}

# mkdir out dir ==================================================
if (!file.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
  sprintf("Create out dir! %s\n", out_dir) %>% cat()
}

# load data ==================================================
{
  data_loaded_list <- list()
  for (load_dir_i in source_dir_vec) {
    loginfo(sprintf("Load: %s", load_dir_i))

    data_path <- file.path(raw_dir, load_dir_i, "Data", "Indicator_Summary.csv")
    data_loaded_list[[load_dir_i]] <- read.csv(data_path)
  }

  data_df <- dplyr::bind_rows(data_loaded_list)
}

# data reshape and rename ========================================
{
  # convert to order mode (The larger the value, the worse)
  data_df_c <- filter(data_df, method %in% c("Complete", used_method_level))
  data_df_c <- gather(data_df, missrate, value, all_of(names(used_mr_level_list))) %>%
    spread(method, value)

  # MAE, Method_Value - Complete_Value
  for (m_i in used_method_level) {
    data_df_c[data_df_c[, "ind"] == "MAE", m_i] <-
      abs(data_df_c[data_df_c[, "ind"] == "MAE", m_i] - data_df_c[data_df_c[, "ind"] == "MAE", "Complete"])
  }

  # AW, abs
  data_df_c[data_df_c[, "ind"] == "AW", used_method_level] <-
    abs(data_df_c[data_df_c[, "ind"] == "AW", used_method_level])

  # CR, * -1
  data_df_c[data_df_c[, "ind"] == "CR", used_method_level] <-
    -(data_df_c[data_df_c[, "ind"] == "CR", used_method_level])

  # remove method and indicators
  data_df_c <- dplyr::filter(data_df_c, ind %in% used_ind_level) %>% select(-all_of("Complete"))

  # rename
  data_df_method <- replace_col_by_list(data_df_c, "missrate", used_mr_level_list)
}

# get order, or z-score indicator ================================
{
  # order
  data_df_method_order <- data_df_method
  data_df_method_order[, used_method_level] <-
    apply(data_df_method_order[, used_method_level], 1, function(x) {
      rank(as.numeric(x), na.last = "keep")
    }) %>% t()

  # z-score
  data_df_method_z <- data_df_method
  data_df_method_z[, used_method_level] <-
    apply(data_df_method_z[, used_method_level], 1, function(x) {
      if (sd(x, na.rm = TRUE) == 0) {
        return(x * 0)
      } else {
        return(scale(x))
      }
    }) %>% t()
}

# indicator rank/z summary, save =================================
{
  # get table
  {
    # rank ---------------------------------------------
    rank_summary <- data_df_method_order[, c("scale_save_name", used_method_level)] %>%
      group_by(scale_save_name) %>%
      summarise(across(everything(),
        list(mean = function(x) {
          mean(x, na.rm = TRUE)
        }),
        .names = "{.col}"
      ))

    rank_summary_rank <- apply(rank_summary[, -1], 1, rank) %>%
      t() %>%
      cbind(rank_summary[, 1], .)

    cat(sprintf("Total Rank of indicators rank:\n"))
    print(rank_summary_rank)

    # summary rank-indicator rank in dataset & missrate
    if (F) {
      rank_summary2 <- data_df_method_order[, c("scale_save_name", "missrate", used_method_level)] %>%
        group_by(scale_save_name, missrate) %>%
        summarise(across(everything(),
          list(mean = function(x) {
            mean(x, na.rm = TRUE)
          }),
          .names = "{.col}"
        ))

      rank_summary2_rank <- rank_summary2
      rank_summary2_rank[, used_method_level] <- apply(rank_summary2[, used_method_level], 1, rank) %>% t()

      arrange(rank_summary2_rank, missrate)

      rank_summary2_rank %>%
        as.data.frame() %>%
        select(-scale_save_name) %>%
        group_by(missrate) %>%
        summarise(across(everything(),
          list(mean = function(x) {
            mean(x, na.rm = TRUE)
          }),
          .names = "{.col}"
        ))
    }

    # z-transform ---------------------------------------------
    z_summary <- data_df_method_z[, c("scale_save_name", used_method_level)] %>%
      group_by(scale_save_name) %>%
      summarise(across(everything(),
        list(mean = function(x) {
          mean(x, na.rm = TRUE)
        }),
        .names = "{.col}"
      ))

    # summary z-indicator rank in dataset & missrate
    if (F) {
      z_summary2 <- data_df_method_z[, c("scale_save_name", "missrate", used_method_level)] %>%
        group_by(scale_save_name, missrate) %>%
        summarise(across(everything(),
          list(mean = function(x) {
            mean(x, na.rm = TRUE)
          }),
          .names = "{.col}"
        ))

      z_summary2_rank <- z_summary2
      z_summary2_rank[, used_method_level] <- apply(z_summary2_rank[, used_method_level], 1, rank) %>% t()

      arrange(z_summary2_rank, missrate)

      z_summary2_rank %>%
        as.data.frame() %>%
        select(-scale_save_name) %>%
        group_by(missrate) %>%
        summarise(across(everything(),
          list(mean = function(x) {
            mean(x, na.rm = TRUE)
          }),
          .names = "{.col}"
        ))
    }
  }

  # save
  {
    write.csv(rank_summary, file = table_rank_out_path, row.names = FALSE)
    write.csv(z_summary, file = table_z_out_path, row.names = FALSE)

    loginfo(sprintf("Table Out: %s", table_rank_out_path))
    loginfo(sprintf("Table Out: %s", table_z_out_path))
  }
}

# heatmap =========================================================
{
  # order --------------------------------------------------------------------
  {
    fig_order_data <- gather(data_df_method_order, method, value, all_of(used_method_level))
    fig_order_data[, "ind"] <- factor(fig_order_data[, "ind"], levels = used_ind_level)
    fig_order_data[, "missrate"] <- factor(fig_order_data[, "missrate"], levels = unlist(used_mr_level_list))
    fig_order_data[, "method"] <- factor(fig_order_data[, "method"], levels = used_method_level)
    fig_order_data[, "scale_save_name"] <- factor(fig_order_data[, "scale_save_name"], levels = scale_level)

    gg_order_ind_heatmap <-
      ggplot(fig_order_data, aes(x = method, y = missrate, fill = value)) +
      geom_tile(aes(fill = value),
        width = 1, height = 1, linewidth = 2, color = "white"
      ) +
      facet_grid(ind ~ scale_save_name) +
      scale_fill_material("orange", reverse = TRUE) +
      labs(x = "", y = "", fill = "Rank", title = "Indicator Rank") +
      theme_bw(base_size = 18) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1
      )

    ggsave(fig_rank_out_path,
      plot = gg_order_ind_heatmap, device = NULL, path = NULL,
      scale = 1, width = 14, height = 14, units = "in",
      dpi = 300, limitsize = TRUE
    )
    loginfo(sprintf("Out: %s\n", fig_rank_out_path))
  }

  # z -------------------------------------------------------------------------
  {
    fig_z_data <- gather(data_df_method_z, method, value, all_of(used_method_level))
    fig_z_data[, "ind"] <- factor(fig_z_data[, "ind"], levels = used_ind_level)
    fig_z_data[, "missrate"] <- factor(fig_z_data[, "missrate"], levels = unlist(used_mr_level_list))
    fig_z_data[, "method"] <- factor(fig_z_data[, "method"], levels = used_method_level)
    fig_z_data[, "scale_save_name"] <- factor(fig_z_data[, "scale_save_name"], levels = scale_level)

    gg_z_ind_heatmap <-
      ggplot(fig_z_data, aes(x = method, y = missrate, fill = value)) +
      geom_tile(aes(fill = value),
        width = 1, height = 1, linewidth = 2, color = "white"
      ) +
      coord_equal() +
      facet_grid(ind ~ scale_save_name) +
      scale_fill_material("orange", reverse = TRUE) +
      labs(x = "", y = "", fill = "Z Score", title = "Indicator Z Score") +
      theme_bw(base_size = 18) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
      )

    ggsave(fig_z_out_path,
      plot = gg_z_ind_heatmap, device = NULL, path = NULL,
      scale = 1, width = 14, height = 14, units = "in",
      dpi = 300, limitsize = TRUE
    )
    loginfo(sprintf("Fig Out: %s", fig_z_out_path))
  }
}
