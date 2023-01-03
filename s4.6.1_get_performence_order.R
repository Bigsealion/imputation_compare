# 2022.11.27 get performence order
# 2023.01.01, must to run after s4.6
rm(list = ls())
library(dplyr)
library(data.table)
library(tidyverse)
library(pheatmap)

# set parameters
raw_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.6.1_JournalOutComplete/CVLTTGMVY1_ReHoGenderY2_3x2"
# raw_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s4.6.1_JournalOutComplete/CVLTTGMV_ReHoGender_3x2"

source_dir <- file.path(raw_dir, "Data")

df_out_path <- file.path(raw_dir, "Data", "Indicator_Summary.csv") # out in raw dir
pheatmap_out_dir <- file.path(raw_dir, "Fig")
logging_path <- file.path(raw_dir, "order_logging.txt")

is_output_pheatmap <- TRUE

remove_col <- c("X", "Indicator")
remove_inds <- c("PCCV", "PCCP", "RMSE")
method_names <- c("CCA", "Pred", "Mean", "EM", "MI")

file_replace_name <- list(
  "ALL_ImpValue_NRMSE.csv" = "NRMSE",
  "ALL_ImpValue_PCC.csv" = "PCCV",
  "Combine_Model_Mean_AW_per_bias.csv" = "AW",
  "Combine_Model_Mean_coverage_rate.csv" = "CR",
  "Combine_Model_Mean_percent_bias.csv" = "PB",
  "Predict_PCC_combine.csv" = "PCCP",
  "Predict_MAE_combine.csv" = "MAE",
  "Predict_RMSE_combine.csv" = "RMSE"
)

method_replace_name <- list(
  "Imp_Method__CCA__" = "CCA",
  "mice_mean" = "Mean",
  "Imp_Method__mice_mean__" = "Mean",
  "mice_norm_pred" = "Pred",
  "mice_pmm" = "MI",
  "vim_em" = "EM",
  "Imp_Method__mice_norm_pred__" = "Pred",
  "Imp_Method__mice_pmm__" = "MI",
  "Imp_Method__vim_em__" = "EM",
  "PMM" = "MI"
)

ind_level <- c("NRMSE", "PCCV", "PB", "CR", "AW", "MAE", "PCCP")
mr_level <- c("TrueMiss", "X20Miss", "X40Miss", "X60Miss", "X80Miss")

annot_colors <- list(
  SubjectMissRate = c(
    TrueMiss = "#d0e9e6",
    X20Miss = "#94ccc3",
    X40Miss = "#549690",
    X60Miss = "#2d645e",
    X80Miss = "#16382d"
  ),
  ind = c(
    NRMSE = "#a6761d",
    PB = "#ca9496",
    CR = "#a9565a",
    AW = "#7f000d",
    MAE = "#8f75c0"
  )
)

# load data ==================================================
data_list <- list()
cat(sprintf("Load data:\n"))
for (f_i in dir(source_dir)) {
  if (f_i %in% names(file_replace_name)) {
    cat(sprintf("\t%s -> %s\n", f_i, file_replace_name[[f_i]]))
    data_list[[file_replace_name[[f_i]]]] <- read.csv(file.path(source_dir, f_i))
  }
}

df <- dplyr::bind_rows(data_list, .id = "ind")

# replace method name
met_replace_df <- t(as.data.frame(method_replace_name)) %>% as.data.frame()
met_replace_df[, "raw_name"] <- rownames(met_replace_df)
colnames(met_replace_df)[1] <- c("rep_name")

match_ind <- match(df$method, met_replace_df$raw_name)
replace_ind <- which(!is.na(match_ind))
df[replace_ind, "method"] <- met_replace_df[match_ind[replace_ind], "rep_name"]

print("Unique method:")
print(unique(df$method))

print("Unique scale_save_name:")
print(unique(df$scale_save_name))

df <- dplyr::filter(df, stat == "mean") %>% as.data.table()
df <- select(df, -all_of(remove_col))

# get order ========================================================
df_melt <- melt(df)
df_met <- spread(data = df_melt, key = "method", value = "value")
df_met <- rename(df_met, SubjectMissRate = variable)

# rank by distance of complete
df_met2 <- df_met
df_met2[is.na(df_met)] <- 0
for (m_i in method_names) {
  eval(parse(text = sprintf(
    "df_met2 <- mutate(df_met2, %s = abs(%s - Complete))",
    m_i, m_i
  )))
}
df_met2[is.na(df_met)] <- NA
# rank of PCCV is reverse, so, * -1
df_met2[df_met2$ind == "PCCV", method_names] <- df_met2[df_met2$ind == "PCCV", ..method_names] * -1
df_met <- df_met2

df_value <- df_met[, ..method_names]
df_order_num <- apply(df_value, 1, rank, na.last = "keep") %>%
  t() %>%
  as.data.table()

df_order <- df_met
df_order[, method_names] <- df_order_num
df_order <- select(df_order, -"Complete")

df_p <- select(df_order, all_of(c("ind", "SubjectMissRate", method_names)))


# draw
df_order_list <- split(df_order, df_order$scale_save_name)
for (sc_i in names(df_order_list)) {
  cat(sprintf("Pheatmap: %s\n", sc_i))


  df_ph1 <- df_order_list[[sc_i]]
  df_ph1 <- dplyr::filter(df_ph1, !(ind %in% remove_inds))

  rownames(df_ph1) <- paste0(df_ph1$ind, df_ph1$SubjectMissRate)
  df_ph1$ind <- factor(df_ph1$ind, levels = ind_level)
  df_ph1$SubjectMissRate <- factor(df_ph1$SubjectMissRate, levels = mr_level)
  df_ph1 <- arrange(df_ph1, ind, SubjectMissRate)

  df_ph_print <- select(df_ph1, all_of(method_names)) %>% as.data.frame()
  rownames(df_ph_print) <- rownames(df_ph1)

  row_annot_df <- select(df_ph1, all_of(c("ind", "SubjectMissRate"))) %>% as.data.frame()
  rownames(row_annot_df) <- rownames(df_ph1)



  if (is_output_pheatmap) {
    out_path <- file.path(pheatmap_out_dir, sprintf("Performance_Rank_%s.pdf", sc_i))
    pheatmap(df_ph_print,
      cluster_cols = F, cluster_rows = F,
      annotation_row = row_annot_df,
      annotation_colors = annot_colors,
      show_rownames = FALSE,
      angle_col = 0,
      gaps_row = seq(5, 25, 5), main = sc_i,
      filename = out_path, silent = T
    )
    cat(sprintf("saved: %s\n", out_path))
  } else {
    cat(sprintf("\tNot save heatmap!\n"))
  }


  cat(sprintf("%s, Colmean:\n", sc_i))
  print(colMeans(df_ph_print, na.rm = T))
}

# out df
{
  write.csv(df, file = df_out_path, row.names = FALSE)
  cat(sprintf("CSV out: %s\n", df_out_path))
}
