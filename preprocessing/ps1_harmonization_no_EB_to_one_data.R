# 2022.11.23 Har data
library(neuroCombat)
library(dplyr)
library(logging)

# set parameter ==========================================================
data_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/Reho_age/clear_csv/"
# data_name <- "clear_Rest_ReHo_fbcdwra_REST_AAL116_3mm_7359ID.csv"
data_name <- "clear_Rest_zReHo_fbcdwra_REST_AAL116_3mm_7359ID.csv"
data_path <- file.path(data_dir, data_name)

out_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/Reho_age/har_csv/"
out_path <- file.path(out_dir, sprintf("HarNoEB_%s", data_name))

logging_out_path <- file.path(out_dir, "log.txt")
# mkdir out dir ==========================================================
if (!file.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    sprintf("Create out dir! %s\n", out_dir) %>% cat()
}

# logging setting ========================================================
basicConfig()
addHandler(writeToFile, file = logging_out_path, level = "DEBUG")
loginfo("=============== RUN START ===============")

# load data ==============================================================
raw_data <- read.csv(data_path)
loginfo(sprintf("loaded: %s", data_path))

center_info <- raw_data$true_center

# har data ===============================================================
har_result <- neuroCombat(
    dat = t(dplyr::select(raw_data, -c("RefID", "true_center"))),
    batch = center_info, eb = FALSE
)

har_data <- cbind(
    raw_data$RefID, center_info,
    as.data.frame(t(har_result$dat.combat))
)
colnames(har_data)[c(1, 2)] <- c("RefID", "true_center")
loginfo("Harmonization without EB end")

# output
write.csv(har_data, file = out_path, row.names = FALSE)
loginfo(sprintf("out: %s\n", out_path))

loginfo("=============== RUN END ===============")
