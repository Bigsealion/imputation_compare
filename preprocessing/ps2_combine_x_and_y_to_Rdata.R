# 2022.11.23 load x and y, combine as a .RData file
# for example, Reho (X) and age (Y), or CVLT (X) and TGMV (Y)
library(dplyr)
library(logging)

# set parameter ==========================================================
# data is X
data_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/Reho_age/har_csv/"
data_name <- "HarNoEB_clear_Rest_zReHo_fbcdwra_REST_AAL116_3mm_7359ID.csv"
out_name <- "HarReho"

# data_name <- "HarNoEB_clear_Rest_zReHo_fbcdwra_REST_AAL116_3mm_7359ID.csv"
# out_name <- "HarzReho"

data_path <- file.path(data_dir, data_name)

# cog path, this is Y
cog_path <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/cog_newgroup/cog_new_1.5.RData"

out_dir <- "/gpfs/lab/liangmeng/members/liyifan/R/imp_compare/s1_data/Reho_age/"
out_path <- file.path(out_dir, sprintf("s1.4_%sAAL116_Gender.RData", out_name))

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
bi_data <- read.csv(data_path)
load(cog_path)  # var name is single_data_list

cog_info <- select(single_data_list$TotalData$Base, c("RefID", "gender"))

out_data <- merge(bi_data, cog_info, by = "RefID", all = FALSE)
out_data <- dplyr::relocate(out_data, gender, .after = "RefID")
out_data <- dplyr::select(out_data, -"true_center")  # Har data, removing center info
# out_data <- dplyr::rename(out_data, matched_center = true_center)

# out =====================================================================
match_data <- list(tmp = out_data)  # rename tmp to out_name
names(match_data)[1] <- out_name

save(match_data, file = out_path)
loginfo(sprintf("out: %s\n", out_path))

loginfo("=============== RUN END ===============")
