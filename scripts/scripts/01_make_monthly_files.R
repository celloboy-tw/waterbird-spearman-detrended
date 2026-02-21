library(tidyverse)
library(lubridate)
library(readr)

# --- Paths ---
in_csv  <- "data/Total_Birds.csv"
out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- Read data ---
df <- read_csv(in_csv, locale = locale(encoding = "UTF-8"))

# --- Basic checks you MUST pass ---
required_cols <- c("YearMonth", "TotalIndividuals")
stopifnot(all(required_cols %in% names(df)))

# Ensure ym is "YYYY-MM" string, and create a date for plotting (first day of month)
df2 <- df %>%
  mutate(
    ym = as.character(YearMonth),
    date = ymd(paste0(ym, "-01"))
  ) %>%
  arrange(date)

# --- 1) monthly_total.csv ---
monthly_total <- df2 %>%
  transmute(
    ym,
    total_ind = as.integer(round(TotalIndividuals))
  )

write_csv(monthly_total, file.path(out_dir, "monthly_total.csv"))

# --- 2) monthly_group.csv (4 dominant orders) ---
# Define species lists by order (names MUST match column names in Total_Birds.csv)
charadriiformes <- c(
  "反嘴鴴","高蹺鴴",
  "小環頸鴴","太平洋金斑鴴","灰斑鴴","東方環頸鴴","鐵嘴鴴",
  "小燕鷗","黑腹燕鷗","黑嘴鷗","裏海燕鷗","鷗嘴燕鷗",
  "三趾濱鷸","小青足鷸","反嘴鷸","田鷸","尖尾濱鷸","赤足鷸","長趾濱鷸",
  "青足鷸","流蘇鷸","紅胸濱鷸","黑尾鷸","黑腹濱鷸","磯鷸","彎嘴濱鷸","鷹斑鷸"
)

anseriformes <- c("小水鴨","白眉鴨","尖尾鴨","赤膀鴨","赤頸鴨","琵嘴鴨","鳳頭潛鴨")

gruiformes <- c("白腹秧雞","紅冠水雞")

pelecaniformes <- c(
  "白琵鷺","埃及聖䴉","黑面琵鷺",
  "大白鷺","小白鷺","中白鷺","夜鷺","栗小鷺","黃小鷺","黃頭鷺","綠簑鷺","蒼鷺"
)

# Safety check: ensure all species columns exist
check_exist <- function(x) x[x %in% names(df2)]
missing_report <- list(
  Charadriiformes_missing = setdiff(charadriiformes, names(df2)),
  Anseriformes_missing    = setdiff(anseriformes, names(df2)),
  Gruiformes_missing      = setdiff(gruiformes, names(df2)),
  Pelecaniformes_missing  = setdiff(pelecaniformes, names(df2))
)
print(missing_report)

# Compute group totals
monthly_group_wide <- df2 %>%
  transmute(
    ym,
    Charadriiformes = rowSums(across(all_of(check_exist(charadriiformes))), na.rm = TRUE),
    Anseriformes    = rowSums(across(all_of(check_exist(anseriformes))), na.rm = TRUE),
    Pelecaniformes  = rowSums(across(all_of(check_exist(pelecaniformes))), na.rm = TRUE),
    Gruiformes      = rowSums(across(all_of(check_exist(gruiformes))), na.rm = TRUE)
  )

monthly_group_long <- monthly_group_wide %>%
  pivot_longer(-ym, names_to = "group", values_to = "n") %>%
  mutate(
    n = as.integer(round(n)),
    group = factor(group, levels = c("Charadriiformes","Anseriformes","Pelecaniformes","Gruiformes"))
  )

write_csv(monthly_group_long, file.path(out_dir, "monthly_group.csv"))

message("Done. Files written to output/: monthly_total.csv, monthly_group.csv")
