library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# ---- 讀入 seasonal window table（Table S4 那張）----
# 你把檔名改成你實際的檔名，例如 data/TableS4.csv
win <- read_csv("data/TableS4.csv", show_col_types = FALSE)

# 1) 先把欄名做「去空白」避免 Excel 匯出怪空格
names(win) <- str_trim(names(win))

# 2) 統一：不管原本叫 window 或 season_window，都整理成 window
if ("season_window" %in% names(win) && !"window" %in% names(win)) {
  win <- win %>% rename(window = season_window)
}

# 如果你欄名是 Window / Seasonal window 這種（大小寫或有空白）
if (!"window" %in% names(win)) {
  cand <- names(win)[1]  # 通常第一欄就是類別欄
  win <- win %>% rename(window = all_of(cand))
}

# 3) 解析 start_ym / end_ym：同時相容 "YYYY-MM" 與 "Nov-14"
parse_ym <- function(x){
  x <- str_trim(as.character(x))
  x <- str_replace_all(x, "/", "-")
  out <- rep(as.Date(NA), length(x))
  
  i <- grepl("^\\d{4}-\\d{2}$", x)                     # 2014-11
  out[i] <- as.Date(paste0(x[i], "-01"))
  
  j <- grepl("^[A-Za-z]{3}-\\d{2}$", x)                # Nov-14
  out[j] <- as.Date(paste0("01-", x[j]), format="%d-%b-%y")
  
  k <- grepl("^[A-Za-z]{3}-\\d{4}$", x)                # Nov-2014（以防萬一）
  out[k] <- as.Date(paste0("01-", x[k]), format="%d-%b-%Y")
  
  out
}

win <- win %>%
  mutate(
    start_date = parse_ym(start_ym),
    end_date   = parse_ym(end_ym)
  ) %>%
  mutate(
    end_date = end_date %m+% months(1)   # 讓 xmax 用「下一個月第一天」（避免色塊斷裂）
  )

# 4) 安全檢查：看有沒有解析失敗（有 NA 就代表你的日期格式還有怪字串）
print(win)
stopifnot(all(!is.na(win$start_date)), all(!is.na(win$end_date)))

