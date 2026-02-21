# =========================================================
# RUN_Figure3_FINAL.R
# Paper 3 - Figure 3 (facets): Monthly time series of focal species
# Output: output/Figure3.pdf + Figure3.png + Figure3.tif (300 dpi)
# =========================================================

# ---------------------------
# 0) Packages
# ---------------------------
pkgs <- c("readr", "dplyr", "tidyr", "lubridate", "ggplot2", "stringr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# Optional (better PNG/TIFF)
# install.packages("ragg")
has_ragg <- requireNamespace("ragg", quietly = TRUE)

# ---------------------------
# 1) Find project root (AUTO, no more setwd pain)
# ---------------------------
get_proj_dir <- function() {
  # A) Use the open script path in RStudio (most reliable)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- try(rstudioapi::getActiveDocumentContext(), silent = TRUE)
    if (!inherits(ctx, "try-error") && nzchar(ctx$path)) {
      script_path <- normalizePath(ctx$path, winslash = "/")
      script_dir  <- dirname(script_path)
      if (basename(script_dir) == "scripts") return(normalizePath(file.path(script_dir, ".."), winslash = "/"))
      return(normalizePath(script_dir, winslash = "/"))
    }
  }
  
  # B) Fallback: infer from working directory
  wd <- normalizePath(getwd(), winslash = "/")
  if (dir.exists(file.path(wd, "data"))) return(wd)
  if (dir.exists(file.path(wd, "..", "data"))) return(normalizePath(file.path(wd, ".."), winslash = "/"))
  
  stop("找不到專案根目錄：請用 RStudio 打開 scripts/RUN_Figure3_FINAL.R 再按 Source。")
}

proj_dir <- get_proj_dir()
message("Project dir = ", proj_dir)

data_dir <- file.path(proj_dir, "data")
out_dir  <- file.path(proj_dir, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

in_csv <- file.path(data_dir, "monthly_species.csv")
in_s4  <- file.path(data_dir, "TableS4.csv")          # optional
focal_file <- file.path(data_dir, "focal_species.txt") # optional

if (!file.exists(in_csv)) stop("找不到檔案：", in_csv, "\n請把 monthly_species.csv 放在 data/")

# ---------------------------
# 2) Read monthly_species.csv (support long OR wide)
# ---------------------------
dat_raw <- readr::read_csv(in_csv, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
names(dat_raw) <- stringr::str_replace_all(names(dat_raw), "\\s+", "")

# wide format: YearMonth + many species columns
is_wide <- ("YearMonth" %in% names(dat_raw)) && !("Species" %in% names(dat_raw)) && !("n" %in% names(dat_raw))
if (is_wide) {
  dat <- dat_raw %>% tidyr::pivot_longer(cols = -YearMonth, names_to = "Species", values_to = "n")
} else {
  dat <- dat_raw
}

if ("ym" %in% names(dat) && !("YearMonth" %in% names(dat))) dat <- dplyr::rename(dat, YearMonth = ym)

need_cols <- c("YearMonth", "Species", "n")
if (!all(need_cols %in% names(dat))) {
  stop("monthly_species.csv 欄位需要：YearMonth, Species, n（或 wide：YearMonth + 多個物種欄位）\n",
       "目前欄位：", paste(names(dat), collapse = ", "))
}

dat <- dat %>%
  mutate(
    YearMonth = as.character(YearMonth),
    Species   = as.character(Species),
    n         = suppressWarnings(as.numeric(n))
  ) %>%
  filter(!is.na(YearMonth), !is.na(Species)) %>%
  mutate(date = lubridate::ym(YearMonth))   # expects YYYY-MM

if (any(is.na(dat$date))) {
  bad <- unique(dat$YearMonth[is.na(dat$date)])
  bad <- bad[1:min(10, length(bad))]
  stop("YearMonth 有無法解析的值（必須 YYYY-MM，如 2016-10）。例如：", paste(bad, collapse = ", "))
}

# ---------------------------
# 3) Choose focal species (13 panels)
# ---------------------------
if (file.exists(focal_file)) {
  focal_species <- readLines(focal_file, warn = FALSE) %>%
    stringr::str_trim() %>%
    (\(x) x[x != ""])()
  if (length(focal_species) == 0) stop("focal_species.txt 是空的，請放 13 個物種（每行一個）")
  message("Using focal_species.txt (n = ", length(focal_species), ").")
} else {
  focal_species <- dat %>%
    group_by(Species) %>%
    summarise(total = sum(n, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total)) %>%
    slice_head(n = 13) %>%
    pull(Species)
  message("No focal_species.txt → auto-select TOP 13 species by total counts:")
  message(paste("  -", focal_species, collapse = "\n"))
}

dat_f <- dat %>%
  filter(Species %in% focal_species) %>%
  mutate(Species = factor(Species, levels = focal_species)) %>%
  arrange(Species, date)

# ---------------------------
# 4) Optional shading by TableS4.csv
#    columns: season_window, start_ym, end_ym
#    start_ym/end_ym can be "Nov-14" OR "2014-11"
# ---------------------------
parse_ym <- function(x) {
  x <- as.character(x)
  out <- rep(as.Date(NA), length(x))
  is_iso <- grepl("^\\d{4}-\\d{2}$", x)
  out[is_iso] <- lubridate::ym(x[is_iso])
  out[!is_iso] <- suppressWarnings(lubridate::my(x[!is_iso]))
  out
}

rect_df <- NULL
if (file.exists(in_s4)) {
  s4 <- readr::read_csv(in_s4, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
  names(s4) <- stringr::str_replace_all(names(s4), "\\s+", "")
  if (all(c("season_window", "start_ym", "end_ym") %in% names(s4))) {
    rect_df <- s4 %>%
      mutate(xmin = parse_ym(start_ym), xmax = parse_ym(end_ym)) %>%
      filter(!is.na(xmin), !is.na(xmax)) %>%
      mutate(xmax = xmax %m+% months(1))
    if (nrow(rect_df) == 0) rect_df <- NULL
  } else {
    message("TableS4.csv 欄位不是 season_window/start_ym/end_ym → 略過背景區塊")
  }
}

# ---------------------------
# 5) Plot
# ---------------------------
p <- ggplot(dat_f, aes(x = date, y = n))

if (!is.null(rect_df)) {
  p <- p + geom_rect(
    data = rect_df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey70",
    alpha = 0.12
  )
}

p <- p +
  geom_line(linewidth = 0.35, na.rm = TRUE) +
  geom_point(size = 0.55, alpha = 0.8, na.rm = TRUE) +
  facet_wrap(~ Species, scales = "free_y", ncol = 4) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(x = "Month", y = "Number of individuals") +
  theme_classic(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(size = 8),
    panel.spacing = unit(0.6, "lines"),
    plot.margin = margin(6, 6, 6, 6)
  )

# ---------------------------
# 6) Export (journal size)
# ---------------------------
W <- 16.7
H <- 22.3

pdf_file <- file.path(out_dir, "Figure3.pdf")
png_file <- file.path(out_dir, "Figure3.png")
tif_file <- file.path(out_dir, "Figure3.tif")

# PDF (vector)
ok_pdf <- TRUE
tryCatch({
  ggsave(pdf_file, plot = p, width = W, height = H, units = "cm", device = grDevices::cairo_pdf)
}, error = function(e) ok_pdf <<- FALSE)

if (!ok_pdf) {
  ggsave(pdf_file, plot = p, width = W, height = H, units = "cm", device = "pdf", useDingbats = FALSE)
}

# PNG/TIFF (300 dpi)
if (has_ragg) {
  ggsave(png_file, plot = p, width = W, height = H, units = "cm", dpi = 300,
         device = ragg::agg_png, background = "white")
  ggsave(tif_file, plot = p, width = W, height = H, units = "cm", dpi = 300,
         device = ragg::agg_tiff, compression = "lzw")
} else {
  ggsave(png_file, plot = p, width = W, height = H, units = "cm", dpi = 300, bg = "white")
  ggsave(tif_file, plot = p, width = W, height = H, units = "cm", dpi = 300,
         device = "tiff", compression = "lzw")
}

message("Done. Output files:")
message(" - ", pdf_file)
message(" - ", png_file)
message(" - ", tif_file)
