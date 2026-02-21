# ==============================
# Figure 3 (facets): 13 focal species monthly series
# Input : data/monthly_species.csv, data/TableS4.csv
# Output: output/Figure3.pdf + Figure3.tif + Figure3.png
# ==============================

pkgs <- c("readr","dplyr","tidyr","lubridate","ggplot2","stringr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Robust project dir (works even if your wd is scripts/)
proj_dir <- {
  # Try RStudio active document path
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(p) && nzchar(p)) normalizePath(file.path(dirname(p), ".."))
    else normalizePath(getwd())
  } else {
    normalizePath(getwd())
  }
}
# If you are inside scripts/ when sourcing, go up one level
if (basename(proj_dir) == "scripts") proj_dir <- normalizePath(file.path(proj_dir, ".."))

in_csv   <- file.path(proj_dir, "data", "monthly_species.csv")
win_csv  <- file.path(proj_dir, "data", "TableS4.csv")
out_dir  <- file.path(proj_dir, "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(in_csv))
stopifnot(file.exists(win_csv))

message("Project dir: ", proj_dir)
message("Input: ", in_csv)
message("Windows: ", win_csv)

# ---- Read data
dat <- readr::read_csv(in_csv, show_col_types = FALSE) %>%
  dplyr::rename(YearMonth = 1, Species = 2, n = 3) %>%
  dplyr::mutate(
    n = as.numeric(n),
    ym_date = lubridate::ym(YearMonth)  # "2014-11" -> 2014-11-01
  ) %>%
  dplyr::filter(!is.na(ym_date))

# ---- Read survey windows (TableS4: e.g., Nov-14)
win <- readr::read_csv(win_csv, show_col_types = FALSE) %>%
  dplyr::mutate(
    start = lubridate::my(start_ym),
    end0  = lubridate::my(end_ym),
    # end = last day of end month
    end   = lubridate::ceiling_date(end0, "month") - lubridate::days(1)
  ) %>%
  dplyr::select(start, end) %>%
  dplyr::filter(!is.na(start) & !is.na(end))

# ---- Split line segments by window to avoid connecting across long gaps
dat <- dat %>%
  dplyr::mutate(
    period = dplyr::case_when(
      ym_date >= win$start[1] & ym_date <= win$end[1] ~ "P1",
      nrow(win) >= 2 & ym_date >= win$start[2] & ym_date <= win$end[2] ~ "P2",
      TRUE ~ "Other"
    )
  )

# ---- Pick 13 focal species (top 13 by total abundance)
focal <- dat %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(total = sum(n, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(total)) %>%
  dplyr::slice_head(n = 13) %>%
  dplyr::pull(Species)

dat13 <- dat %>%
  dplyr::filter(Species %in% focal) %>%
  dplyr::mutate(Species = factor(Species, levels = focal))

# ---- Plot
p <- ggplot() +
  # shade survey windows (optional but consistent with TableS4)
  geom_rect(
    data = win,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    alpha = 0.12
  ) +
  geom_line(
    data = dat13,
    aes(x = ym_date, y = n, group = interaction(Species, period)),
    linewidth = 0.4
  ) +
  geom_point(
    data = dat13,
    aes(x = ym_date, y = n),
    size = 0.8
  ) +
  facet_wrap(~ Species, ncol = 4, scales = "free_y") +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  labs(x = "Month", y = "Number of individuals") +
  theme_bw(base_size = 9) +
  theme(
    strip.background = element_rect(color = NA),
    strip.text = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    panel.grid.minor = element_blank()
  )

# ---- Export (16.7 x 22.3 cm)
W <- 16.7; H <- 22.3
pdf_file <- file.path(out_dir, "Figure3.pdf")
png_file <- file.path(out_dir, "Figure3.png")
tif_file <- file.path(out_dir, "Figure3.tif")

ggsave(pdf_file, plot = p, width = W, height = H, units = "cm")
ggsave(png_file, plot = p, width = W, height = H, units = "cm", dpi = 300, bg = "white")
ggsave(tif_file, plot = p, width = W, height = H, units = "cm", dpi = 300, compression = "lzw")

message("Done. Output files:")
message(" - ", pdf_file)
message(" - ", png_file)
message(" - ", tif_file)
