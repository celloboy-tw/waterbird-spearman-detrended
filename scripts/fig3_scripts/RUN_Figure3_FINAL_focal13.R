# Figure 3: 13 focal species monthly time series
# Output: Figure3.pdf + Figure3.png + Figure3.tif (300 dpi)
# Key fixes:
#   (1) Scientific names standardized (Genus capitalized, species lowercase)
#   (2) Scientific names in italics (strip labels)
#   (3) Remove right-side blank area by trimming saved PNG/TIF via magick

## 0) Packages ----
pkgs <- c("readr","dplyr","tidyr","lubridate","ggplot2","stringr","magick")
need <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(need) > 0) install.packages(need, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

## 1) Locate project dir (must contain /data/monthly_species.csv) ----
proj_dir <- getwd()

if (!file.exists(file.path(proj_dir, "data", "monthly_species.csv"))) {
  # If user is running from scripts/ folder, move up one level
  if (file.exists(file.path(proj_dir, "..", "data", "monthly_species.csv"))) {
    proj_dir <- normalizePath("..")
  }
}

in_csv <- file.path(proj_dir, "data", "monthly_species.csv")
in_s4  <- file.path(proj_dir, "data", "TableS4.csv")
out_dir <- file.path(proj_dir, "output")

if (!file.exists(in_csv)) stop("Cannot find data/monthly_species.csv. Set working directory to paper3_fig3.")
if (!file.exists(in_s4))  stop("Cannot find data/TableS4.csv. Put it under paper3_fig3/data/.")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

message("Project dir: ", proj_dir)
message("Input: ", in_csv)
message("Input: ", in_s4)
message("Output dir: ", out_dir)

## 2) Helper: scientific name formatting ----
fix_sciname <- function(x) {
  x <- gsub("_", " ", x)
  x <- str_squish(x)
  parts <- strsplit(x, " ")
  vapply(parts, function(p) {
    if (length(p) == 0) return(NA_character_)
    p[1] <- paste0(toupper(substr(p[1], 1, 1)), tolower(substr(p[1], 2, nchar(p[1]))))
    if (length(p) >= 2) p[2] <- tolower(p[2])
    if (length(p) >= 3) p[3:length(p)] <- tolower(p[3:length(p)])
    paste(p, collapse = " ")
  }, character(1))
}

## 3) Read data ----
dat <- readr::read_csv(in_csv, show_col_types = FALSE) %>%
  mutate(
    Species  = fix_sciname(as.character(Species)),
    YearMonth = as.character(YearMonth),
    Date = as.Date(paste0(YearMonth, "-01")),
    n = as.numeric(n)
  ) %>%
  filter(!is.na(Date), !is.na(Species), !is.na(n))

## 4) Define the 13 focal species (use your current Figure 3 set) ----
focal_species <- fix_sciname(c(
  "Anas crecca",
  "Mareca penelope",
  "Anas acuta",
  "Spatula clypeata",
  "Charadrius alexandrinus",
  "Charadrius dubius",
  "Pluvialis fulva",
  "Tringa nebularia",
  "Tringa totanus",
  "Actitis hypoleucos",
  "Calidris subminuta",
  "Tringa stagnatilis",
  "Tringa glareola"
))

# If any focal name not found, fall back to top 13 by total abundance (no manual fixes needed)
missing <- setdiff(focal_species, unique(dat$Species))
if (length(missing) > 0) {
  warning("Some focal species not found in data: ", paste(missing, collapse = ", "),
          "\nFalling back to top 13 species by total abundance.")
  focal_species <- dat %>%
    group_by(Species) %>%
    summarise(total = sum(n, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total)) %>%
    slice_head(n = 13) %>%
    pull(Species)
}

## 5) Shade periods from TableS4 (format like "Nov-14") ----
shade <- readr::read_csv(in_s4, show_col_types = FALSE) %>%
  mutate(
    start = lubridate::my(start_ym),
    end_excl = lubridate::ceiling_date(lubridate::my(end_ym), unit = "month")
  ) %>%
  filter(!is.na(start), !is.na(end_excl)) %>%
  transmute(xmin = as.Date(start), xmax = as.Date(end_excl), ymin = -Inf, ymax = Inf)

## 6) Prepare focal time series (complete missing months as 0) ----
rng <- range(dat$Date, na.rm = TRUE)
all_months <- seq(from = as.Date(format(rng[1], "%Y-%m-01")),
                  to   = as.Date(format(rng[2], "%Y-%m-01")),
                  by = "month")

dat_focal <- dat %>%
  filter(Species %in% focal_species) %>%
  mutate(Species = factor(Species, levels = focal_species)) %>%
  group_by(Species) %>%
  tidyr::complete(Date = all_months, fill = list(n = 0)) %>%
  ungroup()

## 7) Plot (facet) ----
p <- ggplot(dat_focal, aes(x = Date, y = n)) +
  geom_rect(data = shade,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "grey92", color = NA) +
  geom_line(linewidth = 0.35) +
  geom_point(size = 0.7) +
  facet_wrap(~ Species, ncol = 4, scales = "free_y") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(x = "Month", y = "Number of individuals") +
  theme_bw(base_size = 11) +
  theme(
    strip.text = element_text(face = "italic"),   # <-- italic scientific names
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

## 8) Save outputs ----
pdf_file <- file.path(out_dir, "Figure3.pdf")
png_file <- file.path(out_dir, "Figure3.png")
tif_file <- file.path(out_dir, "Figure3.tif")

W <- 20  # cm
H <- 24  # cm

ggsave(pdf_file, plot = p, width = W, height = H, units = "cm", device = "pdf")
ggsave(png_file, plot = p, width = W, height = H, units = "cm", dpi = 300, bg = "white")
ggsave(tif_file, plot = p, width = W, height = H, units = "cm", dpi = 300,
       device = "tiff", compression = "lzw", bg = "white")

## 9) Trim right-side blank whitespace (PNG/TIF) ----
trim_one <- function(path) {
  img <- magick::image_read(path)
  img2 <- magick::image_trim(img)     # trims edge whitespace (removes empty facet area)
  magick::image_write(img2, path)
}

trim_one(png_file)
trim_one(tif_file)

message("Done. Output files:")
message(" - ", pdf_file)
message(" - ", png_file, " (trimmed)")
message(" - ", tif_file, " (trimmed)")
