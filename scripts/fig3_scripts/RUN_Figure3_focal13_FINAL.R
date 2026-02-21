# RUN_Figure3_focal13_FINAL.R
# Goal: keep the SAME layout/style as RUN_Figure3_FINAL.R, but replace species panels with focal13.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(magick)
})

# ---- Paths (keep same structure) ----
data_file    <- "monthly_species.csv"
tableS4_file <- file.path("data", "TableS4.csv")

if (!file.exists(data_file)) stop("Cannot find: ", data_file)
if (!file.exists(tableS4_file)) stop("Cannot find: ", tableS4_file)

# ---- FOCAL13 (LOCK) ----
species_order <- c(
  "Anas crecca",             # Eurasian Teal
  "Mareca penelope",         # Eurasian Wigeon
  "Anas acuta",              # Northern Pintail
  "Spatula clypeata",        # Northern Shoveler
  "Charadrius alexandrinus", # Kentish Plover
  "Charadrius dubius",       # Little Ringed Plover
  "Pluvialis fulva",         # Pacific Golden Plover
  "Tringa nebularia",        # Common Greenshank
  "Tringa totanus",          # Common Redshank
  "Actitis hypoleucos",      # Common Sandpiper
  "Calidris subminuta",      # Long-toed Stint
  "Tringa stagnatilis",      # Marsh Sandpiper
  "Tringa glareola"          # Wood Sandpiper
)

# ---- Read data ----
df <- read_csv(data_file, show_col_types = FALSE)

# Expect columns: YearMonth, Species, n  (your existing pipeline)
need <- c("YearMonth", "Species", "n")
if (!all(need %in% names(df))) stop("monthly_species.csv must contain: ", paste(need, collapse=", "))

df <- df %>%
  mutate(
    date = ym(YearMonth),
    Species = as.character(Species),
    n = as.numeric(n)
  ) %>%
  filter(Species %in% species_order) %>%
  mutate(
    Species = factor(Species, levels = species_order),
    Species_clean = Species
  )

# ---- Shading ranges (same logic as your mother script) ----
parse_month <- function(x) {
  as.Date(paste0("01-", x), format = "%d-%b-%y")
}

shading <- read_csv(tableS4_file, show_col_types = FALSE) %>%
  mutate(
    start_date = parse_month(start_ym),
    end_date   = parse_month(end_ym) %m+% months(1) - days(1)
  )

# ---- Plot (keep style consistent) ----
p <- ggplot(df, aes(x = date, y = n, group = 1)) +
  geom_rect(
    data = shading,
    aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "grey90",
    color = NA
  ) +
  geom_line(color = "black", linewidth = 0.5) +
  geom_point(color = "black", size = 0.8) +
  facet_wrap(~ Species_clean, ncol = 4, scales = "free_y") +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  labs(x = "Month", y = "Number of individuals") +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey85", color = "grey50"),
    strip.text = element_text(face = "italic"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# ---- Output (same as mother script) ----
dir.create("output", showWarnings = FALSE)

pdf_file <- file.path("output", "Figure3_focal13.pdf")
png_file <- file.path("output", "Figure3_focal13.png")

ggsave(pdf_file, p, width = 20, height = 24, units = "in", dpi = 300)
ggsave(png_file, p, width = 20, height = 24, units = "in", dpi = 300)

# Optional: trim whitespace like mother script
img <- image_read(png_file) |> image_trim()
image_write(img, path = png_file)

message("DONE: ", pdf_file)
message("DONE: ", png_file)
