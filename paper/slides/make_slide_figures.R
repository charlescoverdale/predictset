# make_slide_figures.R (predictset)
# Prepare slide-ready figures:
#  1. Copy the paper's gallery and deep-dive figures into slides/figures/
#  2. Copy the paper's COMPAS figure as the hero figure
#  3. Generate a QR code pointing to the paper PDF
#
# Usage:  Rscript make_slide_figures.R

suppressPackageStartupMessages({
  if (!requireNamespace("qrcode", quietly = TRUE)) {
    install.packages("qrcode", repos = "https://cloud.r-project.org")
  }
  library(qrcode)
})

fig_dir <- "figures"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Copy paper figures into slides/figures/
#    - fig1_heteroscedastic.pdf   gallery (regression, abs vs norm)
#    - fig2_compare.pdf           deep-dive (split vs CV+ vs Jackknife+)
#    - fig3_aps_sets.pdf          gallery (classification adaptive sets)
#    - fig4_mondrian.pdf          gallery (group-conditional coverage)
#    - fig5_weighted.pdf          gallery (covariate-shift correction)
# ------------------------------------------------------------------
paper_figs <- c(
  "fig1_heteroscedastic.pdf",
  "fig2_compare.pdf",
  "fig3_aps_sets.pdf",
  "fig4_mondrian.pdf",
  "fig5_weighted.pdf"
)

for (f in paper_figs) {
  src <- file.path("..", "figures", f)
  dst <- file.path(fig_dir, f)
  if (file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
    cat("Copied", f, "to", dst, "\n")
  } else {
    stop("Source figure not found: ", src,
         ". Run the paper's make_figures.R first.")
  }
}

# ------------------------------------------------------------------
# 2. Hero figure: COMPAS per-group coverage (marginal vs Mondrian APS)
# ------------------------------------------------------------------
src <- file.path("..", "figures", "fig6_compas.pdf")
dst <- file.path(fig_dir, "hero_figure.pdf")

if (file.exists(src)) {
  file.copy(src, dst, overwrite = TRUE)
  cat("Copied hero figure to", dst, "\n")
} else {
  stop("Source figure not found: ", src,
       ". Run the paper's make_figures.R first.")
}

# ------------------------------------------------------------------
# 3. QR code to the paper PDF on the publications page
# ------------------------------------------------------------------
paper_url <- "https://charlescoverdale.github.io/files/coverdale_predictset_2026.pdf"

qr <- qr_code(paper_url, ecl = "M")
png(
  filename = file.path(fig_dir, "qrcode_paper.png"),
  width = 800, height = 800, res = 300, bg = "white"
)
par(mar = rep(0, 4))
plot(qr)
dev.off()

cat("QR code written to", file.path(fig_dir, "qrcode_paper.png"), "\n")
