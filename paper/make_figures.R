# Figure generator for the predictset R Journal paper.
#
# Produces six PDF figures and one LaTeX table under paper/figures/ and
# paper/tables/. Run from the package root with:
#   RSTUDIO_PANDOC=/Applications/quarto/bin/tools Rscript paper/make_figures.R

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(ggplot2)
  library(showtext)
})

# Helvetica via showtext for reliable embedding.
font_add("HelveticaNeue",
         regular = "/System/Library/Fonts/Helvetica.ttc",
         bold = "/System/Library/Fonts/Helvetica.ttc",
         italic = "/System/Library/Fonts/Helvetica.ttc")
showtext_auto()
showtext_opts(dpi = 300)

fig_dir <- "paper/figures"
tab_dir <- "paper/tables"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
if (!dir.exists(tab_dir)) dir.create(tab_dir, recursive = TRUE)

# Okabe-Ito palette.
ok_blue   <- "#0072B2"
ok_orange <- "#E69F00"
ok_green  <- "#009E73"
ok_red    <- "#D55E00"
ok_purple <- "#CC79A7"
ok_yellow <- "#F0E442"
ok_sky    <- "#56B4E9"

fam <- "HelveticaNeue"

theme_wp <- function(base_size = 10) {
  theme_bw(base_size = base_size, base_family = fam) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey85"),
      axis.line = element_line(linewidth = 0.35, colour = "grey25"),
      axis.ticks = element_line(linewidth = 0.35, colour = "grey25"),
      axis.ticks.length = unit(2.5, "pt"),
      axis.text = element_text(size = base_size, colour = "grey20"),
      axis.title = element_text(size = base_size, colour = "grey20"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1, family = fam),
      legend.key.height = unit(10, "pt"),
      legend.key.width = unit(22, "pt"),
      legend.spacing.x = unit(10, "pt"),
      legend.margin = margin(4, 0, 0, 0),
      plot.margin = margin(6, 10, 6, 6)
    )
}

tex_esc <- function(x) gsub("_", "\\\\_", as.character(x))

# -----------------------------------------------------------------------------
# Figure 1: split conformal intervals on heteroscedastic 1D data.
# -----------------------------------------------------------------------------
set.seed(20260418)
n_train <- 400
x_tr <- matrix(runif(n_train, 0, 10), ncol = 1)
y_tr <- sin(x_tr[, 1]) + rnorm(n_train, sd = 0.15 + 0.25 * x_tr[, 1])
x_new <- matrix(seq(0.1, 9.9, length.out = 200), ncol = 1)
y_new <- sin(x_new[, 1]) + rnorm(200, sd = 0.15 + 0.25 * x_new[, 1])

res_abs <- conformal_split(x_tr, y_tr, model = y_tr ~ .,
                           x_new = x_new, alpha = 0.10,
                           score_type = "absolute")
res_norm <- conformal_split(x_tr, y_tr, model = y_tr ~ .,
                            x_new = x_new, alpha = 0.10,
                            score_type = "normalized")

df_train <- data.frame(x = x_tr[, 1], y = y_tr)
df_abs <- data.frame(
  x = x_new[, 1], pred = res_abs$pred,
  lower = res_abs$lower, upper = res_abs$upper,
  y_new = y_new, score = "Absolute residual"
)
df_norm <- data.frame(
  x = x_new[, 1], pred = res_norm$pred,
  lower = res_norm$lower, upper = res_norm$upper,
  y_new = y_new, score = "Normalised residual"
)
df_fig1 <- rbind(df_abs, df_norm)
df_fig1$score <- factor(df_fig1$score,
                        levels = c("Absolute residual", "Normalised residual"))

p1 <- ggplot() +
  geom_point(data = df_train, aes(x = x, y = y),
             colour = "grey70", size = 0.6, alpha = 0.6) +
  geom_ribbon(data = df_fig1,
              aes(x = x, ymin = lower, ymax = upper, fill = score),
              alpha = 0.25) +
  geom_line(data = df_fig1, aes(x = x, y = pred, colour = score),
            linewidth = 0.6) +
  scale_colour_manual(values = c(ok_blue, ok_red)) +
  scale_fill_manual(values = c(ok_blue, ok_red)) +
  facet_wrap(~ score, ncol = 2) +
  labs(x = "x", y = "y") +
  theme_wp(base_size = 10) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 10, family = fam, colour = "grey20"))

ggsave(file.path(fig_dir, "fig1_heteroscedastic.pdf"),
       p1, width = 5.5, height = 3.2, device = cairo_pdf)

cat(sprintf("fig1: absolute coverage = %.3f, width = %.3f\n",
            coverage(res_abs, y_new),
            mean(interval_width(res_abs))))
cat(sprintf("fig1: normalised coverage = %.3f, width = %.3f\n",
            coverage(res_norm, y_new),
            mean(interval_width(res_norm))))

# -----------------------------------------------------------------------------
# Figure 2: method comparison on a common dataset.
# -----------------------------------------------------------------------------
set.seed(20260418)
n_cmp <- 500
x_cmp <- matrix(rnorm(n_cmp * 4), ncol = 4)
y_cmp <- x_cmp[, 1] * 1.5 + 0.5 * x_cmp[, 2]^2 + rnorm(n_cmp, sd = 1)
x_cmp_new <- matrix(rnorm(400 * 4), ncol = 4)
y_cmp_new <- x_cmp_new[, 1] * 1.5 + 0.5 * x_cmp_new[, 2]^2 + rnorm(400, sd = 1)

cmp <- conformal_compare(
  x_cmp, y_cmp,
  model = y_cmp ~ .,
  x_new = x_cmp_new, y_new = y_cmp_new,
  methods = c("split", "cv", "jackknife"),
  alpha = 0.10
)

cmp_df <- as.data.frame(cmp)
# Expect columns: method, coverage, mean_width, target_coverage (varies)
cmp_df$method_label <- factor(
  c("split" = "Split conformal",
    "cv" = "CV+",
    "jackknife" = "Jackknife+")[cmp_df$method],
  levels = c("Split conformal", "CV+", "Jackknife+")
)

p2 <- ggplot(cmp_df, aes(x = method_label)) +
  geom_col(aes(y = mean_width), fill = ok_blue, width = 0.55) +
  geom_text(aes(y = mean_width,
                label = sprintf("coverage = %.2f", coverage)),
            vjust = -0.6, size = 3.1, family = fam, colour = "grey20") +
  scale_y_continuous(limits = c(0, max(cmp_df$mean_width) * 1.18),
                     expand = c(0, 0)) +
  labs(x = NULL, y = "Mean interval width") +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig2_compare.pdf"),
       p2, width = 5.5, height = 3.2, device = cairo_pdf)

# Emit a LaTeX table summarising the comparison.
tab_lines <- c(
  "\\begin{tabular}{lrrr}",
  "\\toprule",
  "Method & Target & Empirical coverage & Mean width \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(cmp_df))) {
  r <- cmp_df[i, ]
  tab_lines <- c(tab_lines,
    sprintf("%s & %.2f & %.3f & %.3f \\\\",
            r$method_label, 1 - 0.10, r$coverage, r$mean_width))
}
tab_lines <- c(tab_lines, "\\bottomrule", "\\end{tabular}")
writeLines(tab_lines, file.path(tab_dir, "compare.tex"))

# -----------------------------------------------------------------------------
# Figure 3: APS prediction set sizes across difficulty deciles.
# -----------------------------------------------------------------------------
set.seed(20260418)
n_cls <- 900
K <- 3
# Three classes with overlapping but distinguishable regions. The overlap
# region in the middle should yield prediction sets of size >1; the tails
# should yield size 1.
z <- matrix(rnorm(n_cls * 2), ncol = 2)
latent <- z[, 1] + 0.5 * z[, 2] + rnorm(n_cls, sd = 0.4)
y_cls <- factor(ifelse(latent > 1.0, "A",
                ifelse(latent < -1.0, "C", "B")),
                levels = c("A", "B", "C"))

to_df <- function(x) {
  df <- as.data.frame(x)
  names(df) <- paste0("V", seq_len(ncol(x)))
  df
}

clf <- make_model(
  train_fun = function(x, y) {
    ranger::ranger(y ~ ., data = cbind(data.frame(y = y), to_df(x)),
                   probability = TRUE, num.trees = 200)
  },
  predict_fun = function(object, x_new) {
    predict(object, data = to_df(x_new))$predictions
  },
  type = "classification"
)

idx_train <- seq_len(600)
idx_test <- setdiff(seq_len(n_cls), idx_train)
res_aps <- conformal_aps(z[idx_train, ], y_cls[idx_train], model = clf,
                         x_new = z[idx_test, ], alpha = 0.10)

# Difficulty = entropy of the predicted probability vector.
p_test <- clf$predict_fun(
  clf$train_fun(z[idx_train, ], y_cls[idx_train]),
  z[idx_test, ]
)
entropy <- -rowSums(p_test * log(p_test + 1e-12))
decile <- cut(entropy, breaks = quantile(entropy, seq(0, 1, 0.1)),
              include.lowest = TRUE,
              labels = paste0("D", 1:10))

sizes <- vapply(res_aps$sets, length, integer(1))
df3 <- data.frame(decile = decile, size = sizes)
df3_summary <- aggregate(size ~ decile, data = df3, FUN = mean)

p3 <- ggplot(df3_summary, aes(x = decile, y = size)) +
  geom_col(fill = ok_green, width = 0.6) +
  scale_y_continuous(limits = c(0, max(df3_summary$size) * 1.15),
                     expand = c(0, 0)) +
  labs(x = "Difficulty decile (low to high entropy)",
       y = "Mean prediction set size") +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig3_aps_sets.pdf"),
       p3, width = 5.5, height = 3.2, device = cairo_pdf)

cat(sprintf("fig3: APS coverage = %.3f, mean set size = %.2f\n",
            coverage(res_aps, y_cls[idx_test]), mean(sizes)))

# -----------------------------------------------------------------------------
# Figure 4: Mondrian marginal vs group-conditional coverage.
# -----------------------------------------------------------------------------
set.seed(20260418)
n_m <- 1200
x_m <- matrix(rnorm(n_m * 3), ncol = 3)
grp <- factor(ifelse(x_m[, 1] > 0, "high-noise", "low-noise"))
# Heteroscedastic by group: high-noise group has 4x variance.
noise_sd <- ifelse(grp == "high-noise", 3, 0.75)
y_m <- x_m[, 1] * 1.5 + rnorm(n_m, sd = noise_sd)

x_m_new <- matrix(rnorm(400 * 3), ncol = 3)
grp_new <- factor(ifelse(x_m_new[, 1] > 0, "high-noise", "low-noise"))
noise_sd_new <- ifelse(grp_new == "high-noise", 3, 0.75)
y_m_new <- x_m_new[, 1] * 1.5 + rnorm(400, sd = noise_sd_new)

res_marg <- conformal_split(x_m, y_m, model = y_m ~ .,
                            x_new = x_m_new, alpha = 0.10)
res_mond <- conformal_mondrian(x_m, y_m, model = y_m ~ .,
                               x_new = x_m_new,
                               groups = grp, groups_new = grp_new,
                               alpha = 0.10)

cov_marg <- coverage_by_group(res_marg, y_m_new, grp_new)
cov_mond <- coverage_by_group(res_mond, y_m_new, grp_new)

df4 <- rbind(
  data.frame(group = cov_marg$group, coverage = cov_marg$coverage,
             method = "Split conformal (marginal)"),
  data.frame(group = cov_mond$group, coverage = cov_mond$coverage,
             method = "Mondrian (group-conditional)")
)
df4$method <- factor(df4$method,
                     levels = c("Split conformal (marginal)",
                                "Mondrian (group-conditional)"))

p4 <- ggplot(df4, aes(x = group, y = coverage, fill = method)) +
  geom_hline(yintercept = 0.90, linewidth = 0.35,
             colour = "grey50", linetype = "dashed") +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c(ok_orange, ok_blue)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = NULL, y = "Empirical coverage") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig4_mondrian.pdf"),
       p4, width = 5.5, height = 3.2, device = cairo_pdf)

# -----------------------------------------------------------------------------
# Figure 5: weighted conformal under covariate shift.
# -----------------------------------------------------------------------------
set.seed(20260418)
n_w <- 600
x_w <- matrix(rnorm(n_w), ncol = 1)
y_w <- 1.5 * x_w[, 1] + rnorm(n_w, sd = 0.5 + 0.3 * abs(x_w[, 1]))

x_w_new <- matrix(rnorm(400, mean = 1.5, sd = 1), ncol = 1)
y_w_new <- 1.5 * x_w_new[, 1] + rnorm(400, sd = 0.5 + 0.3 * abs(x_w_new[, 1]))

w <- dnorm(x_w[, 1], mean = 1.5, sd = 1) / dnorm(x_w[, 1], mean = 0, sd = 1)

res_std <- conformal_split(x_w, y_w, model = y_w ~ .,
                           x_new = x_w_new, alpha = 0.10)
res_wcp <- conformal_weighted(x_w, y_w, model = y_w ~ .,
                              x_new = x_w_new,
                              weights = w, alpha = 0.10)

df6 <- data.frame(
  method = c("Split conformal", "Weighted conformal"),
  coverage = c(coverage(res_std, y_w_new),
               coverage(res_wcp, y_w_new)),
  width = c(mean(interval_width(res_std)),
            mean(interval_width(res_wcp)))
)
df6$method <- factor(df6$method,
                     levels = c("Split conformal", "Weighted conformal"))

p6 <- ggplot(df6, aes(x = method, y = coverage, fill = method)) +
  geom_col(width = 0.55) +
  geom_hline(yintercept = 0.90, linewidth = 0.35,
             colour = "grey50", linetype = "dashed") +
  geom_text(aes(label = sprintf("width = %.2f", width)),
            vjust = 1.8, size = 3.1, family = fam, colour = "white") +
  scale_fill_manual(values = c(ok_orange, ok_blue)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = NULL, y = "Empirical coverage on shifted test set") +
  guides(fill = "none") +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig5_weighted.pdf"),
       p6, width = 5.5, height = 3.0, device = cairo_pdf)

cat(sprintf("fig5: std coverage = %.3f, weighted coverage = %.3f\n",
            df6$coverage[1], df6$coverage[2]))

# -----------------------------------------------------------------------------
# Figure 6: COMPAS fairness case study (real data).
# -----------------------------------------------------------------------------
set.seed(20260419)
compas <- read.csv("paper/data/compas.csv", stringsAsFactors = FALSE)
# ProPublica's standard screening.
compas <- compas[compas$days_b_screening_arrest <= 30 &
                 compas$days_b_screening_arrest >= -30 &
                 compas$is_recid != -1 &
                 compas$c_charge_degree != "O" &
                 compas$score_text != "N/A", ]
compas <- compas[compas$race %in% c("African-American", "Caucasian"), ]
compas$race <- factor(compas$race,
                      levels = c("Caucasian", "African-American"))
compas$outcome <- factor(as.character(compas$two_year_recid),
                          levels = c("0", "1"),
                          labels = c("No recid", "Recid"))
compas$sex <- factor(compas$sex)

set.seed(20260419)
n <- nrow(compas)
idx_cal <- sample(n, size = floor(n / 2))
train_df <- compas[-idx_cal, ]
cal_df   <- compas[idx_cal, ]

fit <- glm(outcome ~ age + sex + priors_count +
             juv_fel_count + juv_misd_count + c_charge_degree,
           data = train_df, family = "binomial")

feature_cols <- c("age", "sex", "priors_count",
                  "juv_fel_count", "juv_misd_count", "c_charge_degree")
x_cal  <- cal_df[, feature_cols]
y_cal  <- cal_df$outcome
race_cal <- cal_df$race

# Encode features as a numeric matrix (predictset requires numeric x).
make_xmat <- function(df) {
  mm <- model.matrix(~ age + sex + priors_count + juv_fel_count +
                       juv_misd_count + c_charge_degree, data = df)
  mm[, -1, drop = FALSE]  # drop intercept
}

glm_pipeline <- make_model(
  train_fun = function(x, y) {
    glm(y ~ ., data = cbind(data.frame(y = y), as.data.frame(x)),
        family = "binomial")
  },
  predict_fun = function(object, x_new) {
    p <- predict(object, newdata = as.data.frame(x_new),
                 type = "response")
    cbind(`No recid` = 1 - p, `Recid` = p)
  },
  type = "classification"
)

# Calibrate conformal APS on half the training, test on remainder.
inner <- sample(nrow(train_df), size = floor(nrow(train_df) / 2))
x_train <- make_xmat(train_df[inner, ])
y_train <- train_df$outcome[inner]
x_val   <- make_xmat(train_df[-inner, ])
y_val   <- train_df$outcome[-inner]
race_val <- train_df$race[-inner]

res_marginal <- conformal_aps(x_train, y_train, model = glm_pipeline,
                              x_new = x_val, alpha = 0.10)

res_mondrian <- conformal_mondrian_class(
  x_train, y_train, model = glm_pipeline,
  x_new = x_val, alpha = 0.10,
  groups = train_df$race[inner], groups_new = race_val
)

cov_marg_by_race  <- coverage_by_group(res_marginal, y_val, race_val)
cov_mond_by_race  <- coverage_by_group(res_mondrian, y_val, race_val)

df6 <- rbind(
  data.frame(group = cov_marg_by_race$group,
             coverage = cov_marg_by_race$coverage,
             method = "Marginal APS"),
  data.frame(group = cov_mond_by_race$group,
             coverage = cov_mond_by_race$coverage,
             method = "Mondrian APS")
)
df6$group <- factor(df6$group,
                    levels = c("Caucasian", "African-American"))
df6$method <- factor(df6$method,
                     levels = c("Marginal APS", "Mondrian APS"))

p6 <- ggplot(df6, aes(x = group, y = coverage, fill = method)) +
  geom_hline(yintercept = 0.90, linewidth = 0.35,
             colour = "grey50", linetype = "dashed") +
  geom_col(position = position_dodge(width = 0.7), width = 0.55) +
  geom_text(aes(label = sprintf("%.3f", coverage)),
            position = position_dodge(width = 0.7), vjust = 1.8,
            size = 3.1, family = fam, colour = "white") +
  scale_fill_manual(values = c("Marginal APS" = ok_orange,
                                "Mondrian APS" = ok_blue)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = NULL, y = "Empirical coverage at target 0.90") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_wp(base_size = 10)

ggsave(file.path(fig_dir, "fig6_compas.pdf"),
       p6, width = 5.5, height = 3.2, device = cairo_pdf)

cat(sprintf("fig6 COMPAS: marg C=%.3f AA=%.3f; mond C=%.3f AA=%.3f\n",
            cov_marg_by_race$coverage[cov_marg_by_race$group == "Caucasian"],
            cov_marg_by_race$coverage[cov_marg_by_race$group == "African-American"],
            cov_mond_by_race$coverage[cov_mond_by_race$group == "Caucasian"],
            cov_mond_by_race$coverage[cov_mond_by_race$group == "African-American"]))

cat("\n--- done ---\n")
