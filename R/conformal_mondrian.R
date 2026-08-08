#' Mondrian Conformal Prediction Intervals (Group-Conditional)
#'
#' Constructs prediction intervals with group-conditional coverage guarantees.
#' Instead of a single conformal quantile, a separate quantile is computed for
#' each group, ensuring coverage within each subgroup (e.g. by gender, region,
#' or model type).
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A numeric vector of response values.
#' @param model A fitted model object, a [make_model()] specification, or a
#'   formula.
#' @param x_new A numeric matrix or data frame of new predictor variables.
#' @param groups A factor or character vector of group labels for each
#'   observation in `x`, with length equal to `nrow(x)`.
#' @param groups_new A factor or character vector of group labels for each
#'   observation in `x_new`, with length equal to `nrow(x_new)`.
#' @param alpha Miscoverage level. Default `0.10`.
#' @param cal_fraction Fraction of data used for calibration. Default `0.5`.
#' @param seed Optional random seed.
#'
#' @return A `predictset_reg` object. See [conformal_split()] for details.
#'   The `method` component is `"mondrian"`. Additional components include
#'   `groups_new` (the group labels for new data) and `group_quantiles`
#'   (named numeric vector of per-group conformal quantiles).
#'
#' @references
#' Vovk, V., Gammerman, A. and Shafer, G. (2005).
#' *Algorithmic Learning in a Random World*. Springer.
#'
#' @examples
#' set.seed(42)
#' n <- 400
#' x <- matrix(rnorm(n * 3), ncol = 3)
#' groups <- factor(ifelse(x[, 1] > 0, "high", "low"))
#' y <- x[, 1] * 2 + ifelse(groups == "high", 2, 0.5) * rnorm(n)
#' x_new <- matrix(rnorm(50 * 3), ncol = 3)
#' groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))
#'
#' \donttest{
#' result <- conformal_mondrian(x, y, model = y ~ ., x_new = x_new,
#'                               groups = groups, groups_new = groups_new)
#' print(result)
#' }
#'
#' @family regression methods
#' @export
conformal_mondrian <- function(x, y, model, x_new, groups, groups_new,
                                alpha = 0.10, cal_fraction = 0.5,
                                seed = NULL) {
  x <- validate_x(x, "x")
  y <- validate_y_reg(y)
  x_new <- validate_x(x_new, "x_new")
  validate_x_new(x, x_new)
  alpha <- validate_alpha(alpha)

  if (nrow(x) != length(y)) {
    cli_abort("{.arg x} and {.arg y} must have the same number of observations.")
  }

  groups <- as.factor(groups)
  groups_new <- as.factor(groups_new)

  if (length(groups) != nrow(x)) {
    cli_abort("{.arg groups} must have length equal to {.code nrow(x)} ({nrow(x)}).")
  }
  if (length(groups_new) != nrow(x_new)) {
    cli_abort("{.arg groups_new} must have length equal to {.code nrow(x_new)} ({nrow(x_new)}).")
  }

  mod <- resolve_model(model, type = "regression")

  local_seed(seed)

  split <- split_data(nrow(x), cal_fraction)
  x_train <- x[split$train, , drop = FALSE]
  y_train <- y[split$train]
  x_cal <- x[split$cal, , drop = FALSE]
  y_cal <- y[split$cal]
  groups_cal <- groups[split$cal]

  fitted <- mod$train_fun(x_train, y_train)

  yhat_cal <- mod$predict_fun(fitted, x_cal)
  scores <- regression_scores(y_cal, yhat_cal)

  # Compute per-group quantiles
  all_groups <- levels(groups)
  pooled_q <- conformal_quantile(scores, alpha)
  group_quantiles <- setNames(numeric(length(all_groups)), all_groups)

  # A group needs at least ceiling(1/alpha) - 1 calibration points before its
  # own conformal quantile is finite. Below that, conformal_quantile() returns
  # Inf, which is the honest answer: the data cannot support a finite
  # group-conditional interval. Borrowing the pooled quantile instead would
  # silently void the group-conditional guarantee for exactly the groups that
  # need it, so it is not done.
  n_min <- min_cal_size(alpha)
  for (g in all_groups) {
    g_idx <- which(groups_cal == g)
    if (length(g_idx) < n_min) {
      cli_warn(c(
        "Group {.val {g}} has {length(g_idx)} calibration point{?s}, fewer than the {n_min} needed for a finite quantile at {.code alpha = {alpha}}.",
        "i" = "Its interval is unbounded. Collect more data for this group, raise {.arg alpha}, or merge it into a larger group."
      ))
    }
    group_quantiles[g] <- conformal_quantile(scores[g_idx], alpha)
  }

  # Predictions on new data
  yhat_new <- mod$predict_fun(fitted, x_new)
  n_new <- nrow(x_new)
  lower <- numeric(n_new)
  upper <- numeric(n_new)

  for (i in seq_len(n_new)) {
    q_i <- group_quantile_for(groups_new[i], group_quantiles, pooled_q)
    lower[i] <- yhat_new[i] - q_i
    upper[i] <- yhat_new[i] + q_i
  }

  structure(list(
    pred = yhat_new,
    lower = lower,
    upper = upper,
    alpha = alpha,
    method = "mondrian",
    scores = scores,
    quantile = pooled_q,
    n_cal = length(split$cal),
    n_train = length(split$train),
    fitted_model = fitted,
    model = mod,
    groups_new = groups_new,
    group_quantiles = group_quantiles
  ), class = "predictset_reg")
}


#' Mondrian Conformal Prediction Sets for Classification
#'
#' Constructs prediction sets with group-conditional coverage guarantees for
#' classification. Uses LAC-style scoring with per-group conformal quantiles.
#'
#' @param x A numeric matrix or data frame of predictor variables.
#' @param y A factor (or character/integer vector coerced to factor) of class
#'   labels.
#' @param model A [make_model()] specification with `type = "classification"`,
#'   or a fitted model object.
#' @param x_new A numeric matrix or data frame of new predictor variables.
#' @param groups A factor or character vector of group labels for each
#'   observation in `x`.
#' @param groups_new A factor or character vector of group labels for each
#'   observation in `x_new`.
#' @param alpha Miscoverage level. Default `0.10`.
#' @param cal_fraction Fraction of data used for calibration. Default `0.5`.
#' @param allow_empty Logical. If `FALSE` (the default), an empty prediction
#'   set is replaced by the single most probable class.
#' @param seed Optional random seed. Set for the duration of the call only;
#'   the global random stream is restored on exit.
#'
#' @return A `predictset_class` object. See [conformal_lac()] for
#'   details. The `method` component is `"mondrian"`. Additional components
#'   include `groups_new` and `group_quantiles`.
#'
#' @examples
#' set.seed(42)
#' n <- 400
#' x <- matrix(rnorm(n * 4), ncol = 4)
#' groups <- factor(ifelse(x[, 1] > 0, "high", "low"))
#' y <- factor(ifelse(x[,1] + x[,2] > 0, "A", "B"))
#' x_new <- matrix(rnorm(50 * 4), ncol = 4)
#' groups_new <- factor(ifelse(x_new[, 1] > 0, "high", "low"))
#'
#' clf <- make_model(
#'   train_fun = function(x, y) glm(y ~ ., data = data.frame(y = y, x),
#'                                   family = "binomial"),
#'   predict_fun = function(object, x_new) {
#'     df <- as.data.frame(x_new)
#'     names(df) <- paste0("X", seq_len(ncol(x_new)))
#'     p <- predict(object, newdata = df, type = "response")
#'     cbind(A = 1 - p, B = p)
#'   },
#'   type = "classification"
#' )
#'
#' \donttest{
#' result <- conformal_mondrian_class(x, y, model = clf, x_new = x_new,
#'                                     groups = groups, groups_new = groups_new)
#' print(result)
#' }
#'
#' @family classification methods
#' @export
conformal_mondrian_class <- function(x, y, model, x_new, groups, groups_new,
                                      alpha = 0.10, cal_fraction = 0.5,
                                      allow_empty = FALSE, seed = NULL) {
  x <- validate_x(x, "x")
  y <- validate_y_class(y)
  x_new <- validate_x(x_new, "x_new")
  validate_x_new(x, x_new)
  alpha <- validate_alpha(alpha)

  groups <- as.factor(groups)
  groups_new <- as.factor(groups_new)

  if (length(groups) != nrow(x)) {
    cli_abort("{.arg groups} must have length equal to {.code nrow(x)} ({nrow(x)}).")
  }
  if (length(groups_new) != nrow(x_new)) {
    cli_abort("{.arg groups_new} must have length equal to {.code nrow(x_new)} ({nrow(x_new)}).")
  }
  if (nrow(x) != length(y)) {
    cli_abort("{.arg x} and {.arg y} must have the same number of observations.")
  }

  mod <- resolve_model(model, type = "classification")

  local_seed(seed)

  split <- split_data(nrow(x), cal_fraction)
  x_train <- x[split$train, , drop = FALSE]
  y_train <- y[split$train]
  x_cal <- x[split$cal, , drop = FALSE]
  y_cal <- y[split$cal]
  groups_cal <- groups[split$cal]

  fitted <- mod$train_fun(x_train, y_train)

  probs_cal <- label_probs(mod$predict_fun(fitted, x_cal), levels(y))
  validate_probs(probs_cal, levels(y), "calibration probability matrix")

  scores <- lac_scores(probs_cal, y_cal)

  # Per-group quantiles
  all_groups <- levels(groups)
  pooled_q <- conformal_quantile(scores, alpha)
  group_quantiles <- setNames(numeric(length(all_groups)), all_groups)

  # A group needs at least ceiling(1/alpha) - 1 calibration points before its
  # own conformal quantile is finite. Below that, conformal_quantile() returns
  # Inf, which is the honest answer: the data cannot support a finite
  # group-conditional interval. Borrowing the pooled quantile instead would
  # silently void the group-conditional guarantee for exactly the groups that
  # need it, so it is not done.
  n_min <- min_cal_size(alpha)
  for (g in all_groups) {
    g_idx <- which(groups_cal == g)
    if (length(g_idx) < n_min) {
      cli_warn(c(
        "Group {.val {g}} has {length(g_idx)} calibration point{?s}, fewer than the {n_min} needed for a finite quantile at {.code alpha = {alpha}}.",
        "i" = "Its interval is unbounded. Collect more data for this group, raise {.arg alpha}, or merge it into a larger group."
      ))
    }
    group_quantiles[g] <- conformal_quantile(scores[g_idx], alpha)
  }

  # Predictions on new data
  probs_new <- label_probs(mod$predict_fun(fitted, x_new), levels(y))
  validate_probs(probs_new, levels(y), "probability matrix for x_new")

  n_new <- nrow(x_new)
  sets <- vector("list", n_new)
  set_probs <- vector("list", n_new)
  classes <- colnames(probs_new)

  for (i in seq_len(n_new)) {
    q_i <- group_quantile_for(groups_new[i], group_quantiles, pooled_q)
    p <- probs_new[i, ]
    included <- non_empty(classes[p >= 1 - q_i], classes, p, allow_empty)
    sets[[i]] <- included
    set_probs[[i]] <- setNames(p[included], included)
  }

  structure(list(
    sets = sets,
    probs = set_probs,
    alpha = alpha,
    method = "mondrian",
    scores = scores,
    quantile = pooled_q,
    classes = levels(y),
    n_cal = length(split$cal),
    n_train = length(split$train),
    fitted_model = fitted,
    model = mod,
    groups_new = groups_new,
    group_quantiles = group_quantiles,
    allow_empty = allow_empty
  ), class = "predictset_class")
}


# Look up a group's conformal quantile, falling back to the pooled quantile for
# a group never seen during calibration.
group_quantile_for <- function(g, group_quantiles, pooled_q) {
  g <- as.character(g)
  if (g %in% names(group_quantiles)) {
    return(group_quantiles[[g]])
  }
  cli_warn(c(
    "Group {.val {g}} was not seen during calibration.",
    "i" = "Using the pooled quantile, which carries no group-conditional guarantee for this group."
  ))
  pooled_q
}
