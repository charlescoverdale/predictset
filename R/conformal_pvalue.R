#' Conformal P-Values
#'
#' Computes conformal p-values for new observations given calibration
#' nonconformity scores. The p-value indicates how conforming a new
#' observation is relative to the calibration set.
#'
#' @param scores A numeric vector of calibration nonconformity scores.
#' @param new_scores A numeric vector of nonconformity scores for new
#'   observations.
#'
#' @return A numeric vector of p-values, one per element of `new_scores`.
#'   Each p-value is in (0, 1].
#'
#' @examples
#' # Calibration scores from a conformal split
#' set.seed(42)
#' cal_scores <- abs(rnorm(100))
#' new_scores <- abs(rnorm(5))
#'
#' pvals <- conformal_pvalue(cal_scores, new_scores)
#' print(pvals)
#'
#' @export
conformal_pvalue <- function(scores, new_scores) {
  if (!is.numeric(scores) || length(scores) == 0) {
    cli_abort("{.arg scores} must be a non-empty numeric vector.")
  }
  if (!is.numeric(new_scores) || length(new_scores) == 0) {
    cli_abort("{.arg new_scores} must be a non-empty numeric vector.")
  }

  n <- length(scores)
  vapply(new_scores, function(s) {
    (1 + sum(scores >= s)) / (n + 1)
  }, numeric(1))
}


#' Adaptive Conformal Inference
#'
#' Implements basic Adaptive Conformal Inference (ACI) for sequential
#' prediction. The miscoverage level alpha is adjusted online based on
#' whether previous predictions covered the true values, maintaining
#' long-run coverage even under distribution shift.
#'
#' @param y_pred A numeric vector of point predictions (sequential).
#' @param y_true A numeric vector of true values (sequential).
#' @param alpha Target miscoverage level. Default `0.10`.
#' @param gamma Learning rate for alpha adjustment. Default `0.005`. Larger
#'   values adapt faster but are less stable.
#'
#' @return A list with components:
#' \describe{
#'   \item{lower}{Numeric vector of lower bounds.}
#'   \item{upper}{Numeric vector of upper bounds.}
#'   \item{covered}{Logical vector indicating whether each interval covered
#'     the true value.}
#'   \item{alphas}{Numeric vector of the adapted alpha values at each step.}
#'   \item{coverage}{Overall empirical coverage.}
#' }
#'
#' @references
#' Gibbs, I. and Candes, E. (2021).
#' Adaptive conformal inference under distribution shift.
#' *Advances in Neural Information Processing Systems*, 34.
#'
#' @examples
#' set.seed(42)
#' n <- 200
#' y_true <- cumsum(rnorm(n, sd = 0.1)) + rnorm(n)
#' y_pred <- c(0, y_true[-n])  # naive lag-1 prediction
#'
#' result <- conformal_aci(y_pred, y_true, alpha = 0.10, gamma = 0.01)
#' print(result$coverage)
#'
#' @export
conformal_aci <- function(y_pred, y_true, alpha = 0.10, gamma = 0.005) {
  if (!is.numeric(y_pred) || !is.numeric(y_true)) {
    cli_abort("{.arg y_pred} and {.arg y_true} must be numeric vectors.")
  }
  n <- length(y_pred)
  if (n != length(y_true)) {
    cli_abort("{.arg y_pred} and {.arg y_true} must have the same length.")
  }
  if (n < 2) {
    cli_abort("At least 2 observations are required for ACI.")
  }

  alpha <- validate_alpha(alpha)
  if (gamma <= 0) {
    cli_abort("{.arg gamma} must be positive.")
  }

  lower <- numeric(n)
  upper <- numeric(n)
  covered <- logical(n)
  alphas <- numeric(n)

  # Initialize: first prediction uses the target alpha with initial width 0
  alpha_t <- alpha
  residuals <- numeric(0)

  for (t in seq_len(n)) {
    alphas[t] <- alpha_t

    if (length(residuals) == 0) {
      # No calibration data yet; use Inf interval
      lower[t] <- -Inf
      upper[t] <- Inf
    } else {
      q <- conformal_quantile(residuals, alpha_t)
      lower[t] <- y_pred[t] - q
      upper[t] <- y_pred[t] + q
    }

    # Check coverage
    covered[t] <- (y_true[t] >= lower[t]) && (y_true[t] <= upper[t])
    residuals <- c(residuals, abs(y_true[t] - y_pred[t]))

    # Update alpha: increase if covered (tighten), decrease if not (widen)
    err_t <- if (covered[t]) 0 else 1
    alpha_t <- alpha_t + gamma * (err_t - alpha)
    alpha_t <- max(0.001, min(0.999, alpha_t))
  }

  list(
    lower = lower,
    upper = upper,
    covered = covered,
    alphas = alphas,
    coverage = mean(covered)
  )
}
