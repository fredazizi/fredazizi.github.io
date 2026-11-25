# Shared calculation functions for the Shiny app and regression checks.
read_regression_data <- function(path, filename, sheet = NULL) {
  ext <- tolower(tools::file_ext(filename))
  df <- if (ext == "csv") {
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
             na.strings = c("", "NA", "N/A"), fileEncoding = "UTF-8-BOM")
  } else if (ext == "xlsx") {
    as.data.frame(readxl::read_xlsx(path, sheet = sheet %||% 1,
                                  .name_repair = "unique"))
  } else {
    stop("Choose a CSV or XLSX file.", call. = FALSE)
  }
  if (ncol(df) < 2L) stop("The file needs at least two columns.", call. = FALSE)
  names(df) <- make.unique(ifelse(is.na(names(df)) | names(df) == "",
                                 paste0("Column", seq_along(df)), names(df)))
  df
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

prediction_values <- function(text, observed) {
  if (is.null(text) || !nzchar(trimws(text))) return(sort(unique(observed)))
  parts <- strsplit(trimws(text), "[,;[:space:]]+")[[1]]
  if (length(parts) > 1000L) stop("Enter at most 1,000 x values.", call. = FALSE)
  values <- suppressWarnings(as.numeric(parts))
  if (!length(values) || any(!is.finite(values))) {
    stop("Enter finite numeric x values separated by commas or spaces.", call. = FALSE)
  }
  values
}

fit_regression <- function(df, x_name, y_name, ci_level, pi_level, x_text = "") {
  if (length(x_name) != 1L || length(y_name) != 1L ||
      !all(c(x_name, y_name) %in% names(df)) || x_name == y_name) {
    stop("Choose two different numeric columns for x and y.", call. = FALSE)
  }
  for (level in list(ci_level, pi_level)) {
    if (length(level) != 1L || !is.numeric(level) || !is.finite(level) ||
        level <= 0 || level >= 1) {
      stop("Confidence levels must be between 0 and 1.", call. = FALSE)
    }
  }
  x <- df[[x_name]]; y <- df[[y_name]]
  if (!is.numeric(x) || !is.numeric(y)) stop("Choose numeric columns.", call. = FALSE)
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 3L) stop("At least three complete numeric pairs are required.", call. = FALSE)
  if (length(unique(x[keep])) < 2L) stop("x must contain at least two distinct values.", call. = FALSE)
  if (length(unique(y[keep])) < 2L) stop("y must contain at least two distinct values.", call. = FALSE)
  data <- df[keep, c(x_name, y_name), drop = FALSE]
  # Symbols preserve column labels without parsing uploaded names as R code.
  formula <- as.formula(call("~", as.name(y_name), as.name(x_name)))
  model <- stats::lm(formula, data = data, na.action = na.fail)
  if (model$rank != 2L || any(!is.finite(coef(model)))) {
    stop("The selected columns do not give an estimable straight-line model.", call. = FALSE)
  }
  if (sum(residuals(model)^2) <= .Machine$double.eps * sum((y[keep] - mean(y[keep]))^2)) {
    stop("Residual variation is effectively zero; intervals and diagnostics cannot be computed reliably.", call. = FALSE)
  }
  new_x <- prediction_values(x_text, x[keep])
  newdata <- setNames(data.frame(new_x), x_name)
  mean_ci <- stats::predict(model, newdata = newdata, interval = "confidence", level = ci_level)
  individual_pi <- stats::predict(model, newdata = newdata, interval = "prediction", level = pi_level)
  grid <- setNames(data.frame(seq(min(x[keep]), max(x[keep]), length.out = 150)), x_name)
  coefficient_ci <- stats::confint(model, level = ci_level)
  list(model = model, data = data, x_name = x_name, y_name = y_name,
       ci_level = ci_level, pi_level = pi_level,
       n = sum(keep), omitted = sum(!keep), rows = which(keep),
       coefficient_ci = coefficient_ci,
       intervals = data.frame(x = new_x, fit = mean_ci[, "fit"],
                              mean_lower = mean_ci[, "lwr"], mean_upper = mean_ci[, "upr"],
                              prediction_lower = individual_pi[, "lwr"],
                              prediction_upper = individual_pi[, "upr"]),
       grid = grid,
       grid_ci = stats::predict(model, grid, interval = "confidence", level = ci_level),
       grid_pi = stats::predict(model, grid, interval = "prediction", level = pi_level))
}
