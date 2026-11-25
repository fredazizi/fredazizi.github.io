# Run from the repository root: Rscript tests/stat351-regression.R
source("apps/stat351-regression/regression.R")
close_to <- function(actual, expected, tolerance = 1e-6) {
  stopifnot(isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance)))
}
fails <- function(expr, pattern) {
  err <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err)))
}

# Published hprice1 observations used in class, all 88 rows.
# https://justinmshea.github.io/wooldridge/reference/hprice1.html
homes <- read_regression_data("tests/fixtures/housing.csv", "housing.csv")
a <- fit_regression(homes, "sqrft", "price", .95, .95, "2000, 2500")
close_to(coef(a$model), c(11.204145047054794, .14021097729032603))
close_to(a$intervals$fit, c(291.6260996277068, 361.7315882728699))
close_to(a$intervals$mean_lower, c(278.14088108, 344.06103676))
close_to(a$intervals$mean_upper, c(305.11131817, 379.40213978))
close_to(a$intervals$prediction_lower, c(164.44259231, 234.03647559))
close_to(a$intervals$prediction_upper, c(418.80960694, 489.42670096))
stopifnot(a$n == 88L, a$omitted == 0L)

# Separate levels must affect the intended interval and nothing else.
b <- fit_regression(homes, "sqrft", "price", .90, .99, "2000")
stopifnot(b$intervals$mean_lower > a$intervals$mean_lower[1],
          b$intervals$prediction_lower < a$intervals$prediction_lower[1],
          diff(b$coefficient_ci[2, ]) < diff(a$coefficient_ci[2, ]))

# Complete pairs, non-finite values, non-syntactic names, parsing and validation.
d <- homes
d$sqrft[1] <- NA; d$price[2] <- Inf
stopifnot(fit_regression(d, "sqrft", "price", .95, .95)$omitted == 2L)
names(d) <- c("Area (sq ft)", "Price in $1,000")
named <- fit_regression(d, names(d)[1], names(d)[2], .95, .95, "2000")
stopifnot(length(coef(named$model)) == 2L)
close_to(prediction_values("2e3, 2500;\n3000", 1:3), c(2000, 2500, 3000))
close_to(prediction_values("", c(3, 1, 3)), c(1, 3))
fails(prediction_values("2000, nope", 1:3), "finite numeric")
fails(fit_regression(homes, "sqrft", "sqrft", .95, .95), "different")
fails(fit_regression(homes, "sqrft", "price", 1, .95), "between")
fails(fit_regression(data.frame(x = 1, y = 2), "x", "y", .95, .95), "three")
fails(fit_regression(data.frame(x = rep(1, 4), y = 1:4), "x", "y", .95, .95), "distinct")
fails(fit_regression(data.frame(x = 1:4, y = rep(1, 4)), "x", "y", .95, .95), "distinct")
fails(fit_regression(data.frame(x = 1:4, y = 2*(1:4)), "x", "y", .95, .95), "Residual")

# Verify XLSX import using the workbook shipped with readxl.
book <- readxl::readxl_example("datasets.xlsx")
excel <- read_regression_data(book, "datasets.xlsx", "mtcars")
stopifnot(nrow(excel) == 32L, ncol(excel) == 11L)
invisible(fit_regression(excel, "wt", "mpg", .95, .95, "3"))

# Verify the Run button and stale-result clearing in a real Shiny session.
root <- getwd()
app_env <- new.env(parent = globalenv())
setwd("apps/stat351-regression")
sys.source("app.R", envir = app_env)
setwd(root)
shiny::testServer(app_env$server, {
  session$setInputs(file = data.frame(name = "housing.csv", size = 2000,
                                     type = "text/csv", datapath = file.path(root, "tests/fixtures/housing.csv")),
                    ci_level = .95, pi_level = .95, new_x = "2000")
  session$setInputs(x = "sqrft", y = "price")
  stopifnot(is.null(result()))
  session$setInputs(run = 1)
  close_to(analysis()$intervals$fit, 291.6260996277068)
  stopifnot(grepl("sqrft", output$lm_output))
  session$setInputs(pi_level = .99)
  stopifnot(is.null(result()))
  session$setInputs(run = 2)
  stopifnot(analysis()$pi_level == .99)
  session$setInputs(new_x = "invalid")
  session$setInputs(run = 3)
  stopifnot(inherits(result(), "error"))
})
cat("STAT351 regression checks passed.\n")
