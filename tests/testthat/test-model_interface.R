test_that("make_model creates predictset_model object", {
  mod <- make_model(
    train_fun = function(x, y) lm(y ~ ., data = data.frame(y = y, x)),
    predict_fun = function(object, x_new) predict(object, newdata = as.data.frame(x_new)),
    type = "regression"
  )
  expect_s3_class(mod, "predictset_model")
  expect_equal(mod$type, "regression")
  expect_true(is.function(mod$train_fun))
  expect_true(is.function(mod$predict_fun))
})

test_that("make_model validates inputs", {
  expect_error(make_model("not_fun", identity, "regression"))
  expect_error(make_model(identity, "not_fun", "regression"))
  expect_error(make_model(identity, identity, "invalid"))
})

test_that("resolve_model handles formula", {
  mod <- resolve_model(y ~ ., type = "regression")
  expect_s3_class(mod, "predictset_model")
  expect_equal(mod$type, "regression")
})

test_that("resolve_model handles fitted lm", {
  data <- make_regression_data(50, 3)
  fit <- lm(y ~ ., data = data.frame(y = data$y, data$x))
  mod <- resolve_model(fit, type = "regression")
  expect_s3_class(mod, "predictset_model")
})

test_that("resolve_model passes through predictset_model", {
  mod <- test_reg_model
  result <- resolve_model(mod)
  expect_identical(result, mod)
})

test_that("print.predictset_model works", {
  mod <- test_reg_model
  expect_no_error(capture.output(print(mod), type = "message"))
  expect_invisible(print(mod))
})
