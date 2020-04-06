library(mlr3learners.nnet)

test_that("classif.nnet", {
  learner = lrn("classif.nnet")
  fun = nnet::nnet.formula
  exclude = c(
    "formula", # handled via mlr3
    "data", # handled via mlr3
    "weights",# handled via mlr3
    "linout", # handled via nnet::nnet.formula
    "entropy", # handled via nnet::nnet.formula
    "softmax", # handled via nnet::nnet.formula
    "censored", # handled via nnet::nnet.formula
    "skip", # handled via nnet::nnet.formula
    "rang", # handled via nnet::nnet.formula
    "decay", # handled via nnet::nnet.formula
    "maxit", # handled via nnet::nnet.formula
    "Hess", # handled via nnet::nnet.formula
    "trace", # handled via nnet::nnet.formula
    "MaxNWts", # handled via nnet::nnet.formula
    "abstol", # handled via nnet::nnet.formula
    "reltol" # handled via nnet::nnet.formula
  )
  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "
Missing parameters:
",
    paste0("- '", ParamTest$missing, "'", collapse = "
")))
})

test_that("classif.nnet", {
  learner = lrn("classif.nnet  Default")
  fun = nnet::nnet.default
  exclude = c(
    "x", # handled via mlr3
    "y", # handled via mlr3
    "weights",# handled via mlr3
    "subset", # handled via nnet::nnet.default
    "na.action", # handled via nnet::nnet.default
    "contrasts" # handled via nnet::nnet.default
  )
  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "
Missing parameters:
",
    paste0("- '", ParamTest$missing, "'", collapse = "
")))
})
