library(mlr3learners.nnet)

test_that("classif.nnet", {
  learner = lrn("classif.nnet")
  fun = nnet::multinom
  exclude = c(
    "formula", # handled via mlr3
    "data", # handled via mlr3
    "weights" # handled via mlr3
  )
  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "
Missing parameters:
",
    paste0("- '", ParamTest$missing, "'", collapse = "
")))
})


