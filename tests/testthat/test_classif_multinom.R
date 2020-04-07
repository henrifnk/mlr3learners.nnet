context("classif.nnet")

# unit tests fail on maOS devel
skip_on_os("macOS")

test_that("autotest", {
  learner = LearnerClassifmultinom$new()
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
