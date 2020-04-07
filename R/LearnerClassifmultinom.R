#' @title Classification multinom Learner
#'
#' @name mlr_learners_classif.multinom
#'
#' @description
#' A [mlr3::LearnerClassif] implementing classification JRip from package \CRANpkg{nnet}.
#' Calls [nnet::multinom()].
#'
#'
#' @templateVar id classif.multinom
#' @template section_dictionary_learner
#'
#' @references
#' Venables, W (2002).
#' Modern Applied Statistics with S. Fourth Edition.
#'
#' @export
LearnerClassifmultinom = R6Class("LearnerClassifmultinom",
  inherit = LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {

      ps = ParamSet$new(
        params = list(
          ParamUty$new(id = "subset", tags = "train"),
          ParamUty$new(id = "na.action", tags = "train"),
          ParamUty$new(id = "contrasts", default = NULL, tags = "train"),
          ParamLgl$new(id = "Hess", default = FALSE, tags = "train"),
          ParamInt$new(id = "summ", default = 0L, lower = 0L, upper = 3L, tags = "train"),
          ParamLgl$new(id = "censored", default = FALSE, tags = "train"),
          ParamLgl$new(id = "model", default = FALSE, tags = "train")
        )
      )

      super$initialize(
        id = "classif.multinom",
        packages = "nnet",
        feature_types = c("numeric", "factor", "ordered"),
        predict_types = c("prob", "response"),
        param_set = ps,
        properties = c("twoclass", "multiclass", "weights"),
        man = "mlr3learners.nnet::mlr_learners_classif.multinom"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      if ("weights" %in% task$properties) {
        pars = insert_named(pars, list(weights = task$weights$weight))
      }
      f = task$formula()
      data = task$data()
      mlr3misc::invoke(nnet::multinom, formula = f, data = data, .args = pars)
    },

    .predict = function(task) {
      response = NULL
      prob = NULL
      newdata = task$data(cols = task$feature_names)

      if (self$predict_type == "response") {
        response = mlr3misc::invoke(predict, self$model, newdata = newdata, type = "class")
      } else {
        prob = mlr3misc::invoke(predict, self$model, newdata = newdata, type = "probs")
        if (task$properties == "twoclass") {
          prob = cbind(1 - prob, prob)
          colnames(prob) = self$model$lev
        }
      }
      PredictionClassif$new(task = task, response = response, prob = prob)
    }
  )
)
