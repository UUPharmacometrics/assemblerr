ParameterValues <- setClass("ParameterValues")

ParameterValuesOnly <- setClass("ParameterValuesOnly", contains = "ParameterValues")


ParameterValuesMeanSd <- setClass("ParameterValuesMeanSd",
                                   contains = "ParameterValuesOnly",
                                   slots = c(mean = "numeric", sd = "numeric"))

pv_mean_sd <- function(mean, sd) {
  ParameterValuesMeanSd(mean = mean, sd = sd)
}

pv_mean_var <- function(mean, var) {
  ParameterValuesMeanSd(mean = mean, sd = sqrt(var))
}




ParameterValuesLogMeanLogSd <- setClass("ParameterValuesLogMeanLogSd",
                                  contains = "ParameterValuesOnly",
                                  slots = c(log_mean = "numeric", log_sd = "numeric"))

pv_lmean_lsd <- function(log_mean, log_sd) {
  ParameterValuesLogMeanLogSd(log_mean = log_mean, log_sd = log_sd)
}

ParameterValuesMedianLogSd <- setClass("ParameterValuesMedianLogSd",
                                        contains = "ParameterValuesOnly",
                                        slots = c(median = "numeric", log_sd = "numeric"))

pv_median_lsd <- function(median, log_sd) {
  ParameterValuesMedianLogSd(median = median, log_sd = log_sd)
}

add_pv_model <- function(pv, pv_model) {
  to_class <- paste0(class(pv_model),
                     gsub(
                       pattern = "^ParameterValues",
                       replacement = "",
                       x = class(pv)
                      )
                    )
  as(pv, to_class)
}


convert_pv <- function(from, to) {
  super_class <- selectSuperClasses(class(from))
  to_class <- paste0(super_class, snake_case_to_camel_case(to))
  as(from, to_class)
}

ParameterValuesModel <- setClass("ParameterValuesModel", contains = "ParameterValues")


# normal distribution -----------------------------------------------------



ParameterValuesNormal <- setClass("ParameterValuesNormal",
                                  contains = "ParameterValuesModel",
                                  slots = c(.mu = "numeric", .sigma2 = "numeric"))

ParameterValuesNormalMeanVar <- setClass("ParameterValuesNormalMeanVar",
                                         contains = "ParameterValuesNormal",
                                         slots = c(mu = "numeric", sigma2 = "numeric"))
setMethod("initialize",
          signature = "ParameterValuesNormalMeanVar",
          function(.Object, mu, sigma2) {
            callNextMethod(.Object, .mu = mu, .sigma2 = sigma2, mu = mu, sigma2 = sigma2)
          })

setAs("ParameterValuesNormal", "ParameterValuesNormalMeanVar",
      def = function(from) ParameterValuesNormalMeanVar(from@.mu, from@.sigma2)
)

setAs("ParameterValuesMeanSd", "ParameterValuesNormalMeanVar",
      def = function(from) ParameterValuesNormalMeanVar(from@mean, from@sd^2)
)

ParameterValuesNormalMeanSd <- setClass("ParameterValuesNormalMeanSd",
                                         contains = "ParameterValuesNormal",
                                         slots = c(mu = "numeric", sigma = "numeric"))

setMethod("initialize",
          signature = "ParameterValuesNormalMeanSd",
          function(.Object, mu, sigma) {
            callNextMethod(.Object, .mu = mu, .sigma2 = sigma^2, mu = mu, sigma = sigma)
          })

setAs("ParameterValuesNormal", "ParameterValuesNormalMeanSd",
      def = function(from) ParameterValuesNormalMeanSd(mu = from@.mu, sigma = sqrt(from@.sigma2))
)

setAs("ParameterValuesMeanSd", "ParameterValuesNormalMeanSd",
      def = function(from) ParameterValuesNormalMeanSd(mu = from@mean, sigma = from@sd)
)


# log-normal distribution -------------------------------------------------



ParameterValuesLogNormal <- setClass("ParameterValuesLogNormal",
                                  contains = "ParameterValuesModel",
                                  slots = c(.log_mu = "numeric", .log_sigma = "numeric"))

ParameterValuesLogNormalLogMeanLogSd <- setClass("ParameterValuesLogNormalLogMeanLogSd",
                                         contains = "ParameterValuesLogNormal",
                                         slots = c(log_mu = "numeric", log_sigma = "numeric"))
setMethod("initialize",
          signature = "ParameterValuesLogNormalLogMeanLogSd",
          function(.Object, log_mu, log_sigma) {
            callNextMethod(.Object, .log_mu = log_mu, .log_sigma = log_sigma, log_mu = log_mu, log_sigma = log_sigma)
          })

setAs("ParameterValuesLogNormal", "ParameterValuesLogNormalLogMeanLogSd",
      def = function(from) ParameterValuesLogNormalLogMeanLogSd(from@.log_mu, from@.log_sigma)
)

setAs("ParameterValuesLogMeanLogSd", "ParameterValuesLogNormalLogMeanLogSd",
      def = function(from) ParameterValuesLogNormalLogMeanLogSd(from@log_mean, from@log_sd)
)


ParameterValuesLogNormalMedianLogSd <- setClass("ParameterValuesLogNormalMedianLogSd",
                                                 contains = "ParameterValuesLogNormal",
                                                 slots = c(m = "numeric", log_sigma = "numeric"))
setMethod("initialize",
          signature = "ParameterValuesLogNormalMedianLogSd",
          function(.Object, m, log_sigma) {
            callNextMethod(.Object, .log_mu = log(m), .log_sigma = log_sigma, m = m, log_sigma = log_sigma)
          })

setAs("ParameterValuesLogNormal", "ParameterValuesLogNormalMedianLogSd",
      def = function(from) ParameterValuesLogNormalMedianLogSd(m = exp(from@.log_mu), log_sigma = from@.log_sigma)
)

setAs("ParameterValuesMedianLogSd", "ParameterValuesLogNormalMedianLogSd",
      def = function(from) ParameterValuesLogNormalMedianLogSd(from@median, from@log_sd)
)

ParameterValuesLogNormalMeanSd <- setClass("ParameterValuesLogNormalMeanSd",
                                                contains = "ParameterValuesLogNormal",
                                                slots = c(mu = "numeric", sigma = "numeric"))
setMethod("initialize",
          signature = "ParameterValuesLogNormalMeanSd",
          function(.Object, mu, sigma) {
            callNextMethod(.Object, .log_mu = log(mu/sqrt(1 + sigma^2/mu^2)), .log_sigma = sqrt(log(sqrt(1 + sigma^2/mu^2))), mu = mu, sigma = sigma)
          })

setAs("ParameterValuesLogNormal", "ParameterValuesLogNormalMeanSd",
      def = function(from) ParameterValuesLogNormalMeanSd(
        mu = exp(from@.log_mu + 0.5*from@.log_sigma^2),
        sigma = exp(from@.log_mu + 0.5*from@.log_sigma^2) * sqrt(exp(from@.log_sigma^2) - 1)
      )
)

setAs("ParameterValuesMeanSd", "ParameterValuesLogNormalMeanSd",
      def = function(from) ParameterValuesLogNormalMeanSd(from@mean, from@sd)
)
