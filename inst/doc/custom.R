## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(stdReg2)
library(nnet)

## -----------------------------------------------------------------------------
nhefs_dat <- causaldata::nhefs_complete
# the target outcome model
# mfit <- multinom(active ~ qsmk + sex + race + age + I(age^2) + 
#               as.factor(education) + smokeintensity, data = nhefs_dat)

## here we predict the probability of being inactive (level 3)
predict_multinom <- function(...) {
  predict(..., type = "probs")[, 3]
}

std_custom <- standardize(fitter = "multinom", 
                          arguments = list(formula = active ~ qsmk + sex + 
                                             race + age + I(age^2) + 
               as.factor(education) + smokeintensity, trace = FALSE), 
               predict_fun = predict_multinom, 
               data = nhefs_dat, 
               values = list(qsmk = 0:1))
std_custom

## -----------------------------------------------------------------------------

std_custom2 <- standardize_level(fitter_list = list("multinom", "glm"),
                          arguments = list(list(formula = active ~ qsmk + sex + 
                                             race + age + I(age^2) + 
               as.factor(education) + smokeintensity, trace = FALSE), 
               list(formula = I(active == 2) ~ qsmk + sex + 
                                             race + age  + 
               as.factor(education) + smokeintensity, family = binomial)),
               predict_fun_list = list(predict_multinom, 
                                       \(...) predict.glm(..., type = "response")),
               data = nhefs_dat, 
               values = list(qsmk = 0:1))
std_custom2



