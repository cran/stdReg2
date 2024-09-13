## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(stdReg2)

## -----------------------------------------------------------------------------
nhefs_dat <- causaldata::nhefs_complete
summary(nhefs_dat)

## ----echo=FALSE, out.width='30%'----------------------------------------------
library(survival)
states <- c("X", "Z", "Y")
connect <- matrix(0, 3, 3, dimnames = list(states, states))
connect[cbind(c(1, 2, 2), c(3, 1, 3))] <- 1
statefig(cbind(c(.2, .5, .8), c(.3, .6, .3)), connect)

## -----------------------------------------------------------------------------
m <- glm(wt82_71 ~ qsmk + sex + race + age + I(age^2) + 
        as.factor(education) + smokeintensity + I(smokeintensity^2) + 
        smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) +
          wt71 + I(wt71^2), 
        data = nhefs_dat)
summary(m)

## -----------------------------------------------------------------------------
m2 <- standardize_glm(wt82_71 ~ qsmk + sex + race + age + I(age^2) + 
               as.factor(education) + smokeintensity + I(smokeintensity^2) + 
               smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) +
               wt71 + I(wt71^2), 
            data = nhefs_dat, 
            values = list(qsmk = c(0,1)),
            contrasts = c("difference", "ratio"),
            reference = 0)

m2

plot(m2)
plot(m2, contrast = "difference", reference = 0)

## -----------------------------------------------------------------------------
(tidy(m2) -> tidy_m2) |> print()

## -----------------------------------------------------------------------------
m2_dr <- standardize_glm_dr(formula_outcome = wt82_71 ~ qsmk + sex + race + age + I(age^2) + 
               as.factor(education) + smokeintensity + I(smokeintensity^2) + 
               smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) +
               wt71 + I(wt71^2), 
               formula_exposure = qsmk ~ sex + race + age + I(age^2) +
                as.factor(education) + smokeintensity + I(smokeintensity^2) + 
                smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) + 
                wt71 + I(wt71^2),
            data = nhefs_dat, 
            values = list(qsmk = c(0,1)),
            contrast = c("difference", "ratio"),
            reference = 0) 

m2_dr

(tidy(m2_dr) -> tidy_m2_dr) |> print()

## -----------------------------------------------------------------------------
m2_dr$res$fit_outcome
m2_dr$res$fit_exposure

## -----------------------------------------------------------------------------
hist(m2_dr$res$fit_exposure$fitted[nhefs_dat$qsmk == 0], 
    xlim = c(0, 1), main = "qsmk = 0", xlab = "estimated propensity")
hist(m2_dr$res$fit_exposure$fitted[nhefs_dat$qsmk == 1], 
    xlim = c(0, 1), main = "qsmk = 1", xlab = "estimated propensity")

## -----------------------------------------------------------------------------
nhefs_dat$gained_weight <- 1.0 * (nhefs_dat$wt82_71 > 0)
m3 <- standardize_glm(gained_weight ~ qsmk + sex + race + age + I(age^2) + 
               as.factor(education) + smokeintensity + I(smokeintensity^2) + 
               smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) +
               wt71 + I(wt71^2), 
            data = nhefs_dat, 
            family = "binomial",
            values = list(qsmk = c(0,1)),
            contrasts = c("difference", "ratio"),
            reference = 0)

m3

## -----------------------------------------------------------------------------
m3_log <- standardize_glm(gained_weight ~ qsmk + sex + race + age + I(age^2) + 
               as.factor(education) + smokeintensity + I(smokeintensity^2) + 
               smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) +
               wt71 + I(wt71^2), 
            data = nhefs_dat, 
            family = "binomial",
            values = list(qsmk = c(0,1)),
            contrasts = c("difference"),
            transforms = c("log"),
            reference = 0)
m3_log

## -----------------------------------------------------------------------------
tm3_log <- tidy(m3_log)
tm3_log$rr <- exp(tm3_log$Estimate)
tm3_log$rr.lower <- exp(tm3_log$lower.0.95)
tm3_log$rr.upper <- exp(tm3_log$upper.0.95)

subset(tm3_log, contrast == "difference" & qsmk == 1)
m3

## -----------------------------------------------------------------------------
m3_odds <- standardize_glm(gained_weight ~ qsmk + sex + race + age + I(age^2) + 
               as.factor(education) + smokeintensity + I(smokeintensity^2) + 
               smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) +
               wt71 + I(wt71^2), 
            data = nhefs_dat, 
            family = "binomial",
            values = list(qsmk = c(0,1)),
            contrasts = c("ratio"),
            transforms = c("odds"),
            reference = 0)
m3_odds

m3_logit <- standardize_glm(gained_weight ~ qsmk + sex + race + age + I(age^2) + 
               as.factor(education) + smokeintensity + I(smokeintensity^2) + 
               smokeyrs + I(smokeyrs^2) + as.factor(exercise) + as.factor(active) +
               wt71 + I(wt71^2), 
            data = nhefs_dat, 
            family = "binomial",
            values = list(qsmk = c(0,1)),
            contrasts = c("difference"),
            transforms = c("logit"),
            reference = 0)
m3_logit

m3_logOR <- tidy(m3_logit) |> 
  subset(contrast == "difference" & qsmk == 1) 
  
sprintf("%.2f (%.2f to %.2f)", exp(m3_logOR$Estimate), 
        exp(m3_logOR$lower.0.95), exp(m3_logOR$upper.0.95))

