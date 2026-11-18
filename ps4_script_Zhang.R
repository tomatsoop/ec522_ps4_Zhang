library(tidyverse)
library(fpp3)
library(BVAR)

liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

# 2.1a FRQ 

log_sales_auto = autoplot(liquor, sales) +  labs(title = "Seasonal plot: US liquor sales", y = "Log sales")

log_sales_season = gg_season(liquor, log(sales)) + labs(title = "Seasonal plot: US liquor sales", y = "Log sales")

log_sales_auto
log_sales_season

# The seasonal trend of liquor sales remain steady throughout the year however there is a spike in December 
# followed by a dip in January. The auto plot shows growth with the  level of the series and suggests that logging 
# sales may take away that growth in level while capturing the month to month trend.

#2.2 ARIMA and ETS model FRQ

liquor_fit = liquor |> model(arima = ARIMA(log(sales)), ets   = ETS(log(sales)))
liquor_fit_arima = liquor_fit |> select(arima) |> report()
liquor_fit_ets = liquor_fit |> select(ets) |> report()

liquor_fit_arima

liquor_fit_ets

# The ARIMA Model choose the parameters (1,0,2)(2,1,1)[12] with drift. This means that 
# we are lagging it for 2 periods in the seasonal component(P), taking the first difference (D), 1 period of forecast errors (Q) 
# For the non seasonal model, we are lagging it once and incorporating 2 periods of forecast errors (q).
# For the ETS model, it chooses 3 additive components with alpha = 0.329 (level smoothing), beta = 0.007 (trend smoothing), and gamma = 0.0001 (seasonal smoothing).

#2.2b FRQ
liquor_fit_residuals =liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit_ljung = liquor_fit |> select(arima) |> augment() |> features(.innov, ljung_box, lag = 24, dof = 6)

liquor_fit_residuals
liquor_fit_ljung

# The residuals  are clustered around 0 but there still seems to be lagging trends within the residuals that have not been captured by the model. 


# The Ljung p value of 0 means we reject the null hypothesis that the residuals are white noise.

# 2.3 The Horse Race  FRQ
# liquor_cv <- liquor |>
#   stretch_tsibble(.init = 120, .step = 1) |>
#   filter(.id < max(.id))
# 
# liquor_cv_fit <- liquor_cv |>
#   model(snaive = SNAIVE(sales),
#         ets    = ETS(log(sales)),
#         arima  = ARIMA(log(sales)))

#saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

liquor_cv_fit = readRDS("results/liquor_cv_fit.rds")

# 2.3b One Month Ahead Forecast and Accuracy
liquor_cv_fc = liquor_cv_fit |> forecast(h = 1)

liquor_accuracy = liquor_cv_fc |> accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))
liquor_accuracy_table = liquor_accuracy |> select(.model, RMSE, MAE) |> knitr::kable(digits = 2)
liquor_cv_fc
liquor_accuracy_table

#The dates of the first forecast is January 2002 and the last forecast is December 2019. 
liquor_accuracy_table
# The ARIMA model forecasts the most accurately for both the RMSE and the MAE model.

# 2.4 Are the Differences Significant? FRQ

liquor_errs = liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

arima_ets = forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)
arima_snaive = forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)

arima_ets
arima_snaive

# The DM statistic for the ARIMA vs ETS model is -2.9062 with a p-value of 0.004042
# The DM statistic for the ARIMA vs SNAIVE model is -10.755 with a p-value of < 2.2e-16
# 
# This suggests that the ARIMA model shows significant evidence that it is better than both the ETS and the SNAIVE model.
# Both DM statistic reports a negative value favoring the ARIMA model. As a liquor store chain, I would use the ARIMA model.

# Part 3 - Forecast Combination 
#3a Equal weight Combination of ARIMA and ETS
liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc = accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

liquor_combo_fc
# The combo model does significantly better than ETS or SNAIVE but the ARIMA model still is better by 3.8 in the RMSE and 1.1 the MAE.

# 3b Correlation FRQ
cor(liquor_errs$arima, liquor_errs$ets)
# The correlation of the ARIMA and ETS model is 0.87. Generally when two models are correlated in terms of error, 
# then combining the models results in producing similar results. Since the ARIMA and ETS model uses the same data, 
# correlated error does not add more information to the model when averaged.


# Part 4: A Medium Scale VAR vs. a BVAR
qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

# 4.1b 
growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)

# 4.1c TS-CV FRQ
N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]

qd$date[c(121, N)]
# Each variable ill produce 120 forecast, starting in 1990-03-01 till 2019-12-01.

# 4.2. OLS VARs FRQ
var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))

rmse_by_var(fc_var1)
rmse_by_var(fc_var4)

# The correlation of the ARIMA and ETS model is 0.87. Generally when two models are correlated in terms of error, 
# then combining the models results in producing similar results. Since the ARIMA and ETS model uses the same data, 
# correlated error does not add more information to the model when averaged.

#4.3 A BVAR
bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}

set.seed(422)
fc_bvar_03 <- bvar_cv(0.3)
#saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")

fc_bvar_03 = readRDS("results/fc_bvar_03.rds")

# 4.3b FRQ
set.seed(422)
fc_bvar_005 <- bvar_cv(0.05)

var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)

saveRDS(var_results, "results/var_results.rds")

var_results = var_results |> knitr::kable(digits = 3)

var_results


# How does the BVAR(4) with compare to the OLS VAR(4)? 

# Both have the same variables and the same lags, so what explains any difference?
# The BVAR(4) does better than the VAR(4) for all of the variables. There are some variables that does
# better by a large margin. The VAR(4) model is a OLs model using 4 lag periods. The difference in the 
# BVAR(4) also uses 4 lag periods however it applies shrinkage to prevent overfitting. This generalizes 
# the model in a way that the VAR(4) fails to capture.

# How does it compare to the VAR(1)?
# # BVAR(4) compared to VAR(1) does worse for most of the variables except for inv, infl, and gs10. VAR(4) 
# does better in t-1 period sensitive variables like GDP, cons, emp, and unrate. This tells us that some variables
# will do better with a longer lag period and some do better with a shorter lag period.
# # What happens when ? What does this tell you about the amount of shrinkage? Suppose you used this table to choose your forecasting model. Would the winner’s RMSE here be a fair estimate of how accurately it will forecast in the future? Why or why not?
# when lambda = 0.05, BVAR(4) does worse than BVAR(4) lambda = 0.3, however there are some variables where lambda = 0.05 
# does better than VAR(4). This suggests that the amount of shrinkage is important when considering how sensitive the variable 
# is to the long term effects. 

# 4.3c 
e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]

forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

# The Diebold-Marino test for GDP growth for VAR(4) vs BVAR(4) with lambda = 0.3 has a p-value of 0.03. This tells
# us that at the 0.05 significance level, ee reject the null hypothesis and say that there is statistically significant 
# evidence that BVAR(4) is better than the VAR(4). This confirms my result in 4.3b that BVAR(4) does better 
# however the p-value for significance level is not as strong on the 0.05 level.

e_var4 <- actual[, "inv"] - fc_var4[, 3]
e_bvar <- actual[, "inv"] - fc_bvar_03[, 3]
forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

# Part 5 – A Factor Model with FRED-MD
md_raw <- read_csv("data/fredmd_2026-09.csv", show_col_types = FALSE)
tcode <- unlist(md_raw[1, -1])
md <- md_raw[-1, ] |> mutate(date = as.Date(sasdate, "%m/%d/%Y"))

transform_series <- function(x, code) {
  d <- function(v) c(NA, diff(v))
  switch(as.character(code),
         "1" = x,                      # level
         "2" = d(x),                   # first difference
         "3" = d(d(x)),                # second difference
         "4" = log(x),                 # log
         "5" = 100 * d(log(x)),        # growth rate
         "6" = 100 * d(d(log(x))),     # change in the growth rate
         "7" = d(x / dplyr::lag(x) - 1))
}

# 5.1b
fm <- map2_dfc(md[names(tcode)], tcode, transform_series) |>
  mutate(date = md$date, .before = 1) |>
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

fm <- fm[, colSums(is.na(fm)) == 0]

dim(fm)

#5.1c FRQ
tcode[c("INDPRO", "PAYEMS", "CPIAUCSL", "FEDFUNDS")]
# Transformation:
# INDPRO was transformed with 100 * d(log(x)) giving us the growth rate.
# PAYEMS was transformed with 100 * d(log(x)) giving us the growth rate.
# CIPAUCSL was transformed with 100 * d(d(log(x))) giving us the change in growth rate.
# FEDFUNDS was transformed with d(x) giving us the first difference. 
# 
# INDPRO represents the total industrial production index where the growth trends ith the level of the series. 
# The FEDFUNDS is the interest rate set by the Feds and does not follow a trend. Instead we take the difference of

# 5.2. Estimate the Factors

# 5.2a PCA
Z <- fm |> select(-date) |> as.matrix()

pc <- prcomp(scale(Z))

PCA5 = summary(pc)$importance[, 1:5]
PCA5

# The first three components of the PCA explain 0.29 of the variation.

# 5.2b
F1 <- pc$x[, 1] * sign(cor(pc$x[, 1], fm$INDPRO))

First_Factor = tibble(date = fm$date, F1 = F1) |>
  ggplot(aes(date, F1)) +
  geom_line() +
  labs(title = "First Factor from FRED-MD", x = NULL, y = NULL) +
  theme_minimal()
First_Factor

# 5.2c FRQ
pc1 = sort(abs(cor(pc$x[, 1], Z))[1, ], decreasing = TRUE)[1:8]
pc2 = sort(abs(cor(pc$x[, 2], Z))[1, ], decreasing = TRUE)[1:8]:8]
pc1 
pc2
# The names of the series can be looked up in the FRED-MD documentation, FRED-MD_updated_appendix.pdf, which is posted on Canvas. Based on these series, and on your plot, how would you describe what each factor captures?

# 5.3. Replicate the Horse Race from Class FRQ
ip <- fm$INDPRO
n <- nrow(Z)
init <- which(fm$date == as.Date("1989-12-01"))

e_ar <- NULL
e_fa <- NULL
for (T in init:(n - 1)) {
  F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:3]         # factors using data through T
  d <- data.frame(y_next = ip[2:T], y = ip[1:(T - 1)], F_T[1:(T - 1), ])
  new <- data.frame(y = ip[T], t(F_T[T, ]))
  e_ar <- c(e_ar, ip[T + 1] - predict(lm(y_next ~ y, d), new))
  e_fa <- c(e_fa, ip[T + 1] - predict(lm(y_next ~ ., d), new))
}

c(RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)))

# In class we saw an RMSE of 0.617 for the AR(1) and 0.589 with the factors. Confirm that you get the same numbers. Why would it be cheating to estimate the factors once, using the full sample, and use those in the horse race?

#5.4. A Direct Forecast of Inflation FRQ
direct_race <- function(series, h = 12, r = 3) {
  lv <- md |>
    mutate(log_x = log(.data[[series]]),
           g = 100 * (log_x - lag(log_x))) |>
    filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))
  yh <- 100 * (lead(lv$log_x, h) - lv$log_x)      # growth over the next h months
  g  <- lv$g                                      # growth over the latest month
  
  e_ar <- NULL
  e_fa <- NULL
  for (T in init:(n - h)) {
    F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:r, drop = FALSE]
    i <- 1:(T - h)
    d <- data.frame(yh = yh[i], g = g[i], F_T[i, , drop = FALSE])
    new <- data.frame(g = g[T], F_T[T, , drop = FALSE])
    e_ar <- c(e_ar, yh[T] - predict(lm(yh ~ g, d), new))
    e_fa <- c(e_fa, yh[T] - predict(lm(yh ~ ., d), new))
  }
  tibble(series, forecasts = length(e_ar),
         RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)),
         MAE_AR = mean(abs(e_ar)), MAE_FA = mean(abs(e_fa)),
         DM_pvalue = forecast::dm.test(e_ar, e_fa, h = h, power = 2,
                                       varestimator = "bartlett")$p.value)
}

direct_race("CPIAUCSL")
# Do the factors improve the forecast of 12-month CPI inflation? By what percentage do they reduce the RMSE? Is the difference statistically significant?