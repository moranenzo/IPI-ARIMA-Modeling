install.packages("ggplot2")
install.packages("fUnitRoots") 
install.packages("tseries")  
library(tseries)
library(ggplot2)
library(forecast)
library(fUnitRoots)





##### Loading source CSV files and initial observations #####

df = read.csv(file = "valeurs_mensuelles.csv", sep = ";")
df2 = df[-c(1:3), ] # remove unnecessary columns
df3 = df2[order(df2[[1]]), ] # sort by dates
typeof(df3[2,2])
typeof(df3[2,1])
df3[[2]] = as.numeric(df3[[2]])
df3[[1]] = as.Date(paste0(df3[[1]], "-01"))
df3[[1]] = as.Date(df3[[1]])

# Raw series plot

ggplot(df3, aes(x = df3[[1]], y = df3[[2]])) +
  geom_line(color = "steelblue") +
  labs(
    title = "Manufacture of agricultural and forestry machinery",
    x = "Date",
    y = "Values"
  ) +
  theme_minimal() +
  scale_x_date(
    breaks = seq(as.Date("1990-01-01"), max(df3[[1]]), by = "4 years"),
    date_labels = "%Y-%m"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
  )

# Extract the series
serie <- df3[[2]]

# Plot the ACF on the raw series
acf(serie, main = "ACF", lag.max = 100)

# Load the seasonally adjusted series (CVS-CJO)

df_cor = read.csv(file = "valeurs_mensuelles_corrigées.csv", sep = ";")
df_cor2 = df_cor[-c(1:3), ] # remove unnecessary columns
df_cor3 = df_cor2[order(df_cor2[[1]]), ] # sort by dates
df_cor3[[2]] =as.numeric(df_cor3[[2]])
df_cor3[[1]] = as.Date(paste0(df_cor3[[1]], "-01"))

ggplot(df_cor3, aes(x = df_cor3[[1]], y = df_cor3[[2]])) +
  geom_line(color = "steelblue") +
  labs(
    title = "Manufacture of agricultural and forestry machinery, CVS-CJO",
    x = "Date",
    y = "Values"
  ) +
  theme_minimal() +
  scale_x_date(
    breaks = seq(as.Date("1990-01-01"), max(df_cor3[[1]]), by = "4 years"),
    date_labels = "%Y-%m"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
  )

serie_corr=df_cor3[[2]]
acf(serie_corr, main = "ACF")
pacf(serie_corr, main = "PACF")








#### Stationarity #####

# test selection

# regression of our series on the time variable:

temps = 1:length(serie_corr)
modele = lm(serie_corr ~ temps)
summary(modele)

# So we choose ADF with constant

adf <- adfTest(serie_corr, lag=0, type="c") 
adf

# Before interpreting the test, let’s check that the model’s residuals are not autocorrelated, otherwise the test would not be valid: Ljung-Box test

Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}

Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))

# test invalid, so we search for the right number of lags until no more autocorrelated residuals

adfTestvalid <-function(series,kmax,type){ 
  k <- 0
  noautocorr <- 0
  while (noautocorr==0){
    cat(paste0("ADF with ",k, " lags: residuals OK? "))
    adf <- adfTest(series,lags=k,type=type)
    pvals <- Qtests(adf@test$lm$residuals,24,fitdf=length(adf@test$lm$coefficients))[,2]
    if (sum(pvals<0.05,na.rm=T) == 0) {
      noautocorr <- 1; cat("OK \n")}
    else cat("nope \n")
    k <- k + 1
  }
  return(adf)
}

adf <- adfTestvalid(serie_corr,24,"c")

# We have had to consider 11 lags on the ADF test to erase residual autocorrelation

adf <- adfTest(serie_corr, lag=11, type="c")

# We support our result with PP and KPSS tests
pp.test(serie_corr)
kpss.test(serie_corr)

# We conclude that our series is not stationary, so we choose to difference it once

diff_values <- diff(df_cor3[[2]])
df_cor3$diff <- c(NA, diff_values)
df_cor3 <- df_cor3[-1, ]
serie_diff = df_cor3[,3]

# we repeat the procedure to test the stationarity of our differenced series

temps <- 1:length(serie_diff)
modele <- lm(serie_diff ~ temps)
summary(modele)

adf <- adfTest(serie_diff, lag=0, type="nc")
Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))
adf <- adfTestvalid(serie_diff,24,"nc")
adf <- adfTest(serie_diff, lag=4, type="nc")
pp.test(serie_diff)
kpss.test(serie_diff)

# The differenced series is thus stationary

ggplot(df_cor3, aes(x = df_cor3[[1]])) +
  geom_line(aes(y = df_cor3[[2]], color = "Raw serie"), size = 1) +
  geom_line(aes(y = df_cor3[[3]], color = "Diff serie"), size = 1) +
  scale_color_manual(
    values = c("Raw serie" = "steelblue", "Diff serie" = "firebrick"),
    name = ""
  ) +
  labs(
    title = "Manufacture of agricultural and forestry machinery",
    x = "Date",
    y = ""
  ) +
  scale_x_date(
    breaks = seq(as.Date("1990-01-01"), max(df_cor3[[1]]), by = "4 years"),
    date_labels = "%Y"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
  )







#####Identification #####

# ACF 
acf(serie_diff, main = "ACF")

# PACF 
pacf(serie_diff, main = "PACF")

#Test: For each model, display the significance tests of the coefficients, and the test for no autocorrelation of the residuals (Ljung-Box)

Serie_TS=serie_corr
pmax <- 5
qmax<- 2

signif <-
  function(estim) {
    coef <- estim$coef 
    se <- sqrt(diag(estim$var.coef)) 
    t <- coef / se  #compute the Student t-statistic for each coefficient
    pval <- (1 - pnorm(abs(t))) * 2 
    return(rbind(coef, se, pval))
  }

Qtests <- function(series, k, fitdf = 0) {
  pvals <- apply(
    matrix(1:k),
    1,
    FUN = function(l) {
      pval <- if (l <= fitdf)
        NA
      else
        Box.test(series,
                 lag = l,
                 type = "Ljung-Box",
                 fitdf = fitdf)$p.value 
      return(c("lag" = l, "pval" = pval))
    }
  )
  return(t(pvals))
}


modelchoice <- function(p, q, data = Serie_TS, k = 24) {
  estim <-
    try(arima(data, c(p, 1, q), optim.control = list(maxit = 20000)))
  if (class(estim) == "try-error")
    return(c(
      "p" = p,
      "q" = q,
      "arsignif" = NA,
      "masignif" = NA,
      "resnocorr" = NA,
      "ok" = NA
    ))
  arsignif <- if (p == 0) #if p is 0, there is no AR coefficient so return NA
    NA
  else
    signif(estim)[3, p] <= 0.05 #if the p-value of the AR coefficient with the highest lag is below 0.05, return 1 in "arsignif"
  masignif <- if (q == 0) #if q is 0, there is no MA coefficient so return NA
    NA
  else
    signif(estim)[3, p + q] <= 0.05 #if the p-value of the MA coefficient with the highest lag is below 0.05, return 1 in "masignif"
  resnocorr <-
    sum(Qtests(estim$residuals, 24, length(estim$coef) - 1)[, 2] <= 0.05, na.rm =
          T) == 0 #if residuals are not autocorrelated, return 1
  checks <- c(arsignif, masignif, resnocorr) 
  ok <-
    as.numeric(sum(checks, na.rm = T) == (3 - sum(is.na(checks)))) #assign 1 to "ok" only if arsignif, masignif, and resnocorr are all 1 or NA
  return(
    c(
      "p" = p,
      "q" = q,
      "arsignif" = arsignif,
      "masignif" = masignif,
      "resnocorr" = resnocorr,
      "ok" = ok
    )
  )
}

armamodelchoice <- function(pmax, qmax) {
  pqs <- expand.grid(0:pmax, 0:qmax) 
  t(apply(matrix(1:dim(pqs)[1]), 1, function(row) { 
    p <- pqs[row, 1]
    q <- pqs[row, 2]
    cat(paste0("Computing ARMA(", p, ",", q, ") \n"))
    modelchoice(p, q) #run modelchoice on model (p,q)
  }))
}

armamodels <- armamodelchoice(pmax, qmax) 
selec <-
  armamodels[armamodels[, "ok"] == 1 &
               !is.na(armamodels[, "ok"]),] 
#Return significant and valid models 
selec 

#Choose among the valid and significant models using AIC and BIC criteria

mat <- matrix(NA,nrow=pmax+1,ncol=qmax+1) 
rownames(mat) <- paste0("p=",0:pmax) 
colnames(mat) <- paste0("q=",0:qmax) 
AICs <- mat 
BICs <- mat 
pqs <- expand.grid(0:pmax,0:qmax) 
for (row in 1:dim(pqs)[1]){ 
  p <- pqs[row,1] 
  q <- pqs[row,2] 
  estim <- try(arima(Serie_TS,c(p,1,q),include.mean = F)) #try to estimate the ARIMA model 
  AICs[p+1,q+1] <- if (class(estim)=="try-error") NA else estim$aic #store AIC 
  BICs[p+1,q+1] <- if (class(estim)=="try-error") NA else BIC(estim) #store BIC
}

pqs <-
  apply(selec, 1, function(row)
    list("p" = as.numeric(row[1]), "q" = as.numeric(row[2]))) #create a list of p and q orders of candidate models
names(pqs) <-
  paste0("arma(", selec[, 1], ",", selec[, 2], ")") 
models <-
  lapply(pqs, function(pq)
    arima(serie_diff, c(pq[["p"]], 0, pq[["q"]]))) #create a list of estimated models
vapply(models, FUN.VALUE = numeric(2), function(m)
  c("AIC" = AIC(m), "BIC" = BIC(m))) #compute AIC and BIC of the candidate models
#We select the model with the smallest AIC and BIC






##### ESTIMATION #####

# Estimation of the ARIMA(5,1,0) model on the corrected (non-differenced) series
model_arima510 <- arima(serie_corr, order = c(5,1,0), include.mean = FALSE)

# Display of the coefficients
summary(model_arima510)






##### VERIFICATION #####
residuals = residuals(model_arima510)
plot(residuals)
sd_res = sd(residuals)
print(sd_res)
acf(residuals,
    main = "ACF of the residuals from the ARIMA(5,1,0) model",
    ylab = "")

hist(residuals, prob = TRUE, breaks = 30, col = "lightblue",
     main = "Residual distribution compared to the normal density",
     xlab = "Residual values",
     ylim = c(0, max(density(residuals)$y, 
                     dnorm(mean(residuals), mean = mean(residuals), sd = sd(residuals))) * 1.2))
# Empirical density
lines(density(residuals), col = "blue", lwd = 2)
# Estimated normal density
x_vals <- seq(min(residuals), max(residuals), length = 200)
lines(x_vals, dnorm(x_vals, mean = mean(residuals), sd = sd(residuals)),
      col = "red", lwd = 2, lty = 2)

legend("topright", legend = c("Empirical density", "Gaussian density"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

Box.test(residuals, lag = 24, type = "Ljung-Box")
# We therefore do not reject the null hypothesis for the Ljung-Box test: the residuals do not exhibit significant autocorrelation.






##### FORECAST #####

library(forecast)

# Fit of the ARIMA(5,1,0) model
modele_final <- Arima(serie_corr, order = c(5,1,0), include.mean = FALSE)

# Forecast for T+1 and T+2
prevision <- forecast(modele_final, h = 2, level = 95)

n <- length(serie_corr)
dernieres_obs <- window(serie_corr, start = time(serie_corr)[n - 150])
plot(prevision,
     xlim = c(time(dernieres_obs)[1], time(prevision$mean)[2]), # includes T+1 and T+2
     ylim = range(c(dernieres_obs, prevision$lower, prevision$upper)),
     main = "ARIMA(5,1,0) forecast for the last 2 values",
     ylab = "",
     xlab = "")
lines(dernieres_obs, col = "black")






