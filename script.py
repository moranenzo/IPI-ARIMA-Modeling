import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.stattools import adfuller, kpss, acf, pacf
from statsmodels.tsa.arima.model import ARIMA
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.stats.diagnostic import acorr_ljungbox
from scipy.stats import norm
import seaborn as sns

# --- Load Data ---
df = pd.read_csv("data/IPI.csv", sep=";")
df = df.iloc[3:]  # remove first 3 rows
df.iloc[:, 1] = pd.to_numeric(df.iloc[:, 1])
df.iloc[:, 0] = pd.to_datetime(df.iloc[:, 0] + "-01")
df = df.sort_values(df.columns[0])

dates = df.iloc[:, 0]
serie_raw = df.iloc[:, 1].values

# Plot raw series
plt.figure(figsize=(10, 4))
plt.plot(dates, serie_raw, color="steelblue")
plt.title("Manufacture of agricultural and forestry machinery")
plt.xlabel("Date")
plt.ylabel("Value")
plt.xticks(rotation=45)
plt.tight_layout()
plt.show()

# --- Seasonally adjusted series ---
df_cor = pd.read_csv("data/IPI_CVS-CJO.csv", sep=";")
df_cor = df_cor.iloc[3:]
df_cor.iloc[:, 1] = pd.to_numeric(df_cor.iloc[:, 1])
df_cor.iloc[:, 0] = pd.to_datetime(df_cor.iloc[:, 0] + "-01")
df_cor = df_cor.sort_values(df_cor.columns[0])

dates_cor = df_cor.iloc[:, 0]
serie_corr = df_cor.iloc[:, 1].values

plt.figure(figsize=(10, 4))
plt.plot(dates_cor, serie_corr, color="steelblue")
plt.title("Manufacture of agricultural and forestry machinery, CVS-CJO")
plt.xlabel("Date")
plt.ylabel("Value")
plt.xticks(rotation=45)
plt.tight_layout()
plt.show()

# --- Stationarity Tests ---
def adf_test(series):
    result = adfuller(series, autolag="AIC")
    return {"ADF stat": result[0], "p-value": result[1]}

def kpss_test(series, regression="c"):
    statistic, p_value, _, _ = kpss(series, regression=regression, nlags="auto")
    return {"KPSS stat": statistic, "p-value": p_value}

print("ADF:", adf_test(serie_corr))
print("KPSS:", kpss_test(serie_corr))

# First difference
serie_diff = np.diff(serie_corr)

print("ADF (diff):", adf_test(serie_diff))
print("KPSS (diff):", kpss_test(serie_diff))

plt.figure(figsize=(10, 4))
plt.plot(dates_cor[1:], serie_diff, color="firebrick")
plt.title("Differenced series")
plt.show()

# --- ACF/PACF plots ---
plot_acf(serie_diff, lags=40)
plt.show()
plot_pacf(serie_diff, lags=40)
plt.show()

# --- Model Estimation ---
# Based on R script: ARIMA(5,1,0)
model = ARIMA(serie_corr, order=(5,1,0))
model_fit = model.fit()
print(model_fit.summary())

# --- Residual diagnostics ---
residuals = model_fit.resid

plt.figure(figsize=(10, 4))
plt.plot(residuals)
plt.title("Residuals from ARIMA(5,1,0)")
plt.show()

plt.figure(figsize=(6, 4))
sns.histplot(residuals, kde=True, color="lightblue", stat="density", bins=30)
mu, sigma = np.mean(residuals), np.std(residuals)
x_vals = np.linspace(min(residuals), max(residuals), 200)
plt.plot(x_vals, norm.pdf(x_vals, mu, sigma), 'r--', lw=2)
plt.title("Residual distribution vs Normal density")
plt.show()

# Ljung-Box test
lb_test = acorr_ljungbox(residuals, lags=[24], return_df=True)
print(lb_test)

# --- Forecasting ---
forecast_horizon = 2
forecast = model_fit.get_forecast(steps=forecast_horizon)
mean_forecast = forecast.predicted_mean
conf_int = forecast.conf_int(alpha=0.05)

plt.figure(figsize=(10, 4))
plt.plot(dates_cor, serie_corr, label="Observed")
plt.plot(pd.date_range(dates_cor.iloc[-1], periods=forecast_horizon+1, freq="M")[1:], 
         mean_forecast, color="red", label="Forecast")
plt.fill_between(pd.date_range(dates_cor.iloc[-1], periods=forecast_horizon+1, freq="M")[1:], 
                 conf_int.iloc[:, 0], conf_int.iloc[:, 1], color="pink", alpha=0.3)
plt.legend()
plt.title("ARIMA(5,1,0) Forecast")
plt.show()
