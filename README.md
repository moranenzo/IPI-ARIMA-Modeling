# IPI-ARIMA-Modeling

Time series modeling and forecasting of the French Industrial Production Index (IPI) using **ARIMA models** and classical stationarity tests.  

This project reproduces an academic study initially implemented in **R**, with an additional **Python version** for reproducibility.

---

## Objectives
- Analyze the stationarity of the IPI time series (seasonally adjusted data from INSEE).
- Identify and estimate appropriate ARIMA models.
- Perform residual diagnostics (Ljung-Box, ACF/PACF, normality checks).
- Generate short-term forecasts with confidence intervals.

---

## Project Structure
```

data/                    # Raw CSV data (IPI and seasonally adjusted IPI)
report.pdf               # Original academic report (in French)
script.R                 # Original R implementation
script.py                # Python translation of the main pipeline

````

---

## Methods
- **Stationarity tests**: ADF, PP, KPSS  
- **Differencing** to stabilize the series  
- **Model identification**: inspection of ACF/PACF and information criteria (AIC/BIC)  
- **Final model**: ARIMA(5,1,0) selected for best trade-off between fit and parsimony  
- **Diagnostics**: residuals analysis, Ljung-Box test, density comparison  
- **Forecasting**: 2-step ahead forecasts with 95% confidence intervals  

---

## Results
- The differenced series was found to be **stationary**.  
- **ARIMA(5,1,0)** provided the best fit (lowest AIC/BIC among valid models).  
- Residuals showed **no significant autocorrelation** and approximated normality.  
- Short-term forecasts closely followed the observed dynamics with reliable intervals.  

---

## 📖 References

* INSEE: [Indice de Production Industrielle (IPI)](https://www.insee.fr).

---

## Notes

* The R version is the original academic implementation.
* The Python version is a translation of the methodology for easier reuse in a modern data science stack.

```
