# Predicting CO₂ Emissions in Asia Using Machine Learning

**Course:** Introduction to Data Science  
**Institution:** American International University – Bangladesh (AIUB)  
**Section:** K | **Group:** 2  
**Instructor:** Kamrun Nahar Koli

---

## 👥 Team Members

| Name | ID | Key Contributions |
|---|---|---|
| Md. Towhidul Islam | 23-55036-3 | Z-score Normalization, SMOTE Balancing, Train/Validation/Test Split, Introduction, Research Objective, Modeling, Web Scraping |
| Abrar Kabir | 23-55095-3 | Outlier Detection (IQR & Z-score), Winsorization, Missing Value Handling, Merging Datasets, Modeling, Web Scraping |
| Tasnim Hassan Ahona | 23-55085-3 | Correlation Heatmaps (Pearson & Spearman), ANOVA, Chi-Square Test, Data Collection & Web Scraping, Data Import & Loading, Feature Selection |
| Anika Tabassum | 23-55070-3 | EDA Visualizations, Histogram & Density Plots, Scatter Plots, Violin Plots, Descriptive Statistics, Class Distribution Analysis, Conclusion & Future Work |

---

## 📌 Project Overview

This project builds a complete machine learning pipeline to predict CO₂ emissions per capita across **48 Asian countries** using socioeconomic, demographic, urbanization, and energy-related features. Data was collected from four public sources via web scraping and covers **14 selected years from 1970 to 2024**, yielding **588 observations**.

Two ML tasks are performed:
- **Classification** — predict whether a country-year is a *High CO₂ Emitter* (above 75th percentile) or *Low CO₂ Emitter*
- **Regression** — predict the exact log-transformed CO₂ emissions per capita value

---

## 📁 Project Structure

```
dsProj/
│
├── Data (raw & processed)
│   ├── worldometer_asia_population.csv
│   ├── asia_co2_selected_years.csv
│   ├── asia_gdp_growth_fixed.csv
│   ├── asia_energy_per_capita.csv
│   ├── asia_final_clean.csv
│   └── per-capita-energy-use.csv          # downloaded from Our World in Data
│   └── API_NY.GDP.MKTP.KD.ZG_...csv       # downloaded from World Bank
│
├── project_outputs/
│   ├── figures/                           # Fig01 to Fig32 (PNG)
│   └── tables/                            # All CSV result tables
│
└── DataScienceFinalReport.pdf             # Full project report
```

---

## 🌐 Data Sources

| Source | Data Collected |
|---|---|
| [Worldometer – Population](https://www.worldometers.info/population/) | Population, urban %, fertility rate, median age, density |
| [Worldometer – CO₂](https://www.worldometers.info/co2-emissions/) | CO₂ per capita, total emissions, CO₂ change % |
| [World Bank Open Data](https://data.worldbank.org/) | GDP annual growth rate |
| [Our World in Data](https://ourworldindata.org/energy) | Energy consumption per capita (kWh) |

Web scraping was performed using R (`rvest`, `dplyr`, `purrr`) with a `Sys.sleep(1)` delay between requests for ethical compliance.

---

## ⚙️ Requirements

### R Packages

```r
packages <- c(
  "tidyverse", "skimr", "DataExplorer", "naniar",
  "corrplot", "ggcorrplot", "RColorBrewer", "gridExtra",
  "ggpubr", "scales", "viridis", "reshape2",
  "caret", "randomForest", "xgboost", "e1071",
  "smotefamily", "ROSE", "pROC", "MLmetrics",
  "fastshap", "vip", "car", "nortest",
  "moments", "GGally", "ggridges", "knitr",
  "kableExtra", "kernlab", "tibble", "rpart",
  "rpart.plot", "rvest", "dplyr", "stringr",
  "purrr", "tidyr", "readr"
)
install.packages(packages)
```

---

## 🚀 How to Run

1. **Clone or download** this repository into your local machine (e.g. `D:/dsProj/`)
2. **Download external datasets** manually:
   - World Bank GDP CSV → save as `API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_121708.csv`
   - Our World in Data energy CSV → save as `per-capita-energy-use.csv`
3. **Open R** and set working directory:
   ```r
   setwd("D:/dsProj")
   ```
4. **Run the full script** in order — the code is structured in 7 steps:

| Step | Description |
|---|---|
| 1 | Install & load all libraries |
| 2 | Web scrape population & CO₂ data from Worldometer |
| 3 | Load & clean GDP and energy datasets |
| 4 | Merge all 4 datasets; handle missing values & outliers |
| 5 | EDA — 32 publication-quality figures (Fig01–Fig32) |
| 6 | Feature engineering, SMOTE balancing, train/val/test split |
| 7 | Train & evaluate 5 classification + 5 regression models + SHAP |

All figures are saved to `project_outputs/figures/` and all result tables to `project_outputs/tables/`.

---

## 🧪 Methodology Summary

### Preprocessing
- Country-level median imputation for missing values
- Winsorization (1st–99th percentile) for `GDP_growth` and `Energy_Per_Capita_kWh`
- Z-score standardization (fit on training set only)
- SMOTE applied to training set to balance class imbalance (309 Low → 309 High)

### Feature Engineering
| Feature | Description |
|---|---|
| `log_Population` | Log-transformed population |
| `log_Density` | Log-transformed population density |
| `log_Energy` | Log-transformed energy consumption |
| `Energy_x_Urban` | Interaction: energy × urbanization |
| `GDP_x_Urban` | Interaction: GDP growth × urbanization |
| `Urban_x_Fert` | Interaction: urbanization × fertility rate |

### Train / Validation / Test Split
- **70% / 15% / 15%** stratified split → 412 / 88 / 88 rows
- 5-fold cross-validation selected over 3-fold and 10-fold

---

## 📊 Results

### Classification (Test Set)

| Model | Accuracy | AUC-ROC | F1-Score | Recall |
|---|---|---|---|---|
| **Random Forest** ✅ | 0.966 | 0.992 | 0.941 | **1.000** |
| SVM (RBF) | 0.943 | 0.984 | 0.906 | 1.000 |
| Logistic Regression | 0.932 | 0.988 | 0.889 | 1.000 |
| XGBoost | 0.920 | 0.984 | 0.863 | 0.917 |
| Decision Tree | 0.920 | 0.903 | 0.857 | 0.875 |

### Regression (Test Set)

| Model | RMSE | MAE | R² |
|---|---|---|---|
| **Random Forest** ✅ | 0.1127 | 0.0834 | **0.9874** |
| XGBoost | 0.1406 | 0.1038 | 0.9804 |
| Lasso Regression | 0.2302 | 0.1852 | 0.9476 |
| Linear Regression | 0.2359 | 0.1893 | 0.9450 |
| Ridge Regression | 0.2385 | 0.1914 | 0.9437 |

---

## 🔍 Key Findings (SHAP)

- **Energy consumption per capita** is the single most important predictor in both classification and regression models (SHAP: 0.2304 classification, 0.2792 regression)
- **Urbanization** is the second most influential feature
- **Energy × Urban interaction** amplifies CO₂ predictions — high energy use in urban countries produces more than the sum of each factor
- **GDP growth** had near-zero predictive power (Pearson r = −0.044), consistent with SHAP values

---

## ⚠️ Limitations

- Data is not continuous year-by-year — time-series forecasting (ARIMA, LSTM) is not directly applicable
- The 75th percentile threshold for "High Emitter" is data-driven, not a recognized policy benchmark
- High multicollinearity (VIF up to 303 for `Energy_x_Urban`) — causal inference from linear models is unreliable
- Missing confounders: energy mix (renewable vs. fossil), industrial structure, government climate policy

---

## 🔮 Future Work

- Add renewable energy percentage and coal dependency features
- Apply panel regression with fixed/random country effects
- Use continuous annual data for ARIMA or LSTM forecasting
- Study causal impact of Paris Agreement (2015) and COVID-19 (2020) as natural experiments

---

## 📄 License

This project is submitted as academic coursework at AIUB. All data is sourced from publicly available platforms for non-commercial educational use.
