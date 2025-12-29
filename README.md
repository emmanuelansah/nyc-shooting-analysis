# Gun Violence Analysis in New York City (2006-2024)

[![Paper](https://img.shields.io/badge/Paper-PDF-red)](paper/NYPD.pdf)
[![Interactive App](https://img.shields.io/badge/Shiny-Interactive%20Dashboard-blue)](https://tgnqj6-emmanuel-ansah.shinyapps.io/nyc-shooting-analysis/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

Comprehensive temporal and spatial analysis of 29,744 shooting incidents in New York City from 2006 to 2024, with time series forecasting using SARIMA models.

## 🎯 Key Findings

- **Three temporal regimes identified:**
  - 2006-2019: 48.4% sustained decline
  - 2020-2021: 91.2% surge coinciding with COVID-19 pandemic
  - 2022-2024: Post-pandemic stabilization

- **Strong seasonal patterns:** Summer months experience 2.3× more incidents than winter months

- **Extreme spatial concentration:** Top 5 precincts account for 22.7% of all incidents despite representing only 7% of police precincts

- **Demographic disparities:** 71.9% of victims are Black individuals, 81.3% aged 18-44

- **Socioeconomic correlations:** Strong associations between poverty rates and shooting incidents

- **Forecasting performance:** SARIMA(1,1,2)×(1,0,1)₁₂ model achieves 14.3% MAPE on out-of-sample data

## 📊 Interactive Dashboard

Explore the data interactively: **[NYC Shooting Analysis Dashboard](https://tgnqj6-emmanuel-ansah.shinyapps.io/nyc-shooting-analysis/)**

Features:
- Interactive maps showing spatial evolution over time
- Temporal trend visualizations
- Demographic breakdowns
- Socioeconomic correlations
- Borough-level comparisons

## 📁 Repository Structure

```
.
├── README.md                    # This file
├── data/                        # Data files
│   ├── NYPD_cleaned.csv        # Cleaned shooting incident data
│   ├── NYC_Boroughs_SocioEconomic_2006_2024.csv
│   ├── nyc_weather_monthly_2006_2024.csv
│   └── shapefile/              # NYC borough boundaries
│
├── analysis/                    # Analysis scripts
│   ├── ALL-EDA.R               # Main R exploratory analysis
│   ├── ALL-EDA.ipynb           # Python analysis notebook
│   └── R_code.Rmd              # R Markdown report
│
├── figures/                     # Generated visualizations
│   ├── temporal/               # Time series plots
│   ├── spatial/                # Maps (yearly shooting & murder)
│   └── demographic/            # Demographic analysis plots
```

## 📖 Data Sources

1. **NYPD Shooting Incident Data (Historic)**
   - Source: [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8)
   - Coverage: 2006-2024
   - Records: 29,744 incidents

2. **Socioeconomic Data**
   - Source: U.S. Census Bureau
   - Variables: Poverty rate, unemployment rate, median household income, population

3. **Weather Data**
   - Source: NOAA
   - Variables: Monthly temperature, precipitation

4. **Geographic Data**
   - Source: NYC Department of City Planning
   - Boundaries: NYC borough shapefiles

## 🔬 Methodology

### Exploratory Analysis
- Multi-scale temporal decomposition (yearly, monthly, weekly, hourly)
- Spatial clustering and hotspot identification
- Demographic and socioeconomic pattern analysis

### Time Series Modeling
- SARIMA(1,1,2)×(1,0,1)₁₂ specification
- Box-Jenkins methodology for model selection
- Out-of-sample validation (44 months held out)
- Diagnostic checks (Ljung-Box, Jarque-Bera tests)

### Statistical Tests
- Augmented Dickey-Fuller test for stationarity
- Kwiatkowski-Phillips-Schmidt-Shin test
- Residual diagnostics and normality tests

## 🚀 Reproducing the Analysis

### Prerequisites

**R packages:**
```R
install.packages(c("tidyverse", "sf", "lubridate", "forecast", 
                   "tseries", "ggplot2", "gridExtra"))
```

**Python packages:**
```bash
pip install pandas numpy matplotlib seaborn statsmodels scipy
```

### Running the Analysis

**R Analysis:**
```R
# Main exploratory analysis
source("analysis/ALL-EDA.R")

# Generate R Markdown report
rmarkdown::render("analysis/R_code.Rmd")
```

**Python Analysis:**
```bash
# Run Jupyter notebook
jupyter notebook analysis/ALL-EDA.ipynb
```

### Compiling the Paper

```bash
cd paper/
pdflatex nypd_shooting_upgraded.tex
bibtex nypd_shooting_upgraded
pdflatex nypd_shooting_upgraded.tex
pdflatex nypd_shooting_upgraded.tex
```

Or use Overleaf by uploading the files in `paper/`.

## 📈 Main Results

### Temporal Patterns
- **Three distinct regimes** with structural break in 2020
- **Seasonal amplitude:** July averages 184 incidents vs. February's 80 incidents
- **Weekly cycles:** Weekend rates elevated compared to weekdays
- **Diurnal pattern:** Peak incidents between 10 PM and midnight

### Spatial Patterns
- **Top 5 precincts:** 75th (East New York), 73rd (Brownsville), 67th (East Flatbush), 44th (Concourse), 79th (Bedford-Stuyvesant)
- **Borough rates per 100K:** Bronx (33.5) > Brooklyn (20.8) > Queens (8.4) > Manhattan (7.1) > Staten Island (4.2)
- **Persistent hotspots** across 19-year observation period

### Demographic Patterns
- **Gender:** 90.7% male perpetrators and victims
- **Age:** 81.3% of victims aged 18-44
- **Race:** 71.9% of victims Black, 16.2% White Hispanic
- **Intra-group patterns:** Strong within-race and within-age-group clustering

### Forecasting Performance
- **RMSE:** 18.7 incidents
- **MAPE:** 14.3%
- **Model:** SARIMA(1,1,2)×(1,0,1)₁₂
- **Best performance:** Stable periods (2022-2024)
- **Limitations:** Reduced accuracy during pandemic transition

## 📝 Citation

If you use this analysis or data in your research, please cite:

```bibtex
@article{ansah2024nypd,
  title={Firearm Violence in New York City, 2006--2024: A Time Series Analysis},
  author={Ansah, Emmanuel and Gouno, Evans},
  journal={[Journal Name]},
  year={2025},
  note={Manuscript in preparation}
}
```

## 📧 Contact

**Emmanuel Ansah**  
African Institute for Mathematical Sciences (AIMS), Rwanda  
Email: emmanuel.ansah@aims.ac.rw



## 🙏 Acknowledgments

- New York Police Department for making the shooting incident data publicly available
- African Institute for Mathematical Sciences (AIMS) Rwanda for institutional support
- U.S. Census Bureau for socioeconomic data
- NOAA for weather data

## 🔗 Related Work

This analysis provides the foundation for future work on:
- Bayesian spatio-temporal point process models
- Self-exciting Hawkes processes for crime prediction
- Network-based violence transmission models
- Causal inference for policy intervention evaluation

---

**Note:** This repository contains the analysis code. The raw NYPD data is publicly available and should be downloaded directly from NYC Open Data for the most up-to-date version.
