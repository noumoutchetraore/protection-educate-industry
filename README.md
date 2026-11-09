# Does Protection Educate Industry? Replication code

Code and data for the working paper *Does Protection Educate Industry? Tariffs, Productive Power, and Industrialisation in Developing Countries* (Noumoutchè Yaya Traore, 2026).

The script `Traore_Code_Sample_2026.R` runs the whole analysis, from the data import to the final tables and figures:

- panel cleaning and descriptive statistics;
- a PCA-based Index of Productive Power (IPP);
- fixed-effects baselines with Driscoll-Kraay standard errors;
- Hansen (1999) panel threshold tests (single, double and triple threshold) with bootstrap p-values (B = 500);
- likelihood-ratio confidence intervals for the two thresholds;
- the final double-threshold regression with Driscoll-Kraay standard errors, and the country-regime tables and heatmap.

## How to run

1. Clone the repository and open R in its root folder (the script reads `database/LIST_DATA.xlsx` with a relative path).
2. Install the packages:

```r
install.packages(c("readxl", "dplyr", "plm", "naniar", "ggplot2", "tidyr", "gt",
                   "tibble", "lmtest", "sandwich", "patchwork", "magick", "webshot2", "countrycode"))
```

3. Run:

```r
source("Traore_Code_Sample_2026.R", encoding = "UTF-8")
```

Tables and figures are written to the working folder. `gt::gtsave()` exports PNG tables through `webshot2`, which needs Google Chrome or Chromium.

Tested with R 4.6.1 on Windows 11. The three bootstrap tests use B = 500 replications and take most of the running time; set `B <- 5` in each bootstrap block for a quick check that the code runs (about 3 minutes).

## Data

`database/LIST_DATA.xlsx` (sheet `data_treated`) is the analysis panel: 27 developing and emerging economies, 1995 to 2024 (the analysis uses 2000 to 2022). Missing values are coded `..`.

| Variable | Definition | Source |
|---|---|---|
| `mva_pc` | Manufacturing value added per capita (constant 2015 US$) | World Bank, WDI (`NV.IND.MANF.KN`, `PA.NUS.FCRF`, `SP.POP.TOTL`, `NY.GDP.DEFL.ZS.AD`) |
| `gfcf_pc` | Gross fixed capital formation per capita | World Bank, WDI (`NE.GDI.FTOT.KN`) |
| `rore` | Human capital index, exp(r × MYS), following Chen and Dahlman (2004) | MYS (mean years of schooling) from the Global Data Lab; r is the regional Mincerian return from Psacharopoulos and Patrinos (2018), constant over the period |
| `tariff` | Applied tariff rate, weighted mean, all products (%) | World Bank, WDI (`TM.TAX.MRCH.WM.AR.ZS`), from WITS |
| `tnrr` | Total natural resources rents (% of GDP) | World Bank, WDI (`NY.GDP.TOTL.RT.ZS`) |
| `goveff` | Government Effectiveness: Estimate | World Bank, Worldwide Governance Indicators (`GE.EST`) |
| `elec_pc` | Electric power consumption (kWh per capita) | World Bank, WDI (`EG.USE.ELEC.KH.PC`) |
| `rol` | Rule of Law: Estimate | World Bank, Worldwide Governance Indicators (`RL.EST`) |

World Bank data are used under the [CC BY 4.0 license](https://datacatalog.worldbank.org/public-licenses#cc-by). Source: World Bank, World Development Indicators and Worldwide Governance Indicators.

Mean years of schooling: data retrieved from the Global Data Lab, https://globaldatalab.org/. GDL data may be used and shared for noncommercial purposes ([terms of use](https://globaldatalab.org/termsofuse/)). Smits, J. and Permanyer, I. (2019). The Subnational Human Development Database. *Scientific Data*, 6, 190038. https://doi.org/10.1038/sdata.2019.38

## Contact

Noumoutchè Yaya Traore, noumoutchetraore@outlook.com, https://noumoutchetraore.github.io/
