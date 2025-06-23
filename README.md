# Index Performance in R
This script calculates return performance for gics indices on a weekly (WTD), monthly (MTD), quarterly (QTD), and yearly (YTD) basis.
# Features
- Cleanses price data from historical `.rds` sources
- Calculates time-based return series
- Optionally joins with reference metadata
- Produces formatted summary tables by GICS levels
# Requirements
Install required R packages:
```r
install.packages(c("data.table", "dplyr", "tidyr", "lubridate"))
```
Your environment may also require my custom internal functions:
- `CALCULATE_CHANGE_RT_VARPC`
- `TRAINEE_MERGE_COL_FROM_REF`

# Run the script

```r
source("index_performance.R")
```
# Output
- A data frame (`data_final`) summarizing performance across timeframes and GICS levels
