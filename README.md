
# EMSqiTools

**Quality Improvement Tools for EMS**

The `EMSqiTools` package provides a set of easy-to-use functions for creating statistical process control (SPC) charts, importing data, summarizing performance metrics, and checking data quality — designed to support healthcare and operational improvement work in emergency medical services.

---

## ✨ Features

✅ Import CSV, Excel, or SQL data  
✅ Generate SPC charts with control limits and annotations  
✅ Summarize numerator / denominator counts over time  
✅ Check for missing values, duplicates, and outliers  
✅ Profile your dataset with automatic reports  
✅ Save plots and summary tables

---

## 📦 Installation

```r
# Install from local source (after building)
devtools::install("path/to/EMSqiTools")

# Or install from GitHub
devtools::install_github("ChadMetz/EMSqiTools")
---

## 🚀 Quick Start

```r
library(EMSqiTools)

# Import data
df <- import_spreadsheet("my_data.csv")

# Define annotations
annotations <- data.frame(
  Date = as.Date(c("2024-09-01", "2025-02-15")),
  Label = c("Go Live", "Townhall"),
  Side = c("right", "left")
)

# Run SPC chart
summary <- plot_control_chart(
  df,
  date_col = "Incident Date",
  id_col = "pcr",
  num_condition = "hr > 100",
  den_condition = "TRUE",
  time_unit = "week",
  name = "High Heart Rate Cases",
  annotations = annotations
)

# Save plot and table
save_qi_spc(path = ".", width = 18, height = 6, save_table = TRUE)

```

---

## 📊 Main Functions

| Function            | Description                                |
|---------------------|--------------------------------------------|
| `import_spreadsheet()`          | Import CSV or Excel files                  |
| `import_sql()`          | Import data from SQL queries               |
| `plot_table()`        | Generate summary tables over time          |
| `plot_control_chart()`          | Create SPC charts with control limits      |
| `save_qi_spc()`     | Save SPC plot and summary table            |
| `check_missing()`   | Report missing values per column           |
| `check_duplicates()`| Find duplicate rows                        |
| `check_outliers()`  | Identify outliers in numeric data          |
| `describe_data()`   | Summary of numeric and categorical data    |
| `profile_data()`    | Auto-profile dataset with DataExplorer     |

---

## 📑 Documentation

For detailed examples:

```r
browseVignettes("EMSqiTools")
```

You can also explore the bundled R Markdown report template:

```
vignettes/EMSqiTools-report-template.Rmd

```

---

## 🔧 Development Notes

- Use `#' @export` roxygen2 tags for all exported functions  
- Run `devtools::document()` to update help files and NAMESPACE  
- Run `devtools::build()` to package the library for sharing or local install  

---

## 🤝 Contributing

Pull requests and issues are welcome!  
Let’s build a great QI toolkit together.

---

## 📜 License

MIT License

---

## ✉ Contact

Maintainer: Chad Metz
Email: chadmetz@icloud.com

**Maintainer**: Chad Metz  
**Email**: chadmetz@me.com
