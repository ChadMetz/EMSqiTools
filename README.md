
# qiTools

**Quality Improvement Tools for R**

`qiTools` is an R package designed to support EMS quality improvement projects. It provides intuitive functions for generating SPC charts, importing and profiling data, and summarizing performance metrics — enabling teams to identify trends, monitor progress, and improve care.

---

## ✨ Features

- ✅ Import CSV, Excel, or SQL data  
- ✅ Generate SPC charts with control limits and annotations  
- ✅ Summarize numerator/denominator counts over time  
- ✅ Check for missing values, duplicates, and outliers  
- ✅ Profile your dataset with automatic reports  
- ✅ Save plots and summary tables  

---

## 📦 Installation

### From GitHub 

# Install from GitHub
devtools::install_github("Chad-Metz/qitools")
```

### From Local Source

```r
# After building the package locally
devtools::install("path/to/qiTools")
```

---

## 🚀 Quick Start

```r
library(qiTools)

# Import data
df <- qi_csv("my_data.csv")

# Define annotations
annotations <- data.frame(
  Date = as.Date(c("2024-09-01", "2025-02-15")),
  Label = c("Go Live", "Townhall"),
  Side = c("right", "left")
)

# Run SPC chart
summary <- qi_spc(
  df,
  date_col = "Incident Date",
  id_col = "pcr",
  num_condition = "hr > 100",
  den_condition = "TRUE",
  time_unit = "week",
  name = "High Heart Rate Cases",
  annotations = annotations
)

# Save plot and summary table
save_qi_spc(path = ".", width = 18, height = 6, save_table = TRUE)
```

---

## 📊 Main Functions

| Function            | Description                                |
|---------------------|--------------------------------------------|
| `qi_csv()`          | Import CSV or Excel files                  |
| `qi_sql()`          | Import data from SQL queries               |
| `qi_table()`        | Generate summary tables over time          |
| `qi_spc()`          | Create SPC charts with control limits      |
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
browseVignettes("qiTools")
```

You can also explore the bundled R Markdown report template:

```
vignettes/qiTools-report-template.Rmd
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

**Maintainer**: Chad Metz  
**Email**: chadmetz@me.com
