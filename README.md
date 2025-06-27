
# EMSqiTools

**Quality Improvement Tools for EMS**

The `EMSqiTools` package provides a robust toolkit for EMS professionals conducting operational and clinical improvement work. It includes tools for SPC charting, benchmarking against national standards, data profiling, and cleaning — all with minimal setup and tailored for QI workflows.

---

## ✨ Features

✅ Import CSV, Excel, or SQL data  
✅ Create various SPC charts (`p`, `c`, `I-MR`, `X̄`) with segmented control limits and annotations  
✅ Compare your data to NEMSIS benchmarks  
✅ Summarize numerator/denominator performance over time  
✅ Check for missing values, duplicates, and outliers  
✅ Profile your dataset with automatic reports  
✅ Save plots and summary tables for reporting  

---

## 📦 Installation

```r
# Install the package from GitHub
devtools::install_github("ChadMetz/EMSqiTools")
```

---

## 🚀 Quick Start

```r
library(EMSqiTools)

# Import data
df <- import_spreadsheet("my_data.csv")

# Define event annotations
annotations <- data.frame(
  Date = as.Date(c("2024-09-01", "2025-02-15")),
  Label = c("Go Live", "Townhall"),
  Side = c("right", "left")
)

# Run SPC chart (p-chart example)
summary <- plot_p_chart(
  df,
  date_col = "Incident Date",
  num_col = "Aspirin_Admin",
  den_col = "Incident_Internal",
  time_unit = "month",
  name = "Aspirin Administration Rate",
  annotations = annotations
)

# Save outputs
save_qi_spc(path = ".", width = 16, height = 5, save_table = TRUE)
```

---

## 🛠 Main Functions

| Function                      | Description                                                   |
|-------------------------------|---------------------------------------------------------------|
| `import_spreadsheet()`        | Import data from CSV or Excel                                 |
| `import_sql()`                | Import data from SQL query outputs                            |
| `plot_p_chart()`              | Proportion (p) chart with segmented control limits            |
| `plot_c_chart()`              | Count (c) chart for defect/event tracking                     |
| `plot_imr_chart()`            | Individual-Moving Range (I-MR) chart                          |
| `plot_xbar_chart()`           | X-bar chart for continuous data with subgroups                |
| `plot_with_nemsis_benchmark()`| Compare data to national benchmark lines                      |
| `get_nemsis_benchmark()`      | Pull NEMSIS national benchmark reference values               |
| `summary_table()`             | Summarize numerator and denominator performance by time unit  |
| `qi_help()`                   | Show example inputs and formatting guidance                   |
| `check_missing()`             | Identify missing values by column                             |
| `check_duplicates()`          | Identify duplicate rows                                       |
| `check_outliers()`            | Flag numeric outliers                                         |
| `describe_data()`             | Describe numeric/categorical columns                          |
| `profile_data()`              | Auto-profile the dataset using `DataExplorer`                 |
| `save_qi_spc()`               | Save SPC chart and summary table as image and CSV             |

---

## 📊 Choosing the Right SPC Chart

| Chart Type    | Use When...                                                                 |
|---------------|------------------------------------------------------------------------------|
| **p-chart**   | You're monitoring a **proportion** (e.g., aspirin given / total incidents)   |
| **c-chart**   | You're counting **events per unit** (e.g., falls per shift)                  |
| **I-MR chart**| You have **continuous individual values** and no subgroups (e.g., response time) |
| **X-bar chart**| You have **subgrouped continuous data** (e.g., avg on-scene time per week)  |

---

## 📐 Control Chart Rules (Western Electric)

SPC charts include built-in support for shift detection based on these core rules:

1. **Rule 1 – Point beyond control limit**: 1 point outside UCL or LCL
2. **Rule 2 – Run of 8 on one side**: 8 consecutive points above or below the centerline
3. **Rule 3 – Trend**: 6 points all increasing or decreasing
4. **Rule 4 – Cycles or systematic patterns**: Flag unusual repeating patterns (visual)

These help detect **non-random signals** that may indicate process change.

---

## 📄 Documentation

For detailed examples and reports:

```r
browseVignettes("EMSqiTools")
```

Also check:

```
vignettes/EMSqiTools-report-template.Rmd
```

---

## 🤝 Contributing

Pull requests and issues are welcome!  
Let’s build the ultimate QI toolkit for EMS teams.

---

## 📬 Contact

**Maintainer**: Chad Metz  
**Email**: chadmetz@me.com
