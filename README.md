
# EMSqiTools

**EMSqiTools** is an R package for EMS quality improvement analysis. It provides tools for statistical process control (SPC) charting, benchmarking, data profiling, and data validation.

---

## 📦 Installation

```r
# Install devtools if not already installed
install.packages("devtools")

# Install EMSqiTools directly from GitHub
devtools::install_github("ChadMetz/EMSqiTools")
```

---

## 📊 SPC Chart Functions

| Function           | Description |
|--------------------|-------------|
| `plot_p_chart()`   | Proportion chart with annotations, Western Electric Rule 2, and optional benchmark overlay |
| `plot_x_chart()`   | X-bar chart with segmented control limits |
| `plot_c_chart()`   | Count chart for event rates |
| `plot_imr_chart()` | Individuals and Moving Range chart |

---

## 📈 Benchmark

```r
get_nemsis_benchmark("Trauma-02", years = 2023:2025)
```

This retrieves national monthly EMS benchmarks from NEMSIS and can be overlayed on `plot_p_chart()`.

---

## 📋 Data Checks

| Function             | Purpose |
|----------------------|---------|
| `check_missing()`     | Find missing values |
| `check_duplicates()`  | Detect duplicate rows |
| `check_outliers()`    | IQR-based outlier summary |
| `describe_data()`     | Numeric and categorical summaries |
| `profile_data()`      | Generate HTML profile report |
| `import_spreadsheet()`| Load `.csv` or Excel files interactively |

---

## 📚 Documentation

- View function docs via `?plot_p_chart` or RStudio help

---

## 🤝 License

MIT License © 2025
