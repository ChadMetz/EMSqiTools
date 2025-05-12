#' @title SPC Chart Interpretation Rules
#' @description This help file documents the key interpretation rules used in Statistical Process Control (SPC) charts.
#'
#' These rules help identify non-random patterns that may indicate special cause variation in a process. The following are commonly applied SPC rules:
#'
#' @section 1. Single Point Out of Control:
#' - **Rule**: Any single point falls outside of the control limits (±3σ).
#' - **Interpretation**: Sudden or special cause variation.
#'
#' @section 2. Shift in the Process:
#' - **Rule**: 8 or more consecutive points on the same side of the centerline.
#' - **Interpretation**: Suggests a shift in the process mean.
#'
#' @section 3. Trend in the Process:
#' - **Rule**: 6 or more consecutive points steadily increasing or decreasing.
#' - **Interpretation**: A systematic drift in the process.
#'
#' @section 4. Points Near Control Limits:
#' - **Rule**: 2 of 3 consecutive points fall in Zone A (outer third between 2σ and 3σ) or beyond.
#' - **Interpretation**: Suggests potential out-of-control condition not caught by other rules.
#'
#' @section 5. Same Side Repeats:
#' - **Rule**: 4 of 5 consecutive points on the same side of the centerline.
#' - **Interpretation**: Directional bias in process.
#'
#' @section 6. Points Hugging Centerline:
#' - **Rule**: 15 consecutive points within ±1σ of the mean.
#' - **Interpretation**: Unusually low variation — may be artificial control.
#'
#' @section 7. Alternating Points:
#' - **Rule**: 14 consecutive points alternating up and down.
#' - **Interpretation**: Systematic or non-random variation.
#'
#' @section Notes:
#' - SPC distinguishes between **common cause** (natural) and **special cause** (abnormal) variation.
#' - Control limits are based on short-term process variation.
#' - Triggered rules should prompt investigation and corrective action.
#' - Use these patterns for **continuous improvement**.
#'
#' @name spc_rules_help
#' @rdname spc_rules_help
#' @keywords documentation
NULL
