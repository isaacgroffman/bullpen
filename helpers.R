# =============================================================================
# helpers.R — pitch palette, bullpen summary stats, gt theme.
# =============================================================================

pitch_colors <- c(
  "Fastball" = "#3465cb",
  "Four-Seam" = "#3465cb",
  "FourSeamFastBall" = "#3465cb",
  "4-Seam Fastball" = "#3465cb",
  "FF" = "#3465cb",
  "Sinker" = "#e5e501",
  "TwoSeamFastBall" = "#e5e501",
  "Two-Seam" = "#e5e501",
  "2-Seam Fastball" = "#e5e501",
  "SI" = "#e5e501",
  "Slider" = "#65aa02",
  "SL" = "#65aa02",
  "Sweeper" = "#dc4476",
  "SW" = "#dc4476",
  "Curveball" = "#d73813",
  "CB" = "#d73813",
  "Knuckle Curve" = "#d73813",
  "KC" = "#d73813",
  "ChangeUp" = "#980099",
  "Changeup" = "#980099",
  "CH" = "#980099",
  "Splitter" = "#23a999",
  "FS" = "#23a999",
  "SP" = "#23a999",
  "Cutter" = "#ff9903",
  "FC" = "#ff9903",
  "Slurve" = "#9370DB",
  # not in the supplied palette, but present in the Coastal data — without an
  # entry Knuckleballs would render gray in every chart
  "Knuckleball" = "#867A08",
  "Other" = "gray50"
)

drop_untagged <- function(df) {
  dplyr::filter(df, !is.na(TaggedPitchType),
                !TaggedPitchType %in% c("", "Other", "Undefined"))
}

in_zone <- function(side, height) {
  !is.na(side) & !is.na(height) &
    side >= -0.8333 & side <= 0.8333 & height >= 1.5 & height <= 3.5
}

# Spin efficiency arrives as a 0-1 fraction from the practice API.
fmt_pct <- function(x) ifelse(is.na(x), "—", sprintf("%.0f%%", 100 * x))

# Per-pitch-type summary for the gt table. First column must be `Pitch` —
# the renderer colors it via pitch_colors.
calculate_bullpen_summary <- function(df) {
  df <- drop_untagged(df)
  if (!nrow(df)) return(tibble::tibble())
  total <- nrow(df)
  df |>
    dplyr::group_by(Pitch = TaggedPitchType) |>
    dplyr::summarise(
      Count      = dplyr::n(),
      `Usage %`  = sprintf("%.0f%%", 100 * dplyr::n() / total),
      Velo       = round(mean(RelSpeed, na.rm = TRUE), 1),
      Max        = round(suppressWarnings(max(RelSpeed, na.rm = TRUE)), 1),
      Spin       = round(mean(SpinRate, na.rm = TRUE), 0),
      `Eff %`    = fmt_pct(mean(SpinAxis3dSpinEfficiency, na.rm = TRUE)),
      IVB        = round(mean(InducedVertBreak, na.rm = TRUE), 1),
      HB         = round(mean(HorzBreak, na.rm = TRUE), 1),
      VAA        = round(mean(VertApprAngle, na.rm = TRUE), 1),
      `Rel Ht`   = round(mean(RelHeight, na.rm = TRUE), 2),
      `Rel Side` = round(mean(RelSide, na.rm = TRUE), 2),
      Ext        = round(mean(Extension, na.rm = TRUE), 1),
      `Zone %`   = sprintf("%.0f%%",
                           100 * mean(in_zone(PlateLocSide, PlateLocHeight))),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(Count)) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                ~ ifelse(is.nan(.x) | is.infinite(.x), NA, .x)))
}

gt_theme_guardian <- function(gt_tbl, ...) {
  if (requireNamespace("gtExtras", quietly = TRUE)) {
    gtExtras::gt_theme_guardian(gt_tbl, ...)
  } else {
    gt_tbl   # app still renders if gtExtras isn't installed
  }
}

