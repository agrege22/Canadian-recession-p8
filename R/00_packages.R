# ============================================================
# PACKAGES
# ============================================================

cran_packages <- c(
  "here",
  "dplyr",
  "purrr",
  "tidyr",
  "tibble",
  "stringr",
  "lubridate",
  "ggplot2",
  "scales",
  "readxl",
  "readr",
  "xml2",
  "rvest",
  "quantmod",
  "zoo",
  "jsonlite",
  "forecast",
  "glmnet",
  "ranger",
  "future",
  "furrr",
  "progressr",
  "pROC",
  "Matrix"
)

missing_cran <- cran_packages[
  !vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_cran) > 0) {
  install.packages(missing_cran)
}

invisible(lapply(
  cran_packages,
  library,
  character.only = TRUE
))

# ============================================================
# LOCAL / CUSTOM PACKAGES
# ============================================================

local_packages <- c(
  "OCMT",
  "FarmSelect",
  "MultipleTestingBoosting"
)

missing_local <- local_packages[
  !vapply(local_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_local) > 0) {
  stop(
    "These local/custom packages are missing:\n",
    paste(missing_local, collapse = ", "),
    "\n\nInstall them first from your local package folder or make sure they are on your R library path."
  )
}

invisible(lapply(
  local_packages,
  library,
  character.only = TRUE
))

# ============================================================
# PROGRESS SETTINGS
# ============================================================

options(progressr.enable = TRUE)
progressr::handlers("txtprogressbar")