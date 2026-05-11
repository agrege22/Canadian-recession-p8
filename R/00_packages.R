# ============================================================
# PACKAGE SETTINGS
# ============================================================

# Set to TRUE if you want R to install local/custom packages
# from the local_packages/ folder.
install_local_packages <- FALSE

# ============================================================
# CRAN PACKAGES
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
  "Matrix",
  "remotes"
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
  "MultipleTestingBoosting" # BMT package
)

local_package_paths <- c(
  OCMT = here::here("local_packages", "OCMT"),
  FarmSelect = here::here("local_packages", "FarmSelect"),
  MultipleTestingBoosting = here::here("local_packages", "MultipleTestingBoosting")
)

missing_local <- local_packages[
  !vapply(local_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_local) > 0) {
  
  if (isTRUE(install_local_packages)) {
    
    missing_paths <- local_package_paths[missing_local][
      !dir.exists(local_package_paths[missing_local])
    ]
    
    if (length(missing_paths) > 0) {
      stop(
        "These local/custom package folders could not be found:\n",
        paste(names(missing_paths), missing_paths, sep = ": ", collapse = "\n"),
        "\n\nCheck that the package source folders exist in local_packages/."
      )
    }
    
    invisible(lapply(
      local_package_paths[missing_local],
      remotes::install_local,
      upgrade = "never",
      dependencies = TRUE
    ))
    
  } else {
    
    stop(
      "These local/custom packages are missing:\n",
      paste(missing_local, collapse = ", "),
      "\n\nEither install them manually first, or set install_local_packages <- TRUE."
    )
  }
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