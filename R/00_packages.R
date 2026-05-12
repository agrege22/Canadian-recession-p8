# ============================================================
# PACKAGE SETTINGS
# ============================================================

# Set to TRUE if you want R to install local/custom packages
# from the local_packages/ folder.
install_local_packages <- TRUE

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

# ============================================================
# CHRONOS / PYTHON SETTINGS
# ============================================================

use_chronos <- TRUE
install_chronos_python_if_missing <- TRUE

chronos_env_name <- "chronos2-r"

chronos_python <- file.path(
  Sys.getenv("LOCALAPPDATA"),
  "r-miniconda",
  "envs",
  chronos_env_name,
  "python.exe"
)

# Tell reticulate which Python to use BEFORE reticulate initializes
Sys.setenv(RETICULATE_PYTHON = chronos_python)

if (use_chronos) {
  
  if (!file.exists(chronos_python)) {
    
    if (!install_chronos_python_if_missing) {
      stop(
        "Chronos Python was not found at:\n",
        chronos_python,
        "\n\nSet install_chronos_python_if_missing <- TRUE or run the Chronos setup manually."
      )
    }
    
    message("Chronos Python not found. Creating environment: ", chronos_env_name)
    
    if (!requireNamespace("reticulate", quietly = TRUE)) {
      install.packages("reticulate")
    }
    
    library(reticulate)
    
    if (!dir.exists(file.path(Sys.getenv("LOCALAPPDATA"), "r-miniconda"))) {
      reticulate::install_miniconda()
    }
    
    existing_envs <- reticulate::conda_list()
    
    if (!(chronos_env_name %in% existing_envs$name)) {
      reticulate::conda_create(
        envname = chronos_env_name,
        packages = "python=3.11"
      )
    }
    
    if (!file.exists(chronos_python)) {
      stop("Chronos Python environment was created, but python.exe was still not found.")
    }
    
    message("Installing Python packages for Chronos. This may take some time.")
    
    system2(
      chronos_python,
      c("-m", "pip", "install", "-U", "pip", "setuptools", "wheel")
    )
    
    system2(
      chronos_python,
      c(
        "-m", "pip", "install",
        "autogluon.timeseries",
        "--extra-index-url", "https://download.pytorch.org/whl/cpu"
      )
    )
  }
  
  message("Using Chronos Python at: ", chronos_python)
}

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
  FarmSelect = here::here("local_packages", "FarmSelect-master"),
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
    
    for (pkg in missing_local) {
      message("\nInstalling local package: ", pkg)
      message("From: ", local_package_paths[[pkg]])
      
      remotes::install_local(
        path = local_package_paths[[pkg]],
        upgrade = "never",
        dependencies = TRUE,
        force = TRUE
      )
    }
    
  } else {
    
    stop(
      "These local/custom packages are missing:\n",
      paste(missing_local, collapse = ", "),
      "\n\nEither install them manually first, or set install_local_packages <- TRUE."
    )
  }
}

still_missing_local <- local_packages[
  !vapply(local_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(still_missing_local) > 0) {
  stop(
    "These local/custom packages are still missing after attempted installation:\n",
    paste(still_missing_local, collapse = ", "),
    "\n\nCheck the installation messages above. The most likely issue is that the package failed to build."
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