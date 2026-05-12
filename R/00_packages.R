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
  "remotes",
  "reticulate"
)

missing_cran <- cran_packages[
  !vapply(cran_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_cran) > 0) {
  install.packages(missing_cran)
}

# ============================================================
# CHRONOS / PYTHON SETTINGS
# ============================================================

use_chronos <- TRUE
install_chronos_python_if_missing <- TRUE
install_chronos_packages_if_missing <- TRUE

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
  
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    install.packages("reticulate")
  }
  
  library(reticulate)
  
  miniconda_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "r-miniconda")
  
  if (!dir.exists(miniconda_dir)) {
    message("Installing R miniconda...")
    reticulate::install_miniconda()
  }
  
  existing_envs <- reticulate::conda_list()
  
  if (!(chronos_env_name %in% existing_envs$name)) {
    
    if (!install_chronos_python_if_missing) {
      stop(
        "Chronos conda environment was not found: ",
        chronos_env_name,
        "\n\nSet install_chronos_python_if_missing <- TRUE or create it manually."
      )
    }
    
    message("Creating Chronos Python environment: ", chronos_env_name)
    
    reticulate::conda_create(
      envname = chronos_env_name,
      packages = "python=3.11"
    )
  }
  
  if (!file.exists(chronos_python)) {
    stop(
      "Chronos Python was not found at:\n",
      chronos_python,
      "\n\nThe conda environment may exist, but the expected python.exe was not found."
    )
  }
  
  run_python_cmd <- function(args, label = NULL) {
    if (!is.null(label)) {
      message(label)
    }
    
    status <- system2(
      chronos_python,
      args,
      stdout = "",
      stderr = ""
    )
    
    if (!identical(status, 0L)) {
      stop(
        "Python command failed with status ", status, ":\n",
        chronos_python, " ", paste(args, collapse = " ")
      )
    }
    
    invisible(status)
  }
  
  run_python_file <- function(code) {
    tmp <- tempfile(fileext = ".py")
    writeLines(code, tmp)
    on.exit(unlink(tmp), add = TRUE)
    
    status <- system2(
      chronos_python,
      tmp,
      stdout = TRUE,
      stderr = TRUE
    )
    
    attr_status <- attr(status, "status")
    
    if (!is.null(attr_status) && attr_status != 0) {
      cat(paste(status, collapse = "\n"), "\n")
      stop("Python test file failed with status: ", attr_status)
    }
    
    status
  }
  
  py_has_module <- function(module) {
    code <- paste0(
      "import importlib.util\n",
      "import sys\n",
      "sys.exit(0 if importlib.util.find_spec('", module, "') else 1)\n"
    )
    
    tmp <- tempfile(fileext = ".py")
    writeLines(code, tmp)
    on.exit(unlink(tmp), add = TRUE)
    
    status <- system2(
      chronos_python,
      tmp,
      stdout = FALSE,
      stderr = FALSE
    )
    
    identical(status, 0L)
  }
  
  required_python_modules <- c(
    "numpy",
    "pandas",
    "torch",
    "autogluon.timeseries"
  )
  
  missing_python_modules <- required_python_modules[
    !vapply(required_python_modules, py_has_module, logical(1))
  ]
  
  if (length(missing_python_modules) > 0) {
    
    if (!install_chronos_packages_if_missing) {
      stop(
        "Chronos Python exists, but these Python modules are missing:\n",
        paste(missing_python_modules, collapse = ", "),
        "\n\nInstall them manually or set install_chronos_packages_if_missing <- TRUE."
      )
    }
    
    message(
      "Installing missing Chronos Python modules: ",
      paste(missing_python_modules, collapse = ", ")
    )
    
    run_python_cmd(
      c("-m", "ensurepip", "--upgrade"),
      "Ensuring that pip is available..."
    )
    
    run_python_cmd(
      c("-m", "pip", "install", "-U", "pip", "setuptools", "wheel"),
      "Upgrading pip, setuptools, and wheel..."
    )
    
    run_python_cmd(
      c("-m", "pip", "install", "numpy", "pandas"),
      "Installing numpy and pandas..."
    )
    
    run_python_cmd(
      c(
        "-m", "pip", "install",
        "torch",
        "--index-url", "https://download.pytorch.org/whl/cpu"
      ),
      "Installing CPU version of torch..."
    )
    
    run_python_cmd(
      c(
        "-m", "pip", "install",
        "autogluon",
        "--extra-index-url", "https://download.pytorch.org/whl/cpu"
      ),
      "Installing AutoGluon. This may take some time, especially the first time..."
    )
    
    still_missing_python_modules <- required_python_modules[
      !vapply(required_python_modules, py_has_module, logical(1))
    ]
    
    if (length(still_missing_python_modules) > 0) {
      stop(
        "These Chronos Python modules are still missing after attempted installation:\n",
        paste(still_missing_python_modules, collapse = ", "),
        "\n\nCheck the Python installation messages above."
      )
    }
  }
  
  chronos_test <- run_python_file("
import sys
print('Python:', sys.executable)

modules = ['numpy', 'pandas', 'torch', 'autogluon.timeseries']

for m in modules:
    try:
        __import__(m)
        print('OK:', m)
    except Exception as e:
        print('MISSING/ERROR:', m, '->', e)
")
  
  cat(paste(chronos_test, collapse = "\n"), "\n")
  
  message("Using Chronos Python at: ", chronos_python)
}

# ============================================================
# LOAD CRAN PACKAGES
# ============================================================

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