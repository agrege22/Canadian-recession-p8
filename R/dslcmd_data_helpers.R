# ============================================================
# DS-LCMD / CAN-MD DATA HELPER FUNCTIONS
# ============================================================

# ------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------

.dslcmd_lag_vec <- function(x, k) {
  k <- as.integer(k)
  n <- length(x)
  if (k <= 0) return(x)
  if (k >= n) return(rep(NA, n))
  c(rep(NA, k), x[seq_len(n - k)])
}

.dslcmd_lead_vec <- function(x, h) {
  h <- as.integer(h)
  n <- length(x)
  if (h <= 0) return(x)
  if (h >= n) return(rep(NA, n))
  c(x[(1 + h):n], rep(NA, h))
}

.dslcmd_detect_date_col <- function(df) {
  nm <- intersect(names(df), c("Date", "date"))
  if (length(nm) == 0) return(NA_character_)
  nm[1]
}

.dslcmd_reorder_date_target_first <- function(df, date_col, target_col) {
  rest <- setdiff(names(df), c(date_col, target_col))
  df[, c(date_col, target_col, rest), drop = FALSE]
}

.dslcmd_month_start <- function(d) {
  as.Date(format(d, "%Y-%m-01"))
}

.dslcmd_extract_vintage <- function(file) {
  mon_map <- c(
    january = 1, february = 2, march = 3, april = 4, may = 5, june = 6,
    july = 7, august = 8, september = 9, october = 10, november = 11, december = 12,
    jan = 1, feb = 2, mar = 3, apr = 4, jun = 6, jul = 7, aug = 8,
    sep = 9, sept = 9, oct = 10, nov = 11, dec = 12
  )
  
  s <- tolower(as.character(file))
  
  m <- stringr::str_match(
    s,
    "(january|february|march|april|may|june|july|august|september|october|november|december|jan|feb|mar|apr|jun|jul|aug|sep|sept|oct|nov|dec)\\D*((?:19|20)\\d{2})"
  )
  
  if (is.na(m[1, 1])) return(NA_integer_)
  
  mm <- unname(mon_map[m[1, 2]])
  yy <- suppressWarnings(as.integer(m[1, 3]))
  
  if (is.na(mm) || is.na(yy)) return(NA_integer_)
  
  as.integer(yy * 100L + mm)
}

# ------------------------------------------------------------
# Add Yahoo-based returns
# ------------------------------------------------------------

dslcmd_add_yahoo_returns <- function(panel,
                                     date_col = "date",
                                     lag_months = 1,
                                     sp500_symbol = "^GSPC",
                                     dj_symbol = "^DJI",
                                     sp500_name = "SP500_yahoo_ret",
                                     dj_name = "DJ_CLO_yahoo_ret",
                                     from_buffer_days = 90,
                                     quiet = TRUE) {
  out <- panel
  
  if (!date_col %in% names(out)) stop("date_col not found in panel.")
  
  if (!inherits(out[[date_col]], "Date")) {
    suppressWarnings({
      d <- as.Date(out[[date_col]])
      if (!all(is.na(d))) out[[date_col]] <- d
    })
  }
  
  if (!inherits(out[[date_col]], "Date")) {
    stop("date_col must be Date or coercible to Date.")
  }
  
  if (!requireNamespace("quantmod", quietly = TRUE)) {
    stop("Package 'quantmod' is required. Install it with install.packages('quantmod').")
  }
  
  dmin <- min(out[[date_col]], na.rm = TRUE)
  from <- dmin - as.integer(from_buffer_days)
  
  sp_xts <- suppressWarnings(
    quantmod::getSymbols(
      sp500_symbol,
      src = "yahoo",
      from = from,
      auto.assign = FALSE,
      quiet = quiet
    )
  )
  
  dj_xts <- suppressWarnings(
    quantmod::getSymbols(
      dj_symbol,
      src = "yahoo",
      from = from,
      auto.assign = FALSE,
      quiet = quiet
    )
  )
  
  sp_ret <- quantmod::monthlyReturn(quantmod::Ad(sp_xts), type = "log")
  dj_ret <- quantmod::monthlyReturn(quantmod::Ad(dj_xts), type = "log")
  
  sp_df <- data.frame(
    month = as.Date(format(zoo::as.Date(zoo::index(sp_ret)), "%Y-%m-01")),
    sp = as.numeric(sp_ret),
    stringsAsFactors = FALSE
  )
  
  dj_df <- data.frame(
    month = as.Date(format(zoo::as.Date(zoo::index(dj_ret)), "%Y-%m-01")),
    dj = as.numeric(dj_ret),
    stringsAsFactors = FALSE
  )
  
  yahoo_df <- merge(sp_df, dj_df, by = "month", all = TRUE, sort = TRUE)
  
  out$.month_key <- .dslcmd_month_start(out[[date_col]])
  
  out <- merge(
    out,
    yahoo_df,
    by.x = ".month_key",
    by.y = "month",
    all.x = TRUE,
    sort = FALSE
  )
  
  lag_months <- as.integer(lag_months)
  
  if (!is.na(lag_months) && lag_months > 0) {
    out$sp <- .dslcmd_lag_vec(out$sp, lag_months)
    out$dj <- .dslcmd_lag_vec(out$dj, lag_months)
  }
  
  out[[sp500_name]] <- out$sp
  out[[dj_name]]    <- out$dj
  
  out$sp <- NULL
  out$dj <- NULL
  out$.month_key <- NULL
  
  out <- out[order(out[[date_col]]), , drop = FALSE]
  
  out
}

# ------------------------------------------------------------
# Recession target
# ------------------------------------------------------------

dslcmd_add_recession <- function(panel,
                                 date_col = NULL,
                                 h = 1,
                                 recession_periods = data.frame(
                                   start = as.Date(c(
                                     "1981-06-01",
                                     "1990-03-01",
                                     "2008-10-01",
                                     "2020-02-01"
                                   )),
                                   end = as.Date(c(
                                     "1982-10-01",
                                     "1992-05-01",
                                     "2009-05-01",
                                     "2020-04-01"
                                   ))
                                 )) {
  out <- panel
  
  if (is.null(date_col)) {
    date_col <- .dslcmd_detect_date_col(out)
    if (is.na(date_col)) {
      stop("Could not find a date column named Date/date. Supply date_col.")
    }
  } else {
    if (!date_col %in% names(out)) stop("date_col not found in panel.")
  }
  
  if (!inherits(out[[date_col]], "Date")) {
    suppressWarnings({
      d <- as.Date(out[[date_col]])
      if (!all(is.na(d))) out[[date_col]] <- d
    })
  }
  
  out <- out[order(out[[date_col]]), , drop = FALSE]
  date <- out[[date_col]]
  
  rec_t <- rep(FALSE, length(date))
  
  for (i in seq_len(nrow(recession_periods))) {
    rec_t <- rec_t | (
      date >= recession_periods$start[i] &
        date <= recession_periods$end[i]
    )
  }
  
  rec_t <- as.integer(rec_t)
  
  target_name <- paste0("recession_tplus", as.integer(h))
  out[[target_name]] <- .dslcmd_lead_vec(rec_t, h)
  
  out <- .dslcmd_reorder_date_target_first(out, date_col, target_name)
  
  out
}

# ------------------------------------------------------------
# DS-LCMD download helpers
# ------------------------------------------------------------

dslcmd_links <- function(url = "https://www.stevanovic.uqam.ca/DS_LCMD.html",
                         encoding = "latin1") {
  page <- xml2::read_html(url, encoding = encoding)
  
  a <- rvest::html_elements(page, "a")
  text <- rvest::html_text(a, trim = TRUE)
  href <- rvest::html_attr(a, "href")
  
  out <- tibble::tibble(
    text = text,
    href = href
  )
  
  out <- out[!is.na(out$href), , drop = FALSE]
  
  out$url  <- xml2::url_absolute(out$href, url)
  out$file <- basename(out$url)
  
  out
}

dslcmd_list_files <- function(url = "https://www.stevanovic.uqam.ca/DS_LCMD.html",
                              encoding = "latin1",
                              exts = c("zip", "xlsx", "csv")) {
  links <- dslcmd_links(url = url, encoding = encoding)
  
  ext_regex <- paste0("\\.(", paste(exts, collapse = "|"), ")$")
  
  keep <- stringr::str_detect(
    links$file,
    stringr::regex(ext_regex, ignore_case = TRUE)
  )
  
  out <- links[keep, , drop = FALSE]
  out <- out[!duplicated(out$url), , drop = FALSE]
  
  out
}

dslcmd_download_latest_zip <- function(dest_dir = "data_dslcmd",
                                       url = "https://www.stevanovic.uqam.ca/DS_LCMD.html",
                                       encoding = "latin1",
                                       dataset_regex = "CAN[-_]?MD|CAN[-_]?QD|LCMD|LCDMA",
                                       pick = c("vintage", "year", "page_last", "filename"),
                                       overwrite = FALSE,
                                       quiet = TRUE,
                                       verbose = FALSE) {
  pick <- match.arg(pick)
  
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  
  files <- dslcmd_list_files(url = url, encoding = encoding)
  
  is_dataset <- stringr::str_detect(
    files$file,
    stringr::regex(dataset_regex, ignore_case = TRUE)
  )
  
  is_zip <- stringr::str_detect(
    files$file,
    stringr::regex("\\.zip$", ignore_case = TRUE)
  )
  
  cand <- files[is_dataset & is_zip, , drop = FALSE]
  
  if (nrow(cand) == 0) {
    stop("No CAN-MD/CAN-QD/LCMD zip files found on DS_LCMD page.")
  }
  
  cand$vintage <- vapply(cand$file, .dslcmd_extract_vintage, integer(1))
  cand$is_quarterly <- stringr::str_detect(tolower(cand$file), "_q_")
  
  if (isTRUE(verbose)) {
    ord <- order(
      cand$vintage,
      cand$is_quarterly,
      cand$file,
      na.last = TRUE,
      decreasing = TRUE
    )
    
    print(utils::head(cand[ord, c("file", "text", "vintage", "url")], 20))
  }
  
  if (pick == "page_last") {
    idx <- nrow(cand)
    
  } else if (pick == "filename") {
    idx <- order(cand$file, na.last = TRUE)[nrow(cand)]
    
  } else if (pick == "year") {
    yrs <- suppressWarnings(
      as.integer(stringr::str_extract(cand$file, "(?:19|20)\\d{2}"))
    )
    
    if (all(is.na(yrs))) {
      idx <- order(cand$file, na.last = TRUE)[nrow(cand)]
    } else {
      maxyr <- max(yrs, na.rm = TRUE)
      ties <- which(yrs == maxyr)
      
      if (!all(is.na(cand$vintage[ties]))) {
        maxv <- max(cand$vintage[ties], na.rm = TRUE)
        ties2 <- ties[which(cand$vintage[ties] == maxv)]
        ties2 <- ties2[order(cand$is_quarterly[ties2], cand$file[ties2])]
        idx <- ties2[1]
      } else {
        idx <- ties[order(cand$file[ties], na.last = TRUE)[length(ties)]]
      }
    }
    
  } else {
    if (all(is.na(cand$vintage))) {
      idx <- order(cand$file, na.last = TRUE)[nrow(cand)]
    } else {
      maxv <- max(cand$vintage, na.rm = TRUE)
      ties <- which(cand$vintage == maxv)
      ties <- ties[order(cand$is_quarterly[ties], cand$file[ties])]
      idx <- ties[1]
    }
  }
  
  pick_row <- cand[idx, , drop = FALSE]
  
  zip_path <- file.path(dest_dir, pick_row$file)
  
  if (!file.exists(zip_path) || isTRUE(overwrite)) {
    utils::download.file(
      pick_row$url,
      zip_path,
      mode = "wb",
      quiet = quiet
    )
  }
  
  out_dir <- file.path(
    dest_dir,
    stringr::str_remove(pick_row$file, "\\.zip$")
  )
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(zip_path, exdir = out_dir)
  
  list(
    zip = zip_path,
    folder = out_dir,
    picked = pick_row
  )
}

dslcmd_read_balanced_panel <- function(unzipped_folder,
                                       name_regex = "(balanced|stationary|panel)") {
  all_files <- list.files(
    unzipped_folder,
    recursive = TRUE,
    full.names = TRUE
  )
  
  ok_name <- stringr::str_detect(tolower(all_files), name_regex)
  ok_ext  <- stringr::str_detect(tolower(all_files), "\\.(csv|xlsx)$")
  
  cand <- all_files[ok_name & ok_ext]
  
  if (length(cand) == 0) {
    msg <- paste0(
      "No obvious balanced/stationary/panel file found in: ",
      unzipped_folder,
      "\nTop files:\n",
      paste(utils::head(all_files, 50), collapse = "\n")
    )
    
    stop(msg)
  }
  
  f <- cand[1]
  
  if (stringr::str_detect(tolower(f), "\\.csv$")) {
    return(readr::read_csv(f, show_col_types = FALSE))
  }
  
  sheet <- readxl::excel_sheets(f)[1]
  readxl::read_excel(f, sheet = sheet)
}

# ------------------------------------------------------------
# Clean Canadian panel
# ------------------------------------------------------------

dslcmd_clean_panel <- function(panel,
                               date_col = NULL,
                               trade_totals = c("Imp_BP_new", "Exp_BP_new"),
                               geo_drop = c(
                                 "QC", "PEI", "NS", "NB", "ON", "ONT",
                                 "MAN", "MB", "SAS", "SK", "ALB", "AB",
                                 "BC", "NF", "NL", "YT", "NT", "NU"
                               ),
                               drop_tsx_hilo = TRUE) {
  panel_names <- names(panel)
  
  if (is.null(date_col)) {
    date_col <- intersect(panel_names, c("Date", "date"))[1]
    if (is.na(date_col)) {
      stop("Could not find a date column named Date/date. Supply date_col.")
    }
  } else {
    if (!date_col %in% panel_names) stop("date_col not found in panel.")
  }
  
  series <- setdiff(panel_names, date_col)
  
  strip_new <- function(x) stringr::str_remove(x, "_new$")
  series_base <- strip_new(series)
  
  token <- stringr::str_match(series_base, ".*_([^_]+)$")[, 2]
  
  base_no_token <- ifelse(
    !is.na(token),
    stringr::str_replace(series_base, "_[^_]+$", ""),
    series_base
  )
  
  is_code_token <- !is.na(token) & stringr::str_detect(token, "^[A-Z0-9]{2,}$")
  has_aggregate <- base_no_token %in% series_base
  
  drop_geo    <- token %in% geo_drop
  drop_disagg <- is_code_token & has_aggregate
  
  is_trade_component <- stringr::str_detect(
    series,
    stringr::regex("^(EX_|IM_|IMP_|EXP_|EOIL_|IOIL_)", ignore_case = TRUE)
  ) & !(series %in% trade_totals)
  
  is_discontinued <- stringr::str_detect(
    series,
    stringr::regex("discontinued$", ignore_case = TRUE)
  )
  
  is_cb <- stringr::str_detect(series_base, "_cb$")
  cb_root <- stringr::str_remove(series_base, "_cb$")
  non_cb_exists <- cb_root %in% series_base
  drop_cb <- is_cb & non_cb_exists
  
  drop_tsx <- rep(FALSE, length(series))
  
  if (isTRUE(drop_tsx_hilo)) {
    drop_tsx <- series %in% c("TSX_HI", "TSX_LO")
  }
  
  drop_any <- drop_geo |
    drop_disagg |
    is_trade_component |
    is_discontinued |
    drop_cb |
    drop_tsx
  
  keep_vars <- series[!drop_any]
  
  out <- panel
  names(out)[names(out) == date_col] <- "date"
  
  out <- out[, c("date", keep_vars), drop = FALSE]
  
  if ("date" %in% names(out)) {
    if (is.character(out$date)) {
      suppressWarnings({
        d <- as.Date(out$date)
        if (!all(is.na(d))) out$date <- d
      })
    }
    
    out <- out[order(out$date), , drop = FALSE]
  }
  
  attr(out, "dslcmd_series_before") <- length(series)
  attr(out, "dslcmd_series_after")  <- length(keep_vars)
  
  out
}

# ------------------------------------------------------------
# Construct recession panel and lag panels
# ------------------------------------------------------------

dslcmd_make_panel2 <- function(panel,
                               date_col = "date",
                               h = 1,
                               recession_periods = data.frame(
                                 start = as.Date(c(
                                   "1981-06-01",
                                   "1990-03-01",
                                   "2008-10-01",
                                   "2020-02-01"
                                 )),
                                 end = as.Date(c(
                                   "1982-10-01",
                                   "1992-05-01",
                                   "2009-05-01",
                                   "2020-04-01"
                                 ))
                               )) {
  out <- dslcmd_add_recession(
    panel,
    date_col = date_col,
    h = h,
    recession_periods = recession_periods
  )
  
  target_name <- paste0("recession_tplus", as.integer(h))
  
  others <- setdiff(names(out), c(date_col, target_name))
  out[others] <- lapply(out[others], as.numeric)
  
  out <- out[, c(date_col, target_name, others), drop = FALSE]
  out <- out[!is.na(out[[target_name]]), , drop = FALSE]
  
  if (date_col != "date") {
    names(out)[names(out) == date_col] <- "date"
  }
  
  out
}

dslcmd_make_panel_lags <- function(panel2,
                                   lags = 1:12,
                                   h = 1,
                                   x_names = NULL) {
  target_name <- paste0("recession_tplus", as.integer(h))
  
  need <- c("date", target_name)
  
  if (!all(need %in% names(panel2))) {
    stop("panel2 must contain date and recession_tplus<h>.")
  }
  
  if (is.null(x_names)) {
    x_names <- setdiff(names(panel2), need)
  }
  
  df <- data.frame(
    date = panel2$date,
    target = as.numeric(panel2[[target_name]]),
    check.names = FALSE
  )
  
  names(df)[names(df) == "target"] <- target_name
  
  for (k in lags) {
    for (v in x_names) {
      nm <- paste0(v, "_L", k)
      df[[nm]] <- .dslcmd_lag_vec(as.numeric(panel2[[v]]), k)
    }
  }
  
  df
}

dslcmd_best_lags <- function(panel2,
                             max_lag = 20,
                             h = 1,
                             x_names = NULL) {
  target_name <- paste0("recession_tplus", as.integer(h))
  
  need <- c("date", target_name)
  
  if (!all(need %in% names(panel2))) {
    stop("panel2 must contain date and recession_tplus<h>.")
  }
  
  if (is.null(x_names)) {
    x_names <- setdiff(names(panel2), need)
  }
  
  y <- as.numeric(panel2[[target_name]])
  
  out <- vector("list", length(x_names))
  
  for (i in seq_along(x_names)) {
    v <- x_names[i]
    x <- as.numeric(panel2[[v]])
    
    cors <- rep(NA_real_, max_lag + 1)
    
    for (k in 0:max_lag) {
      xl <- .dslcmd_lag_vec(x, k)
      
      cors[k + 1] <- suppressWarnings(
        stats::cor(xl, y, use = "pairwise.complete.obs")
      )
    }
    
    abs_cors <- abs(cors)
    
    if (all(is.na(abs_cors))) {
      out[[i]] <- data.frame(
        series = v,
        lag = NA_integer_,
        corr = NA_real_,
        abs_corr = NA_real_
      )
    } else {
      j <- which.max(abs_cors)
      
      out[[i]] <- data.frame(
        series = v,
        lag = j - 1,
        corr = cors[j],
        abs_corr = abs_cors[j]
      )
    }
  }
  
  res <- do.call(rbind, out)
  res <- res[order(-res$abs_corr), , drop = FALSE]
  rownames(res) <- NULL
  
  res
}

dslcmd_make_panel_blags <- function(panel2,
                                    max_lag = 20,
                                    K = NULL,
                                    h = 1) {
  target_name <- paste0("recession_tplus", as.integer(h))
  
  need <- c("date", target_name)
  
  if (!all(need %in% names(panel2))) {
    stop("panel2 must contain date and recession_tplus<h>.")
  }
  
  best_per_series <- dslcmd_best_lags(
    panel2,
    max_lag = max_lag,
    h = h
  )
  
  sel <- best_per_series
  
  if (!is.null(K)) {
    sel <- sel[seq_len(min(K, nrow(sel))), , drop = FALSE]
  }
  
  df <- data.frame(
    date = panel2$date,
    target = as.numeric(panel2[[target_name]]),
    check.names = FALSE
  )
  
  names(df)[names(df) == "target"] <- target_name
  
  for (i in seq_len(nrow(sel))) {
    v <- sel$series[i]
    k <- sel$lag[i]
    
    if (is.na(k)) next
    
    nm <- paste0(v, "_L", k)
    df[[nm]] <- .dslcmd_lag_vec(as.numeric(panel2[[v]]), k)
  }
  
  list(
    panel_blags = df,
    best_per_series = best_per_series
  )
}

# ------------------------------------------------------------
# One-call helper
# ------------------------------------------------------------

dslcmd_get_panel <- function(dest_dir = "data_dslcmd",
                             clean = TRUE,
                             add_recession = TRUE,
                             make_lags = TRUE,
                             h = 1,
                             recession_periods = data.frame(
                               start = as.Date(c(
                                 "1981-06-01",
                                 "1990-03-01",
                                 "2008-10-01",
                                 "2020-02-01"
                               )),
                               end = as.Date(c(
                                 "1982-10-01",
                                 "1992-05-01",
                                 "2009-05-01",
                                 "2020-04-01"
                               ))
                             ),
                             lags = 1:12,
                             max_lag = 20,
                             K_best = NULL,
                             add_yahoo_returns = TRUE,
                             yahoo_lag_months = 1,
                             yahoo_quiet = TRUE,
                             drop_tsx_hilo = TRUE,
                             overwrite = FALSE,
                             quiet = TRUE,
                             verbose = FALSE,
                             pick = "vintage") {
  dl <- dslcmd_download_latest_zip(
    dest_dir = dest_dir,
    pick = pick,
    overwrite = overwrite,
    quiet = quiet,
    verbose = verbose
  )
  
  raw0 <- dslcmd_read_balanced_panel(dl$folder)
  
  raw <- raw0
  
  if (isTRUE(add_recession)) {
    raw <- dslcmd_add_recession(
      raw,
      date_col = NULL,
      h = h,
      recession_periods = recession_periods
    )
  }
  
  if (!isTRUE(clean)) {
    return(
      list(
        download = dl,
        panel_raw = raw
      )
    )
  }
  
  cln <- dslcmd_clean_panel(
    raw0,
    drop_tsx_hilo = drop_tsx_hilo
  )
  
  if (isTRUE(add_recession)) {
    cln <- dslcmd_add_recession(
      cln,
      date_col = "date",
      h = h,
      recession_periods = recession_periods
    )
  }
  
  if (isTRUE(add_yahoo_returns)) {
    cln <- dslcmd_add_yahoo_returns(
      cln,
      date_col = "date",
      lag_months = yahoo_lag_months,
      sp500_name = "SP500_yahoo_ret",
      dj_name = "DJ_CLO_yahoo_ret",
      quiet = yahoo_quiet
    )
  }
  
  out <- list(
    download = dl,
    panel_raw = raw,
    panel = cln
  )
  
  if (isTRUE(make_lags)) {
    panel2 <- dslcmd_make_panel2(
      cln,
      date_col = "date",
      h = h,
      recession_periods = recession_periods
    )
    
    panel_lags <- dslcmd_make_panel_lags(
      panel2,
      lags = lags,
      h = h
    )
    
    bl <- dslcmd_make_panel_blags(
      panel2,
      max_lag = max_lag,
      K = K_best,
      h = h
    )
    
    out$panel2 <- panel2
    out$panel_lags <- panel_lags
    out$panel_blags <- bl$panel_blags
    out$best_per_series <- bl$best_per_series
  }
  
  out
}