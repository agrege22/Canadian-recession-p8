dslcmd_get_panel <- function(dest_dir = "data_dslcmd",
                             clean = TRUE,
                             add_recession = TRUE,
                             make_lags = TRUE,
                             h = 1,
                             recession_periods = data.frame(
                               start = as.Date(c("1981-06-01","1990-03-01","2008-10-01","2020-02-01")),
                               end   = as.Date(c("1982-10-01","1992-05-01","2009-05-01","2020-04-01"))
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
    return(list(download = dl, panel_raw = raw))
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

# ============================================================
# SCRAPING / DATA CONSTRUCTION
# ============================================================

cat("\nRunning scraping/data construction...\n")

dslcmd_res <- dslcmd_get_panel(
  dest_dir = dslcmd_dest_dir,
  h = scraping_h,
  make_lags = scraping_make_lags,
  add_yahoo_returns = scraping_add_yahoo_returns,
  yahoo_lag_months = scraping_yahoo_lag_months,
  overwrite = scraping_overwrite
)

panel_nonprov <- dslcmd_res$panel
panel_lags    <- dslcmd_res$panel_lags
panel_blag    <- dslcmd_res$panel_blags

saveRDS(dslcmd_res, file.path(scraping_dir, "dslcmd_res.rds"))
saveRDS(panel_nonprov, file.path(scraping_dir, "panel_nonprov.rds"))
saveRDS(panel_lags, file.path(scraping_dir, "panel_lags.rds"))
saveRDS(panel_blag, file.path(scraping_dir, "panel_blag.rds"))

cat("\nSaved scraping outputs to:\n")
cat(scraping_dir, "\n")