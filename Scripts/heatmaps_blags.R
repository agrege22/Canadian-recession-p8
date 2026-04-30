# ============================================================
# HEATMAPS WITH NONPROV AND BEST-LAG PANEL
# ============================================================

cat("\nRunning heatmap script...\n")

# ============================================================
# LOAD DATA
# ============================================================

panel_nonprov <- readRDS(file.path(scraping_dir, "panel_nonprov.rds"))
panel_lags    <- readRDS(file.path(scraping_dir, "panel_lags.rds"))
panel_blag    <- readRDS(file.path(scraping_dir, "panel_blag.rds"))

make_date_first_numeric_rest <- function(df) {
  df[[1]] <- as.Date(df[[1]])
  names(df)[1] <- "date"
  
  if (ncol(df) > 1) {
    df[-1] <- lapply(df[-1], function(x) suppressWarnings(as.numeric(x)))
  }
  
  df
}

panel_nonprov <- make_date_first_numeric_rest(panel_nonprov)
panel_lags    <- make_date_first_numeric_rest(panel_lags)
panel_blag    <- make_date_first_numeric_rest(panel_blag)

# ============================================================
# HELPERS
# ============================================================

parse_h_from_target <- function(y_col) {
  h <- suppressWarnings(as.integer(stringr::str_extract(y_col, "\\d+$")))
  if (is.na(h)) 1L else h
}

robust_z <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  s <- stats::mad(x, na.rm = TRUE)
  
  if (is.na(s) || s == 0) {
    return(ifelse(is.na(x), NA_real_, 0))
  }
  
  (x - stats::median(x, na.rm = TRUE)) / s
}

top_cor_vars <- function(df2, y_col, K = 20) {
  x_names <- setdiff(names(df2), c("date", y_col))
  
  if (length(x_names) == 0) {
    return(list(
      cor_tbl  = tibble::tibble(series = character(), corr = numeric(), abs_corr = numeric()),
      top_vars = character()
    ))
  }
  
  y <- suppressWarnings(as.numeric(df2[[y_col]]))
  
  cors <- sapply(x_names, function(v) {
    x <- suppressWarnings(as.numeric(df2[[v]]))
    suppressWarnings(stats::cor(x, y, use = "pairwise.complete.obs"))
  })
  
  cor_tbl <- tibble::tibble(
    series = x_names,
    corr = as.numeric(cors)
  ) |>
    dplyr::mutate(abs_corr = abs(.data$corr)) |>
    dplyr::filter(!is.na(.data$abs_corr)) |>
    dplyr::arrange(dplyr::desc(.data$abs_corr))
  
  if (nrow(cor_tbl) == 0) {
    return(list(cor_tbl = cor_tbl, top_vars = character()))
  }
  
  kk <- min(as.integer(K), nrow(cor_tbl))
  
  list(
    cor_tbl = cor_tbl,
    top_vars = cor_tbl$series[seq_len(kk)]
  )
}

make_heatmap <- function(Zy, start_date, end_date, title, ordered_series, label_lookup) {
  start_date <- lubridate::floor_date(start_date, "month")
  end_date   <- lubridate::floor_date(end_date, "month")
  
  grid_df <- tidyr::expand_grid(
    date = seq.Date(start_date, end_date, by = "month"),
    series = ordered_series
  ) |>
    dplyr::left_join(label_lookup, by = "series") |>
    dplyr::left_join(
      Zy |> dplyr::select(.data$date, .data$series, .data$z),
      by = c("date", "series")
    )
  
  y_levels <- label_lookup$var_label[match(ordered_series, label_lookup$series)]
  
  grid_df <- grid_df |>
    dplyr::mutate(
      var_label = factor(.data$var_label, levels = rev(y_levels))
    )
  
  x_breaks <- seq.Date(start_date, end_date, by = "6 months")
  
  ggplot2::ggplot(grid_df, ggplot2::aes(x = .data$date, y = .data$var_label, fill = .data$z)) +
    ggplot2::geom_tile(height = 0.95, linewidth = 0.15, color = "white") +
    ggplot2::scale_fill_distiller(
      palette = "RdBu",
      direction = -1,
      limits = c(-4, 4),
      oob = scales::squish,
      na.value = "grey95",
      breaks = c(-4, -2, 0, 2, 4)
    ) +
    ggplot2::scale_x_date(
      breaks = x_breaks,
      labels = function(d) format(d, "%Y-%m"),
      expand = c(0, 0)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(barheight = grid::unit(45, "mm"))
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 9),
      axis.text.x = ggplot2::element_text(size = 9, angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.margin = ggplot2::margin(6, 12, 6, 6)
    ) +
    ggplot2::labs(
      title = title,
      fill = "robust z"
    )
}

panel_heatmaps <- function(df, panel_name = "panel", K = 20, months_pad = 24) {
  if (ncol(df) < 2) {
    stop(panel_name, ": need at least 2 columns: date and target.")
  }
  
  date_col <- names(df)[1]
  y_col <- names(df)[2]
  h <- parse_h_from_target(y_col)
  
  df2 <- df |>
    dplyr::transmute(
      date = as.Date(.data[[date_col]]),
      dplyr::across(
        -dplyr::all_of(date_col),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) |>
    dplyr::mutate(date = as.Date(.data$date)) |>
    dplyr::filter(!is.na(.data[[y_col]]))
  
  tc <- top_cor_vars(df2, y_col = y_col, K = K)
  
  top_vars <- tc$top_vars
  plot_vars <- c(y_col, top_vars)
  
  Zy <- df2 |>
    dplyr::select(.data$date, dplyr::all_of(plot_vars)) |>
    tidyr::pivot_longer(
      -date,
      names_to = "series",
      values_to = "value"
    ) |>
    dplyr::group_by(.data$series) |>
    dplyr::mutate(
      z = if (dplyr::first(.data$series) == y_col) {
        ifelse(as.numeric(.data$value) == 1, 4, -4)
      } else {
        robust_z(.data$value)
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      var_label = dplyr::case_when(
        .data$series == y_col ~ paste0("Recession (t+", h, ")"),
        stringr::str_detect(.data$series, "_L\\d+$") ~ {
          base <- stringr::str_remove(.data$series, "_L\\d+$")
          lagN <- stringr::str_extract(.data$series, "(?<=_L)\\d+$")
          paste0(base, " (t-", lagN, ")")
        },
        TRUE ~ .data$series
      )
    )
  
  ordered_series <- c(y_col, top_vars)
  
  label_lookup <- Zy |>
    dplyr::distinct(.data$series, .data$var_label)
  
  recessions <- tibble::tibble(
    label = c("1981-82", "1990-92", "2008-09", "2020"),
    peak = as.Date(c("1981-06-01", "1990-03-01", "2008-10-01", "2020-02-01")),
    trough = as.Date(c("1982-10-01", "1992-05-01", "2009-05-01", "2020-04-01"))
  )
  
  windows <- recessions |>
    dplyr::transmute(
      label = .data$label,
      start = lubridate::`%m-%`(
        .data$peak,
        lubridate::period(months = h + months_pad)
      ),
      end = lubridate::`%m+%`(
        lubridate::`%m-%`(
          .data$trough,
          lubridate::period(months = h)
        ),
        lubridate::period(months = months_pad)
      )
    )
  
  plots <- purrr::pmap(
    list(windows$start, windows$end, windows$label),
    function(st, en, lab) {
      make_heatmap(
        Zy = Zy,
        start_date = st,
        end_date = en,
        title = paste0(panel_name, ": Forecast heatmap around ", lab),
        ordered_series = ordered_series,
        label_lookup = label_lookup
      )
    }
  )
  
  names(plots) <- paste0("p", seq_along(plots))
  
  list(
    panel_name = panel_name,
    target_col = y_col,
    h = h,
    cor_tbl = tc$cor_tbl,
    top_vars = top_vars,
    windows = windows,
    plots = plots
  )
}

save_heatmaps <- function(hm,
                          panel_name,
                          out_dir,
                          width = 12,
                          height = 6,
                          dpi_png = 300,
                          save_pdf = TRUE,
                          save_png = TRUE) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  safe_lab <- function(x) gsub("[^0-9A-Za-z]+", "_", x)
  
  for (i in seq_along(hm$plots)) {
    lab <- hm$windows$label[i]
    p <- hm$plots[[i]]
    
    base <- file.path(out_dir, paste0(panel_name, "_", safe_lab(lab)))
    
    if (isTRUE(save_pdf)) {
      ggplot2::ggsave(
        filename = paste0(base, ".pdf"),
        plot = p,
        device = grDevices::cairo_pdf,
        width = width,
        height = height,
        units = "in"
      )
    }
    
    if (isTRUE(save_png)) {
      ggplot2::ggsave(
        filename = paste0(base, ".png"),
        plot = p,
        width = width,
        height = height,
        units = "in",
        dpi = dpi_png
      )
    }
  }
  
  utils::write.csv(
    hm$cor_tbl,
    file = file.path(out_dir, paste0(panel_name, "_top_correlations.csv")),
    row.names = FALSE
  )
  
  utils::write.csv(
    hm$windows,
    file = file.path(out_dir, paste0(panel_name, "_windows.csv")),
    row.names = FALSE
  )
  
  saveRDS(
    hm,
    file = file.path(out_dir, paste0(panel_name, "_heatmap_object.rds"))
  )
  
  message("Saved heatmaps for ", panel_name, " to: ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))
}

# ============================================================
# RUN
# ============================================================

available_panels <- list(
  panel_nonprov = panel_nonprov,
  panel_lags = panel_lags,
  panel_blag = panel_blag
)

heatmap_panels_use <- intersect(heatmap_panels, names(available_panels))

if (length(heatmap_panels_use) == 0) {
  stop("No valid heatmap panels selected.")
}

heatmap_results <- list()

for (panel_name in heatmap_panels_use) {
  cat("\nCreating heatmaps for:", panel_name, "\n")
  
  hm <- panel_heatmaps(
    df = available_panels[[panel_name]],
    panel_name = panel_name,
    K = heatmap_K,
    months_pad = heatmap_months_pad
  )
  
  heatmap_results[[panel_name]] <- hm
  
  if (isTRUE(heatmap_print_plots)) {
    purrr::walk(hm$plots, print)
  }
  
  save_heatmaps(
    hm = hm,
    panel_name = panel_name,
    out_dir = heatmap_dir,
    width = heatmap_width,
    height = heatmap_height,
    dpi_png = heatmap_dpi_png,
    save_pdf = heatmap_save_pdf,
    save_png = heatmap_save_png
  )
}

saveRDS(
  heatmap_results,
  file.path(heatmap_dir, "heatmap_results_all.rds")
)

cat("\nHeatmap script finished.\n")