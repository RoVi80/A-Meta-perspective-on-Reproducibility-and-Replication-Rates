# Publication figures for the meta-of-meta replication paper.
# This script intentionally keeps only the code needed to build:
# - funnel_below_above_p_values
# - funnel_below_above_ci_values
# - final_plot
# - pv_reg
# - ci_reg
# - pv_reg_funnel
# - ci_reg_funnel
# - final_reg_funnel

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/")))
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/")))
  }

  getwd()
}

script_dir <- normalizePath(get_script_dir(), winslash = "/", mustWork = FALSE)
project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)

publication_author_labels <- c(
  "Klein (2014)",
  "Nosek (2015)",
  "Dreber (2015)",
  "Camerer (2016)",
  "Patil (2016)",
  "Ebersole (2016)",
  "Schweinsberg (2016)",
  "Camerer (2018)",
  "Cova (2018)",
  "Tajika (2015)",
  "Klein (2018)",
  "Niven (2018)",
  "Naudet (2018)",
  "Kelly (2019)",
  "Soto (2019)",
  "Kvarven (2020)",
  "Ebersole (2020)",
  "Maassen (2020)",
  "Errington (2021)",
  "Avelino (2021)",
  "Hardwicke (2021)",
  "Davis (2022)",
  "Gonçalves (2022)",
  "Sobkow (2022)",
  "Nosek (2022)",
  "Boyce (2023)",
  "Protzko (2023)",
  "MTRP (2024)",
  "Huber et al. (2024)",
  "Marcoci et al. (2024)",
  "Tyner et al. (2026)"
)

get_active_document_dir <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    return(NULL)
  }

  doc_path <- tryCatch(
    rstudioapi::getActiveDocumentContext()$path,
    error = function(e) ""
  )

  if (!nzchar(doc_path)) {
    return(NULL)
  }

  dirname(normalizePath(doc_path, winslash = "/", mustWork = FALSE))
}

candidate_data_dirs <- function() {
  base_dirs <- Filter(
    Negate(is.null),
    unique(c(
      script_dir,
      getwd(),
      get_active_document_dir()
    ))
  )

  search_dirs <- unlist(lapply(base_dirs, function(base_dir) {
    c(
      base_dir,
      dirname(base_dir),
      file.path(base_dir, "second_stage"),
      file.path(dirname(base_dir), "second_stage"),
      file.path(base_dir, "vscode")
    )
  }), use.names = FALSE)

  unique(normalizePath(search_dirs, winslash = "/", mustWork = FALSE))
}

find_data_file <- function(filename) {
  candidate_paths <- file.path(candidate_data_dirs(), filename)
  existing_paths <- candidate_paths[file.exists(candidate_paths)]

  if (length(existing_paths) == 0) {
    stop(
      "Could not find the summarized dataset. Checked these locations:\n",
      paste(candidate_paths, collapse = "\n"),
      call. = FALSE
    )
  }

  existing_paths[1]
}

default_output_dir <- function() {
  active_document_dir <- get_active_document_dir()

  if (!is.null(active_document_dir) && dir.exists(active_document_dir)) {
    return(active_document_dir)
  }

  if (dir.exists(script_dir)) {
    return(script_dir)
  }

  getwd()
}

required_packages <- c("ggplot2", "ggrepel", "metafor", "readxl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the required packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

get_publication_author_labels <- function(data) {
  if (nrow(data) != length(publication_author_labels)) {
    stop(
      "The summarized dataset has ", nrow(data),
      " rows, but the publication label vector has ",
      length(publication_author_labels), " entries.",
      call. = FALSE
    )
  }

  publication_author_labels
}

compute_final_rep_rate <- function(rep_rate_p_value, rep_rate_interval) {
  both_present <- !is.na(rep_rate_p_value) & !is.na(rep_rate_interval)
  only_p_value <- !is.na(rep_rate_p_value) & is.na(rep_rate_interval)
  only_interval <- is.na(rep_rate_p_value) & !is.na(rep_rate_interval)

  rep_rate_final <- rep(NA_real_, length(rep_rate_p_value))
  rep_rate_final[both_present] <- (rep_rate_p_value[both_present] + rep_rate_interval[both_present]) / 2
  rep_rate_final[only_p_value] <- rep_rate_p_value[only_p_value]
  rep_rate_final[only_interval] <- rep_rate_interval[only_interval]
  rep_rate_final
}

load_publication_data <- function(
  data_path = find_data_file("final_dataset_summarized_new_stage2.xlsx")
) {
  if (!file.exists(data_path)) {
    stop("Could not find the summarized dataset at: ", data_path, call. = FALSE)
  }

  data <- as.data.frame(readxl::read_excel(data_path))

  required_columns <- c(
    "rep_author",
    "rep_rate_p_value",
    "rep_rate_interval",
    "se_p",
    "se_ci",
    "SJR_of_rep_project"
  )

  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "The summarized dataset is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  data$rep_author_publication <- get_publication_author_labels(data)
  data$rep_author_funnel <- data$rep_author_publication
  data$rep_rate_final <- compute_final_rep_rate(
    rep_rate_p_value = data$rep_rate_p_value,
    rep_rate_interval = data$rep_rate_interval
  )

  data
}

prepare_funnel_data <- function(data, value_col, se_col, label_col = "rep_author_funnel", size_divisor = 6) {
  keep <- complete.cases(data[, c(value_col, se_col, label_col, "SJR_of_rep_project")])

  plot_data <- data.frame(
    observed = data[[value_col]][keep],
    se = data[[se_col]][keep],
    label = data[[label_col]][keep],
    size = data$SJR_of_rep_project[keep] / size_divisor
  )

  if (nrow(plot_data) == 0) {
    stop("No complete cases were available to build the funnel plot for ", value_col, ".", call. = FALSE)
  }

  plot_data
}

prepare_meta_regression_data <- function(data) {
  required_columns <- c(
    "SJR_of_rep_project_cat",
    "average_SJR_of_original_papers_cat",
    "ave_age_original_papers_cat",
    "publication_year_rep_project_cat",
    "se_final"
  )

  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "The summarized dataset is missing regression columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  regression_data <- data
  regression_data$SJR_of_rep_project_cat <- stats::relevel(
    factor(regression_data$SJR_of_rep_project_cat),
    ref = "Impact higher than average (3)"
  )
  regression_data$average_SJR_of_original_papers_cat <- stats::relevel(
    factor(regression_data$average_SJR_of_original_papers_cat),
    ref = "Impact higher than average (5)"
  )
  regression_data$ave_age_original_papers_cat <- stats::relevel(
    factor(regression_data$ave_age_original_papers_cat),
    ref = "New"
  )
  regression_data$publication_year_rep_project_cat <- stats::relevel(
    factor(regression_data$publication_year_rep_project_cat),
    ref = "Old"
  )

  regression_data
}

build_funnel_guides <- function(
  mean_effect,
  se_max,
  z_values = c(1.645, 1.96, 2.576),
  conf_levels = c("90%", "95%", "99%"),
  n_points = 100
) {
  se_range <- seq(0, se_max, length.out = n_points)

  ci_data <- do.call(
    rbind,
    lapply(seq_along(z_values), function(i) {
      data.frame(
        x = c(
          mean_effect - z_values[i] * se_range,
          rev(mean_effect + z_values[i] * se_range)
        ),
        y = c(se_range, rev(se_range)),
        level = conf_levels[i]
      )
    })
  )

  triangle_data <- data.frame(
    x = c(
      mean_effect - z_values[length(z_values)] * se_range,
      rev(mean_effect + z_values[length(z_values)] * se_range)
    ),
    y = c(se_range, rev(se_range))
  )

  list(ci_data = ci_data, triangle_data = triangle_data)
}

publication_funnel_theme <- function() {
  ggplot2::theme_classic(base_size = 15) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 25)),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.15),
      axis.ticks = ggplot2::element_line(color = "black"),
      panel.grid.major = ggplot2::element_line(color = "white"),
      panel.background = ggplot2::element_rect(fill = "grey80", colour = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

build_split_funnel_plot <- function(data, value_col, se_col, title, size_divisor = 6) {
  overall_data <- prepare_funnel_data(data, value_col, se_col, size_divisor = size_divisor)
  split_threshold <- mean(overall_data$observed, na.rm = TRUE)

  below_data <- overall_data[overall_data$observed < split_threshold, , drop = FALSE]
  above_data <- overall_data[overall_data$observed >= split_threshold, , drop = FALSE]

  if (nrow(below_data) == 0 || nrow(above_data) == 0) {
    stop("The split funnel plot for ", value_col, " requires data on both sides of the average.", call. = FALSE)
  }

  overall_mean <- mean(overall_data$observed, na.rm = TRUE)
  overall_se_max <- max(overall_data$se, na.rm = TRUE)
  common_se_max <- max(c(below_data$se, above_data$se), na.rm = TRUE)

  overall_guides <- build_funnel_guides(overall_mean, overall_se_max)
  below_guides <- build_funnel_guides(mean(below_data$observed, na.rm = TRUE), common_se_max)
  above_guides <- build_funnel_guides(mean(above_data$observed, na.rm = TRUE), common_se_max)

  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = below_guides$triangle_data,
      ggplot2::aes(x = x, y = y),
      fill = "white",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_polygon(
      data = above_guides$triangle_data,
      ggplot2::aes(x = x, y = y),
      fill = "white",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_polygon(
      data = overall_guides$triangle_data,
      ggplot2::aes(x = x, y = y),
      fill = "grey60",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = data.frame(x = overall_mean, yend = overall_se_max),
      ggplot2::aes(x = x, xend = x, y = 0, yend = yend),
      linetype = "dashed",
      color = "black",
      linewidth = 0.75,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = overall_guides$ci_data,
      ggplot2::aes(x = x, y = y, group = level),
      linetype = "dashed",
      color = "black",
      linewidth = 0.75,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = data.frame(
        x = mean(below_data$observed, na.rm = TRUE),
        yend = common_se_max
      ),
      ggplot2::aes(x = x, xend = x, y = 0, yend = yend),
      linetype = "dashed",
      color = "black",
      linewidth = 0.15,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = data.frame(
        x = mean(above_data$observed, na.rm = TRUE),
        yend = common_se_max
      ),
      ggplot2::aes(x = x, xend = x, y = 0, yend = yend),
      linetype = "dashed",
      color = "black",
      linewidth = 0.15,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = below_guides$ci_data,
      ggplot2::aes(x = x, y = y, group = level),
      linetype = "dashed",
      color = "black",
      linewidth = 0.15,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = above_guides$ci_data,
      ggplot2::aes(x = x, y = y, group = level),
      linetype = "dashed",
      color = "black",
      linewidth = 0.15,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = below_data,
      ggplot2::aes(x = observed, y = se, size = size),
      color = "black"
    ) +
    ggplot2::geom_point(
      data = above_data,
      ggplot2::aes(x = observed, y = se, size = size),
      color = "black"
    ) +
    ggrepel::geom_text_repel(
      data = below_data,
      ggplot2::aes(x = observed, y = se, label = label),
      size = 5,
      color = "black",
      max.overlaps = Inf
    ) +
    ggrepel::geom_text_repel(
      data = above_data,
      ggplot2::aes(x = observed, y = se, label = label),
      size = 5,
      color = "black",
      max.overlaps = Inf
    ) +
    ggplot2::labs(
      title = title,
      x = "Observed Outcome",
      y = "Standard Error"
    ) +
    ggplot2::scale_y_reverse(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(
      breaks = seq(-0.2, 1.2, by = 0.2),
      limits = c(-0.2, 1.2)
    ) +
    ggplot2::scale_size_continuous() +
    ggplot2::coord_fixed(ratio = 5) +
    publication_funnel_theme()
}

build_final_plot <- function(data) {
  ordered_data <- data[order(data$rep_rate_final), , drop = FALSE]
  study_index <- seq_len(nrow(ordered_data))

  pv_data <- data.frame(
    study = ordered_data$rep_author_publication,
    y = study_index - 0.12,
    estimate = ordered_data$rep_rate_p_value,
    se = ordered_data$se_p,
    model = "PV"
  )

  ci_data <- data.frame(
    study = ordered_data$rep_author_publication,
    y = study_index + 0.12,
    estimate = ordered_data$rep_rate_interval,
    se = ordered_data$se_ci,
    model = "CI"
  )

  plot_data <- rbind(pv_data, ci_data)
  plot_data <- plot_data[complete.cases(plot_data[, c("estimate", "se")]), , drop = FALSE]
  plot_data$lower <- plot_data$estimate - 1.96 * plot_data$se
  plot_data$upper <- plot_data$estimate + 1.96 * plot_data$se

  mean_effect_final <- mean(ordered_data$rep_rate_final, na.rm = TRUE)
  model_colors <- c("PV" = "#F8766D", "CI" = "#00BFC4")

  ggplot2::ggplot(plot_data, ggplot2::aes(x = estimate, y = y, color = model)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = lower, xend = upper, yend = y),
      linewidth = 0.6,
      alpha = 0.9
    ) +
    ggplot2::geom_point(size = 2.7) +
    ggplot2::geom_vline(
      xintercept = mean_effect_final,
      linetype = "dashed",
      color = "black",
      linewidth = 0.2
    ) +
    ggplot2::scale_y_continuous(
      breaks = study_index,
      labels = ordered_data$rep_author_publication,
      expand = ggplot2::expansion(add = c(0.75, 0.75))
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 1, by = 0.25),
      labels = function(x) sprintf("%.2f", x)
    ) +
    ggplot2::scale_color_manual(values = model_colors, name = "model") +
    ggplot2::labs(
      y = "Studies (ordered by the final replication rate)",
      x = "Coefficient estimates and 95% confidence intervals",
      title = "Studies by Final Replication Rate"
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1.08)) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = 25,
        face = "bold",
        margin = ggplot2::margin(t = 15, b = 25)
      ),
      axis.text.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 16, margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 16),
      panel.grid.minor = ggplot2::element_blank()
    )
}

save_publication_figures <- function(output_dir = default_output_dir(), width = 10, height = 7, dpi = 300) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(
    filename = file.path(output_dir, "Three_funnel_PVRC.jpg"),
    plot = funnel_below_above_p_values,
    width = width,
    height = height,
    dpi = dpi
  )

  ggplot2::ggsave(
    filename = file.path(output_dir, "Three_funnel_CIRC.jpg"),
    plot = funnel_below_above_ci_values,
    width = width,
    height = height,
    dpi = dpi
  )

  ggplot2::ggsave(
    filename = file.path(output_dir, "final_plot.jpg"),
    plot = final_plot,
    width = width,
    height = height,
    dpi = dpi
  )
}

dataset_replication_projects_summarized <- load_publication_data()
dataset_regression_ready <- prepare_meta_regression_data(dataset_replication_projects_summarized)

pv_reg <- metafor::rma(
  yi = rep_rate_p_value,
  se = se_p,
  method = "ML",
  slab = rep_author_publication,
  mods = ~
    SJR_of_rep_project_cat +
    average_SJR_of_original_papers_cat +
    ave_age_original_papers_cat +
    publication_year_rep_project_cat,
  data = dataset_regression_ready
)

pv_reg

ci_reg <- metafor::rma(
  yi = rep_rate_interval,
  se = se_ci,
  method = "ML",
  slab = rep_author_publication,
  mods = ~
    SJR_of_rep_project_cat +
    average_SJR_of_original_papers_cat +
    ave_age_original_papers_cat +
    publication_year_rep_project_cat,
  data = dataset_regression_ready
)

ci_reg

pv_reg_funnel <- metafor::rma(
  yi = rep_rate_p_value,
  se = se_p,
  method = "ML",
  slab = rep_author_funnel,
  data = dataset_replication_projects_summarized
)

pv_reg_funnel

ci_reg_funnel <- metafor::rma(
  yi = rep_rate_interval,
  se = se_ci,
  method = "ML",
  slab = rep_author_funnel,
  data = dataset_replication_projects_summarized
)

ci_reg_funnel

final_reg_funnel <- metafor::rma(
  yi = rep_rate_final,
  se = se_final,
  method = "ML",
  slab = rep_author_publication,
  data = dataset_replication_projects_summarized
)

final_reg_funnel

funnel_below_above_p_values <- build_split_funnel_plot(
  data = dataset_replication_projects_summarized,
  value_col = "rep_rate_p_value",
  se_col = "se_p",
  title = "Funnel Plot: Below and Above Average P Values"
)

funnel_below_above_ci_values <- build_split_funnel_plot(
  data = dataset_replication_projects_summarized,
  value_col = "rep_rate_interval",
  se_col = "se_ci",
  title = "Funnel Plot: Below and Above Average CI Values"
)

final_plot <- build_final_plot(dataset_replication_projects_summarized)

if (interactive()) {
  print(funnel_below_above_p_values)
  print(funnel_below_above_ci_values)
  print(final_plot)
}

# Uncomment to write the publication figures to disk.
# save_publication_figures()
