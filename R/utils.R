#' Create publication-ready PCA, t-SNE, or UMAP plots
#'
#' This function generates dimensionality reduction plots (PCA, t-SNE, UMAP)
#' with options for custom labels, titles, density plots, and faceting.
#' It allows users to visualize high-dimensional data using various
#' dimensionality reduction techniques.
#'
#' @param data A data frame containing the data to be plotted. Must include both feature columns (numeric) and metadata columns (categorical).
#' @param metadata_cols A character vector of column names or a numeric vector of column indices for the metadata columns. These columns are used for grouping and faceting.
#' @param mode The dimensionality reduction method to use. One of \code{"pca"} (Principal Component Analysis), \code{"tsne"} (t-Distributed Stochastic Neighbor Embedding), or \code{"umap"} (Uniform Manifold Approximation and Projection).
#' @param scale Logical indicating whether to scale features (default: \code{TRUE} for PCA). Not used for \code{"tsne"} or \code{"umap"}.
#' @param x_pc Name of the principal component or dimension to plot on the x-axis (default: \code{"PC1"} for PCA).
#' @param y_pc Name of the principal component or dimension to plot on the y-axis (default: \code{"PC2"} for PCA).
#' @param color_var Name of the column used to color points in the plot (default: \code{"Species"}). Should be a categorical variable in the data.
#' @param ellipse Logical indicating whether to add confidence ellipses for groups (only supported for PCA; default: \code{TRUE}).
#' @param ellipse_level Confidence level for ellipses (default: \code{0.9}).
#' @param ellipse_type Type of ellipse to plot, e.g., "norm" for normal distribution (default: \code{"norm"}).
#' @param ellipse_alpha Transparency level for ellipses, where 0 is fully transparent and 1 is fully opaque (default: \code{0.9}).
#' @param point_size Size of the points in the plot (default: \code{3}).
#' @param point_alpha Transparency level for the points, where 0 is fully transparent and 1 is fully opaque (default: \code{0.6}).
#' @param facet_var Formula for faceting the plot (e.g., \code{Category ~ .}), allowing users to split the plot by different groups.
#' @param tsne_perplexity Perplexity parameter for t-SNE, which balances local and global aspects of the data (default: \code{30}).
#' @param umap_n_neighbors Number of neighbors for UMAP, which determines the local structure (default: \code{15}).
#' @param density_plot Controls whether to add density plots for the x, y, or both axes. Accepts one of \code{"none"}, \code{"x"}, \code{"y"}, or \code{"both"} (default: \code{"none"}).
#' @param color_palette Name of the color palette to use for the plot. Supports \code{"Set1"}, \code{"Set2"}, etc. from \code{RColorBrewer} (default: \code{"Set1"}).
#' @param xlab Custom x-axis label (default: \code{NULL}, will be auto-generated based on the data).
#' @param ylab Custom y-axis label (default: \code{NULL}, will be auto-generated based on the data).
#' @param title Plot title (default: \code{NULL}).
#' @param subtitle Plot subtitle (default: \code{NULL}).
#' @param caption Plot caption (default: \code{NULL}).
#'
#' @return A \code{ggplot2} object representing the dimensionality reduction plot, including scatter plots, optional density plots, and faceting options. The plot can be further customized using \code{ggplot2} functions.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point theme_bw labs theme element_blank
#' @importFrom ggplot2 element_text margin scale_color_brewer stat_ellipse
#' @importFrom ggplot2 facet_grid geom_density coord_flip
#' @importFrom dplyr bind_cols select_if
#' @importFrom cowplot insert_xaxis_grob insert_yaxis_grob ggdraw
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom rlang .data
#'
#' @examples
#' \donttest{
#' # Load dataset
#' pca_data <- read.csv(system.file("extdata", "example.csv", package = "ggpca"))
#'
#' # PCA example
#' p_pca_y_group <- ggpca(
#'   pca_data,
#'   metadata_cols = c(1:6),
#'   mode = "pca",
#'   color_var = "group",
#'   ellipse = TRUE,
#'   density_plot = "y",
#'   title = "PCA with Y-axis Density Plot",
#'   subtitle = "Example dataset, colored by group",
#'   caption = "Data source: Example dataset"
#' )
#' print(p_pca_y_group)
#'
#' # t-SNE example
#' p_tsne_time <- ggpca(
#'   pca_data,
#'   metadata_cols = c(1:6),
#'   mode = "tsne",
#'   color_var = "time",
#'   tsne_perplexity = 30,
#'   title = "t-SNE Plot of Example Dataset",
#'   subtitle = "Colored by time",
#'   caption = "Data source: Example dataset"
#' )
#' print(p_tsne_time)
#' }

ggpca <- function(data,
                  metadata_cols,
                  mode = c("pca", "tsne", "umap"),
                  scale = TRUE,
                  x_pc = "PC1",
                  y_pc = "PC2",
                  color_var = "Species",
                  ellipse = TRUE,
                  ellipse_level = 0.9,
                  ellipse_type = "norm",
                  ellipse_alpha = 0.9,
                  point_size = 3,
                  point_alpha = 0.6,
                  facet_var = NULL,
                  tsne_perplexity = 30,
                  umap_n_neighbors = 15,
                  density_plot = "none",
                  color_palette = "Set1",
                  # Customization options
                  xlab = NULL,
                  ylab = NULL,
                  title = NULL,
                  subtitle = NULL,
                  caption = NULL) {
  # Validate mode input
  mode <- match.arg(mode)
  density_plot <- match.arg(density_plot, choices = c("x", "y", "both", "none"))

  # Determine if metadata_cols is numeric or character and subset accordingly
  if (is.numeric(metadata_cols)) {
    metadata <- data[, metadata_cols]
    features <- data[, -metadata_cols]
  } else if (is.character(metadata_cols)) {
    metadata <- data[, metadata_cols]
    features <- data[, !names(data) %in% metadata_cols]
  } else {
    stop("metadata_cols should be either a numeric vector or a character vector.")
  }

  # Ensure features are numeric for PCA calculation
  features <- dplyr::select_if(features, is.numeric)

  # Perform dimensionality reduction based on mode
  explained_variance <- NULL
  if (mode == "pca") {
    pca <- stats::prcomp(features, scale. = scale)
    scores <- as.data.frame(pca$x)
    colnames(scores) <- paste0("PC", seq_len(ncol(scores)))
    explained_variance <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
    xlab <- ifelse(is.null(xlab), paste0(x_pc, " (", explained_variance[1], "% variance)"), xlab)
    ylab <- ifelse(is.null(ylab), paste0(y_pc, " (", explained_variance[2], "% variance)"), ylab)
  } else if (mode == "tsne") {
    tsne_result <- Rtsne::Rtsne(as.matrix(features), perplexity = tsne_perplexity, check_duplicates = FALSE)
    scores <- as.data.frame(tsne_result$Y)
    colnames(scores) <- c("Dim1", "Dim2")
    x_pc <- "Dim1"
    y_pc <- "Dim2"
    xlab <- ifelse(is.null(xlab), x_pc, xlab)
    ylab <- ifelse(is.null(ylab), y_pc, ylab)
  } else if (mode == "umap") {
    umap_result <- umap::umap(as.matrix(features), n_neighbors = umap_n_neighbors)
    scores <- as.data.frame(umap_result$layout)
    colnames(scores) <- c("UMAP1", "UMAP2")
    x_pc <- "UMAP1"
    y_pc <- "UMAP2"
    xlab <- ifelse(is.null(xlab), x_pc, xlab)
    ylab <- ifelse(is.null(ylab), y_pc, ylab)
  }

  # Combine metadata with dimensionality reduction results
  plot_data <- dplyr::bind_cols(metadata, scores)

  # Create base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_pc]], y = .data[[y_pc]], color = .data[[color_var]])) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = xlab,
      y = ylab
    ) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5), size = 10),
      axis.title.y = ggplot2::element_text(face = "bold", margin = ggplot2::margin(t = 5), size = 10),
      axis.text.x = ggplot2::element_text(size = 8, face = "bold", color = "black"),
      axis.text.y = ggplot2::element_text(size = 8, face = "bold", color = "black")
    ) +
    ggplot2::scale_color_brewer(palette = color_palette)

  # Add ellipses if requested
  if (ellipse && mode == "pca") {
    p <- p + ggplot2::stat_ellipse(ggplot2::aes(group = .data[[color_var]]),
      level = ellipse_level,
      type = ellipse_type,
      alpha = ellipse_alpha
    )
  }

  # Add faceting based on the specified formula
  if (!is.null(facet_var)) {
    if (inherits(facet_var, "formula")) {
      p <- p + ggplot2::facet_grid(facet_var)
    } else {
      stop("facet_var should be a formula (e.g., x ~ y, x ~ ., or . ~ y).")
    }
  }

  # Add density plots based on the specified option
  if (density_plot %in% c("x", "both")) {
    # Marginal density for x-axis
    xdens <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[x_pc]], fill = .data[[color_var]])) +
      ggplot2::geom_density(alpha = 0.7, linewidth = 0.2) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = color_palette)

    # Insert the x-axis density plot
    p <- cowplot::insert_xaxis_grob(p, xdens, grid::unit(0.2, "null"), position = "top")
  }

  if (density_plot %in% c("y", "both")) {
    # Marginal density for y-axis
    ydens <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[y_pc]], fill = .data[[color_var]])) +
      ggplot2::geom_density(alpha = 0.7, linewidth = 0.2) +
      ggplot2::theme_classic() +
      ggplot2::coord_flip() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = color_palette)

    # Insert the y-axis density plot
    p <- cowplot::insert_yaxis_grob(p, ydens, grid::unit(0.2, "null"), position = "right")
  }

  # Draw the combined plot
  if (density_plot != "none") {
    p <- cowplot::ggdraw(p)
  }

  return(p)
}
