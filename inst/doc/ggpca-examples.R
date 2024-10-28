## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ggpca)
library(tibble)
library(dplyr)

## ----iris-examples, fig.width=7, fig.height=6---------------------------------
# Example using the iris dataset
iris_data <- tibble::as_tibble(iris)

# PCA example with custom labels and titles
p_pca <- ggpca(iris_data, metadata_cols = "Species", mode = "pca", color_var = "Species", ellipse = TRUE,
               title = "PCA Plot of Iris Dataset", subtitle = "Colored by Species",
               caption = "Data source: iris dataset")
print(p_pca)

# t-SNE example with custom labels and titles
p_tsne <- ggpca(iris_data, metadata_cols = "Species", mode = "tsne", color_var = "Species", ellipse = FALSE,
                tsne_perplexity = 30, title = "t-SNE Plot of Iris Dataset",
                subtitle = "Colored by Species", caption = "Data source: iris dataset")
print(p_tsne)

# UMAP example with custom labels and titles
p_umap <- ggpca(iris_data, metadata_cols = "Species", mode = "umap", color_var = "Species", ellipse = FALSE,
                umap_n_neighbors = 15, title = "UMAP Plot of Iris Dataset",
                subtitle = "Colored by Species", caption = "Data source: iris dataset")
print(p_umap)

# PCA example with x-axis density plot only
p_pca_x <- ggpca(iris_data, metadata_cols = "Species", mode = "pca", color_var = "Species", ellipse = TRUE,
                 density_plot = "x", title = "PCA with X-axis Density Plot",
                 subtitle = "Iris dataset, colored by Species", caption = "Data source: iris dataset")
print(p_pca_x)

# PCA example with y-axis density plot only
p_pca_y <- ggpca(iris_data, metadata_cols = "Species", mode = "pca", color_var = "Species", ellipse = TRUE,
                 density_plot = "y", title = "PCA with Y-axis Density Plot",
                 subtitle = "Iris dataset, colored by Species", caption = "Data source: iris dataset")
print(p_pca_y)

# PCA example with both density plots
p_pca_both <- ggpca(iris_data, metadata_cols = "Species", mode = "pca", color_var = "Species", ellipse = TRUE,
                    density_plot = "both", title = "PCA with Both Density Plots",
                    subtitle = "Iris dataset, colored by Species", caption = "Data source: iris dataset")
print(p_pca_both)

# Generate a categorical variable based on Petal.Width
iris_data <- iris_data |>
  mutate(Category = cut(Petal.Width, breaks = c(-Inf, 0.5, 1.5, Inf), labels = c("low", "medium", "high")))

# PCA example with faceting by Category and Species using a formula
p_pca_faceting_x <- ggpca(iris_data, metadata_cols = c("Species", "Category"), mode = "pca", color_var = "Species", ellipse = FALSE,
                          facet_var = . ~ Species, density_plot = "none", title = "PCA Faceted by Petal Width Category",
                          subtitle = "Facet along X-axis", caption = "Data source: iris dataset")
print(p_pca_faceting_x)

p_pca_faceting_y <- ggpca(iris_data, metadata_cols = c("Species", "Category"), mode = "pca", color_var = "Species", ellipse = FALSE,
                          facet_var = Category ~ ., density_plot = "none", title = "PCA Faceted by Species",
                          subtitle = "Facet along Y-axis", caption = "Data source: iris dataset")
print(p_pca_faceting_y)

p_pca_faceting_both <- ggpca(iris_data, metadata_cols = c("Species", "Category"), mode = "pca", color_var = "Species", ellipse = FALSE,
                             facet_var = Category ~ Species, density_plot = "none", title = "PCA Faceted by Petal Width Category and Species",
                             subtitle = "Facet along both axes", caption = "Data source: iris dataset")
print(p_pca_faceting_both)

## ----example-data, fig.width=7, fig.height=6----------------------------------
# Load the example dataset
pca_data <- read.csv(system.file("extdata", "example.csv", package = "ggpca"))

# t-SNE example with custom labels and titles
p_tsne_time <- ggpca(
  pca_data,
  metadata_cols = c(1:6),
  mode = "tsne",
  color_var = "time",
  ellipse = FALSE,
  tsne_perplexity = 30,
  title = "t-SNE Plot of Example Dataset",
  subtitle = "Colored by time",
  caption = "Data source: Example dataset"
)
print(p_tsne_time)

# UMAP example with custom labels and titles
p_umap_group <- ggpca(
  pca_data,
  metadata_cols = c(1:6),
  mode = "umap",
  color_var = "group",
  ellipse = FALSE,
  umap_n_neighbors = 15,
  title = "UMAP Plot of Example Dataset",
  subtitle = "Colored by group",
  caption = "Data source: Example dataset"
)
print(p_umap_group)

# PCA example with x-axis density plot only
p_pca_x_time <- ggpca(
  pca_data,
  metadata_cols = c(1:6),
  mode = "pca",
  color_var = "time",
  ellipse = TRUE,
  density_plot = "x",
  title = "PCA with X-axis Density Plot",
  subtitle = "Example dataset, colored by time",
  caption = "Data source: Example dataset"
)
print(p_pca_x_time)

# PCA example with y-axis density plot only
p_pca_y_group <- ggpca(
  pca_data,
  metadata_cols = c(1:6),
  mode = "pca",
  color_var = "group",
  ellipse = TRUE,
  density_plot = "y",
  title = "PCA with Y-axis Density Plot",
  subtitle = "Example dataset, colored by group",
  caption = "Data source: Example dataset"
)
print(p_pca_y_group)

# PCA example with both density plots
p_pca_both_time <- ggpca(
  pca_data,
  metadata_cols = c(1:6),
  mode = "pca",
  color_var = "time",
  ellipse = TRUE,
  density_plot = "both",
  title = "PCA with Both Density Plots",
  subtitle = "Example dataset, colored by time",
  caption = "Data source: Example dataset"
)
print(p_pca_both_time)

# PCA example with faceting by time_category and group
p_pca_faceting_x <- ggpca(
  pca_data,
  metadata_cols = c(1:6),
  mode = "pca",
  color_var = "time",
  ellipse = FALSE,
  facet_var = . ~ group,
  density_plot = "none",
  title = "PCA Faceted by Group",
  subtitle = "Facet along X-axis, colored by time",
  caption = "Data source: Example dataset"
)
print(p_pca_faceting_x)

p_pca_faceting_y <- ggpca(
  pca_data,
  metadata_cols = c(1:6),
  mode = "pca",
  color_var = "time",
  ellipse = FALSE,
  facet_var = time ~ .,
  density_plot = "none",
  title = "PCA Faceted by Time Category",
  subtitle = "Facet along Y-axis, colored by time",
  caption = "Data source: Example dataset"
)
print(p_pca_faceting_y)

p_pca_faceting_both <- ggpca(
  pca_data,
  metadata_cols = c(1:6),
  mode = "pca",
  color_var = "type",
  ellipse = FALSE,
  facet_var = time ~ group,
  density_plot = "none",
  title = "PCA Faceted by Time Category and Group",
  subtitle = "Facet along both axes",
  caption = "Data source: Example dataset"
)
print(p_pca_faceting_both)

