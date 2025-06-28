if (!require(ggplot2)) install.packages("ggplot2")
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(readr)) install.packages("readr")
if (!require(moments)) install.packages("moments")
if (!require(GGally)) install.packages("GGally")

library(GGally)
library(ggplot2)
library(gridExtra)
library(readr)
library(moments)

data <- read_csv("E:/avocado_ripeness_dataset.csv")
data <- na.omit(data) 

numeric_cols <- names(data)[sapply(data, is.numeric)]
categorical_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
data[categorical_cols] <- lapply(data[categorical_cols], as.factor)

target <- "ripeness"
for (col in numeric_cols) {
  hist(data[[col]], main = paste("Histogram of", col),
       xlab = col, col = "lightblue", border = "black")
}

for (col in numeric_cols) {
  x <- data[[col]]
  hist(x, prob = TRUE, main = paste("Histogram + Density of", col),
       xlab = col, col = "lightblue", border = "black")
  lines(density(x), col = "blue", lwd = 2)
}

for (col in numeric_cols) {
  x <- data[[col]]
  dens <- density(x)
  skew <- round(skewness(x), 2)
  mean_x <- round(mean(x), 2)
  median_x <- round(median(x), 2)
  mode_x <- round(dens$x[which.max(dens$y)], 2)
  
  plot(dens, main = paste0("Density of ", col,
                           "\nMean: ", mean_x,
                           ", Median: ", median_x,
                           ", Mode: ", mode_x,
                           ", Skewness: ", ifelse(skew > 0, paste0("+", skew), skew)),
       xlab = col, col = "blue", lwd = 2)
  polygon(dens, col = rgb(0, 0, 1, 0.3), border = "blue")
}

for (col in categorical_cols) {
  counts <- table(data[[col]])
  barplot(counts,
          main = paste("Bar Chart of", col),
          xlab = col,
          ylab = "Count",
          col = rainbow(length(counts)),
          border = "black", las = 2)
}

for (col in numeric_cols) {
  print(
    ggplot(data, aes_string(x = target, y = col, fill = target)) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Set1") +
      ggtitle(paste("Boxplot of", col, "by", target)) +
      theme_minimal() +
      labs(x = target, y = col)
  )
}

for (col in numeric_cols) {
  print(
    ggplot(data, aes_string(x = target, y = col, fill = target)) +
      geom_violin(trim = FALSE, alpha = 0.6) +
      geom_boxplot(width = 0.1, outlier.shape = NA, color = "black") +  # Box for median & IQR, no outliers
      stat_summary(fun = median, geom = "point", shape = 21, size = 2, fill = "white", color = "black") +  # Highlight median
      scale_fill_brewer(palette = "Pastel1") +
      ggtitle(paste("Violin Plot of", col, "by", target)) +
      theme_minimal() +
      labs(x = "Target", y = col)
  )
}

scatter_plot <- function(x, y, color_col) {
  ggplot(data, aes_string(x = x, y = y, color = color_col)) +
    geom_point(alpha = 0.7, size = 1.5) +
    scale_color_brewer(palette = "Set1", name = "Ripeness Level") +
    theme_minimal() +
    labs(x = x, y = y)
}

plots <- list()
for (i in seq_along(numeric_cols)) {
  for (j in seq_along(numeric_cols)) {
    if (i != j) {
      plots[[length(plots) + 1]] <- scatter_plot(numeric_cols[j], numeric_cols[i], target)
    } else {
      plots[[length(plots) + 1]] <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = numeric_cols[i], size = 6) + 
        theme_void()
    }
  }
}

get_legend <- function(plot) {
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  tmp$grobs[[leg]]
}

legend_plot <- scatter_plot(numeric_cols[1], numeric_cols[2], target) + 
  theme(legend.position = "bottom")
legend <- get_legend(legend_plot)

plots <- lapply(plots, function(p) p + theme(legend.position = "none"))

gridExtra::grid.arrange(
  arrangeGrob(grobs = plots, ncol = length(numeric_cols)),
  legend,
  nrow = 2,
  heights = c(10, 1)
)

if (target %in% names(data)) {
  numeric_data <- data[c(numeric_cols, target)]
  ggpairs(numeric_data, aes(color = !!sym(target), alpha = 0.6),
          title = paste("Scatter Matrix Colored by", target))
} else {
  warning(paste("Target variable", target, "not found in dataset!"))
}
