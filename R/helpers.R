# Helper functions for analysis.R.
# Code uses plain words (count, mean, sd); reports use APA symbols (n, M, SD).

# Count, mean, sd, and 95% confidence-interval half-width of a numeric vector
describe <- function(x) {
  count <- length(x)
  data.frame(
    count = count,
    mean  = mean(x),
    sd    = sd(x),
    ci95  = qt(0.975, count - 1) * sd(x) / sqrt(count)
  )
}

# describe() for each group: one row per group, row names = group names
describe_by_group <- function(values, groups) {
  rows <- lapply(split(values, groups), describe)
  do.call(rbind, rows)
}

# Cohen's d: difference in means divided by the pooled standard deviation
cohens_d <- function(stats, group_a, group_b) {
  pooled_var <- sum((stats$count - 1) * stats$sd^2) / (sum(stats$count) - 2)
  (stats[group_a, "mean"] - stats[group_b, "mean"]) / sqrt(pooled_var)
}

# p-value in APA style: no leading zero; "< .001" for very small values
apa_p <- function(p) {
  if (p < 0.001) return("< .001")
  paste("=", sub("^0", "", sprintf("%.3f", p)))
}

# Markdown table with APA columns n, M, SD
apa_table <- function(stats, group_label) {
  c(
    sprintf("| %s | n | M | SD |", group_label),
    "|---|---|---|---|",
    sprintf("| %s | %d | %.1f | %.1f |", rownames(stats), stats$count, stats$mean, stats$sd)
  )
}

# Bar chart of group means with 95% CI error bars (cut at 0: a percentage cannot be negative)
plot_means_ci <- function(stats, file, y_label, colors) {
  png(file, 700, 700, res = 130)
  top <- max(stats$mean + stats$ci95) * 1.1
  bars <- barplot(stats$mean, names.arg = rownames(stats), col = colors, ylim = c(0, top), ylab = y_label)
  arrows(bars, pmax(stats$mean - stats$ci95, 0), bars, stats$mean + stats$ci95,
         angle = 90, code = 3, length = 0.08)
  invisible(dev.off())
}
