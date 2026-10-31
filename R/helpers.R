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

# For each group: number of items, number flagged TRUE, and % flagged (highest % first)
rate_by_group <- function(flags, groups) {
  count   <- tapply(flags, groups, length)
  flagged <- tapply(flags, groups, sum)
  rates <- data.frame(count = as.vector(count), flagged = as.vector(flagged),
                      pct = as.vector(100 * flagged / count), row.names = names(count))
  rates[order(-rates$pct), ]
}

# Horizontal bar chart of % per group; groups in `highlight` are red
plot_rates <- function(rates, file, x_label, highlight) {
  png(file, 1000, 650, res = 130)
  par(mar = c(5, 11, 2, 1))
  ordered <- rates[order(rates$pct), ]
  colors  <- ifelse(rownames(ordered) %in% highlight, "firebrick", "grey70")
  barplot(ordered$pct, names.arg = rownames(ordered), horiz = TRUE, las = 1, col = colors, xlab = x_label)
  invisible(dev.off())
}

# Write one results file: table, then Result (APA), Meaning (plain words), Limitation
write_report <- function(file, title, table, result, meaning, limitation) {
  writeLines(c(paste("#", title), "", table, "",
               "## Result", result, "",
               "## Meaning", meaning, "",
               "## Limitation", limitation), file)
}

# Welch t-test of values between two groups, plus group stats and Cohen's d (group_a minus group_b)
compare_two <- function(values, groups, group_a, group_b) {
  stats <- describe_by_group(values, groups)[c(group_b, group_a), ]
  test  <- t.test(values[groups == group_a], values[groups == group_b])
  list(stats = stats, test = test, d = cohens_d(stats, group_a, group_b))
}

# APA sentence for a compare_two() result
apa_ttest <- function(cmp, label_a, label_b, measure, unit = "") {
  s <- cmp$stats
  a <- rownames(s)[2]
  b <- rownames(s)[1]
  sprintf("%s had %s %s (*M* = %.1f%s, *SD* = %.1f) than %s (*M* = %.1f%s, *SD* = %.1f), %s, *t*(%.1f) = %.2f, *p* %s, *d* = %.2f.",
          label_a, if (cmp$d > 0) "more" else "fewer", measure, s[a, "mean"], unit, s[a, "sd"],
          label_b, s[b, "mean"], unit, s[b, "sd"],
          if (cmp$test$p.value < 0.05) "and the difference was significant" else "but the difference was not significant",
          cmp$test$parameter, abs(cmp$test$statistic), apa_p(cmp$test$p.value), cmp$d)
}
