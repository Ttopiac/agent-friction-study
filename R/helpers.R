# Helper functions for analysis.R.
# Code uses plain words (count, mean, sd); reports use APA symbols (n, M, SD).
# Report text follows ASD-STE100: short sentences, active voice, one term for one concept.

COHEN <- "Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Erlbaum."

# ---- Statistics ----

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

# Welch t-test of values between two groups, plus group stats and Cohen's d (group_a minus group_b)
compare_two <- function(values, groups, group_a, group_b) {
  stats <- describe_by_group(values, groups)[c(group_b, group_a), ]
  test  <- t.test(values[groups == group_a], values[groups == group_b])
  list(stats = stats, test = test, d = cohens_d(stats, group_a, group_b))
}

# For each group: number of items, number flagged TRUE, and % flagged (highest % first)
rate_by_group <- function(flags, groups) {
  count   <- tapply(flags, groups, length)
  flagged <- tapply(flags, groups, sum)
  rates <- data.frame(count = as.vector(count), flagged = as.vector(flagged),
                      pct = as.vector(100 * flagged / count), row.names = names(count))
  rates[order(-rates$pct), ]
}

# Two-way ANOVA (a x b). Each main effect is tested after the other one (Type II sums of squares),
# so unequal group sizes do not change the result. Rows: a, b, a:b, Residual.
two_way_anova <- function(y, a, b) {
  fit_ab <- anova(lm(y ~ a * b))  # gives b adjusted for a, and the interaction
  fit_ba <- anova(lm(y ~ b * a))  # gives a adjusted for b
  rows   <- rbind(fit_ba["a", ], fit_ab["b", ], fit_ab["a:b", ], fit_ab["Residuals", ])
  ss_res <- fit_ab["Residuals", "Sum Sq"]
  data.frame(ss = rows$`Sum Sq`, df = rows$Df, ms = rows$`Mean Sq`, f = rows$`F value`, p = rows$`Pr(>F)`,
             partial_eta_sq = c(rows$`Sum Sq`[1:3] / (rows$`Sum Sq`[1:3] + ss_res), NA),
             row.names = c("a", "b", "a:b", "Residual"))
}

# ---- APA formatting ----

# p-value in APA style: no leading zero; "< .001" for very small values
apa_p <- function(p) {
  if (p < 0.001) return("< .001")
  paste("=", sub("^0", "", sprintf("%.3f", p)))
}

# Number in APA style for values that cannot exceed 1 (e.g. eta-squared): no leading zero
no_zero <- function(x, digits = 2) {
  sub("^0", "", sprintf(paste0("%.", digits, "f"), x))
}

# Markdown table from a header vector and a list of row vectors
md_table <- function(header, rows) {
  c(paste("|", paste(header, collapse = " | "), "|"),
    paste0(strrep("|---", length(header)), "|"),
    vapply(rows, function(r) paste("|", paste(r, collapse = " | "), "|"), ""))
}

# Variables table: one row per variable (name, role, definition)
variables_table <- function(...) md_table(c("Variable", "Role", "Definition"), list(...))

# Table of n, M, SD per group
group_table <- function(stats, group_label) {
  md_table(c(group_label, "*n*", "*M*", "*SD*"),
           lapply(rownames(stats), function(g) c(g, stats[g, "count"], sprintf("%.1f", stats[g, "mean"]), sprintf("%.1f", stats[g, "sd"]))))
}

# Table of the t-test result: t, df, p, d
ttest_table <- function(cmp) {
  md_table(c("*t*", "*df*", "*p*", "*d*"),
           list(c(sprintf("%.2f", abs(cmp$test$statistic)), sprintf("%.1f", cmp$test$parameter),
                  sub("^= ", "", apa_p(cmp$test$p.value)), sprintf("%.2f", cmp$d))))
}

# ANOVA table: df, F, p, partial eta-squared (only df for the Residual row)
anova_table <- function(effects, names) {
  rows <- lapply(seq_len(nrow(effects)), function(i) {
    e <- effects[i, ]
    if (rownames(effects)[i] == "Residual") return(c(names[i], e$df, "", "", ""))
    c(names[i], e$df, sprintf("%.2f", e$f), sub("^= ", "", apa_p(e$p)), no_zero(e$partial_eta_sq))
  })
  md_table(c("Source", "*df*", "*F*", "*p*", "η²p"), rows)
}

# ---- Interpretation in plain words (ASD-STE100) ----

# Size label with Cohen's benchmarks: below the first cut = negligible
size_label <- function(x, cuts) c("negligible", "small", "medium", "large")[findInterval(abs(x), cuts) + 1]

# One line per effect: real or chance, and the effect size
read_effect <- function(name, p, size_text) {
  sprintf("- **%s:** %s (*p* %s); %s.", name, if (p < 0.05) "real" else "can be chance", apa_p(p), size_text)
}
d_text   <- function(d)   sprintf("%s effect (*d* = %.2f)", size_label(d, c(0.2, 0.5, 0.8)), d)
eta_text <- function(eta) sprintf("%s effect (η²p = %s, %s%% of the differences)", size_label(eta, c(0.01, 0.06, 0.14)),
                                  no_zero(eta), format(signif(100 * eta, 2)))
SCALE_NOTE <- "Real: *p* < .05. Small, medium, and large: Cohen (1988)."

# ---- Output ----

# Markdown helpers: one paragraph per item, or one bullet per item
paragraphs <- function(...) head(as.vector(rbind(c(...), "")), -1)
bullets    <- function(...) paste("-", c(...))

# Write one results file. `sections` is a named list: name = heading, value = text lines.
write_report <- function(file, title, sections) {
  body <- unlist(lapply(names(sections), function(name) c(paste("##", name), sections[[name]], "")))
  writeLines(c(paste("#", title), "", body), file)
}

# ---- Figures: one style for all ----
# Title = the finding. Subtitle = the key statistics. Light grid, no box, value labels on the data.
COLORS <- c(base = "#9AA5B1", highlight = "#C0392B", grid = "#E6E9EC", ink = "#2D3436", muted = "#6B7378")

start_figure <- function(file, width = 1100, height = 800, mar = c(4.5, 5, 6.5, 2)) {
  png(file, width, height, res = 150)
  par(mar = mar, las = 1, col.axis = COLORS["muted"], col.lab = COLORS["ink"], fg = COLORS["muted"],
      mgp = c(3, 0.6, 0), tcl = 0, bty = "n")
}

# Title in bold, then the subtitle: one or two lines (a vector), split at a natural point by the caller
figure_title <- function(title, subtitle) {
  n <- length(subtitle)
  mtext(title, side = 3, line = 3.4 + 0.9 * (n - 1), adj = 0, font = 2, cex = 1.05, col = COLORS["ink"])
  for (i in seq_len(n)) mtext(subtitle[i], side = 3, line = 1.9 + 0.9 * (n - i), adj = 0, cex = 0.75, col = COLORS["muted"])
}

grid_lines <- function(ticks, horizontal = TRUE) {
  if (horizontal) abline(h = ticks, col = COLORS["grid"]) else abline(v = ticks, col = COLORS["grid"])
}

pct_ticks <- function(top) pretty(c(0, top), n = 4)

# Bar chart of group means (%) with 95% CI error bars, value labels, and n under each bar
plot_means_ci <- function(stats, file, title, subtitle, y_label, highlight) {
  start_figure(file, 900, 850)
  high  <- stats$mean + stats$ci95
  low   <- pmax(stats$mean - stats$ci95, 0)  # a percentage cannot be negative
  ticks <- pct_ticks(max(high) * 1.15)
  plot.new()
  plot.window(xlim = c(0.3, nrow(stats) * 1.6 + 0.2), ylim = range(ticks))
  grid_lines(ticks)
  colors <- ifelse(rownames(stats) == highlight, COLORS["highlight"], COLORS["base"])
  bars <- barplot(stats$mean, col = colors, border = NA, space = 0.6, add = TRUE, axes = FALSE, names.arg = rep("", nrow(stats)))
  arrows(bars, low, bars, high, angle = 90, code = 3, length = 0.05, col = COLORS["ink"])
  text(bars, high, sprintf("%.1f%%", stats$mean), pos = 3, font = 2, col = COLORS["ink"])
  axis(2, at = ticks, labels = paste0(ticks, "%"), lwd = 0)
  mtext(sprintf("%s\n(n = %d)", rownames(stats), stats$count), side = 1, at = bars, line = 1.8, col = COLORS["ink"])
  title(ylab = y_label)
  figure_title(title, subtitle)
  invisible(dev.off())
}

# Horizontal bar chart of % per group, with labels "% (flagged/count)"; groups in `highlight` are red
plot_rates <- function(rates, file, title, subtitle, x_label, highlight) {
  start_figure(file, 1200, 800, mar = c(4.5, 11, 6.5, 6))
  ordered <- rates[order(rates$pct), ]
  ticks <- pct_ticks(max(ordered$pct) * 1.25)
  plot.new()
  plot.window(xlim = range(ticks), ylim = c(0.2, nrow(ordered) * 1.2 + 0.2))
  grid_lines(ticks, horizontal = FALSE)
  colors <- ifelse(rownames(ordered) %in% highlight, COLORS["highlight"], COLORS["base"])
  bars <- barplot(ordered$pct, horiz = TRUE, col = colors, border = NA, add = TRUE, axes = FALSE,
                  names.arg = rownames(ordered), cex.names = 0.9, col.axis = COLORS["ink"])
  text(ordered$pct, bars, sprintf("%.1f%% (%d/%d)", ordered$pct, ordered$flagged, ordered$count),
       pos = 4, cex = 0.8, col = COLORS["ink"])
  axis(1, at = ticks, labels = paste0(ticks, "%"), lwd = 0)
  title(xlab = x_label)
  figure_title(title, subtitle)
  invisible(dev.off())
}

# Interaction plot: x = levels of b, one line per level of a, 95% CI bars, % labels, and the rise per line.
# Parallel lines = no interaction.
plot_interaction <- function(y, a, b, file, title, subtitle, x_label, y_label, highlight) {
  start_figure(file, 1100, 850, mar = c(4.5, 5, 6.5, 8))
  cells <- describe_by_group(y, interaction(a, b, sep = "|"))
  means <- tapply(y, list(a, b), mean)
  ci    <- matrix(cells[paste(rep(rownames(means), ncol(means)), rep(colnames(means), each = nrow(means)), sep = "|"), "ci95"],
                  nrow(means), dimnames = dimnames(means))
  ticks <- pct_ticks(max(means + ci) * 1.1)
  x <- seq_len(ncol(means))
  plot.new()
  plot.window(xlim = c(0.7, ncol(means) + 0.3), ylim = range(ticks))
  grid_lines(ticks)
  for (level in rownames(means)) {
    color <- if (level == highlight) COLORS["highlight"] else COLORS["base"]
    shift <- if (level == highlight) 0.03 else -0.03  # small shift so the CI bars do not overlap
    has_ci <- ci[level, ] > 0  # a group with SD = 0 has no CI bar
    arrows((x + shift)[has_ci], pmax(means[level, ] - ci[level, ], 0)[has_ci], (x + shift)[has_ci], (means[level, ] + ci[level, ])[has_ci],
           angle = 90, code = 3, length = 0.04, col = color)
    lines(x + shift, means[level, ], col = color, lwd = 3)
    points(x + shift, means[level, ], pch = 19, cex = 1.4, col = color)
    side <- ifelse(x == 1, 2, 4)  # label left of the first point, right of the others, so it does not cover the CI bar
    text(x + shift, means[level, ], sprintf("%.1f%%", means[level, ]), pos = side,
         offset = 0.8, font = 2, cex = 0.85, col = color)
    rise <- means[level, ncol(means)] - means[level, 1]
    mtext(sprintf("%s\n%+.1f pts", level, rise), side = 4, at = means[level, ncol(means)], line = 0.8,
          adj = 0, font = 2, cex = 0.85, col = color)
  }
  axis(1, at = x, labels = colnames(means), lwd = 0, col.axis = COLORS["ink"])
  axis(2, at = ticks, labels = paste0(ticks, "%"), lwd = 0)
  title(xlab = x_label, ylab = y_label)
  figure_title(title, subtitle)
  invisible(dev.off())
}
