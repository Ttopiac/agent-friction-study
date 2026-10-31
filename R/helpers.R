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

# Bar chart of group means with 95% CI error bars (cut at 0: a percentage cannot be negative)
plot_means_ci <- function(stats, file, y_label, colors) {
  png(file, 700, 700, res = 130)
  top <- max(stats$mean + stats$ci95) * 1.1
  bars <- barplot(stats$mean, names.arg = rownames(stats), col = colors, ylim = c(0, top), ylab = y_label)
  arrows(bars, pmax(stats$mean - stats$ci95, 0), bars, stats$mean + stats$ci95,
         angle = 90, code = 3, length = 0.08)
  invisible(dev.off())
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

# Line plot of cell means: x = levels of b, one line per level of a. Non-parallel lines = interaction.
plot_interaction <- function(y, a, b, file, y_label, colors) {
  means <- tapply(y, list(a, b), mean)
  png(file, 800, 700, res = 130)
  matplot(t(means), type = "b", pch = 16, lty = 1, lwd = 2, col = colors, xaxt = "n",
          ylim = c(0, max(means) * 1.25), xlab = "", ylab = y_label)
  axis(1, at = seq_len(ncol(means)), labels = colnames(means))
  legend("topleft", rownames(means), col = colors, lty = 1, lwd = 2, pch = 16, bty = "n")
  invisible(dev.off())
}
