# Agent friction study: analysis of data/messages.csv. Base R only. Run: Rscript analysis.R
# Outputs hold aggregates and public labels only: no text, dates, or session IDs.
# Reporting follows APA style (7th ed.): tables use n, M, SD; each test gets one APA sentence
# with the test value, df, p, and an effect size (e.g. t(15.2) = 0.92, p = .371, d = 0.41).
source("R/helpers.R")
# All settings (dissatisfied tones, session gap, minimum session size) live in config/rules.json.
# friction.py applies them and writes the columns dissatisfied, session_no, and session_kept.
# See results/0_definitions.md for the human-readable version.

# ---- Messages: one row per included message ----
messages <- read.csv("data/messages.csv", stringsAsFactors = FALSE)
messages <- messages[messages$step != 0, ]  # step 0 = excluded
messages$dissatisfied <- as.logical(messages$dissatisfied)  # Python writes "True"/"False"
messages$session_kept <- as.logical(messages$session_kept)
messages$visual <- messages$output_type == "Visual"

# ---- Sessions: one row per kept session ----
kept <- messages[messages$session_kept, ]
sessions <- aggregate(cbind(count = 1, dissatisfied, visual) ~ session_no, data = kept, FUN = sum)
sessions$pct_dissatisfied <- 100 * sessions$dissatisfied / sessions$count
sessions$visual_share <- 100 * sessions$visual / sessions$count
sessions$session_type <- ifelse(sessions$visual_share >= 50, "Visual", "Text")  # Visual = at least half Visual messages

cat("Messages:", nrow(messages), " Sessions:", nrow(sessions), "\n")
dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# ---- 1. t-test: % dissatisfied messages per session, Visual vs Text ----
# Welch's t-test: does not assume equal standard deviations in the two groups.
stats <- describe_by_group(sessions$pct_dissatisfied, sessions$session_type)
test  <- t.test(pct_dissatisfied ~ session_type, data = sessions)
d     <- cohens_d(stats, "Visual", "Text")

direction    <- if (d > 0) "more" else "fewer"
significance <- if (test$p.value < 0.05) "and the difference was significant" else "but the difference was not significant"
sentence <- sprintf(
  "Visual sessions had %s dissatisfied messages (*M* = %.1f%%, *SD* = %.1f) than text sessions (*M* = %.1f%%, *SD* = %.1f), %s, *t*(%.1f) = %.2f, *p* %s, *d* = %.2f.",
  direction,
  stats["Visual", "mean"], stats["Visual", "sd"],
  stats["Text", "mean"], stats["Text", "sd"],
  significance,
  test$parameter, abs(test$statistic), apa_p(test$p.value), d
)
writeLines(c("# t-test: % dissatisfied messages per session", "", apa_table(stats, "Session type"), "", sentence),
           "results/1_ttest.md")
plot_means_ci(stats, "figures/1_ttest.png", "% dissatisfied messages per session", c("grey70", "firebrick"))

# ---- 2. Descriptive: % dissatisfied messages by domain ----
rates <- rate_by_group(messages$dissatisfied, messages$domain)
total <- sum(messages$dissatisfied)
others_max <- max(rates$pct[-(1:2)])
sentence <- sprintf(
  "Of %d messages, %d (%.1f%%) were dissatisfied. The rate was highest for %s (%.1f%%) and %s (%.1f%%), and at most %.1f%% in every other domain.",
  nrow(messages), total, 100 * total / nrow(messages),
  rownames(rates)[1], rates$pct[1], rownames(rates)[2], rates$pct[2], others_max
)
table <- c(
  "| Domain | n | Dissatisfied | % |",
  "|---|---|---|---|",
  sprintf("| %s | %d | %d | %.1f |", rownames(rates), rates$count, rates$flagged, rates$pct)
)
writeLines(c("# Descriptive: dissatisfied messages by domain", "", table, "", sentence), "results/2_descriptive.md")
plot_rates(rates, "figures/2_descriptive.png", "% dissatisfied messages", highlight = c("Slide deck", "Web page"))
