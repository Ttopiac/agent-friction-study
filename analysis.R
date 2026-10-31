# Agent friction study: analysis of data/messages.csv. Base R only. Run: Rscript analysis.R
# Outputs hold aggregates and public labels only: no text, dates, or session IDs.
# Reporting follows APA style (7th ed.): tables use n, M, SD; each test gets one APA sentence
# with the test value, df, p, and an effect size (e.g. t(15.2) = 0.92, p = .371, d = 0.41).
source("R/helpers.R")

DISSATISFIED <- c("Dissatisfied", "Mixed")  # tones that show the user is dissatisfied
MIN_MESSAGES <- 3                           # sessions with fewer messages are dropped (their % is too noisy)
GAP_MINUTES  <- 60                          # a gap longer than this starts a new session

# ---- Messages: one row per included message ----
messages <- read.csv("data/messages.csv", stringsAsFactors = FALSE)
messages <- messages[messages$step != 0, ]  # step 0 = excluded
messages$time <- as.POSIXct(messages$local_time, tz = "UTC")
messages <- messages[order(messages$time), ]
messages$dissatisfied <- messages$tone %in% DISSATISFIED
messages$visual <- messages$output_type == "Visual"
new_session <- c(TRUE, diff(as.numeric(messages$time)) > GAP_MINUTES * 60)
messages$session <- cumsum(new_session)

# ---- Sessions: one row per session with >= MIN_MESSAGES messages ----
sessions <- aggregate(cbind(count = 1, dissatisfied, visual) ~ session, data = messages, FUN = sum)
sessions <- sessions[sessions$count >= MIN_MESSAGES, ]
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
