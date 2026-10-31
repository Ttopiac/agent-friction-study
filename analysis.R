# Agent friction study: analysis of data/messages.csv. Base R only. Run: Rscript analysis.R
# Outputs hold aggregates and public labels only: no text, dates, or session IDs.
# Reporting follows APA style (7th ed.): tables use n, M, SD; each test gets one APA sentence
# with the test value, df, p, and an effect size (e.g. t(15.2) = 0.92, p = .371, d = 0.41).
source("R/helpers.R")
# All settings live in config/rules.json. friction.py applies them and writes ready-made columns
# (dissatisfied, reply_group, ...). See results/0_definitions.md for the human-readable version.

# ---- Messages: one row per included message ----
all_rows <- read.csv("data/messages.csv", stringsAsFactors = FALSE)
all_rows$dissatisfied <- as.logical(all_rows$dissatisfied)  # Python writes "True"/"False"
messages <- all_rows[all_rows$step != 0, ]                  # step 0 = excluded
messages$visual <- messages$output_type == "Visual"

# ---- Prompts: each prompt with the tone of the user's next message ----
# Prompts that were themselves complaints are dropped, so complaint chains do not count.
prompts <- all_rows[all_rows$reply_group != "" & !all_rows$dissatisfied, ]
prompts$bad_reply <- 100 * (prompts$reply_group == "Dissatisfied")  # 100 = next reply dissatisfied, 0 = not

cat("Messages:", nrow(messages), " Prompts:", nrow(prompts), "\n")
dir.create("results", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
# The t-tests compare the mean of bad_reply, which is the % of prompts followed by a dissatisfied reply.
# Welch's t-test: does not assume equal standard deviations in the two groups.

# ---- 1a. t-test: short vs long prompts ----
median_words <- median(prompts$words)
prompts$length_group <- ifelse(prompts$words > median_words, "Long", "Short")
cmp <- compare_two(prompts$bad_reply, prompts$length_group, "Long", "Short")
sentence <- apa_ttest(cmp, "Long prompts", "short prompts", "dissatisfied replies", "%")
meaning <- if (cmp$test$p.value < 0.05) {
  sprintf("Prompt length matters: %s prompts were followed by a dissatisfied reply more often.", if (cmp$d > 0) "long" else "short")
} else {
  "Prompt length did not change how often the agent's work led to a dissatisfied reply. The cause of friction lies elsewhere."
}
limitation <- sprintf("Split at the median (%g words). Prompts from the same session are not independent, and the outcome is yes/no, so the t-test compares percentages only approximately.",
                      median_words)
write_report("results/1a_ttest_length.md", "t-test: dissatisfied replies after short vs long prompts",
             apa_table(cmp$stats, "Prompt length"), sentence, meaning, limitation)
plot_means_ci(cmp$stats, "figures/1a_ttest_length.png", "% prompts followed by a dissatisfied reply", c("grey70", "firebrick"))

# ---- 1b. t-test: visual vs text prompts ----
typed <- prompts[prompts$output_type != "", ]  # pastes have no output type
cmp <- compare_two(typed$bad_reply, typed$output_type, "Visual", "Text")
sentence <- apa_ttest(cmp, "Visual prompts", "text prompts", "dissatisfied replies", "%")
meaning <- if (cmp$test$p.value < 0.05) {
  sprintf("Output type matters: prompts about %s output were followed by a dissatisfied reply more often.", if (cmp$d > 0) "visual" else "text")
} else {
  "Output type did not significantly change how often the agent's work led to a dissatisfied reply."
}
limitation <- sprintf("%d pastes have no output type and are left out. Prompts from the same session are not independent, and the outcome is yes/no, so the t-test compares percentages only approximately.",
                      nrow(prompts) - nrow(typed))
write_report("results/1b_ttest_visual.md", "t-test: dissatisfied replies after visual vs text prompts",
             apa_table(cmp$stats, "Output type"), sentence, meaning, limitation)
plot_means_ci(cmp$stats, "figures/1b_ttest_visual.png", "% prompts followed by a dissatisfied reply", c("grey70", "firebrick"))

# ---- 1c. t-test: early vs late prompts in a session ----
# Does friction build up as a session gets longer? (LLMs "get lost" in long conversations: Laban et al., ICLR 2026)
in_session <- prompts[as.logical(prompts$session_kept), ]  # sessions with >= min_messages messages; pastes have no session
position <- ave(seq_len(nrow(in_session)), in_session$session_no, FUN = function(i) seq_along(i) / length(i))
in_session$half <- ifelse(position <= 0.5, "Early", "Late")  # first half vs second half of each session's prompts
cmp <- compare_two(in_session$bad_reply, in_session$half, "Late", "Early")
sentence <- apa_ttest(cmp, "Late prompts", "early prompts", "dissatisfied replies", "%")
meaning <- if (cmp$test$p.value < 0.05) {
  sprintf("Friction %s as a session goes on: prompts in the second half of a session were followed by a dissatisfied reply %s often.",
          if (cmp$d > 0) "builds up" else "fades", if (cmp$d > 0) "more" else "less")
} else {
  "Friction did not build up within a session: prompts late in a session were not followed by a dissatisfied reply more often than early ones."
}
limitation <- "The link can run backwards: a session may become long because the agent failed early, not the other way around. Prompts from the same session are not independent, and the outcome is yes/no, so the t-test compares percentages only approximately."
write_report("results/1c_ttest_position.md", "t-test: dissatisfied replies after early vs late prompts in a session",
             apa_table(cmp$stats, "Position in session"), sentence, meaning, limitation)
plot_means_ci(cmp$stats, "figures/1c_ttest_position.png", "% prompts followed by a dissatisfied reply", c("grey70", "firebrick"))

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
top_two     <- rownames(rates)[1:2]
top_share   <- 100 * sum(rates$flagged[1:2]) / total                               # % of all dissatisfied messages
top_visual  <- 100 * mean(messages$visual[messages$domain %in% top_two])           # % of their messages about visual output
meaning <- sprintf("Dissatisfaction was concentrated: %s and %s hold %.0f%% of all dissatisfied messages, and %.0f%% of their messages are about visual output. Other work, such as forms, documents, and code, rarely caused friction.",
                   top_two[1], top_two[2], top_share, top_visual)
limitation <- sprintf("This is a description, not a test. Domains differ in size (%d to %d messages), so percentages for small domains are unstable.",
                      min(rates$count), max(rates$count))
write_report("results/2_descriptive.md", "Descriptive: dissatisfied messages by domain",
             table, sentence, meaning, limitation)
plot_rates(rates, "figures/2_descriptive.png", "% dissatisfied messages", highlight = c("Slide deck", "Web page"))

