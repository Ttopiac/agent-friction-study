# Agent friction study: analysis of data/messages.csv. Base R only. Run: Rscript analysis.R
# Outputs hold aggregates and public labels only: no text, dates, or session IDs.
# Each results file has: Purpose, Variables, tables, Results (APA 7th ed.), Interpretation, Limitation, References.
# Report text follows ASD-STE100. All settings live in config/rules.json; see results/0_definitions.md.
source("R/helpers.R")

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

# ---- Shared report text ----
DV <- c("Dissatisfied reply", "Dependent variable",
        "100 if the next user message in the same session is Dissatisfied or Mixed. 0 if not. The mean is the % of prompts with a dissatisfied reply.")
IV_TYPE <- c("Output type", "Independent variable",
             "Visual: the prompt is about visual output (HTML, PPTX, PDF, slides, figures, layout). Text: all other prompts.")
IV_STAGE <- c("Session stage", "Independent variable", "Early: the first half of the prompts in a session. Late: the second half.")
CAVEAT <- c("Prompts in the same session are not independent.",
            "The dependent variable has only two values (0 and 100). Thus, the test gives an approximate result.")
LABAN <- "Laban, P., et al. (2025). LLMs get lost in multi-turn conversation. arXiv:2505.06120."
Y_LABEL <- "% prompts followed by a dissatisfied reply"

# Figure title (the finding) and subtitle (the statistics) for a compare_two() t-test
figure_finding <- function(cmp, if_real, if_chance) if (cmp$test$p.value < 0.05) if_real else if_chance
highlight_if_real <- function(cmp, group) if (cmp$test$p.value < 0.05) group else ""  # red only for a real effect
stats_line <- function(cmp) {
  c(sprintf("t(%.1f) = %.2f, p %s, d = %.2f.", cmp$test$parameter, abs(cmp$test$statistic), apa_p(cmp$test$p.value), cmp$d),
    "Bars = 95% CI.")
}

# Results file for a compare_two() t-test (Welch's t-test: no equal-variance assumption)
write_ttest_report <- function(cmp, file, title, purpose, iv, group_label, label_a, label_b, if_real, if_chance,
                               limitation = CAVEAT, references = COHEN) {
  s <- cmp$stats
  p <- cmp$test$p.value
  write_report(file, title, list(
    "Purpose"   = purpose,
    "Variables" = variables_table(DV, iv),
    "Table 1. Descriptive statistics" = group_table(s, group_label),
    "Table 2. Welch's t-test"         = ttest_table(cmp),
    "Results"   = sprintf("%s had %s dissatisfied replies than %s (%.1f%% and %.1f%%). The difference was %s (Table 2).",
                          label_a, if (cmp$d > 0) "more" else "fewer", tolower(label_b), s[2, "mean"], s[1, "mean"],
                          if (p < 0.05) "significant" else "not significant"),
    "Interpretation" = c(read_effect("Difference", p, d_text(cmp$d)), "", if (p < 0.05) if_real else if_chance, "", SCALE_NOTE),
    "Limitation" = bullets(limitation),
    "References" = references))
}

# ---- 1. Descriptive: % dissatisfied messages by domain ----
rates <- rate_by_group(messages$dissatisfied, messages$domain)
total <- sum(messages$dissatisfied)
top_two    <- rownames(rates)[1:2]
top_share  <- 100 * sum(rates$flagged[1:2]) / total                      # % of all dissatisfied messages
top_visual <- 100 * mean(messages$visual[messages$domain %in% top_two])  # % of their messages about visual output
write_report("results/1_descriptive.md", "Descriptive statistics: dissatisfied messages by domain", list(
  "Purpose"   = "This analysis shows where friction occurs. It counts the dissatisfied messages in each domain.",
  "Variables" = variables_table(
    c("Dissatisfied message", "Measure", "A message with the tone Dissatisfied or Mixed."),
    c("Domain", "Group", "The kind of work in the message. See 0_definitions.md.")),
  "Table 1. Dissatisfied messages by domain" = md_table(c("Domain", "Messages", "Dissatisfied", "%"),
    lapply(rownames(rates), function(d) c(d, rates[d, "count"], rates[d, "flagged"], sprintf("%.1f", rates[d, "pct"])))),
  "Results" = c(sprintf("There were %d messages. Of these, %d (%.1f%%) were dissatisfied.", nrow(messages), total, 100 * total / nrow(messages)),
                sprintf("%s had the highest rate (%.1f%%). %s had the second highest rate (%.1f%%).",
                        top_two[1], rates$pct[1], top_two[2], rates$pct[2]),
                sprintf("All other domains had a rate of %.1f%% or less.", max(rates$pct[-(1:2)]))),
  "Interpretation" = paragraphs(sprintf("%s and %s have %.0f%% of all dissatisfied messages.", top_two[1], top_two[2], top_share),
                       sprintf("In these two domains, %.0f%% of the messages are about visual output.", top_visual),
                       if (top_visual > 50) "Thus, friction occurs mostly in visual work." else "Thus, friction is not limited to visual work."),
  "Limitation" = bullets("This analysis is a description. It does not include a statistical test.",
                   sprintf("The domains have different sizes (%d to %d messages). The %% for a small domain is not stable.",
                           min(rates$count), max(rates$count)))))
plot_rates(rates, "figures/1_descriptive.png",
           title = sprintf("%s and %s have %.0f%% of all dissatisfied messages", top_two[1], top_two[2], top_share),
           subtitle = c("% of messages in each domain that were dissatisfied.",
                        sprintf("Overall: %d of %d messages were dissatisfied.", total, nrow(messages))),
           x_label = "% dissatisfied messages", highlight = c("Slide deck", "Web page"))

# ---- 2a. t-test: short vs long prompts ----
median_words <- median(prompts$words)
prompts$length_group <- ifelse(prompts$words > median_words, "Long", "Short")
cmp <- compare_two(prompts$bad_reply, prompts$length_group, "Long", "Short")
write_ttest_report(cmp, "results/2a_ttest_length.md", "t-test: short and long prompts",
  purpose = "This test finds if prompt length has an effect on the agent's results.",
  iv = c("Prompt length", "Independent variable",
         sprintf("Long: more than %g words (the median). Short: %g words or fewer.", median_words, median_words)),
  group_label = "Prompt length", label_a = "Long prompts", label_b = "Short prompts",
  if_real = "Prompt length has an effect on the agent's results.",
  if_chance = "Prompt length has no effect on the agent's results. Other factors cause the friction.")
plot_means_ci(cmp$stats, "figures/2a_ttest_length.png", figure_finding(cmp, "Prompt length makes a difference", "Prompt length makes no difference"), stats_line(cmp), Y_LABEL, highlight_if_real(cmp, "Long"))

# ---- 2b. t-test: visual vs text prompts ----
typed <- prompts[prompts$output_type != "", ]  # pastes have no output type
cmp <- compare_two(typed$bad_reply, typed$output_type, "Visual", "Text")
write_ttest_report(cmp, "results/2b_ttest_visual.md", "t-test: visual and text prompts",
  purpose = "This test finds if prompts about visual output get more dissatisfied replies than prompts about text.",
  iv = IV_TYPE, group_label = "Output type", label_a = "Visual prompts", label_b = "Text prompts",
  if_real = "Visual prompts get more dissatisfied replies than text prompts.",
  if_chance = "Output type has no effect on the agent's results.",
  limitation = c(sprintf("%d pastes have no output type. The test does not include them.", nrow(prompts) - nrow(typed)), CAVEAT))
plot_means_ci(cmp$stats, "figures/2b_ttest_visual.png", figure_finding(cmp, "Visual prompts get more dissatisfied replies", "Output type makes no difference"), stats_line(cmp), Y_LABEL, highlight_if_real(cmp, "Visual"))

# ---- 2c. t-test: early vs late prompts in a session ----
in_session <- prompts[as.logical(prompts$session_kept), ]  # sessions with >= min_messages messages; pastes have no session
stage <- ave(seq_len(nrow(in_session)), in_session$session_no, FUN = function(i) seq_along(i) / length(i))
in_session$half <- ifelse(stage <= 0.5, "Early", "Late")  # first half vs second half of each session's prompts
cmp <- compare_two(in_session$bad_reply, in_session$half, "Late", "Early")
write_ttest_report(cmp, "results/2c_ttest_stage.md", "t-test: early and late prompts in a session",
  purpose = "This test finds if friction increases as a session continues. Language models can lose track in long conversations (Laban et al., 2025).",
  iv = IV_STAGE, group_label = "Session stage", label_a = "Late prompts", label_b = "Early prompts",
  if_real = "Friction increases as a session continues.",
  if_chance = "Friction does not increase as a session continues.",
  limitation = c("The relation can go in the other direction. A session can become long because the agent failed early.", CAVEAT),
  references = c(COHEN, "", LABAN))
plot_means_ci(cmp$stats, "figures/2c_ttest_stage.png", figure_finding(cmp, "Friction increases late in a session", "Session stage makes no difference"), stats_line(cmp), Y_LABEL, highlight_if_real(cmp, "Late"))

# ---- 3. Two-way ANOVA: output type x session stage ----
y   <- in_session$bad_reply
typ <- in_session$output_type  # Text / Visual
pos <- in_session$half         # Early / Late
effects <- two_way_anova(y, typ, pos)
cells <- describe_by_group(y, paste(typ, pos, sep = ", "))
cell  <- tapply(y, list(typ, pos), mean)
zero_sd <- rownames(cells)[cells$sd == 0]
conclusion <- if (effects["a:b", "p"] < 0.05) {
  c("The effect of session stage changes with the output type.",
    sprintf("From early to late, the rate increases by %.1f points for visual prompts and by %.1f points for text prompts.",
            cell["Visual", "Late"] - cell["Visual", "Early"], cell["Text", "Late"] - cell["Text", "Early"]))
} else if (effects["a", "p"] < 0.05 && effects["b", "p"] < 0.05) {
  c("Output type and session stage are two separate factors in friction. Their effects add.",
    sprintf("The highest rate is for visual prompts late in a session (%.1f%%).", cell["Visual", "Late"]),
    sprintf("The lowest rate is for text prompts early in a session (%.1f%%).", cell["Text", "Early"]))
} else {
  sprintf("Output type %s. Session stage %s.", if (effects["a", "p"] < 0.05) "has an effect" else "has no effect",
          if (effects["b", "p"] < 0.05) "has an effect" else "has no effect")
}
sig <- function(row) if (effects[row, "p"] < 0.05) "significant" else "not significant"
write_report("results/3_anova.md", "Two-way ANOVA: output type and session stage", list(
  "Purpose" = c("Tests 2b and 2c found more friction for visual prompts and for late prompts.",
                "This ANOVA tests each effect with the other effect held constant. It also tests if the effect of session stage changes with the output type (the interaction)."),
  "Variables" = variables_table(DV, IV_TYPE, IV_STAGE),
  "Table 1. Descriptive statistics" = md_table(c("Output type, Session stage", "*n*", "*M*", "*SD*"),
    lapply(rownames(cells), function(g) c(g, cells[g, "count"], sprintf("%.1f", cells[g, "mean"]), sprintf("%.1f", cells[g, "sd"])))),
  "Table 2. Two-way ANOVA" = c(anova_table(effects, c("Output type", "Session stage", "Output type × Session stage", "Residual")), "",
                               "*Note.* Type II sums of squares. η²p = partial eta-squared."),
  "Results" = sprintf("The main effect of output type was %s. The main effect of session stage was %s. The interaction was %s (Table 2).",
                      sig("a"), sig("b"), sig("a:b")),
  "Interpretation" = c(read_effect("Output type", effects["a", "p"], eta_text(effects["a", "partial_eta_sq"])),
                       read_effect("Session stage", effects["b", "p"], eta_text(effects["b", "partial_eta_sq"])),
                       read_effect("Interaction", effects["a:b", "p"], eta_text(effects["a:b", "partial_eta_sq"])),
                       "", paste(conclusion, collapse = " "), "", SCALE_NOTE),
  "Limitation" = bullets(sprintf("The groups are small (%d to %d prompts).", min(cells$count), max(cells$count)),
                         if (length(zero_sd) > 0) sprintf("The group %s has no dissatisfied replies (*SD* = 0). Thus, the groups do not have equal variance.", zero_sd),
                         CAVEAT),
  "References" = COHEN))
plot_interaction(y, typ, pos, "figures/3_anova.png",
                 title = if (effects["a", "p"] < 0.05 && effects["b", "p"] < 0.05 && effects["a:b", "p"] >= 0.05)
                   "Visual output and a late session stage add up" else "Output type and session stage",
                 subtitle = c(sprintf("Output type: p %s. Session stage: p %s. Interaction: p %s.",
                                      apa_p(effects["a", "p"]), apa_p(effects["b", "p"]), apa_p(effects["a:b", "p"])),
                              "Parallel lines = no interaction. Bars = 95% CI."),
                 x_label = "Session stage", y_label = Y_LABEL, highlight = "Visual")
