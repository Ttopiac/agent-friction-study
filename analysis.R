# Agent friction study: descriptive, t-test, ANOVA, correlation + regression on data/messages.csv.
# Base R only. Outputs hold aggregates and public labels only: no text, dates, or session IDs.
NEG <- c("Negative", "Mixed")  # tones that contain negative feedback
MIN_MSGS <- 3                  # sessions with fewer messages are dropped (their % is too noisy)
GAP_MIN <- 60                  # a gap longer than this starts a new session
VISUAL_DOMAINS <- c("Slide deck", "Web page")  # highlighted in figure 1
dir.create("results", showWarnings = FALSE); dir.create("figures", showWarnings = FALSE)
sink("results/output.txt", split = TRUE)

m <- read.csv("data/messages.csv", stringsAsFactors = FALSE)
m <- m[m$step != 0, ]                                   # step 0 = excluded
m$time <- as.POSIXct(m$local_time, tz = "UTC"); m <- m[order(m$time), ]
m$neg <- m$tone %in% NEG
m$visual <- m$output_type == "Visual"
m$session <- cumsum(c(TRUE, diff(as.numeric(m$time)) > GAP_MIN * 60))

pct_table <- function(by) {
  t <- aggregate(cbind(n = 1, negative = neg) ~ get(by), m, sum); names(t)[1] <- by
  t$pct_negative <- round(100 * t$negative / t$n, 1); t[order(-t$pct_negative), ]
}
cat("== 1. Descriptive ==\n")
for (by in c("group", "domain", "output_type")) print(pct_table(by), row.names = FALSE)
cat("\nWords per message by domain (mean, median):\n")
print(aggregate(words ~ domain, m, function(x) round(c(mean = mean(x), median = median(x)), 1)))
cat("\nMessages per local hour:\n"); print(table(format(m$time, "%H")))

s <- aggregate(cbind(n = 1, negative = neg, visual = visual) ~ session, m, sum)
s <- s[s$n >= MIN_MSGS, ]
s$pct_negative <- 100 * s$negative / s$n
s$visual_share <- 100 * s$visual / s$n
s$visual_session <- ifelse(s$visual_share >= 50, "Visual", "Text")
cat("\nSessions with >=", MIN_MSGS, "messages:", nrow(s), "\n"); print(table(s$visual_session))

cat("\n== 2. t-test (Welch): % negative per session, Visual vs Text sessions ==\n")
print(aggregate(pct_negative ~ visual_session, s, function(x) round(c(mean = mean(x), sd = sd(x), n = length(x)), 2)))
print(t.test(pct_negative ~ visual_session, s))

cat("\n== 3. One-way ANOVA: words per message by domain ==\n")
a <- aov(words ~ domain, m[m$domain != "Unclassified", ]); print(summary(a))
ss <- summary(a)[[1]][["Sum Sq"]]; cat("eta-squared:", round(ss[1] / sum(ss), 3), "\n")
print(TukeyHSD(a))

cat("\n== 4. Correlation + regression: % negative per session ==\n")
print(cor.test(s$visual_share, s$pct_negative))
print(cor.test(s$n, s$pct_negative))
print(summary(lm(pct_negative ~ visual_share + n, s)))
sink()

png("figures/1_pct_negative_by_domain.png", 1100, 700, res = 130); par(mar = c(5, 11, 2, 1))
d <- pct_table("domain"); d <- d[order(d$pct_negative), ]
barplot(d$pct_negative, names.arg = d$domain, horiz = TRUE, las = 1, xlab = "% negative messages",
        col = ifelse(d$domain %in% VISUAL_DOMAINS, "firebrick", "grey70")); invisible(dev.off())
png("figures/2_session_negative_visual_vs_text.png", 800, 700, res = 130)
boxplot(pct_negative ~ visual_session, s, ylab = "% negative per session", xlab = "", col = c("grey70", "firebrick"))
stripchart(pct_negative ~ visual_session, s, vertical = TRUE, add = TRUE, pch = 16, method = "jitter"); invisible(dev.off())
png("figures/3_words_by_domain.png", 1100, 700, res = 130); par(mar = c(5, 11, 2, 1))
boxplot(words ~ domain, m[m$domain != "Unclassified", ], horizontal = TRUE, las = 1, xlab = "Words per message", ylab = "", outline = FALSE); invisible(dev.off())
png("figures/4_visual_share_vs_negative.png", 900, 700, res = 130)
plot(s$visual_share, s$pct_negative, pch = 16, cex = sqrt(s$n) / 3, xlab = "% Visual messages in session", ylab = "% negative in session")
abline(lm(pct_negative ~ visual_share, s), col = "firebrick"); invisible(dev.off())
