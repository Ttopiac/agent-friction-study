# Agent friction study: analysis of data/messages.csv. Base R only.
# Outputs hold aggregates and public labels only: no text, dates, or session IDs.
DISSATISFIED <- c("Dissatisfied", "Mixed")  # tones that show the user is dissatisfied
MIN_MSGS <- 3   # sessions with fewer messages are dropped (their % is too noisy)
GAP_MIN <- 60   # a gap longer than this starts a new session

# Messages: one row per included message
m <- read.csv("data/messages.csv", stringsAsFactors = FALSE)
m <- m[m$step != 0, ]                                   # step 0 = excluded
m$time <- as.POSIXct(m$local_time, tz = "UTC"); m <- m[order(m$time), ]
m$dissatisfied <- m$tone %in% DISSATISFIED
m$visual <- m$output_type == "Visual"
m$session <- cumsum(c(TRUE, diff(as.numeric(m$time)) > GAP_MIN * 60))

# Sessions: one row per session with >= MIN_MSGS messages
s <- aggregate(cbind(n = 1, dissatisfied = dissatisfied, visual = visual) ~ session, m, sum)
s <- s[s$n >= MIN_MSGS, ]
s$pct_dissatisfied <- 100 * s$dissatisfied / s$n
s$visual_share <- 100 * s$visual / s$n
s$session_type <- ifelse(s$visual_share >= 50, "Visual", "Text")  # Visual = at least half Visual messages

cat("Messages:", nrow(m), " Sessions:", nrow(s), "\n")
