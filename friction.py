"""Agent friction study: label each user message in Claude Code logs. Run: python3 friction.py -> data/messages.csv"""
import csv, glob, json, os, re
from collections import Counter
from dataclasses import dataclass, asdict
from datetime import date, datetime, timedelta


@dataclass
class Message:
    local_time: datetime
    session: str
    text: str
    words: int = 0
    domain: str = ""
    group: str = ""
    step: int = -1  # which DomainRule step set the domain (0-9, see DomainRule); -1 = not set yet
    matched: str = ""
    all_domains: str = ""
    output_type: str = ""
    tone: str = ""
    tone_matched: str = ""
    dissatisfied: bool = False  # tone is in rules.json tone.dissatisfied
    session_no: int = 0         # work-session number (see rules.json "session")
    session_kept: bool = False  # session has >= min_messages messages


class Config:
    """Public rules.json, plus private local.json (falls back to local.example.json)."""

    def __init__(self, folder="config"):
        self.rules = json.load(open(f"{folder}/rules.json"))
        local = json.load(open(next(p for p in (f"{folder}/local.json", f"{folder}/local.example.json") if os.path.exists(p))))
        for label, words in self.rules["domain"]["lists"]:  # private words are added to the public lists
            words += local["extra_words"].get(label, [])
        self.exclude_from = date.fromisoformat(local["exclude_from"])
        self.offsets = [(date.fromisoformat(d), h) for d, h in local["utc_offsets"]]
        self.gap = timedelta(minutes=self.rules["session"]["gap_minutes"])


class Reader:
    def __init__(self, config, log_dir="~/.claude/projects"):
        self.offsets, self.log_dir = config.offsets, os.path.expanduser(log_dir)

    def read(self):
        msgs = []
        for path in glob.glob(self.log_dir + "/*/*.jsonl"):  # top level only: skips subagent logs
            for line in open(path, errors="ignore"):
                try:
                    rec = json.loads(line)
                except json.JSONDecodeError:  # skip a broken line (e.g. a session still being written)
                    continue
                if rec.get("type") != "user" or rec.get("isMeta") or "timestamp" not in rec:
                    continue
                utc = datetime.fromisoformat(rec["timestamp"].replace("Z", "+00:00")).replace(tzinfo=None)
                local = utc + timedelta(hours=next(h for d, h in self.offsets if utc.date() < d))
                c = rec.get("message", {}).get("content")
                texts = [c] if isinstance(c, str) else [b.get("text", "") for b in c or [] if b.get("type") == "text"]
                msgs += [Message(local, os.path.basename(path)[:8], t.strip(), len(t.split())) for t in texts if t.strip()]
        return sorted(msgs, key=lambda m: m.local_time)


class RuleMatcher:
    """Base: ordered word lists from config.rules[KEY]["lists"]. Whole-word, case-insensitive. The first match wins."""
    KEY = ""
    def __init__(self, config):
        self.config = config
        self.rules = [(label, self.pattern(words)) for label, words in config.rules[self.KEY]["lists"]]

    @staticmethod
    def pattern(words):  # one regex for a word list; hyphenated words (I-9) match as one word
        return re.compile(r"(?<![\w-])(" + "|".join(words) + r")(?![\w-])", re.I)

    def matches(self, text):  # all (list number 1..n, label, matched word), in rule order
        return [(i, label, r.group(1)) for i, (label, p) in enumerate(self.rules, 1) if (r := p.search(text))]

    def inherit(self, msgs, field, default):
        """Fill an empty field from the previous message if it is <= gap before and not default; else use default."""
        prev = None
        for m in (m for m in msgs if m.step != 0):  # excluded messages do not take part
            if not getattr(m, field):
                ok = prev and m.local_time - prev.local_time <= self.config.gap and getattr(prev, field) != default
                setattr(m, field, getattr(prev, field) if ok else default)
            prev = m


class DomainRule(RuleMatcher):
    """Sets the domain of each message. Steps are checked in order; the first step that applies sets m.step.
    0 = excluded (summary, paste, ...); 1-7 = word list number in rules.json "domain";
    8 = no word matched, so the domain is copied from the previous message (<= gap); 9 = Unclassified."""
    KEY = "domain"
    def exclude_reason(self, m):  # step 0
        t, unit = m.text, self.config.rules["unit"]
        return ("study period" if m.local_time.date() >= self.config.exclude_from else
                "summary" if t.startswith("This session is being continued") else
                "interruption" if t.startswith("[Request interrupted") else
                "command" if t.startswith(("/", "<")) else
                "paste" if m.words > unit["paste_words"] or re.match(rf"({'|'.join(unit['greeting'])}) \w", t) else None)

    def apply(self, msgs):
        group = {d: g for g, ds in self.config.rules["domain"]["groups"].items() for d in ds}
        for m in msgs:
            if reason := self.exclude_reason(m):
                m.domain, m.step, m.matched = "Excluded", 0, reason
            elif hits := self.matches(m.text):
                m.step, m.domain, m.matched = hits[0]
            m.all_domains = "|".join(h[1] for h in hits) if m.step != 0 else ""
        self.inherit(msgs, "domain", "Unclassified")
        for m in msgs:
            m.step = m.step if m.step != -1 else 8 if m.domain != "Unclassified" else 9
            m.group = group.get(m.domain, m.domain)
        return msgs


class OutputTypeRule(RuleMatcher):
    """Visual or Text from words; else the previous message's type; else Text."""
    KEY = "output_type"
    def apply(self, msgs):
        for m in msgs:
            if m.step != 0 and (hits := self.matches(m.text)):
                m.output_type = hits[0][1]
        self.inherit(msgs, "output_type", "Text")
        return msgs


class ToneRule(RuleMatcher):
    """Mixed if Dissatisfied and Satisfied both match; else the matching list; else Neutral. Interruptions = Interrupted."""
    KEY = "tone"
    def apply(self, msgs):
        for m in msgs:
            hits = self.matches(m.text) if m.step != 0 else []
            m.tone = ("Interrupted" if m.matched == "interruption" else "" if m.step == 0 else
                      "Mixed" if len(hits) == 2 else hits[0][1] if hits else "Neutral")
            m.tone_matched = "|".join(h[2] for h in hits)
            m.dissatisfied = m.tone in self.config.rules["tone"]["dissatisfied"]
        return msgs


def assign_sessions(msgs, config):
    """Session = included messages with no gap > gap_minutes. Kept if it has >= min_messages messages."""
    included = [m for m in msgs if m.step != 0]
    for prev, m in zip([None] + included, included):
        m.session_no = 1 if prev is None else prev.session_no + (m.local_time - prev.local_time > config.gap)
    sizes = Counter(m.session_no for m in included)
    for m in included:
        m.session_kept = sizes[m.session_no] >= config.rules["session"]["min_messages"]


def run(out="data/messages.csv"):
    config = Config()
    msgs = Reader(config).read()
    for rule in (DomainRule, OutputTypeRule, ToneRule):
        msgs = rule(config).apply(msgs)
    assign_sessions(msgs, config)
    os.makedirs(os.path.dirname(out), exist_ok=True)
    with open(out, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(asdict(msgs[0])))
        writer.writeheader()
        writer.writerows(asdict(m) for m in msgs)
    print(f"Wrote {len(msgs)} rows to {out}")


if __name__ == "__main__":
    run()
