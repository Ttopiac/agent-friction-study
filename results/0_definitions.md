# Definitions

Generated from `config/rules.json` by `friction.py`. Do not edit by hand.
Patterns: `?` = the letter before is optional; `\d+` = a number; `\w*` = any word ending; `(a|b)` = a or b.

## Unit
One message typed by the user. Excluded: session summaries, interruption markers, commands, and pastes (more than 120 words, or starting with: Hello, Hi, Dear, Hey).

## Session
A new session starts after a gap of more than 60 minutes. Sessions with fewer than 3 messages are dropped from session-level tests.

## Domain
The first list with a matching word sets the domain. No match: the domain of the previous message (within the session gap), else Unclassified. Groups: Project X = Admin forms (B), Web page, Slide deck.
- **Admin forms (A):** `register`, `enroll`, `course credit`, `internship course`, `professional course`, `dissertation`, `advisor`
- **Documents & email:** `resume`, `resume_all`, `stories`, `cover letter`, `job description`, `JD`, `apply for`, `job application`, `recruiter`, `interview`
- **Slide deck:** `slides?`, `talk`, `presentation`, `pages? \d+`, `citations?`, `pptx`
- **Web page:** `intro`, `introduction`, `page`, `website`
- **Admin forms (B):** `background check`, `drug`, `screening`, `onboarding`, `I-9`, `W-4`, `HR`, `direct deposit`
- **Research & code:** `spatial`, `DLPFC`, `GCN`, `benchmark`, `experiment`, `nicheformer`, `scGPT`, `re-run`, `run the experiment`
- **Tools & setup:** `install`, `claude`, `git`, `github`, `commit`, `push`, `repo`, `chrome`, `disappear`, `scroll`, `memory`

## Output type
The first list with a matching word sets the type. No match: the type of the previous message (within the session gap), else Text.
- **Visual:** `html`, `pptx`, `pdf`, `png`, `jpg`, `gif`, `slides?`, `pages? \d+`, `figures?`, `images?`, `font`, `colou?r`, `layout`
- **Text:** `txt`, `py`, `md`, `csv`, `tex`, `R`, `json`, `email`, `message`, `resume`, `cover letter`

## Tone
Both lists match: Mixed. One list matches: that tone. No match: Neutral. A dissatisfied message has tone: Dissatisfied, Mixed.
- **Dissatisfied:** `bad`, `wrong`, `unclear`, `broken`, `unprofessional`, `not (good|professional|clear)`, `not as good`, `not a good`, `way too`, `too (small|big|long|short|light|general|many)`, `disappear\w*`, `(didn'?t|did not) (see|change|show|update)`, `you (didn'?t|did not)`, `still (different|broken|not)`, `does not look`, `doesn'?t look`, `not the same`, `looks the same`, `don'?t like`, `do not like`, `what happened`, `hate`, `drives? nuts?`, `oh my god`, `piss\w*`, `already said`, `why (my|all|did|do you)`
- **Satisfied:** `great`, `good job`, `nice`, `thanks?`, `thank you`, `cool`, `excellent`, `very good`, `much better`, `looks better`, `love`

## Reply
Prompt and reply: each prompt (an included message, or a paste, because the agent received that text) is paired with the user's next included message, if it comes within the session gap. The reply's tone group (Dissatisfied, Satisfied, or Neutral) shows how the agent's work on that prompt was received.

## Limits
Word matching misses sarcasm and indirect complaints. Private word lists (names, organizations) are kept local and are not shown here.
