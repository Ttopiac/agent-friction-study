# t-test: visual and text prompts

## Purpose
This test finds if prompts about visual output get more dissatisfied replies than prompts about text.

## Variables
| Variable | Role | Definition |
|---|---|---|
| Dissatisfied reply | Dependent variable | 100 if the next user message in the same session is Dissatisfied or Mixed. 0 if not. The mean is the % of prompts with a dissatisfied reply. |
| Output type | Independent variable | Visual: the prompt is about visual output (HTML, PPTX, PDF, slides, figures, layout). Text: all other prompts. |

## Table 1. Descriptive statistics
| Output type | *n* | *M* | *SD* |
|---|---|---|---|
| Text | 145 | 4.1 | 20.0 |
| Visual | 140 | 14.3 | 35.1 |

## Table 2. Welch's t-test
| *t* | *df* | *p* | *d* |
|---|---|---|---|
| 2.98 | 218.9 | .003 | 0.36 |

## Results
Visual prompts had more dissatisfied replies than text prompts (14.3% and 4.1%). The difference was significant (Table 2).

## Interpretation
- **Difference:** real (*p* = .003); small effect (*d* = 0.36).

Visual prompts get more dissatisfied replies than text prompts.

Real: *p* < .05. Small, medium, and large: Cohen (1988).

## Limitation
- 25 pastes have no output type. The test does not include them.
- Prompts in the same session are not independent.
- The dependent variable has only two values (0 and 100). Thus, the test gives an approximate result.

## References
Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Erlbaum.

