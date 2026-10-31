# t-test: early and late prompts in a session

## Purpose
This test finds if friction increases as a session continues. Language models can lose track in long conversations (Laban et al., 2025).

## Variables
| Variable | Role | Definition |
|---|---|---|
| Dissatisfied reply | Dependent variable | 100 if the next user message in the same session is Dissatisfied or Mixed. 0 if not. The mean is the % of prompts with a dissatisfied reply. |
| Session stage | Independent variable | Early: the first half of the prompts in a session. Late: the second half. |

## Table 1. Descriptive statistics
| Session stage | *n* | *M* | *SD* |
|---|---|---|---|
| Early | 135 | 3.7 | 19.0 |
| Late | 149 | 14.1 | 34.9 |

## Table 2. Welch's t-test
| *t* | *df* | *p* | *d* |
|---|---|---|---|
| 3.16 | 232.8 | .002 | 0.36 |

## Results
Late prompts had more dissatisfied replies than early prompts (14.1% and 3.7%). The difference was significant (Table 2).

## Interpretation
- **Difference:** real (*p* = .002); small effect (*d* = 0.36).

Friction increases as a session continues.

Real: *p* < .05. Small, medium, and large: Cohen (1988).

## Limitation
- The relation can go in the other direction. A session can become long because the agent failed early.
- Prompts in the same session are not independent.
- The dependent variable has only two values (0 and 100). Thus, the test gives an approximate result.

## References
Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Erlbaum.

Laban, P., et al. (2025). LLMs get lost in multi-turn conversation. arXiv:2505.06120.

