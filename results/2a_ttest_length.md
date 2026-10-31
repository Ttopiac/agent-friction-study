# t-test: short and long prompts

## Purpose
This test finds if prompt length has an effect on the agent's results.

## Variables
| Variable | Role | Definition |
|---|---|---|
| Dissatisfied reply | Dependent variable | 100 if the next user message in the same session is Dissatisfied or Mixed. 0 if not. The mean is the % of prompts with a dissatisfied reply. |
| Prompt length | Independent variable | Long: more than 17 words (the median). Short: 17 words or fewer. |

## Table 1. Descriptive statistics
| Prompt length | *n* | *M* | *SD* |
|---|---|---|---|
| Short | 162 | 8.6 | 28.2 |
| Long | 148 | 8.8 | 28.4 |

## Table 2. Welch's t-test
| *t* | *df* | *p* | *d* |
|---|---|---|---|
| 0.04 | 305.0 | .965 | 0.01 |

## Results
Long prompts had more dissatisfied replies than short prompts (8.8% and 8.6%). The difference was not significant (Table 2).

## Interpretation
- **Difference:** can be chance (*p* = .965); negligible effect (*d* = 0.01).

Prompt length has no effect on the agent's results. Other factors cause the friction.

Real: *p* < .05. Small, medium, and large: Cohen (1988).

## Limitation
- Prompts in the same session are not independent.
- The dependent variable has only two values (0 and 100). Thus, the test gives an approximate result.

## References
Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Erlbaum.

