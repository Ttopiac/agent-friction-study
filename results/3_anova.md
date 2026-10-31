# Two-way ANOVA: output type and session stage

## Purpose
Tests 2b and 2c found more friction for visual prompts and for late prompts.
This ANOVA tests each effect with the other effect held constant. It also tests if the effect of session stage changes with the output type (the interaction).

## Variables
| Variable | Role | Definition |
|---|---|---|
| Dissatisfied reply | Dependent variable | 100 if the next user message in the same session is Dissatisfied or Mixed. 0 if not. The mean is the % of prompts with a dissatisfied reply. |
| Output type | Independent variable | Visual: the prompt is about visual output (HTML, PPTX, PDF, slides, figures, layout). Text: all other prompts. |
| Session stage | Independent variable | Early: the first half of the prompts in a session. Late: the second half. |

## Table 1. Descriptive statistics
| Output type, Session stage | *n* | *M* | *SD* |
|---|---|---|---|
| Text, Early | 73 | 0.0 | 0.0 |
| Text, Late | 71 | 8.5 | 28.0 |
| Visual, Early | 62 | 8.1 | 27.5 |
| Visual, Late | 78 | 19.2 | 39.7 |

## Table 2. Two-way ANOVA
| Source | *df* | *F* | *p* | η²p |
|---|---|---|---|---|
| Output type | 1 | 8.03 | .005 | .03 |
| Session stage | 1 | 8.51 | .004 | .03 |
| Output type × Session stage | 1 | 0.16 | .686 | .00 |
| Residual | 280 |  |  |  |

*Note.* Type II sums of squares. η²p = partial eta-squared.

## Results
The main effect of output type was significant. The main effect of session stage was significant. The interaction was not significant (Table 2).

## Interpretation
- **Output type:** real (*p* = .005); small effect (η²p = .03, 2.8% of the differences).
- **Session stage:** real (*p* = .004); small effect (η²p = .03, 2.9% of the differences).
- **Interaction:** can be chance (*p* = .686); negligible effect (η²p = .00, 0.059% of the differences).

Output type and session stage are two separate factors in friction. Their effects add. The highest rate is for visual prompts late in a session (19.2%). The lowest rate is for text prompts early in a session (0.0%).

Real: *p* < .05. Small, medium, and large: Cohen (1988).

## Limitation
- The groups are small (62 to 78 prompts).
- The group Text, Early has no dissatisfied replies (*SD* = 0). Thus, the groups do not have equal variance.
- Prompts in the same session are not independent.
- The dependent variable has only two values (0 and 100). Thus, the test gives an approximate result.

## References
Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Erlbaum.

