source("prep.R")

# Widget gains (post - pre) as a function of starting value. One row per
# student x assignment x widget.
widget_gains_long <- widget_by_problem %>%
  filter(homework_title %in% ASSIGNMENTS_WITH_WIDGET_DATA) %>%
  select(student_id, homework_title, w1_pre, w1_change, w2_pre, w2_change) %>%
  pivot_longer(
    c(w1_pre, w1_change, w2_pre, w2_change),
    names_to = c("widget", ".value"),
    names_pattern = "w(\\d)_(pre|change)"
  ) %>%
  mutate(widget = paste0("Widget ", widget)) %>%
  filter(!is.na(pre), !is.na(change)) %>%
  mutate(improvement = case_when(
    change <= 0 ~ "No improvement",
    change <= 2 ~ "Small improvement",
    TRUE        ~ "Big improvement"
  ),
  improvement = factor(improvement,
                       levels = c("No improvement", "Small improvement", "Big improvement")))

# Ceiling-effect check: the scale tops out at 10, so a high starting value
# mechanically caps how much a student can still gain - which alone would
# produce a negative pre -> change relationship, regardless of any real
# effect. Restrict to the region where that ceiling can't bind for anyone
# included: starting value <= 5 (so a full 5-point gain is always
# achievable) and observed gain <= 5 (so we exclude gains that were only
# reachable by students who started especially low).
widget_gains_no_ceiling <- widget_gains_long %>%
  filter(pre <= 5, change <= 5) %>%
  filter(change>=0)

# Logistic version: instead of the size of the gain, model whether a
# student lands in a small/no-improvement bucket (change <= 2) or a big-
# improvement bucket (change >= 3). Starting value is grouped into
# "0-2" / "3-4" / "5" (not a 6-level factor(pre)) - checked the pre x
# outcome and pre_group x outcome crosstabs first and only this grouping is
# free of zero cells for this split, so factor(pre) would still hit
# separation.
widget_binary <- widget_gains_no_ceiling %>%
  filter(change <= 2 | change >= 3) %>%
  mutate(high_improvement = change >= 3,
         pre_group = case_when(
           pre <= 2 ~ "0-2",
           pre <= 4 ~ "3-4",
           TRUE     ~ "5"
         ),
         pre_group = factor(pre_group, levels = c("0-2", "3-4", "5")))

# Each student contributes up to 8 rows (4 assignments x 2 widgets), so a
# random intercept per student accounts for the within-student correlation
# that the assignment and widget dummies don't capture.
model_improvement_logistic <- glmer(high_improvement ~ pre_group + homework_title + widget +
                                      (1 | student_id),
                                    data = widget_binary, family = binomial)
summary(model_improvement_logistic)

stars <- function(p) {
  case_when(p < 0.001 ~ "***",
            p < 0.01  ~ "**",
            p < 0.05  ~ "*",
            p < 0.1   ~ "+",
            TRUE      ~ "")
}

# LaTeX table for the logistic model. glm's coefficient table uses
# "Pr(>|z|)" (not "Pr(>|t|)" like lm), so model_row isn't reused as-is.
logistic_term_labels <- c(
  "(Intercept)"                = "Intercept",
  "pre_group3-4"                = "Pre = 3-4",
  "pre_group5"                  = "Pre = 5",
  "homework_titleAssignment 4"  = "Assignment 4",
  "homework_titleAssignment 5"  = "Assignment 5",
  "homework_titleAssignment 6"  = "Assignment 6",
  "widgetWidget 2"              = "Widget 2"
)

logistic_row <- function(model, term) {
  co <- summary(model)$coefficients
  if (!term %in% rownames(co)) return(list(est = NA_real_, se = NA_real_, stars = ""))
  list(est = co[term, "Estimate"], se = co[term, "Std. Error"], stars = stars(co[term, "Pr(>|z|)"]))
}

logistic_table_lines <- c(
  "\\begin{table}",
  "\\centering",
  "\\begin{tabular}{lc}",
  "\\toprule",
  " & Big improvement (change $>$ 2) \\\\",
  "\\midrule"
)

for (term in names(logistic_term_labels)) {
  r <- logistic_row(model_improvement_logistic, term)
  logistic_table_lines <- c(
    logistic_table_lines,
    sprintf("%s & %s \\\\",
            logistic_term_labels[[term]],
            if (is.na(r$est)) "" else sprintf("%.3f%s", r$est, r$stars)),
    sprintf(" & %s \\\\",
            if (is.na(r$se)) "" else sprintf("(%.3f)", r$se))
  )
}

logistic_table_lines <- c(
  logistic_table_lines,
  "\\midrule",
  sprintf("N & %d \\\\", nobs(model_improvement_logistic)),
  sprintf("Students & %d \\\\", ngrps(model_improvement_logistic)[["student_id"]]),
  sprintf("AIC & %.1f \\\\", AIC(model_improvement_logistic)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\caption{Mixed-effects logistic regression with a random intercept per student: probability of big improvement (change $\\geq$ 3) vs. small/no improvement (change $\\leq$ 2), by starting-value group, assignment, and widget type. Ceiling-restricted sample. Standard errors in parentheses. + p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001}",
  "\\label{tab:improvement_logistic}",
  "\\end{table}"
)

cat(paste(logistic_table_lines, collapse = "\n"), "\n")
writeLines(logistic_table_lines, "tables/improvement_logistic_table.tex")