# prep.R - loads the data files and builds the tables used by the figure and
# table scripts. Sourced at the top of each script; run scripts from the
# submission/ folder (see README.md).

library(tidyverse)
library(lme4)

# make sure output folders exist
dir.create("tables", showWarnings = FALSE, recursive = TRUE)
dir.create("figs",   showWarnings = FALSE, recursive = TRUE)

# ---- load -----------------------------------------------------------------
# LLteacher data: one row per student x assignment (widget self-ratings and
# message activity).
data_llteacher        <- read.csv("data/llteacher_data.csv", stringsAsFactors = FALSE)
# Survey: one row per respondent (Q1-Q17); linked to the above by student_id.
data_survey_responses <- read.csv("data/survey_data.csv", stringsAsFactors = FALSE)

# ---- constants ------------------------------------------------------------
# Assignments 3-6 have native widget pre/post values; Assignments 1-2 were
# extracted by hand (see README.md).
ASSIGNMENTS_WITH_WIDGET_DATA <- c("Assignment 3", "Assignment 4", "Assignment 5", "Assignment 6")

question_short_labels <- c(
  "Q1"  = "Q1: increased interest in Statistics",
  "Q2"  = "Q2: helped understand course concepts",
  "Q3"  = "Q3: improved ability to interpret analyses",
  "Q4"  = "Q4: improved learning in this class",
  "Q5"  = "Q5: made me think critically about AI",
  "Q6"  = "Q6: LLteacher confused me",
  "Q7"  = "Q7: found LLteacher useful",
  "Q8"  = "Q8: would like to use LLteacher again",
  "Q9"  = "Q9: enjoyed using LLteacher",
  "Q10" = "Q10: increased interest in AI tools",
  "Q11" = "Q11: now use AI tools more",
  "Q12" = "Q12: more confident using AI tools",
  "Q13" = "Q13: want more classes to use AI"
)

# strongly disagree - Strongly agree: 1-5
likert_levels <- c("Strongly Disagree", "Disagree", "Neutral",
                   "Agree", "Strongly Agree")

# ---- widget values: one row per student x assignment ----------------------
# Only student x assignment pairs with at least one widget rating.
widget_by_problem <- data_llteacher %>%
  filter(!is.na(widget1_pre) | !is.na(widget2_pre) |
         !is.na(widget1_post) | !is.na(widget2_post)) %>%
  transmute(student_id, homework_title,
            w1_pre  = widget1_pre,  w2_pre  = widget2_pre,
            w1_post = widget1_post, w2_post = widget2_post,
            w1_change = widget1_post - widget1_pre,
            w2_change = widget2_post - widget2_pre)

# ---- message activity: one row per student x assignment -------------------
# Only student x assignment pairs with at least one student message on the
# two graded problems (n_messages = 0 rows have widget values only).
summary_by_assignment <- data_llteacher %>%
  filter(n_messages > 0) %>%
  transmute(student_id, homework_title, n_messages,
            avg_chars_per_message = avg_message_length)
