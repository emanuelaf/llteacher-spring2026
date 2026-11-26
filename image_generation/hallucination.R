source("prep.R")
library(patchwork)

# Exclude one respondent whose Q16/Q17 answers contradict each other
# (Q16 = "yes, more than once" but Q17 = "it did not hallucinate"). Their
# other survey answers are fine, so this only removes them from analyses
# in this file, not from data_survey_responses generally.
data_survey_responses <- data_survey_responses %>%
  filter(student_id != "S012")

# Q16/Q17 hallucination response distributions. Q17 only makes sense for
# students who said Yes to Q16 (it's asking how problematic the
# hallucination was) - the "No" respondents are dropped before counting
# Q17, so that bar only reflects students who actually experienced one.
hallu_long <- bind_rows(
  data_survey_responses %>%
    filter(!is.na(Q16), Q16 != "NA") %>%
    transmute(question_id = "Q16", response = Q16),
  data_survey_responses %>%
    filter(!is.na(Q16), Q16 != "NA", Q16 != "no",
           !is.na(Q17), Q17 != "NA") %>%
    transmute(question_id = "Q17", response = Q17)
) %>%
  mutate(
    # raw Q16/Q17 text comes through lowercase from the CSV (unlike Q1-Q13,
    # which are title-cased when the dataset is built) - match on lowercase here.
    response = recode(response,
                      "no"                                   = "No",
                      "yes, once"                            = "Yes, once",
                      "yes, more than once"                  = "Yes, > once",
                      "yes, but i don't remember how often"   = "Yes, unsure how often",
                      "it did not hallucinate"                = "Did not hallucinate",
                      "not problematic at all"                = "Not problematic",
                      "slightly problematic, it slowed me down, but i figured it out pretty quickly" = "Slightly problematic",
                      "very problematic, it severely hindered my learning" = "Very problematic",
                      "i don't know"                          = "Don't know"),
    response = factor(response, levels = rev(c(
      "No", "Yes, once", "Yes, > once", "Yes, unsure how often",
      "Did not hallucinate", "Not problematic", "Slightly problematic",
      "Very problematic", "Don't know"))),
    question_id = recode(question_id,
                         Q16 = "Q16: Did LLteacher hallucinate?",
                         Q17 = "Q17: How problematic was it? (students who said Yes to Q16)")
  )

# Two separate panels (not facets) placed side by side with a dashed "zoom"
# funnel between them, so it's visually obvious Q17 (right) zooms into the
# Yes-only slice of Q16 (left) - the funnel is a stylized cue, narrow on
# the left (matching where the Yes rows sit in the Q16 panel) and opening
# to the full panel height on the right.
ZOOM_COLOR <- "#4a90d9"

n_q16 <- hallu_long %>% filter(question_id == "Q16: Did LLteacher hallucinate?") %>% nrow()
n_q17 <- hallu_long %>% filter(question_id != "Q16: Did LLteacher hallucinate?") %>% nrow()

# Bars show percentages (each panel sums to 100%) so they match the
# percentages reported in the paper; Q17's denominator is the students who
# said Yes to Q16, not all Q16 respondents.
p_hallu_q16 <- hallu_long %>%
  filter(question_id == "Q16: Did LLteacher hallucinate?") %>%
  ggplot(aes(y = response, x = after_stat(count / sum(count)), fill = response == "No")) +
  geom_bar() +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c(`TRUE` = "grey70", `FALSE` = ZOOM_COLOR), guide = "none") +
  labs(title = paste0("Q16: Did LLteacher hallucinate? (N = ", n_q16, ")"),
       x = "Percent of respondents", y = NULL) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 11))

p_hallu_q17 <- hallu_long %>%
  filter(question_id != "Q16: Did LLteacher hallucinate?") %>%
  ggplot(aes(y = response, x = after_stat(count / sum(count)))) +
  geom_bar(fill = ZOOM_COLOR) +
  scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Q17: How problematic was it?",
       subtitle = paste0("(students who said Yes to Q16, N = ", n_q17, ")"),
       x = "Percent of respondents", y = NULL) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9, color = "grey30"))

# Position the funnel's narrow left opening to match where the Yes rows
# sit within Q16's y-axis: "No" is one category out of n, sitting at the
# top; the Yes rows are the bottom n-1. With ggplot's default discrete-axis
# expansion (add = 0.6 category-widths on each end), the boundary between
# them sits at (n - 0.9) / (n + 0.2) of the way up the panel.
n_q16_cats <- hallu_long %>%
  filter(question_id == "Q16: Did LLteacher hallucinate?") %>%
  pull(response) %>%
  n_distinct()
frac_yes_top <- (n_q16_cats - 0.9) / (n_q16_cats + 0.2)

zoom_funnel <- ggplot() +
  geom_polygon(aes(x = c(0, 1, 1, 0), y = c(0, 0, 1, frac_yes_top)),
               fill = ZOOM_COLOR, alpha = 0.15, color = NA) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), color = ZOOM_COLOR, linetype = "dashed") +
  geom_segment(aes(x = 0, xend = 1, y = frac_yes_top, yend = 1), color = ZOOM_COLOR, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_void()

p_hallu_long <- p_hallu_q16 | zoom_funnel | p_hallu_q17
p_hallu_long <- p_hallu_long + plot_layout(widths = c(5, 0.8, 4))
ggsave("figs/hallucination_q16_q17.png", p_hallu_long, width = 12, height = 5, dpi = 300)