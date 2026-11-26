source("prep.R")

# Likert distribution across all 13 survey items, grouped into the 3
# dimensions of student experience these questions were designed around.
question_dimension <- c(
  Q1  = "AI and Stat attitude",
  Q2  = "Learning outcomes",
  Q3  = "Learning outcomes",
  Q4  = "Learning outcomes",
  Q5  = "Learning outcomes",
  Q6  = "Satisfaction",
  Q7  = "Learning outcomes",
  Q8  = "Satisfaction",
  Q9  = "Satisfaction",
  Q10 = "AI and Stat attitude",
  Q11 = "AI and Stat attitude",
  Q12 = "AI and Stat attitude",
  Q13 = "AI and Stat attitude"
)
# Within each dimension, order questions from most to least agreement
# (share of respondents who answered Agree or Strongly Agree).
question_agreement <- data_survey_responses %>%
  select(Q1:Q13) %>%
  pivot_longer(everything(), names_to = "question_id", values_to = "response") %>%
  filter(!is.na(response), response != "NA", response != "") %>%
  mutate(response = factor(response, levels = likert_levels)) %>%
  filter(!is.na(response)) %>%
  group_by(question_id) %>%
  summarise(agreement_pct = mean(response %in% c("Agree", "Strongly Agree")), .groups = "drop") %>%
  mutate(dimension = factor(question_dimension[question_id],
                            levels = c("Learning outcomes", "Satisfaction",
                                       "AI and Stat attitude"))) %>%
  arrange(dimension, desc(agreement_pct))

question_order <- question_agreement$question_id

survey_long <- data_survey_responses %>%
  select(Q1:Q13) %>%
  pivot_longer(everything(), names_to = "question_id", values_to = "response") %>%
  filter(!is.na(response), response != "NA", response != "") %>%
  mutate(dimension   = factor(question_dimension[question_id],
                              levels = c("Learning outcomes", "Satisfaction",
                                         "AI and Stat attitude")),
         response    = factor(response, levels = likert_levels),
         question_id = factor(question_short_labels[question_id],
                              levels = rev(question_short_labels[question_order]))) %>%
  filter(!is.na(response))

# Diverging layout: Neutral is centered on 0% (split evenly across the
# line), Agree/Strongly Agree stack out to the left, Disagree/Strongly
# Disagree stack out to the right - segment boundaries are computed
# explicitly (rather than relying on ggplot's stacking order for signed
# values) so the adjacency to zero is guaranteed correct.
diverging_wide <- survey_long %>%
  count(dimension, question_id, response) %>%
  group_by(dimension, question_id) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  select(dimension, question_id, response, pct) %>%
  pivot_wider(names_from = response, values_from = pct, values_fill = 0) %>%
  mutate(
    neutral_half            = Neutral / 2,
    agree_xmax              = -neutral_half,
    agree_xmin              = agree_xmax - Agree,
    strongly_agree_xmax     = agree_xmin,
    strongly_agree_xmin     = strongly_agree_xmax - `Strongly Agree`,
    disagree_xmin           = neutral_half,
    disagree_xmax           = disagree_xmin + Disagree,
    strongly_disagree_xmin  = disagree_xmax,
    strongly_disagree_xmax  = strongly_disagree_xmin + `Strongly Disagree`
  )

diverging_long <- bind_rows(
  diverging_wide %>% transmute(dimension, question_id, response = "Strongly Agree",
                                xmin = strongly_agree_xmin, xmax = strongly_agree_xmax),
  diverging_wide %>% transmute(dimension, question_id, response = "Agree",
                                xmin = agree_xmin, xmax = agree_xmax),
  diverging_wide %>% transmute(dimension, question_id, response = "Neutral",
                                xmin = -neutral_half, xmax = neutral_half),
  diverging_wide %>% transmute(dimension, question_id, response = "Disagree",
                                xmin = disagree_xmin, xmax = disagree_xmax),
  diverging_wide %>% transmute(dimension, question_id, response = "Strongly Disagree",
                                xmin = strongly_disagree_xmin, xmax = strongly_disagree_xmax)
) %>%
  mutate(response = factor(response, levels = likert_levels),
         y_num    = as.numeric(question_id))

y_breaks <- seq_along(levels(survey_long$question_id))
y_labels <- levels(survey_long$question_id)

p_survey <- ggplot(diverging_long) +
  geom_rect(aes(xmin = xmin, xmax = xmax,
                ymin = y_num - 0.4, ymax = y_num + 0.4, fill = response)) +
  geom_vline(xintercept = 0, color = "grey30", linewidth = 0.3) +
  facet_grid(dimension ~ ., scales = "free_y", space = "free_y") +
  scale_y_continuous(breaks = y_breaks, labels = y_labels) +
  scale_x_continuous(labels = function(x) scales::percent(abs(x))) +
  scale_fill_manual(values = c("Strongly Disagree" = "#b2182b",
                               "Disagree"          = "#ef8a62",
                               "Neutral"           = "#bdbdbd",
                               "Agree"             = "#67a9cf",
                               "Strongly Agree"    = "#2166ac"),
                    na.translate = FALSE,
                    guide = guide_legend(reverse = TRUE)) +
  labs(x = "Percent of respondents", y = NULL, fill = NULL) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        legend.margin = margin(b = 8),
        strip.text.y = element_text(face = "bold"),
        plot.margin = margin(t = 10, r = 12, b = 5, l = 5))
ggsave("figs/survey_likert.png", p_survey, width = 8, height = 6.3, dpi = 300)

# Priority items (Q14/Q15 free-text, tallied)
q1415_labels <- c(
  Q14 = "What aspects did you like the most about LLteacher?",
  Q15 = "What aspects did you like the least about LLteacher?"
)

q1415_long <- data_survey_responses %>%
  select(Q14, Q15) %>%
  pivot_longer(everything(), names_to = "question_id", values_to = "response") %>%
  filter(response != "NA", response != "") %>%
  separate_rows(response, sep = ";") %>%
  mutate(response = str_squish(response)) %>%
  filter(response != "") %>%
  group_by(question_id) %>%
  mutate(n_resp = n_distinct(row_number())) %>%
  ungroup() %>%
  count(question_id, response, name = "n") %>%
  filter(n > 1) %>%
  mutate(pct = n / 121 * 100,
         question_id = factor(q1415_labels[question_id], levels = q1415_labels))

p_q1415 <- ggplot(q1415_long, aes(x = pct, y = reorder(response, pct), fill = question_id)) +
  geom_col() +
  facet_wrap(~ question_id, ncol = 1, scales = "free_y",
             labeller = labeller(question_id = label_wrap_gen(50))) +
  scale_fill_manual(values = setNames(c("#2ca25f", "#de2d26"), q1415_labels),
                     guide = "none") +
  labs(x = "Percent of respondents (%)", y = NULL) +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))
ggsave("figs/priority_q14_q15.png", p_q1415, width = 8, height = 6, dpi = 300)
