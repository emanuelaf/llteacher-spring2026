source("prep.R")

# Change in self-rating (Post - Pre) by starting-score group, pooling both
# widgets and Assignments 3-6. One row per student x assignment x widget.
# Starting scores are grouped because single-score cells are small
# (0, 1, 9 have n < 30); the 0-2 / 3-4 / 5 split matches the logistic model.
gains_by_start <- widget_by_problem %>%
  filter(homework_title %in% ASSIGNMENTS_WITH_WIDGET_DATA) %>%
  select(student_id, homework_title, w1_pre, w1_change, w2_pre, w2_change) %>%
  pivot_longer(
    c(w1_pre, w1_change, w2_pre, w2_change),
    names_to = c("widget", ".value"),
    names_pattern = "w(\\d)_(pre|change)"
  ) %>%
  filter(!is.na(pre), !is.na(change)) %>%
  mutate(start_group = case_when(
    pre <= 2 ~ "0-2",
    pre <= 4 ~ "3-4",
    pre == 5 ~ "5",
    pre <= 7 ~ "6-7",
    TRUE     ~ "8-10"
  ),
  start_group = factor(start_group, levels = c("0-2", "3-4", "5", "6-7", "8-10")))

group_summary <- gains_by_start %>%
  group_by(start_group) %>%
  summarise(n = n(),
            mean_change = mean(change),
            pct_declined = 100 * mean(change < 0),
            .groups = "drop") %>%
  mutate(x_label = sprintf("Start %s\n(n = %d)", start_group, n),
         pct_label = sprintf("%.0f%% declined", pct_declined))

print(as.data.frame(group_summary))

gains_by_start <- gains_by_start %>%
  left_join(group_summary %>% select(start_group, x_label), by = "start_group") %>%
  mutate(x_label = factor(x_label, levels = group_summary$x_label))

group_summary <- group_summary %>%
  mutate(x_label = factor(x_label, levels = group_summary$x_label))

# Distribution of the pre -> post change within each starting-score group, in
# the same style as widget_diff_dist_by_assignment.png (figure of change by
# assignment). Shown as % of ratings in the group so that groups of very
# different size (n = 95 to 326) are comparable.
change_counts <- gains_by_start %>%
  count(x_label, change) %>%
  group_by(x_label) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  mutate(direction = case_when(change > 0 ~ "Improved",
                               change < 0 ~ "Declined",
                               TRUE       ~ "No change"),
         direction = factor(direction, levels = c("Declined", "No change", "Improved")))

p_gain_by_start <- ggplot(change_counts, aes(change, pct, fill = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_col(width = 1, color = "white", linewidth = 0.2) +
  facet_wrap(~ x_label, nrow = 1) +
  scale_x_continuous(breaks = c(-5, 0, 5, 10)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = c(Declined = "#de2d26",
                               "No change" = "grey",
                               Improved = "#2ca25f")) +
  labs(x = "Change in self-rating (Post \u2212 Pre)",
       y = "Percent of ratings in group", fill = NULL) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "top")

ggsave("figs/gains_by_start_group.png", p_gain_by_start, width = 10, height = 3.4, dpi = 300)
