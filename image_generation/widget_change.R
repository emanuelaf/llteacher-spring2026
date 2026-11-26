source("prep.R")

# Widget self-ratings before/after, per assignment, with per-student trajectories
widget_long <- widget_by_problem %>%
  select(student_id, homework_title, w1_pre, w1_post, w2_pre, w2_post) %>%
  filter(homework_title != "Assignment 1" & homework_title != "Assignment 2") %>%
  pivot_longer(c(w1_pre, w1_post, w2_pre, w2_post),
               names_to = c("widget", "phase"),
               names_pattern = "w(\\d)_(pre|post)",
               values_to = "value") %>%
  mutate(
    phase  = factor(str_to_title(phase), levels = c("Pre", "Post")),
    widget = paste("Widget", widget),
    homework_title = reorder(homework_title,
                             as.integer(gsub("\\D", "", homework_title))),
    line_id = interaction(student_id, homework_title, widget)
  )

p_widget_box <- ggplot(widget_long, aes(x = phase, y = value)) +
  geom_line(aes(group = line_id),
            color = "blue", alpha = 0.22, linewidth = 0.3) +
  geom_boxplot(aes(group = phase),
               width = 0.35, fill = "white",
               outlier.size = 1, linewidth = 0.5) +
  facet_grid(widget ~ homework_title) +
  scale_y_continuous(limits = c(1, 10), breaks = seq(2, 10, 2)) +
  labs(x = NULL, y = "Self-rating (1–10)") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))
ggsave("figs/widget_diff_by_assignment.png", p_widget_box, width = 9, height = 6, dpi = 300)

# Distribution of the pre -> post change, per widget/assignment
change_long <- widget_by_problem %>%
  filter(homework_title != "Assignment 1" & homework_title != "Assignment 2") %>%
  pivot_longer(c(w1_change, w2_change),
               names_to = "widget", values_to = "change") %>%
  mutate(
    widget         = recode(widget, w1_change = "Widget 1", w2_change = "Widget 2"),
    homework_title = reorder(homework_title,
                             as.integer(gsub("\\D", "", homework_title))),
    direction = case_when(change > 0 ~ "Improved",
                          change < 0 ~ "Declined",
                          TRUE       ~ "No change"),
    direction = factor(direction, levels = c("Declined", "No change", "Improved"))
  )

p_change_hist <- ggplot(change_long, aes(change, fill = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_histogram(binwidth = 1, center = 0, color = "white", linewidth = 0.2) +
  facet_grid(widget ~ homework_title) +
  scale_x_continuous(breaks = seq(-10, 10, 2)) +
  scale_fill_manual(values = c(Declined = "#de2d26",
                               "No change" = "grey",
                               Improved = "#2ca25f")) +
  labs(x = "Change in self-rating (Post − Pre)",
       y = "Number of students", fill = NULL) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "top")
ggsave("figs/widget_diff_dist_by_assignment.png", p_change_hist, width = 9, height = 6, dpi = 300)
