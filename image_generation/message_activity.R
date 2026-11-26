source("prep.R")

# Avg characters per message per student, by assignment
p_avgchars_box <- ggplot(summary_by_assignment,
                         aes(reorder(homework_title, as.integer(gsub("\\D", "", homework_title))),
                             avg_chars_per_message)) +
  geom_boxplot(width = 0.5, outlier.size = 1) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 0.8) +
  labs(x = NULL, y = "Avg characters / message") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())
ggsave("figs/avgchars_box_by_assignment.png", p_avgchars_box, width = 7, height = 5, dpi = 300)

# Number of messages per student, by assignment
p_interactions <- ggplot(summary_by_assignment,
                         aes(reorder(homework_title, as.integer(gsub("\\D", "", homework_title))),
                             n_messages)) +
  geom_boxplot(width = 0.5, outlier.size = 1) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 0.8) +
  labs(x = NULL, y = "Number of messages") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank())
ggsave("figs/interactions_by_assignment.png", p_interactions, width = 7, height = 5, dpi = 300)

# Cluster students into 4 engagement patterns (message length x number of
# messages) and show the clusters on a scatterplot of avg chars vs. number of
# messages, by assignment.
set.seed(1)
cluster_data <- summary_by_assignment %>%
  filter(!is.na(n_messages), !is.na(avg_chars_per_message))

cluster_fit <- kmeans(
  scale(cluster_data[, c("n_messages", "avg_chars_per_message")]),
  centers = 4, nstart = 25
)
cluster_data$cluster <- as.character(cluster_fit$cluster)

# Label each cluster by how its centroid ranks against the other clusters
# on each axis (rank 1 = lowest, 4 = highest), rather than fixed thresholds -
# so the labels reflect whatever the data actually produced.
cluster_centroids <- cluster_data %>%
  group_by(cluster) %>%
  summarise(mean_n_messages = mean(n_messages),
            mean_avg_chars  = mean(avg_chars_per_message),
            .groups = "drop") %>%
  mutate(rank_n_messages = rank(mean_n_messages),
         rank_avg_chars  = rank(mean_avg_chars),
         rank_diff       = rank_avg_chars - rank_n_messages,
         rank_sum        = rank_avg_chars + rank_n_messages)

long_short  <- cluster_centroids$cluster[which.max(cluster_centroids$rank_diff)]
short_long  <- cluster_centroids$cluster[which.min(cluster_centroids$rank_diff)]
remaining   <- cluster_centroids %>% filter(!cluster %in% c(long_short, short_long))
short_short <- remaining$cluster[which.min(remaining$rank_sum)]
medium      <- setdiff(remaining$cluster, short_short)

cluster_labels <- setNames(
  c("Long msg / short convo", "Short msg / long convo",
    "Short msg / short convo", "Medium"),
  c(long_short, short_long, short_short, medium)
)

cluster_data <- cluster_data %>%
  mutate(cluster_label = factor(cluster_labels[cluster],
                                 levels = c("Long msg / short convo",
                                            "Short msg / short convo",
                                            "Short msg / long convo",
                                            "Medium")))

p_avgchars_vs_nmsg_clusters <- cluster_data %>%
  mutate(homework_title = reorder(homework_title,
                                  as.integer(gsub("\\D", "", homework_title)))) %>%
  ggplot(aes(n_messages, avg_chars_per_message, color = cluster_label)) +
  geom_point(alpha = 0.6, size = 1.2) +
  facet_wrap(~ homework_title, ncol = 3, scales = "free") +
  labs(x = "Number of messages", y = "Avg characters / message",
       color = "Engagement cluster") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "none")
ggsave("figs/avgchars_vs_nmsg_clusters.png", p_avgchars_vs_nmsg_clusters, width = 9, height = 6, dpi = 300)

