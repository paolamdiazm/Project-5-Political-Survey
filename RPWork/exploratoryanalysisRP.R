
library(haven)
library(dplyr)
library(ggplot2)

# Load the prepared datasets
path <- "Project-5-Political-Survey/"
survey <- readRDS(paste0(path, "mexico_survey_1979_original.rds"))
survey_english <- readRDS(paste0(path, "mexico_survey_1979_english.rds"))

# Load the English dictionaries
variable_labels_en <- read.csv(paste0(path, "mexico_variable_labels_english.csv"))
response_labels_en <- read.csv(paste0(path, "mexico_response_labels_english.csv"))

# Verify dimensions
dim(survey)
dim(survey_english)

# Inspect important variables
summary(survey[, c("v6", "v36", "v70", "v111")])

# Variables of interest
variables <- c("v1", "v6", "v17", "v36", "v62",
               "v68", "v69", "v70", "v71", "v72",
               "v73", "v74", "v92a", "v111")

# Calculate missing values
missing_summary <- data.frame(
  variable = variables,
  missing = colSums(is.na(survey[variables])),
  total = nrow(survey)
) %>%
  mutate(missing_percent = round(100 * missing / total, 2))

print(missing_summary)

# Examine the distribution of important variables
for (var in variables) {
  cat("\nVariable:", var, "\n")
  print(table(survey[[var]], useNA = "ifany"))
}

# Prepare education groups
survey_graph <- survey %>%
  mutate(
    age = as.numeric(v6),
    education = as.numeric(v111),
    education_group = cut(
      education,
      breaks = c(-1, 6, 12, 21),
      labels = c("0-6 years", "7-12 years", "13+ years")
    )
  )

# Check the number of respondents in each group
table(survey_graph$education_group, useNA = "ifany")

# Graph 1: Education vs. government censorship
graph1_data <- survey_graph %>%
  filter(!is.na(education_group), between(as.numeric(v70), 1, 10)) %>%
  group_by(education_group) %>%
  summarise(
    mean_approval = mean(as.numeric(v70)),
    n = n(),
    .groups = "drop"
  )

print(graph1_data)

graph1 <- ggplot(graph1_data, aes(education_group, mean_approval)) +
  geom_point(size = 4, color = "steelblue") +
  geom_line(aes(group = 1), color = "steelblue") +
  geom_text(aes(label = paste0("n = ", n)), vjust = -1, size = 3.5) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(
    title = "Approval of Government Censorship by Education",
    subtitle = "Guadalajara Political Survey, 1979",
    x = "Years of Schooling",
    y = "Mean Approval (1 = Strongly Disapprove, 10 = Strongly Approve)"
  ) +
  theme_minimal()

print(graph1)

# Graph 2: Education vs. government trust
graph2_data <- survey_graph %>%
  filter(!is.na(education_group), as.numeric(v36) %in% 1:4) %>%
  mutate(
    trust = factor(
      as.numeric(v36),
      levels = 1:4,
      labels = c("Almost always", "Most of the time",
                 "Some of the time", "Never")
    )
  )

# Check counts
table(graph2_data$education_group, graph2_data$trust)

graph2 <- ggplot(graph2_data, aes(education_group, fill = trust)) +
  geom_bar(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Trust in the Federal Government by Education",
    subtitle = "Guadalajara Political Survey, 1979",
    x = "Years of Schooling",
    y = "Percentage of Respondents",
    fill = "Government Trust"
  ) +
  theme_minimal()

print(graph2)

# Graph 3: Age vs. banning demonstrations
graph3_data <- survey_graph %>%
  filter(!is.na(v6), between(as.numeric(v68), 1, 10)) %>%
  mutate(
    age = as.numeric(v6),
    approval = as.numeric(v68)
  )

# Check sample size
nrow(graph3_data)

graph3 <- ggplot(graph3_data, aes(age, approval)) +
  geom_jitter(width = 0.4, height = 0.15,
              alpha = 0.35, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  scale_y_continuous(breaks = 1:10) +
  coord_cartesian(ylim = c(1, 10)) +
  labs(
    title = "Approval of Banning Demonstrations by Age",
    subtitle = "Guadalajara Political Survey, 1979",
    x = "Age (Years)",
    y = "Approval (1 = Strongly Disapprove, 10 = Strongly Approve)"
  ) +
  theme_minimal()

print(graph3)

# Graph 4: Education vs. demonstration rights
graph4_data <- survey_graph %>%
  filter(!is.na(education_group), between(as.numeric(v72), 1, 10)) %>%
  group_by(education_group) %>%
  summarise(
    mean_approval = mean(as.numeric(v72)),
    n = n(),
    .groups = "drop"
  )

print(graph4_data)

graph4 <- ggplot(graph4_data, aes(education_group, mean_approval)) +
  geom_point(size = 4, color = "steelblue") +
  geom_line(aes(group = 1), color = "steelblue") +
  geom_text(aes(label = paste0("n = ", n)), vjust = -1, size = 3.5) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(
    title = "Support for Demonstration Rights by Education",
    subtitle = "Guadalajara Political Survey, 1979",
    x = "Years of Schooling",
    y = "Mean Approval (1 = Strongly Disapprove, 10 = Strongly Approve)"
  ) +
  theme_minimal()

print(graph4)

# Compare usable sample sizes
sample_sizes <- data.frame(
  graph = c(
    "Education vs. Censorship",
    "Education vs. Government Trust",
    "Age vs. Banning Demonstrations",
    "Education vs. Demonstration Rights"
  ),
  n = c(
    sum(graph1_data$n),
    nrow(graph2_data),
    nrow(graph3_data),
    sum(graph4_data$n)
  )
)

print(sample_sizes)

#Graph 5: Eduation vs Political Attitudes
graph5 <- ggplot(
  graph5_data,
  aes(x = education_group, y = mean_approval,
      color = attitude, group = attitude)
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper, fill = attitude),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  geom_text(
    aes(label = round(mean_approval, 2)),
    vjust = -1.5,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Government Censorship" = "darkred",
      "Demonstration Rights" = "steelblue"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Government Censorship" = "darkred",
      "Demonstration Rights" = "steelblue"
    ),
    guide = "none"
  ) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(
    title = "Education and Tolerance of Political Dissent",
    subtitle = "Guadalajara Political Survey, 1979",
    x = "Years of Schooling",
    y = "Mean Approval (1-10)",
    color = "Political Attitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(graph5)

# Save all five graphs
output_dir <- "Project-5-Political-Survey/RPWork/figures"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(output_dir, "graph1_censorship.png"),
       plot = graph1, width = 10, height = 6, dpi = 300)

ggsave(file.path(output_dir, "graph2_trust.png"),
       plot = graph2, width = 10, height = 6, dpi = 300)

ggsave(file.path(output_dir, "graph3_age_demonstrations.png"),
       plot = graph3, width = 10, height = 6, dpi = 300)

ggsave(file.path(output_dir, "graph4_demonstration_rights.png"),
       plot = graph4, width = 10, height = 6, dpi = 300)

ggsave(file.path(output_dir, "graph5_political_dissent.png"),
       plot = graph5, width = 10, height = 6, dpi = 300)
