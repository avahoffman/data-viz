library(tidyverse)

df <- tibble(
  item = numeric(),
  activity = character(),
  Team = character(),
  start = character(),
  end = character(),
  font = character()
)

fill = list(
  list(1, "Project Management", "JHU", NA, NA, "bold"),
  list(2, "Recruit / hire", "JHU", "2021-09-15", "2021-12-31", "plain"),
  list(3, "Finalize partner institutions", "JHU", "2021-09-15", "2021-12-31", "plain"),
  list(4, "Manage real-time support network", "JHU", "2022-01-01", "2023-09-14", "plain"),
  list(5, "Core Curriculum and electives", "JHU", NA, NA, "bold"),
  list(6, "Develop Core Curriculum", "JHU", "2021-09-15", "2022-05-14", "plain"),
  list(7, "Learn Core Curriculum", "Teaching", "2022-05-15", "2022-09-14", "plain"),
  list(8, "Support teaching faculty", "JHU", "2022-05-15", "2022-09-14", "plain"),
  list(9, "Co-teach Core Curriculum", "JHU", "2022-09-15", "2023-05-14", "plain"),
  list(10, "Lead teach Core Curriculum", "Teaching", "2022-09-15", "2023-05-14", "plain"),
  list(11, "Pursue accreditation", "JHU", "2023-05-15", "2023-09-14", "plain"),
  list(12, "Develop short elective courses", "Teaching", "2023-05-15", "2023-09-14", "plain"),
  list(13, "Research experiences", "JHU", NA, NA, "bold"),
  list(14, "Identify research questions", "Medical", "2022-01-01", "2022-05-14", "plain")
)

for (i in 1:length(fill)){
  df[i, ] = fill[[i]]
}

acts <- df$activity
face <- df$font[length(df$font):1]

df <- df %>% pivot_longer(c(start, end), "start_or_end") %>%
  mutate(date_ = as.Date(value, "%Y-%m-%d"),
         activity=factor(activity, acts[length(acts):1]))

ggplot(df, aes(date_, activity, color = Team, group = item)) +
  geom_line(size = 10) +
  labs(x="Project year", y=NULL, title="Timeline") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y") +
  theme(axis.text.y = element_text(face = face))
  