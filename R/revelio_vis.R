# Revelio Vis 



# graph data
library(zoo)
library(ggplot2)
library(scales)


### Job Postings graph
data_cosmos_group <- read_csv("data/revelio/revelio_cosmos_weekly_jobs_2021-01-2025-06.csv")

# make columns for month and year
data_cosmos_graph <- data_cosmos_group %>%
  mutate(month = month(week_start, label = TRUE, abbr = TRUE),
         year = year(week_start),
         week = week(week_start),
         week = if_else(week > 52, 52L, week),  # ← collapse week 53 into 52
         new_posts_1k = count_new_posts/1000) %>%
  group_by(year, week) %>%
  summarise(
    new_posts_1k = sum(new_posts_1k)) %>%
  arrange(year, week) %>%
  mutate(                                     # make moving average
    new_posts_ma2 = rollmean(new_posts_1k, k = 2, align = "right", fill = NA)
  )


# make month to week key for graph
month_ticks <- c(
  "Jan" = 1,
  "Feb" = 5,
  "Mar" = 9,
  "Apr" = 14,
  "May" = 18,
  "Jun" = 22,
  "Jul" = 27,
  "Aug" = 31,
  "Sep" = 36,
  "Oct" = 40,
  "Nov" = 45,
  "Dec" = 49
)


# visualize trends in  job postings
cosmos_all_line <- data_cosmos_graph %>%
  filter(year>2021) %>%
  ggplot(aes(x=week, y=new_posts_1k, group=year, color=factor(year))) +
  geom_line() +
  scale_x_continuous(
    breaks = month_ticks,
    labels = names(month_ticks),
    expand = c(0.1, 0) ) +
  scale_y_continuous(
    breaks = pretty_breaks(n=8)
  ) +
  labs(
    x = "Month",
    y = "New Job Posts (1000s)",
    color = "Year"
  )

print(cosmos_all_line)
#
# pdf("./graphs/cosmos_jobs_2022-2025.pdf", height = 7, width = 9)
# lin_all_line
# dev.off()
