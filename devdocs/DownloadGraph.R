#### downloads graph
library(ggplot2)

cranlogs::cran_downloads(
  from = "2022-05-01",
  to = lubridate::today(),
  packages = c("IPEDSuploadables")
) %>%

  #pictures
  ggplot(data = ., mapping = aes(x = date, y = count)) +
  geom_vline(
    xintercept = c(
      lubridate::as_date("2022-05-12"),
      lubridate::as_date("2022-06-08"),
      lubridate::today()
    ),
    color = "blue",
    size = 1
  ) +
  geom_line(color = "black", linewidth = 1) +

  #general visuals
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line()
  ) +
  ggtitle("The bots got excited in May, and IR is getting caught up in June?\n") +
  ylab("Downloads from CRAN\n") +
  xlab("Date") +
  annotate(
    "text",
    x = c(lubridate::as_date(c("20220512", "20220608")), lubridate::today()),
    y = 28,
    label = c("CRAN release", "AIR talk", "Today"),
    hjust = 1.1,
    color = "blue"
  )


####################
#### Totals!
library(dplyr)

cranlogs::cran_downloads(
  from = "2022-05-01",
  to = lubridate::today(),
  packages = c("IPEDSuploadables")
) %>%
  mutate(Year = lubridate::year(date),
         Month = lubridate::month(date, label = TRUE)) %>%
  group_by(Year, Month) %>%
  summarize(Downloads = sum(count)) %>%
  ungroup() %>%
  mutate(RunningTotal = cumsum(.data$Downloads))

####################
#### Trying new, uninflated package counts?
library(packageRank)
starting <- cranDownloads(
  from = "2022-05-01",
  to = lubridate::today(),
  packages = c("IPEDSuploadables")
)

#SLOOOWWWWWW
packageRank(package = "IPEDSuploadables", date = "2023-09-16")
#defaults to current date
filteredDownloads(package = "IPEDSuploadables", all.filters = TRUE)
#Can only download one date at time, and it's like 100MB per.
#A couple dates show that generally only a couple results are filtered on a given day
#Which could suggest a blanket filter of ~60 per month removed
filteredDownloads(package = "IPEDSuploadables", date = "2023-05-07",  all.filters = TRUE)

#implement rough filter
cranlogs::cran_downloads(
  from = "2022-05-01",
  to = lubridate::today(),
  packages = c("IPEDSuploadables")
) %>%
  mutate(Year = lubridate::year(date),
         Month = lubridate::month(date, label = TRUE)) %>%
  group_by(Year, Month) %>%
  summarize(Downloads = sum(count),
            SomeRemoved = Downloads-60) %>%
  ungroup() %>%
  mutate(RunningTotalAll = cumsum(.data$Downloads),
    RunningTotalSomeRemoved = cumsum(.data$SomeRemoved))

