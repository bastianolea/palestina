library(readr)
library(dplyr)

gaza <- read_csv("killed-in-gaza.csv")

gaza


gaza <- read_csv("casualties_daily.csv") |> 
  select(-ends_with("cum"))

gaza |> glimpse()

library(ggplot2)



install.packages("ggbeeswarm")
library(ggbeeswarm)


gaza |> 
  ggplot() +
  aes(killed, report_date) +
  geom_beeswarm(cex = 2)


library(tidyr)
gaza_unc <- gaza |> 
  filter(!is.na(killed)) |> 
  uncount(weights = killed)
  

gaza_unc |> 
  slice(1:10000) |> 
  ggplot() +
  aes(1, report_date) +
  geom_quasirandom(size = 0.1, bandwidth = 0.3)

gaza_unc |> 
  slice(1:10000) |> 
  ggplot() +
  aes(1, report_date) +
  geom_beeswarm(size = 0.1)


gaza_unc |> 
  # slice(1:20000) |> 
  ggplot() +
  aes(1, report_date) +
  geom_violin(trim = F, adjust = .4) +
  # scale_y_date(date_breaks = "months", date_labels = "%m/%Y",
               # )
  # scale_y_date(date_breaks = "months") +
  scale_y_continuous(n.breaks = 5) +
  coord_cartesian(ylim = c(max(gaza_unc$report_date), min(gaza_unc$report_date)),
                  expand = TRUE)
