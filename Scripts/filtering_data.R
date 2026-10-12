library(tidyverse)

all_Valleys <- read_csv("C:/Users/jandr/Videos/videos prueba/data_extraction/non_filtered_data/all_valleys.csv")

conditions <- all_Valleys |>
  distinct(Site, Temperature, Gradient) |>
  nrow()

all_Valleys <- all_Valleys |>
  group_by(Snail) |>
  dplyr::filter(
    n_distinct(Site, Temperature, Gradient) == conditions
  ) |>
  ungroup()

all_Valleys |>
  count(Snail) |> nrow()

all_Valleys |>
  count(Site, Temperature, Snail, Gradient) |> nrow()

nrow(all_Valleys)
unique(all_Valleys$Snail)

all_Valleys <-  all_Valleys |> dplyr::filter(!Snail %in% c("A3","A4","B4","D4","E7","F2","G2","G7","H5","H8","I7","J1","J5","K1","K2","K4","K9"))
