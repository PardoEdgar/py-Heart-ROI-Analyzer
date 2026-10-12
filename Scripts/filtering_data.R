library(tidyverse)

all_Valleys <- read_csv("C:/Users/jandr/Videos/videos prueba/data_extraction/non_filtered_valleys/all_valleys.csv")

all_Valleys |>
  count(Snail) |> nrow()

all_Valleys |>
  count(Site, Temperature, Snail, Gradient) |> nrow()
"""
Exclude data: 

"""

Exclude <- tibble(
  Snail = c("A3", "A4", "B4"),
  Site = c("Bogota", "LaVega", "Bogota"),
  Temperature = c("3.5C", "33C", "40C"),
  Gradient = c("UP", "DOWN", "DOWN")
)

all_Valleys_clean <- all_Valleys |>
  anti_join(
    Exclude,
    by = c("Snail", "Site", "Temperature", "Gradient")
  )
all_Valleys |>
  count(Snail) |> nrow()

all_Valleys |>
  count(Site, Temperature, Snail, Gradient) |> nrow()

nrow(all_Valleys)
unique(all_Valleys$Snail)
