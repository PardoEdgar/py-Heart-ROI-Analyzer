library(tidyverse)

all_Valleys <- read_csv("C:/Users/jandr/Videos/videos prueba/data_extraction/non_filtered_valleys/all_valleys.csv")

all_Valleys |>
  count(Snail) |> nrow()

all_Valleys |>
  count(Site, Temperature, Snail, Gradient) |> nrow()
"""
Exclude data: 
A1_High_Vega_Elevation_UP
A1_High_Vega_Elevation
-A3_Cold_Bogota_Elevation revisar video
-A3_ENV_Bogota_Elevation_Up revisar video por artifact
A3_High_Bogota_Elevation 
A3_High_LaVega_Elevation 
A3_High_LaVega_Elevation_UP
A8_Cold_Bogota_Elevation_UP
-A8_High_LaVega_Elevation un skipped beat arreglable

-B2_Cold_LaVega_Elevation_Up artifact revisar video
-B2_High_Bogota_Elevation_ artifacts 
-B2_High_LaVega_Elevation_ artifacts 
-B3_Cold_Bogota_Elevation_Up artifacts 
B4_Cold_LaVega_Elevation_Up  
-B4_Cold_LaVega_Elevation artifacts
-B6_Cold_LaVega_Elevation revisar video
-B7_Cold_LaVega_Elevation_up artifact revisar video
-B7_High_Bogota_Elevation artifiact

C1_Cold_Bogota_Elevation 
-C1_Cold_Bogota_Elevation_Up artifact
C1_Cold_Vega_Elevation
C1_ENV_Vega_Elevation
C1_ENV_Vega_Elevation_UP
C1_High_Bogota_Elevation
-C3_ENV_Bogota_Elevation_UP revisar video
C3_High_Bogota_Elevation
C5_Cold_Vega_Elevation
C6_Cold_Bogota_Elevation_UP
-C6_ENV_Bogota_Elevation artifact
C6_ENV_Vega_Elevation_Up
-C8_High_Bogota_Elevation revisar artifact


D4_High_Bogota_Elevation
-D4_High_Vega_Elevation_UP artifact
D6_High_Bogota_Elevation 
-D7_Cold_Vega_Elevation_Up
D8_Cold_Vega_Elevation

-E1_High_Vega_Elevation skipped beat
E2_Cold_Bogota_Elevation_up
-E2_ENV_Bogota_Elevation artifact
-E3_ENV_Bogota_Elevation artifact
-E4_Cold_Vega_Elevation_Up artifact
-E4_Cold_Vega_Elevation artifact
E4_High_Vega_Elevation 
-E5_Cold_Bogota_Elevation_UP artifact
-E6_Cold_Bogota_Elevation_UP 
-E7_Cold_Vega_Elevation_UP artifact
E7_ENV_Bogota_Elevation artifact
-E7_High_Bogota_Elevation revisar video
E9_High_Bogota_Elevation 


F2_cOLD_Bogota_ElevationU_UP
-F2_hIGH_Bogota_Elevation_UP ARTIFCAT
F3_ENV_Bogota_Elevation_UP 
-F5_Cold_Bogota_Elevation_UP  artifact
-F7_Cold_Bogota_Elevation_UP  artifact
-F8_Cold_Bogota_Elevation_UP  artifact
-F9_Cold_Bogota_Elevation  artifact
F9_ENV_Bogota_Elevation_UP 
F9_High_Bogota_Elevation 


-G1_cold_Bogota_Up artifact
-G1_cold_Vega_Up artifact
G2_cold_Vega
G5_ENV_Vega
G6_ENV_Vega_up
G7_ENV_Bogota_up
G7_High_Vega_up

-G9_Cold_Vega artifact
-H1_Cold_bOGOTA_UP artifact
-H4_Cold_Vega_UP artifact
-H5_Cold_Vega_UP artifact
H5_High_Bogota 
H5_High_Vega_Up
H6_Cold_Vega_Up
H7_Cold_Bogota_Up
H7_ENV_Vega_Up
-H7_High_Bogota_Up
H8_Cold_Bogota
H8_Cold_Bogota_up
H8_high_Vega
H9_high_Bogota_up

-I1_Cold_Bogota_up artifact
-I2_Cold_Bogota_up artifact
I3_High_Bogota
-I3_cold_Bogota artifact
-I4_cold_Vega_Up artifact
-I4_ENV_Bogota artifact
I5_ENV_Vega 
I6_COLD_BOGOTA_UP
-I6_COLD_VEGA_UP ARTIFACT
I6_High_Bogota 
I6_High_Bogota_up
I6_High_vega
I6_High_vega_up
-I7_ENV_BOGOTA artifact
I7_High_Vega
I7_High_Vega_up
-I9_cold_bogota
I9_high_bogota
J1_cold_bogota_up
-J1_cold_vega artifact
J1_High_bogota
J1_High_bogota_up
-J2_ENV_bogota
-J5_Cold_Vega-Up
J5_Env_Vega
-J5_High_bOGOTA
-J5_High_VEGA_UP
-J7_COLD_VEGA_UP
J7_ENV_VEGA
J7_HIGH_BOGOTA
J7_HIGH_VEGA
J8_HIGH_VEGA

K1_HIGH_Bogota
K1_HIGH_Vega
K2_ENV_Vega_UP
K2_High_Vega
K3_cold_bogota
K4_cold_Vega
K6_cold_vega
K6_Hihg_Vega_Up
K8_High_vega
K9_High_vega
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
