###############################################################################
# HRV metrics extraction
# Author: Edgar Alejandro Pardo-Sarmiento
###############################################################################
library(readxl)
library(RHRV)
library(tidyverse)

all_valleys <- read_csv("C:/Users/jandr/Videos/videos prueba/data_extraction/filtered_valleys/all_valleys.csv")
Snail_size <- read_csv("C:/Users/jandr/OneDrive - Universidad del rosario/Vega_exp_Snail_T_exp/Data/Size_data/Exp_2/Snail_size.csv")
all_valleys$time <- as.numeric(all_valleys$time)

extract_HRV_metrics <- function(hrv_object, Snail) {
    time_analysis <- hrv_object$TimeAnalysis[[1]]
    RR_ms <- hrv_object$Beat
    data.frame(
      Snail = Snail,
      MeanHR = mean(hrv_object$HR, na.rm = TRUE),
      SDNN = time_analysis$SDNN,
      RMSSD = time_analysis$rMSSD,
      pNN50 = time_analysis$pNN50,
      NNi = mean(RR_ms$RR),
      stringsAsFactors = FALSE
    )
  }

data_by_labels <-all_valleys %>% group_by(Temperature, Site, Gradient, Snail) %>% group_split()
Data_labels <- all_valleys %>% group_by(Temperature, Site, Gradient, Snail)  %>%  group_keys()
results_HRV <- lapply(data_by_labels, function(df_snail) {
  HRV <- CreateHRVData()
  HRV <- LoadBeatVector(HRV, df_snail$time)
  HRV <- BuildNIHR(HRV)
  HRV <- InterpolateNIHR(HRV)
  HRV <- CreateTimeAnalysis(HRV, size = 60, interval = 7.8125)
  return(HRV)
})

names(results_HRV) <- paste0(
  Data_labels$Temperature,
"_",
  Data_labels$Snail, "_", 
  Data_labels$Gradient, "_",
  Data_labels$Site
)

HRV_metrics_total <- imap_dfr(results_HRV, ~ extract_HRV_metrics(.x, .y))

HRV_metrics_total <- HRV_metrics_total %>%
  tidyr::separate(Snail, into = c("Temperature", "Snail","Gradient", "Site"), sep = "_")

extract_RR <- function(hrv_object, Snail) {
    RR_ms <- hrv_object$Beat
    data.frame(
      Snail = Snail,
      RRms = (RR_ms$RR),
      stringsAsFactors = FALSE
    )
  }

RR_data <- imap_dfr(results_HRV, ~ extract_RR(.x, .y))
RR_data <- RR_data %>%
  tidyr::separate(Snail, into = c("Temperature", "Snail","Gradient", "Site"), sep = "_")

cv_function <- function(rr) {
  mean <- mean(rr)
  sd <- sd(rr)
  CV <- (sd / mean) * 100
  return(CV)
}

CV_data <- RR_data %>%
  group_by(Temperature, Snail, Site, Gradient) %>%
  summarise(CV = cv_function(RRms), .groups = "drop")

HRV_metrics_total <- HRV_metrics_total %>%
  left_join(CV_data, by = c("Temperature", "Snail", "Site", "Gradient"))
HRV_metrics_total$Temperature_numeric_C <-  case_when(
    HRV_metrics_total$Temperature == "3.5C" ~ 3.5,
    HRV_metrics_total$Temperature == "22C" ~ 22,
    HRV_metrics_total$Temperature == "40C" ~ 40
)
HRV_metrics_total <- HRV_metrics_total |> left_join(Snail_size, by="Snail")
