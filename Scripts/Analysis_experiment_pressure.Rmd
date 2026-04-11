---
title: "Pressure"
author: "E. Alejandro Pardo"
date: "2025-10-05"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(readxl)
library(ggrepel)
library(corrplot)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(ggplot2)
library(e1071)
library(scales)
library(gridExtra)


setwd("C:/Users/jandr/Downloads")
list.files()
View(DataPressure)
DataPressure <- read_xlsx("Snail-Info-Elevation.xlsx", sheet = "Lowtohigh") %>%  mutate(Experiment="Low to High")
Presion <- read_xlsx("Presion_data.xlsx")
DataPressure2 <- read_xlsx("Snail-Info-Elevation.xlsx", sheet = "hightolow") %>% mutate(Experiment= "High to Low")
DataPressure$Elevation <- factor(DataPressure$Elevation,
                            levels = c("320","919", "1200","2100", "2620", "3012"))
DataPressure2$Elevation <- factor(DataPressure2$Elevation,
                            levels = c("320","919", "1200","2100", "2620", "3012"))

Data_pressure_complet <- bind_rows(DataPressure, DataPressure2)
Data_pressure_complet$Elevation <- factor(Data_pressure_complet$Elevation,
                            levels = c("320","919", "1200","2100", "2620", "3012"))

ggplot(Presion, aes(x= time, y= Presion$`Pressure(mmHg)`)) + geom_point(size = 0.3, alpha=0.4) + geom_line( size = 0.3, alpha=0.4)  + xlab("Time (s)") + ylab("Barometric pressure (mmHg)") + annotate("text", x = 6, y = 100*7.5, label = "320 masl", size = 3.5, color = "black", fontface = "bold")+ annotate("text", x = 2400, y = 94*7.5, label = "919 masl", size = 3.5, color = "black", fontface = "bold")+ annotate("text", x = 4000, y = 88*7.5, label = "1200  masl", size = 3.5, color = "black", fontface = "bold") + annotate("text", x = 6500, y = 80*7.5, label = "2100 masl", size = 3.5, color = "black", fontface = "bold")  + theme_classic()


pd <- position_dodge(0.6)

Plot_Elevation <- ggplot(Data_pressure_complet,
       aes(x = Elevation,
           y = HR)) +
  geom_boxplot(
    aes(group = interaction(Elevation, Experiment),
        linetype = Experiment),
    outlier.shape = NA,
    alpha = 0.4,
    size = 0.4,
    width = 0.5,
    position = pd
  ) +
  geom_point(
    aes(group = Experiment),
    position = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width = 0.8
    ),size = 0.4, alpha = 0.4
  ) + geom_smooth(
    aes(group = Experiment,
        linetype = Experiment),
    se = TRUE,
    color = "black",
    fill = "grey70",
    size = 0.8,
    fullrange = TRUE
  ) +  stat_summary(
    aes(group = Experiment),
    fun = mean,
    geom = "point",
    shape = 21,
    fill = "darkred",
    color = "black",
    size = 1.5,
    position = pd
  ) + xlab("Elevation (masl)") +
  ylab("Heart Rate (BPM)") +
  annotate("text", x = 1, y = 108, label = "30.9°C", size = 3.5, fontface = "bold") +
  annotate("text", x = 2, y = 108, label = "38.8°C", size = 3.5, fontface = "bold") +
  annotate("text", x = 3, y = 108, label = "28.3°C", size = 3.5, fontface = "bold") +
  annotate("text", x = 4, y = 108, label = "23.8°C", size = 3.5, fontface = "bold") +
  annotate("text", x = 5, y = 108, label = "19.8°C", size = 3.5, fontface = "bold") +
  annotate("text", x = 6, y = 108, label = "16.9°C", size = 3.5, fontface = "bold") +

  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

kruskal.test(`FC (lpm)` ~ Site , data = DataPressure)

library("FSA")  # o 'dunn.test' si prefieres
dunnTest(`FC (lpm)` ~ Site, data = DataPressure2, method = "bonferroni")

temps <- data.frame(
  Site = c("Basal", "Fusagasugá", "Granada", "Chinauta", "Melgar", "Patios"),
  Temp = c(19.8, 30.6, 26.7, 35, 31.9, 15)
)

ggplot(DataPressure, aes(x= Site, y= DataPressure$HR))  + geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5,size =0.2) + geom_point(
  position = position_jitter(width = 0.001, height = 0), 
  size = 1.8, 
  alpha = 0.6
)+  xlab("Sitio") + ylab("FrecCardiaca (BPM)") + ggtitle("low to high")+ theme(
  plot.title = element_text(hjust = 0.5)  # ← centra el título
  ) +   annotate("text", x = 2, y = 100, label = "31.9°C - 8 am", size = 3.5, color = "black", fontface = "italic")+  annotate("text", x = 3, y = 100, label = "35°C -11 am", size = 3.5, color = "steelblue4", fontface = "italic")+  annotate("text", x = 4, y = 100, label = "30.6°C -1 pm", size = 3.5, color = "steelblue4", fontface = "italic")+  annotate("text", x = 5, y = 100, label = "26.7°C -3 pm", size = 3.5, color = "steelblue4", fontface = "italic") + annotate("text", x = 1, y = 100, label = "19.8°C", size = 3.5, color = "steelblue4", fontface = "italic")+ stat_summary(fun = mean, geom = "point", shape = 21, fill = "darkred", color = "black", size = 1.5) 

shapiro.test(DataPressure$HR)
anova <- aov(HR ~ Site, data = DataPressure)
summary(anova)
posthoc <- TukeyHSD(anova)
posthoc
summary(posthoc)


```


```{r setup, include=FALSE}
#No parametrica pareada
HRV_metrics <- read_csv("C:/Users/jandr/Videos/videos prueba/data_extraction/HRV_metrics.csv")
colnames(HRV_metrics)

HRV_metrics_num <- HRV_metrics %>%
  mutate(
    Temperature = recode(
      Temperature,
      "Cold" = 3,
      "ENV" = 22,
      "High" = 40
    )
  )
##Temperature mass

colnames(HRV_metrics)
mass_HR <- ggplot(HRV_metrics, aes(x = Body_mass_g, y = MeanHR, color = Temperature)) +
  geom_point(size=0.8,alpha = 0.1) +
  geom_smooth(method = "lm", se = T, linewidth = 1, alpha=0.3, fullrange=TRUE) + labs(
    x = "Mass (g)",
    y = "Heart rate (BPM)"
  )  + scale_color_manual(values= c("darkblue","darkorange", "darkred")) + theme(
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) + coord_cartesian(ylim = c(5, 80)) + theme(legend.position = "Top") + theme_classic()


mass_CV <- ggplot(HRV_metrics, aes(x = Body_mass_g, y = CV, color = Temperature)) +
  geom_point(size=0.8,alpha = 0.1) +
  geom_smooth(method = "lm", se = T, linewidth = 1, alpha=0.3, fullrange=TRUE) + labs(
    x = "Mass (g)",
    y = "Heart rate variability (CV%)"
  )  + scale_color_manual(values= c("darkblue","darkorange", "darkred")) + theme(
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) + coord_cartesian(ylim = c(0, 30)) + theme_classic() 

#Temperature 
MeanHR_ELV <- ggplot(HRV_metrics_num, aes(x=Temperature, y = MeanHR))+
  geom_boxplot(aes(group = Temperature),
    fill = "white",
    color = "black",
    alpha = 0.3,size=0.4,width = 5,
    outlier.shape = NA,
    position = position_dodge(width = 0.8)
  ) + geom_point(
    position = position_jitterdodge(
      jitter.width = 5,
      dodge.width = 0.8), size = 0.3, alpha=0.3) + stat_summary(
    fun = mean,
    geom = "point",
    color = "darkred",
    fill = "darkred",
    size = 1.5,
    shape = 21,
    position = position_dodge(width = 0.8)
  ) +geom_smooth(se = TRUE,
              color = "black", alpha=0.3, size=0.8, fullrange=T)+scale_x_continuous(
    breaks = sort(unique(HRV_metrics_num$Temperature))
  )+
  labs(x = "Temperature (°C)", y = "Heart rate (BPM)") +coord_cartesian(ylim = c(15, 70)) +
  theme_classic()



CV_ELV <- ggplot(HRV_metrics_num, aes(x=Temperature, y = CV))+
  geom_boxplot(aes(group = Temperature),
    fill = "white",
    color = "black",
    alpha = 0.3,size=0.4,width = 5,
    outlier.shape = NA,
    position = position_dodge(width = 0.8)
  ) + geom_point(
    position = position_jitterdodge(
      jitter.width = 5,
      dodge.width = 0.8),  size = 0.3, alpha=0.3) + stat_summary(
    fun = mean,
    geom = "point",
    color = "darkred",
    fill = "darkred",
    size = 1.5,
    shape = 21,
    position = position_dodge(width = 0.8)
  ) +geom_smooth(se = TRUE,
              color = "black", alpha=0.3, size=0.8, fullrange=T)+scale_x_continuous(
    breaks = sort(unique(HRV_metrics_num$Temperature))
  )+
  labs(x = "Temperature (°C)", y = "Heart rate variability (CV%)") +coord_cartesian(ylim = c(0, 40)) +
  theme_classic()

##Site

MeanHR_ELV <- ggplot(HRV_metrics_num, aes(x=Temperature, y = MeanHR))+
  geom_boxplot(aes(group = interaction(Temperature, Site), linetype = Site),
    fill = "white",
    color = "black",
    alpha = 0.1,size=0.4,width = 5,
    outlier.shape = NA,
    position = position_dodge(width = 7)
  ) + geom_point(aes(group = interaction(Temperature, Site)),
    position = position_jitterdodge(
      jitter.width = 5,
      dodge.width = 7), size = 0.3, alpha=0.1) + stat_summary(
    aes(group = interaction(Temperature,Site)),
    fun = mean,
    geom = "point",
    color = "darkred",
    fill = "darkred",
    size = 1.5,
    shape = 21,
    position = position_dodge(width = 7)
  ) +
  labs(x = "Temperature (°C)", y = "Heart rate (BPM)") +
  geom_smooth(
    aes(group = Site,
        linetype = Site),se = TRUE, color = "black", alpha=0.3, size=0.8, fullrange=T)+scale_x_continuous(
    breaks = sort(unique(HRV_metrics_num$Temperature))
  )+ coord_cartesian(ylim = c(10, 70)) +
  theme_classic() + theme(legend.position = "top")



CV_ELV <- ggplot(HRV_metrics_num, aes(x=Temperature, y = CV))+
  geom_boxplot(aes(group = interaction(Temperature, Site), linetype = Site),
    fill = "white",
    color = "black",
    alpha = 0.1,size=0.4,width = 5,
    outlier.shape = NA,
    position = position_dodge(width = 7)
  ) + geom_point(aes(group = interaction(Temperature, Site)),
    position = position_jitterdodge(
      jitter.width = 5,
      dodge.width = 7), size = 0.3, alpha=0.1) + stat_summary(
    aes(group = interaction(Temperature,Site)),
    fun = mean,
    geom = "point",
    color = "darkred",
    fill = "darkred",
    size = 1.5,
    shape = 21,
    position = position_dodge(width = 7)
  ) +
  labs(x = "Temperature (°C)", y = "Heart rate Variability (CV%)") +
  geom_smooth(
    aes(group = Site,
        linetype = Site),se = TRUE, color = "black", alpha=0.3, size=0.8, fullrange=T)+scale_x_continuous(
    breaks = sort(unique(HRV_metrics_num$Temperature))
  )+ 
coord_cartesian(ylim = c(0, 40)) +
  theme_classic()+ theme(legend.position = "top")


#Gradient

MeanHR_ELV <- ggplot(HRV_metrics_num, aes(x=Temperature, y = MeanHR ))+
  geom_boxplot(aes(group = interaction(Temperature,Gradient), linetype = Gradient),
    fill = "white",
    color = "black",
    alpha = 0.1,size=0.4,width = 5,
    outlier.shape = NA,
    position = position_dodge(width = 6)
  ) + geom_point(aes(group = interaction(Temperature,Gradient)),
    position = position_jitterdodge(
      jitter.width = 5,
      dodge.width = 6), size = 0.5, alpha=0.10) + stat_summary(
    aes(group = interaction(Temperature,Gradient)),
    fun = mean,
    geom = "point",
    color = "darkred",
    fill = "darkred",
    size = 1.5,
    shape = 21,
    position = position_dodge(width = 6)
  ) + 
  geom_smooth(
    aes(group = Gradient,
        linetype = Gradient),se = TRUE, color = "black", alpha=0.3, size=0.8, fullrange=T)+scale_x_continuous(
    breaks = sort(unique(HRV_metrics_num$Temperature))
  )+ 
  labs(x = "Temperature (°C)", y = "Heart rate (BPM)") +coord_cartesian(ylim = c(5, 70)) +
  theme_classic()+ theme(legend.position = "top")


CV_ELV  <-  ggplot(HRV_metrics_num, aes(x=Temperature, y = CV ))+
  geom_boxplot(aes(group = interaction(Temperature,Gradient), linetype = Gradient),
    fill = "white",
    color = "black",
    alpha = 0.1,size=0.4,width = 5,
    outlier.shape = NA,
    position = position_dodge(width = 6)
  ) + geom_point(aes(group = interaction(Temperature,Gradient)),
    position = position_jitterdodge(
      jitter.width = 5,
      dodge.width = 6), size = 0.5, alpha=0.1) + stat_summary(
    aes(group = interaction(Temperature,Gradient)),
    fun = mean,
    geom = "point",
    color = "darkred",
    fill = "darkred",
    size = 1.5,
    shape = 21,
    position = position_dodge(width = 6)
  ) + 
  geom_smooth(
    aes(group = Gradient,
        linetype = Gradient),se = TRUE, color = "black", alpha=0.3, size=0.8, fullrange=T)+scale_x_continuous(
    breaks = sort(unique(HRV_metrics_num$Temperature))
  )+ 
  labs(x = "Temperature (°C)", y = "Heart rate (BPM)") +coord_cartesian(ylim = c(0, 40)) +
  theme_classic()+ theme(legend.position = "top")#Análisis HR y HRV por T, Presión y gradiente

#+  coord_cartesian(ylim = c(2.5, 8)) 




#Análisis de HR y HRV por la masa?




```

```{r setup, include=FALSE}

ggplot(HRV_metrics,
       aes(x = Body_mass_g, y = MeanHR, color = Temperature)) +
  geom_point(size = 0.9, alpha = 0.4) +
   geom_smooth(method = "lm", se = T, linewidth = 1.3, alpha=0.10, fullrange=T)+
  scale_color_manual(values = c("darkblue","lightblue","yellow","darkorange","red",                           "darkred"))+
  labs(
    x = "Mass (g)",
    y = "Heart Rate Variability (CV%)"
  ) +
   theme_classic2()


ggplot(HRV_metrics,
       aes(x = Body_mass_g, y = MeanHR, color = Site)) +
  geom_point(size = 0.9, alpha = 0.4) +
   geom_smooth(method = "lm", se = T, linewidth = 1.3, alpha=0.10, fullrange=T)+
  scale_color_manual(values = c("darkblue",                           "darkred"))+
  labs(
    x = "Mass (g)",
    y = "Heart Rate Variability (CV%)"
  ) +
   theme_classic2()



ggplot(HRV_metrics,
       aes(x = Body_mass_g, y = CV, color = Temperature)) +
  geom_point(size = 0.9, alpha = 0.4) +
   geom_smooth(method = "lm", se = T, linewidth = 1.3, alpha=0.10, fullrange=T)+
  scale_color_manual(values = c("darkblue","lightblue","yellow","darkorange","red",                           "darkred"))+
  labs(
    x = "Mass (g)",
    y = "Heart Rate Variability (CV%)"
  ) +coord_cartesian(ylim = c(0, 80)) +
   theme_classic2()


ggplot(HRV_metrics,
       aes(x = Body_mass_g, y = CV, color = Site)) +
  geom_point(size = 0.9, alpha = 0.4) +
   geom_smooth(method = "lm", se = T, linewidth = 1.3, alpha=0.10, fullrange=T)+
  scale_color_manual(values = c("darkblue","lightblue","yellow","darkorange","red",                           "darkred"))+
  labs(
    x = "Mass (g)",
    y = "Heart Rate Variability (CV%)"
  ) +coord_cartesian(ylim = c(0, 80)) +
   theme_classic2()


```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
