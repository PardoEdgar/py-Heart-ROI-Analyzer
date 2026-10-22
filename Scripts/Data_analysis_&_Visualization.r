library(tidyverse)
library(gridExtra)
library(glmmTMB)
library(emmeans)

###############################
#Exp_1
###############################
#MeanHR
data_exp_1 <- read_xlsx("C:/Users/jandr/Downloads/Data_Exp_1.xlsx", sheet="HRV_metrics")
data_exp_1 <- data_exp_1 |>  dplyr::mutate(Temperature = factor(Temperature), Site = factor(Site),
                                    Gradient = factor(Gradient))
  

view(data_exp_1)
x <- 1
plots <- list()
data_exp_1$Temperature <- factor(data_exp_1$Temperature, levels = c("3.5C","22C","40C"))
colors <- c("darkblue", "orange", "red")
for (y in 6:11)  {

  name <- paste0("plot","_",x)
  plot <- ggplot(data_exp_1) + geom_density(aes(x=.data[[names(data_exp_1)[y]]],color= Temperature, fill = Temperature), alpha = 0.5)+
  facet_grid(cols = vars(Site), rows = vars(Gradient)) +
    scale_color_manual(values=colors)+
    scale_fill_manual(values=colors)+
  theme_classic()
  
  assign(name, plot)
  x <- x+1
  plots[[name]] <- plot
  
}

grid.arrange(grobs= plots, ncol = 2, nrow= 3) 
data_exp_1$Body_mass_g

Model_1 <- glmmTMB(log(MeanHR) ~ Temperature + Site + Gradient + Body_mass_g + (1 | Snail ) ,data=data_exp_1, family = gaussian())
Model_2 <- glmmTMB(log(MeanHR) ~ Temperature * Site  * Gradient * Body_mass_g + (1 | Snail ) ,data=data_exp_1, family = gaussian())

summary(Model_1)
summary(Model_2)

qqnorm(resid(Model_1))
qqline(resid(Model_1))

qqnorm(resid(Model_2))
qqline(resid(Model_2))

Model_res <- simulateResiduals(Model_1 , n=2000)
plot(Model_res)

Model_res <- simulateResiduals(Model_2 , n=2000)
plot(Model_res)

AIC(Model_1, Model_2)

data <-  expand.grid(Temperature = factor(levels(data_exp_1$Temperature), levels=levels(data_exp_1$Temperature)),Site = factor(levels(data_exp_1$Site), levels=levels(data_exp_1$Site)), Gradient = factor(levels(data_exp_1$Gradient), levels=levels(data_exp_1$Gradient)),   Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE))
predictions <- predict(Model_2, newdata = data, re.form = ~0, se.fit=TRUE) 

Plot_data <- data |>  data.frame(
  Mean = predictions$fit,
  SE = predictions$se.fit) |> dplyr::mutate(
    Max_CI = Mean + 1.96 * SE, 
    Min_CI = Mean - 1.96 * SE, 
    Temperature = factor(Temperature, levels = c("3.5C","22C","40C")),
    Temperature_numeric_C = as.numeric(
      sub("C", "", as.character(Temperature)))) |> mutate(Mean = exp(Mean), Max_CI=exp(Max_CI), Min_CI=exp(Min_CI))

shapes = c("solid","dashed")
ggplot(data_exp_1, aes(x=Temperature_numeric_C,color="black", y=MeanHR)) + 
  geom_boxplot(aes(linetype = Site, group=interaction(Temperature,Site)), color = "black",position = position_dodge(width = 5), width = 4)+
  geom_point(aes(group=interaction(Temperature,Site)),color = "black",position= position_jitterdodge(jitter.width = 3, dodge.width = 5), alpha = 4, size=0.3)+
  geom_smooth(aes(linetype = Site, group = Site),color = "black")+
  geom_point(data =Plot_data, aes(x=Temperature_numeric_C, group= Site,y=Mean),color="red",position = position_dodge(width = 5))+ 
  geom_errorbar(data=Plot_data, aes(x = Temperature_numeric_C,ymax= Max_CI, ymin =Min_CI, group=Site), position = position_dodge(width = 5),color="red", width = 1, inherit.aes = FALSE) + 
  
  facet_grid(cols=vars(Gradient))+
  scale_linetype_manual(values = shapes) +
  theme_classic() + labs(x = "Temperature (°C)", y= "Heart Rate (BPM)")+
  theme(legend.position = "top")


emm <- emmeans(
  Model_2,
  ~ Temperature | Site * Gradient,
  at = list(
    Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE)
  )
)

emm_site <- emmeans(
  Model_2,
  ~ Site | Gradient * Temperature
)


emm_grad <- emmeans(
  Model_2,
  ~ Gradient | Site * Temperature
)
pairs(emm, adjust = "tukey")
pairs(emm_site, adjust = "tukey")
pairs(emm_grad, adjust = "tukey")

summary(
  pairs(emm),
  by = NULL,
  adjust = "holm"
)

summary(
  pairs(emm_site),
  by = NULL,
  adjust = "holm"
)
summary(
  pairs(emm_grad),
  by = NULL,
  adjust = "holm"
)
####################
#NNi
data_exp_1$NNi
Model_1 <- glmmTMB(log(NNi) ~ Temperature + Site + Gradient + Body_mass_g + (1 | Snail ) ,data=data_exp_1, family = gaussian())
Model_2 <- glmmTMB(log(NNi) ~ Temperature * Site  * Gradient * Body_mass_g + (1 | Snail ) ,data=data_exp_1, family = gaussian())

summary(Model_1)
summary(Model_2)

qqnorm(resid(Model_1))
qqline(resid(Model_1))

qqnorm(resid(Model_2))
qqline(resid(Model_2))

Model_res <- simulateResiduals(Model_1 , n=2000)
plot(Model_res)

Model_res <- simulateResiduals(Model_2 , n=2000)
plot(Model_res)

AIC(Model_1, Model_2)

data <-  expand.grid(Temperature = factor(levels(data_exp_1$Temperature), levels=levels(data_exp_1$Temperature)),Site = factor(levels(data_exp_1$Site), levels=levels(data_exp_1$Site)), Gradient = factor(levels(data_exp_1$Gradient), levels=levels(data_exp_1$Gradient)),   Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE))
predictions <- predict(Model_2, newdata = data, re.form = ~0, se.fit=TRUE) 

Plot_data <- data |>  data.frame(
  Mean = predictions$fit,
  SE = predictions$se.fit) |> dplyr::mutate(
    Max_CI = Mean + 1.96 * SE, 
    Min_CI = Mean - 1.96 * SE, 
    Temperature = factor(Temperature, levels = c("3.5C","22C","40C")),
    Temperature_numeric_C = as.numeric(
      sub("C", "", as.character(Temperature)))) |> mutate(Mean = exp(Mean), Max_CI=exp(Max_CI), Min_CI=exp(Min_CI))

shapes = c("solid","dashed")
ggplot(data_exp_1, aes(x=Temperature_numeric_C,color="black", y=NNi)) + 
  geom_boxplot(aes(linetype = Site, group=interaction(Temperature,Site)), color = "black",position = position_dodge(width = 5), width = 4)+
  geom_point(aes(group=interaction(Temperature,Site)),color = "black",position= position_jitterdodge(jitter.width = 3, dodge.width = 5), alpha = 4, size=0.3)+
  geom_smooth(aes(linetype = Site, group = Site),color = "black")+
  geom_point(data =Plot_data, aes(x=Temperature_numeric_C, group= Site,y=Mean),color="red",position = position_dodge(width = 5))+ 
  geom_errorbar(data=Plot_data, aes(x = Temperature_numeric_C,ymax= Max_CI, ymin =Min_CI, group=Site), position = position_dodge(width = 5),color="red", width = 1, inherit.aes = FALSE) + 
  
  facet_grid(cols=vars(Gradient))+
  scale_linetype_manual(values = shapes) +
  theme_classic() + labs(x = "Temperature (°C)", y= "Heart Rate (BPM)")+
  theme(legend.position = "top")


emm <- emmeans(
  Model_2,
  ~ Temperature | Site * Gradient,
  at = list(
    Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE)
  )
)

emm_site <- emmeans(
  Model_2,
  ~ Site | Gradient * Temperature
)


emm_grad <- emmeans(
  Model_2,
  ~ Gradient | Site * Temperature
)
pairs(emm, adjust = "tukey")
pairs(emm_site, adjust = "tukey")
pairs(emm_grad, adjust = "tukey")

summary(
  pairs(emm),
  by = NULL,
  adjust = "holm"
)

summary(
  pairs(emm_site),
  by = NULL,
  adjust = "holm"
)
summary(
  pairs(emm_grad),
  by = NULL,
  adjust = "holm"
)
###############33
###CV
data_exp_1$NNi
Model_1 <- glmmTMB(log(CV) ~ Temperature + Site + Gradient  + (1 | Snail ) ,data=data_exp_1, family = gaussian())
Model_2 <- glmmTMB(log(CV) ~ Temperature * Site  * Gradient * Body_mass_g + (1 | Snail ) ,data=data_exp_1, family = gaussian())

summary(Model_1)
summary(Model_2)

qqnorm(resid(Model_1))
qqline(resid(Model_1))

qqnorm(resid(Model_2))
qqline(resid(Model_2))

Model_res <- simulateResiduals(Model_1 , n=2000)
plot(Model_res)

Model_res <- simulateResiduals(Model_2 , n=2000)
plot(Model_res)

AIC(Model_1, Model_2)

data <-  expand.grid(Temperature = factor(levels(data_exp_1$Temperature), levels=levels(data_exp_1$Temperature)),Site = factor(levels(data_exp_1$Site), levels=levels(data_exp_1$Site)), Gradient = factor(levels(data_exp_1$Gradient), levels=levels(data_exp_1$Gradient)),   Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE))
predictions <- predict(Model_2, newdata = data, re.form = ~0, se.fit=TRUE) 

Plot_data <- data |>  data.frame(
  Mean = predictions$fit,
  SE = predictions$se.fit) |> dplyr::mutate(
    Max_CI = Mean + 1.96 * SE, 
    Min_CI = Mean - 1.96 * SE, 
    Temperature = factor(Temperature, levels = c("3.5C","22C","40C")),
    Temperature_numeric_C = as.numeric(
      sub("C", "", as.character(Temperature)))) |> mutate(Mean = exp(Mean), Max_CI=exp(Max_CI), Min_CI=exp(Min_CI))

shapes = c("solid","dashed")
ggplot(data_exp_1, aes(x=Temperature_numeric_C,color="black", y=CV)) + 
  geom_boxplot(aes(linetype = Site, group=interaction(Temperature,Site)), color = "black",position = position_dodge(width = 5), width = 4)+
  geom_point(aes(group=interaction(Temperature,Site)),color = "black",position= position_jitterdodge(jitter.width = 3, dodge.width = 5), alpha = 4, size=0.3)+
  geom_smooth(aes(linetype = Site, group = Site),color = "black")+
  geom_point(data =Plot_data, aes(x=Temperature_numeric_C, group= Site,y=Mean),color="red",position = position_dodge(width = 5))+ 
  geom_errorbar(data=Plot_data, aes(x = Temperature_numeric_C,ymax= Max_CI, ymin =Min_CI, group=Site), position = position_dodge(width = 5),color="red", width = 1, inherit.aes = FALSE) + 
  
  facet_grid(cols=vars(Gradient))+
  scale_linetype_manual(values = shapes) +
  theme_classic() + labs(x = "Temperature (°C)", y= "Heart Rate (BPM)")+
  theme(legend.position = "top")


emm <- emmeans(
  Model_2,
  ~ Temperature | Site * Gradient,
  at = list(
    Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE)
  )
)

emm_site <- emmeans(
  Model_2,
  ~ Site | Gradient * Temperature
)


emm_grad <- emmeans(
  Model_2,
  ~ Gradient | Site * Temperature
)
pairs(emm, adjust = "tukey")
pairs(emm_site, adjust = "tukey")
pairs(emm_grad, adjust = "tukey")

summary(
  pairs(emm),
  by = NULL,
  adjust = "holm"
)

summary(
  pairs(emm_site),
  by = NULL,
  adjust = "holm"
)
summary(
  pairs(emm_grad),
  by = NULL,
  adjust = "holm"
)
######
#RMSSD
data_exp_1$NNi
Model_1 <- glmmTMB(log(RMSSD) ~ Temperature + Site + Gradient + body_mass_g  + (1 | Snail ) ,data=data_exp_1, family = gaussian())
Model_2 <- glmmTMB(log(RMSSD) ~ Temperature * Site  * Gradient * Body_mass_g + (1 | Snail ) ,data=data_exp_1, family = gaussian())

summary(Model_1)
summary(Model_2)

qqnorm(resid(Model_1))
qqline(resid(Model_1))

qqnorm(resid(Model_2))
qqline(resid(Model_2))

Model_res <- simulateResiduals(Model_1 , n=2000)
plot(Model_res)

Model_res <- simulateResiduals(Model_2 , n=2000)
plot(Model_res)

AIC(Model_1, Model_2)

data <-  expand.grid(Temperature = factor(levels(data_exp_1$Temperature), levels=levels(data_exp_1$Temperature)),Site = factor(levels(data_exp_1$Site), levels=levels(data_exp_1$Site)), Gradient = factor(levels(data_exp_1$Gradient), levels=levels(data_exp_1$Gradient)),   Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE))
predictions <- predict(Model_2, newdata = data, re.form = ~0, se.fit=TRUE) 

Plot_data <- data |>  data.frame(
  Mean = predictions$fit,
  SE = predictions$se.fit) |> dplyr::mutate(
    Max_CI = Mean + 1.96 * SE, 
    Min_CI = Mean - 1.96 * SE, 
    Temperature = factor(Temperature, levels = c("3.5C","22C","40C")),
    Temperature_numeric_C = as.numeric(
      sub("C", "", as.character(Temperature)))) |> mutate(Mean = exp(Mean), Max_CI=exp(Max_CI), Min_CI=exp(Min_CI))

shapes = c("solid","dashed")
ggplot(data_exp_1, aes(x=Temperature_numeric_C,color="black", y=CV)) + 
  geom_boxplot(aes(linetype = Site, group=interaction(Temperature,Site)), color = "black",position = position_dodge(width = 5), width = 4)+
  geom_point(aes(group=interaction(Temperature,Site)),color = "black",position= position_jitterdodge(jitter.width = 3, dodge.width = 5), alpha = 4, size=0.3)+
  geom_smooth(aes(linetype = Site, group = Site),color = "black")+
  geom_point(data =Plot_data, aes(x=Temperature_numeric_C, group= Site,y=Mean),color="red",position = position_dodge(width = 5))+ 
  geom_errorbar(data=Plot_data, aes(x = Temperature_numeric_C,ymax= Max_CI, ymin =Min_CI, group=Site), position = position_dodge(width = 5),color="red", width = 1, inherit.aes = FALSE) + 
  
  facet_grid(cols=vars(Gradient))+
  scale_linetype_manual(values = shapes) +
  theme_classic() + labs(x = "Temperature (°C)", y= "Heart Rate (BPM)")+
  theme(legend.position = "top")


emm <- emmeans(
  Model_2,
  ~ Temperature | Site * Gradient,
  at = list(
    Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE)
  )
)

emm_site <- emmeans(
  Model_2,
  ~ Site | Gradient * Temperature
)


emm_grad <- emmeans(
  Model_2,
  ~ Gradient | Site * Temperature
)
pairs(emm, adjust = "tukey")
pairs(emm_site, adjust = "tukey")
pairs(emm_grad, adjust = "tukey")

summary(
  pairs(emm),
  by = NULL,
  adjust = "holm"
)

summary(
  pairs(emm_site),
  by = NULL,
  adjust = "holm"
)
summary(
  pairs(emm_grad),
  by = NULL,
  adjust = "holm"
)




##Temperature vs mass

mass_HR <- ggplot(data_exp_1, aes(x = Body_mass_g, y = MeanHR, color = Temperature)) +
  geom_point(size=0.8,alpha = 0.1) +
  geom_smooth(method = "lm", se = T, linewidth = 1, alpha=0.3, fullrange=TRUE) + labs(
    x = "Mass (g)",
    y = "Heart rate (BPM)"
  )  + scale_color_manual(values= c("darkblue","darkorange", "darkred")) + theme(
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) + coord_cartesian(ylim = c(25, 70)) + theme(legend.position = "Top") + theme_classic()

mass_CV <- ggplot(data_exp_1, aes(x = Body_mass_g, y = CV, color = Temperature)) +
  geom_point(size=0.8,alpha = 0.1) +
  geom_smooth(method = "lm", se = T, linewidth = 1, alpha=0.3, fullrange=TRUE) + labs(
    x = "Mass (g)",
    y = "Heart rate variability (CV%)"
  )  + scale_color_manual(values= c("darkblue","darkorange", "darkred")) + theme(
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) + coord_cartesian(ylim = c(0, 10)) + theme_classic() 



###################
#Exp_2
#####################
Exp_2_HR <- read_xlsx("C:/Users/jandr/Downloads/Data_Exp_2.xlsx", sheet="HR")
Exp_2_Sensor <- read_xlsx("C:/Users/jandr/Downloads/Data_Exp_2.xlsx", sheet="Sensors")
Exp_2_HR$Elevation <- factor(data_exp_1$Elevation,
                            levels = c("320","919", "1200","2100", "2620", "3012"))

plot_1<-  ggplot(Exp_2_Sensor, aes(x= Time_s, y=Pressure_mmHg))+ 
  geom_point(size = 0.3, alpha=0.4) + geom_line( size = 0.3, alpha=0.4)  +
  xlab("Time (s)") + ylab("Barometric pressure (mmHg)") +
  annotate("text", x = 6, y = 100*7.5, label = "320 masl", size = 3.5, color = "black", fontface = "bold")+ 
  annotate("text", x = 2400, y = 94*7.5, label = "919 masl", size = 3.5, color = "black", fontface = "bold")+ 
  annotate("text", x = 4000, y = 88*7.5, label = "1200  masl", size = 3.5, color = "black", fontface = "bold") +
  annotate("text", x = 6500, y = 80*7.5, label = "2100 masl", size = 3.5, color = "black", fontface = "bold")  + 
  theme_classic()

plot_2 <-ggplot(Exp_2_Sensor, aes(x= Time_s, y=`Temperature_°C` ))+ 
  geom_point(size = 0.3, alpha=0.4) + geom_line( size = 0.3, alpha=0.4)  +
  xlab("Time (s)") + ylab("Temperature") +theme_classic()

pd <- position_dodge(0.6)

ggplot(Exp_2_HR,
       aes(x = Elevation,
           y = HR)) +
  geom_boxplot(
    aes(group = interaction(Elevation, Gradient),
        linetype = Gradient),
    outlier.shape = NA,
    alpha = 0.4,
    size = 0.4,
    width = 0.5,
    position = pd
  ) +
  geom_point(
    aes(group = Gradient),
    position = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width = 0.8
    ),size = 0.4, alpha = 0.4
  ) + geom_smooth(
    aes(group = Gradient,
        linetype = Gradient),
    se = TRUE,
    color = "black",
    fill = "grey70",
    size = 0.8,
    fullrange = TRUE
  ) +  stat_summary(
    aes(group = Gradient),
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
  theme(legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

plots <- c(plot_1,plot_2)
grid.arrange( grobs = plots, ncols=1)





kruskal.test(`FC (lpm)` ~ Site , data = data_exp_1)

library("FSA")  
dunnTest(`FC (lpm)` ~ Site, data = data_exp_12, method = "bonferroni")

temps <- data.frame(
  Site = c("Basal", "Fusagasugá", "Granada", "Chinauta", "Melgar", "Patios"),
  Temp = c(19.8, 30.6, 26.7, 35, 31.9, 15)
)

ggplot(data_exp_1, aes(x= Site, y= data_exp_1$HR))  + geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.5,size =0.2) + geom_point(
  position = position_jitter(width = 0.001, height = 0), 
  size = 1.8, 
  alpha = 0.6
)+  xlab("Sitio") + ylab("FrecCardiaca (BPM)") + ggtitle("low to high")+ theme(
  plot.title = element_text(hjust = 0.5)  # ← centra el título
  ) +   annotate("text", x = 2, y = 100, label = "31.9°C - 8 am", size = 3.5, color = "black", fontface = "italic")+  annotate("text", x = 3, y = 100, label = "35°C -11 am", size = 3.5, color = "steelblue4", fontface = "italic")+  annotate("text", x = 4, y = 100, label = "30.6°C -1 pm", size = 3.5, color = "steelblue4", fontface = "italic")+  annotate("text", x = 5, y = 100, label = "26.7°C -3 pm", size = 3.5, color = "steelblue4", fontface = "italic") + annotate("text", x = 1, y = 100, label = "19.8°C", size = 3.5, color = "steelblue4", fontface = "italic")+ stat_summary(fun = mean, geom = "point", shape = 21, fill = "darkred", color = "black", size = 1.5) 

shapiro.test(data_exp_1$HR)
anova <- aov(HR ~ Site, data = data_exp_1)
summary(anova)
posthoc <- TukeyHSD(anova)
posthoc
summary(posthoc)

