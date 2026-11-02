library(tidyr)
library(readxl)
library(ggrepel)
library(gridExtra)
library(glmmTMB)
library(emmeans)
library(DHARMa)
library(dplyr)
###############################
#Exp_1
###############################
#MeanHR
data_exp_1 <- read_xlsx("C:/Users/jandr/Downloads/Data_Exp_1.xlsx", sheet="HRV_metrics")
data_exp_1 <- data_exp_1 |>  dplyr::mutate(Temperature = factor(Temperature), Site = factor(Site),
                                    Direction = factor(Direction),
                                    Snail = factor(Snail))
x <- 1
plots <- list()
data_exp_1$Temperature <- factor(data_exp_1$Temperature, levels = c("3.5C","22C","40C"))
colors <- c("darkblue", "orange", "red")
for (y in 6:11)  {
  name <- paste0("plot","_",x)
  variable <- 
  plot <- ggplot(data_exp_1) + geom_density(aes(x=.data[[names(data_exp_1)[y]]],color= Temperature, fill = Temperature), alpha = 0.5)+
  facet_grid(cols = vars(Site), rows = vars(Direction)) +
    scale_color_manual(values=colors)+
    scale_fill_manual(values=colors)+
  theme_classic()
  
  assign(name, plot)
  x <- x+1
  plots[[name]] <- plot
  
}

grid.arrange(grobs= plots, ncol = 2, nrow= 3) 
data_exp_1$Body_mass_g

full_model <- glmmTMB(log(MeanHR) ~  Temperature * Site  * Direction * Body_mass_g  + (1 | Snail) ,data=data_exp_1, family = gaussian())

qqnorm(resid(full_model))
qqline(resid(full_model))

Model_res <- simulateResiduals(full_model, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

var_1 <-update(full_model, dispformula = ~ Temperature + Direction)

Model_res <- simulateResiduals(var_1, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

var_2 <-update(full_model, dispformula = ~ Temperature  * Direction)

Model_res <- simulateResiduals(var_2, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

AIC(full_model, var_1, var_2)
anova(full_model, var_1, var_2)
Model_1 <- glmmTMB(log(MeanHR) ~  Temperature * Site  * Direction * Body_mass_g  + (1 | Snail ) ,data=data_exp_1, dispformula = ~ Temperature * Direction, family = gaussian())
Model_2 <- glmmTMB(log(MeanHR) ~  Temperature * Site  * Direction + Body_mass_g  + (1 | Snail ) ,data=data_exp_1, dispformula = ~ Temperature * Direction, family = gaussian())
Model_3 <- glmmTMB(log(MeanHR) ~ (Temperature + Site + Direction)^2 + Body_mass_g + (1 | Snail),data = data_exp_1,dispformula = ~ Temperature * Direction, family = gaussian())
Model_4 <- glmmTMB(log(MeanHR) ~ Temperature + Site + Direction+ Body_mass_g + (1 | Snail),data = data_exp_1, dispformula = ~ Temperature * Direction, family = gaussian())

Model_res <- simulateResiduals(Model_1, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_res <- simulateResiduals(Model_2, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_res <- simulateResiduals(Model_3, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)


Model_res <- simulateResiduals(Model_4, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

AIC(Model_1, Model_2, Model_3, Model_4)
anova(Model_1, Model_2, Model_3, Model_4)


Model_HR <- glmmTMB(log(MeanHR) ~ (Temperature + Site + Direction)^2 + Body_mass_g + (1 | Snail),data = data_exp_1,dispformula = ~ Temperature * Direction, family = gaussian())

data <-  expand.grid(Temperature = factor(levels(data_exp_1$Temperature), levels=levels(data_exp_1$Temperature)),Site = factor(levels(data_exp_1$Site), levels=levels(data_exp_1$Site)), Direction = factor(levels(data_exp_1$Direction), levels=levels(data_exp_1$Direction)),   Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE))
predictions <- predict(Model_HR, newdata = data, re.form = ~0, se.fit=TRUE) 

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
  
  facet_grid(cols=vars(Direction))+
  scale_linetype_manual(values = shapes) +
  theme_classic() + labs(x = "Temperature (°C)", y= "Heart Rate (BPM)")+
  theme(legend.position = "top")


emm_tem <- emmeans(
  Model_HR,
  ~ Temperature | Site * Direction, type = "response")
Values_tem <- contrast(emm_tem, "revpairwise", adjust = "bonferroni") |> confint()
Values_tem <- contrast(emm_tem, "revpairwise", adjust = "bonferroni") 
summary(Values_tem, infer = TRUE)

emm_site <- emmeans(
  Model_HR, 
  ~ Site | Direction * Temperature, type = "response")
Values_site <- contrast(emm_site, "revpairwise", adjust = "bonferroni") |> confint()
Values_site <- contrast(emm_site, "revpairwise", adjust = "bonferroni") 
summary(Values_site, infer = TRUE)

emm_grad <- emmeans(
  Model_HR,
  ~ Direction | Site * Temperature, type = "response"
)
contrast(emm_grad, "pairwise", adjust = "bonferroni") |> confint()
Values_grad <- contrast(emm_grad, "revpairwise", adjust = "bonferroni") 
summary(Values_grad, infer = TRUE)

####################
#NNi

full_model <- glmmTMB(log(NNi) ~  Temperature * Site  * Direction * Body_mass_g  + (1 | Snail),data=data_exp_1, family = gaussian())

qqnorm(resid(full_model))
qqline(resid(full_model))

Model_res <- simulateResiduals(full_model, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

var_1 <-update(full_model, dispformula = ~Temperature + Site + Direction)

Model_res <- simulateResiduals(var_1, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)


var_2 <-update(full_model, dispformula = ~ Temperature * Direction)

Model_res <- simulateResiduals(var_2, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

var_3 <-update(full_model, dispformula = ~ Temperature * Site * Direction)

Model_res <- simulateResiduals(var_3, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)


AIC(full_model, var_1, var_2, var_3)
anova(full_model, var_1, var_2, var_3)

Model_1 <- glmmTMB(log(NNi) ~  Temperature * Site  * Direction * Body_mass_g  + (1 | Snail ) ,data=data_exp_1, dispformula = ~  Temperature + Site + Direction, family = gaussian())
Model_2 <- glmmTMB(log(NNi) ~  Temperature * Site  * Direction + Body_mass_g  + (1 | Snail ) ,data=data_exp_1, dispformula = ~  Temperature + Site + Direction, family = gaussian())
Model_3 <- glmmTMB(log(NNi) ~  (Temperature + Site + Direction)^2 + Body_mass_g + (1 | Snail),data = data_exp_1,dispformula = ~ Temperature + Site + Direction, family = gaussian())
Model_4 <- glmmTMB(log(NNi) ~  Temperature + Site + Direction+ Body_mass_g + (1 | Snail),data = data_exp_1, dispformula = ~ Temperature + Site + Direction, family = gaussian())

Model_res <- simulateResiduals(Model_1, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_res <- simulateResiduals(Model_2, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_res <- simulateResiduals(Model_3, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)


Model_res <- simulateResiduals(Model_4, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

AIC(Model_1, Model_2, Model_3, Model_4)
anova(Model_1, Model_2, Model_3, Model_4)

Model_NNi <- glmmTMB(log(NNi) ~  (Temperature + Site + Direction)^2 + Body_mass_g + (1 | Snail),data = data_exp_1,dispformula = ~ Temperature + Site + Direction, family = gaussian())

data <-  expand.grid(Temperature = factor(levels(data_exp_1$Temperature), levels=levels(data_exp_1$Temperature)),Site = factor(levels(data_exp_1$Site), levels=levels(data_exp_1$Site)), Direction = factor(levels(data_exp_1$Direction), levels=levels(data_exp_1$Direction)),   Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE))
predictions <- predict(Model_NNi, newdata = data, re.form = ~0, se.fit=TRUE) 

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
  
  facet_grid(cols=vars(Direction))+
  scale_linetype_manual(values = shapes) +
  theme_classic() + labs(x = "Temperature (°C)", y= "NN Intervals (ms)")+
  theme(legend.position = "top")


emm_tem <- emmeans(
  Model_NNi,
  ~ Temperature | Site * Direction, type = "response")
Values_tem <- contrast(emm_tem, "revpairwise", adjust = "bonferroni") |> confint()
Values_tem <- contrast(emm_tem, "revpairwise", adjust = "bonferroni") 
summary(Values_tem, infer = TRUE)

emm_site <- emmeans(
  Model_NNi, 
  ~ Site | Direction * Temperature, type = "response")
Values_site <- contrast(emm_site, "revpairwise", adjust = "bonferroni") |> confint()
Values_site <- contrast(emm_site, "revpairwise", adjust = "bonferroni") 
summary(Values_site, infer = TRUE)

emm_grad <- emmeans(
  Model_NNi,
  ~ Direction | Site * Temperature, type = "response"
)
contrast(emm_grad, "pairwise", adjust = "bonferroni") |> confint()
Values_grad <- contrast(emm_grad, "revpairwise", adjust = "bonferroni") 
summary(Values_grad, infer = TRUE)

###############33
###CV
full_model <- glmmTMB(log(CV) ~  Temperature * Site  * Direction * Body_mass_g  + (1 | Snail),data=data_exp_1, family = gaussian())

qqnorm(resid(full_model))
qqline(resid(full_model))

Model_res <- simulateResiduals(full_model, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_1 <- glmmTMB(log(CV) ~  Temperature * Site  * Direction * Body_mass_g  + (1 | Snail ) ,data=data_exp_1, family = gaussian())
Model_2 <- glmmTMB(log(CV) ~  Temperature * Site  * Direction + Body_mass_g  + (1 | Snail ) ,data=data_exp_1, family = gaussian())
Model_3 <- glmmTMB(log(CV) ~  (Temperature + Site + Direction)^2 + Body_mass_g + (1 | Snail),data = data_exp_1, family = gaussian())
Model_4 <- glmmTMB(log(CV) ~  Temperature + Site + Direction + Body_mass_g + (1 | Snail),data = data_exp_1, family = gaussian())

Model_res <- simulateResiduals(Model_1, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_res <- simulateResiduals(Model_2, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_res <- simulateResiduals(Model_3, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)


Model_res <- simulateResiduals(Model_4, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

AIC(Model_1, Model_2, Model_3, Model_4)
anova(Model_1, Model_2, Model_3, Model_4)

Model <- glmmTMB(log(CV) ~  Temperature + Site + Direction + Body_mass_g + (1 | Snail),data = data_exp_1, family = gaussian())

data <-  expand.grid(Temperature = factor(levels(data_exp_1$Temperature), levels=levels(data_exp_1$Temperature)),Site = factor(levels(data_exp_1$Site), levels=levels(data_exp_1$Site)), Direction = factor(levels(data_exp_1$Direction), levels=levels(data_exp_1$Direction)),   Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE))
predictions <- predict(Model, newdata = data, re.form = ~0, se.fit=TRUE) 

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
  
  facet_grid(cols=vars(Direction))+
  scale_linetype_manual(values = shapes) + 
  theme_classic() + labs(x = "Temperature (°C)", y= "CV (%)")+
  theme(legend.position = "top")

emm_tem <- emmeans(
  Model,
  ~ Temperature | Site * Direction, type = "response")
Values_tem <- contrast(emm_tem, "revpairwise", adjust = "bonferroni") |> confint()
Values_tem <- contrast(emm_tem, "revpairwise", adjust = "bonferroni") 
summary(Values_tem, infer = TRUE)

emm_site <- emmeans(
  Model, 
  ~ Site | Direction * Temperature, type = "response")
Values_site <- contrast(emm_site, "revpairwise", adjust = "bonferroni") |> confint()
Values_site <- contrast(emm_site, "revpairwise", adjust = "bonferroni") 
summary(Values_site, infer = TRUE)

emm_grad <- emmeans(
  Model,
  ~ Direction | Site * Temperature, type = "response"
)
contrast(emm_grad, "pairwise", adjust = "bonferroni") |> confint()
Values_grad <- contrast(emm_grad, "revpairwise", adjust = "bonferroni") 
summary(Values_grad, infer = TRUE)

######
#RMSSD
full_model <- glmmTMB(log(RMSSD) ~  Temperature * Site  * Direction * Body_mass_g  + (1 | Snail),data=data_exp_1, family = gaussian())

qqnorm(resid(full_model))
qqline(resid(full_model))

Model_res <- simulateResiduals(full_model, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

var_1 <-update(full_model, dispformula = ~Temperature  + Direction)

Model_res <- simulateResiduals(var_1, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)


var_2 <-update(full_model, dispformula = ~ Temperature * Direction)

Model_res <- simulateResiduals(var_2, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

AIC(full_model, var_1, var_2)
anova(full_model, var_1, var_2)

Model_1 <- glmmTMB(log(RMSSD) ~  Temperature * Site  * Direction * Body_mass_g  + (1 | Snail ) ,data=data_exp_1, dispformula = ~  Temperature + Direction, family = gaussian())
Model_2 <- glmmTMB(log(RMSSD) ~  Temperature * Site  * Direction + Body_mass_g  + (1 | Snail ) ,data=data_exp_1, dispformula = ~  Temperature + Direction, family = gaussian())
Model_3 <- glmmTMB(log(RMSSD) ~  (Temperature + Site + Direction)^2 + Body_mass_g + (1 | Snail),data = data_exp_1,dispformula = ~ Temperature + Direction, family = gaussian())
Model_4 <- glmmTMB(log(RMSSD) ~  Temperature + Site + Direction+ Body_mass_g + (1 | Snail),data = data_exp_1, dispformula = ~ Temperature + Direction, family = gaussian())

Model_res <- simulateResiduals(Model_1, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_res <- simulateResiduals(Model_2, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_res <- simulateResiduals(Model_3, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)


Model_res <- simulateResiduals(Model_4, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)
plotResiduals(res_model, data_exp_1$Body_mass_g)    
plotResiduals(res_model, data_exp_1$Snail) 

AIC(Model_1, Model_2, Model_3, Model_4)
anova(Model_1, Model_2, Model_3, Model_4)

Model_RMSSD <- glmmTMB(log(RMSSD) ~  Temperature + Site + Direction+ Body_mass_g + (1 | Snail),data = data_exp_1, dispformula = ~ Temperature + Direction, family = gaussian())

data <-  expand.grid(Temperature = factor(levels(data_exp_1$Temperature), levels=levels(data_exp_1$Temperature)),Site = factor(levels(data_exp_1$Site), levels=levels(data_exp_1$Site)), Direction = factor(levels(data_exp_1$Direction), levels=levels(data_exp_1$Direction)),   Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE))
predictions <- predict(Model_RMSSD, newdata = data, re.form = ~0, se.fit=TRUE) 

Plot_data <- data |>  data.frame(
  Mean = predictions$fit,
  SE = predictions$se.fit) |> dplyr::mutate(
    Max_CI = Mean + 1.96 * SE, 
    Min_CI = Mean - 1.96 * SE, 
    Temperature = factor(Temperature, levels = c("3.5C","22C","40C")),
    Temperature_numeric_C = as.numeric(
      sub("C", "", as.character(Temperature)))) |> mutate(Mean = exp(Mean), Max_CI=exp(Max_CI), Min_CI=exp(Min_CI))

shapes = c("solid","dashed")
ggplot(data_exp_1, aes(x=Temperature_numeric_C,color="black", y=RMSSD)) + 
  geom_boxplot(aes(linetype = Site, group=interaction(Temperature,Site)), color = "black",position = position_dodge(width = 5), width = 4)+
  geom_point(aes(group=interaction(Temperature,Site)),color = "black",position= position_jitterdodge(jitter.width = 3, dodge.width = 5), alpha = 4, size=0.3)+
  geom_smooth(aes(linetype = Site, group = Site),color = "black")+
  geom_point(data =Plot_data, aes(x=Temperature_numeric_C, group= Site,y=Mean),color="red",position = position_dodge(width = 5))+ 
  geom_errorbar(data=Plot_data, aes(x = Temperature_numeric_C,ymax= Max_CI, ymin =Min_CI, group=Site), position = position_dodge(width = 5),color="red", width = 1, inherit.aes = FALSE) + 
  
  facet_grid(cols=vars(Direction))+
  scale_linetype_manual(values = shapes) +
  theme_classic() + labs(x = "Temperature (°C)", y= "RMSSD (ms)")+
  theme(legend.position = "top") + coord_cartesian(ylim = c(0,400))


emm_tem <- emmeans(
  Model_RMSSD,
  ~ Temperature | Site * Direction, type = "response")
Values_tem <- contrast(emm_tem, "revpairwise", adjust = "bonferroni") |> confint()
Values_tem <- contrast(emm_tem, "revpairwise", adjust = "bonferroni") 
summary(Values_tem, infer = TRUE)

emm_site <- emmeans(
  Model_RMSSD, 
  ~ Site | Direction * Temperature, type = "response")
Values_site <- contrast(emm_site, "revpairwise", adjust = "bonferroni") |> confint()
Values_site <- contrast(emm_site, "revpairwise", adjust = "bonferroni") 
summary(Values_site, infer = TRUE)

emm_grad <- emmeans(
  Model_RMSSD,
  ~ Direction | Site * Temperature, type = "response"
)
contrast(emm_grad, "pairwise", adjust = "bonferroni") |> confint()
Values_grad <- contrast(emm_grad, "revpairwise", adjust = "bonferroni") 
summary(Values_grad, infer = TRUE)

#SDNN

full_model <- glmmTMB(log(SDNN) ~  Temperature * Site  * Direction * Body_mass_g  + (1 | Snail),data=data_exp_1, family = gaussian())

qqnorm(resid(full_model))
qqline(resid(full_model))

Model_res <- simulateResiduals(full_model, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)


Model_1 <- glmmTMB(log(SDNN) ~  Temperature * Site  * Direction * Body_mass_g  + (1 | Snail ) ,data=data_exp_1, family = gaussian())
Model_2 <- glmmTMB(log(SDNN) ~  Temperature * Site  * Direction + Body_mass_g  + (1 | Snail ) ,data=data_exp_1, family = gaussian())
Model_3 <- glmmTMB(log(SDNN) ~  (Temperature + Site + Direction)^2 + Body_mass_g + (1 | Snail),data = data_exp_1, family = gaussian())
Model_4 <- glmmTMB(log(SDNN) ~  Temperature + Site + Direction+ Body_mass_g + (1 | Snail),data = data_exp_1, family = gaussian())

Model_res <- simulateResiduals(Model_1, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_res <- simulateResiduals(Model_2, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_res <- simulateResiduals(Model_3, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)


Model_res <- simulateResiduals(Model_4, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

AIC(Model_1, Model_2, Model_3, Model_4)
anova(Model_1, Model_2, Model_3, Model_4)

Model_SDNN <- glmmTMB(log(SDNN) ~  Temperature + Site + Direction+ Body_mass_g + (1 | Snail),data = data_exp_1, family = gaussian())

data <-  expand.grid(Temperature = factor(levels(data_exp_1$Temperature), levels=levels(data_exp_1$Temperature)),Site = factor(levels(data_exp_1$Site), levels=levels(data_exp_1$Site)), Direction = factor(levels(data_exp_1$Direction), levels=levels(data_exp_1$Direction)),   Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE))
predictions <- predict(Model_SDNN, newdata = data, re.form = ~0, se.fit=TRUE) 

Plot_data <- data |>  data.frame(
  Mean = predictions$fit,
  SE = predictions$se.fit) |> dplyr::mutate(
    Max_CI = Mean + 1.96 * SE, 
    Min_CI = Mean - 1.96 * SE, 
    Temperature = factor(Temperature, levels = c("3.5C","22C","40C")),
    Temperature_numeric_C = as.numeric(
      sub("C", "", as.character(Temperature)))) |> mutate(Mean = exp(Mean), Max_CI=exp(Max_CI), Min_CI=exp(Min_CI))

shapes = c("solid","dashed")
ggplot(data_exp_1, aes(x=Temperature_numeric_C,color="black", y=SDNN)) + 
  geom_boxplot(aes(linetype = Site, group=interaction(Temperature,Site)), color = "black",position = position_dodge(width = 5), width = 4)+
  geom_point(aes(group=interaction(Temperature,Site)),color = "black",position= position_jitterdodge(jitter.width = 3, dodge.width = 5), alpha = 4, size=0.3)+
  geom_smooth(aes(linetype = Site, group = Site),color = "black")+
  geom_point(data =Plot_data, aes(x=Temperature_numeric_C, group= Site,y=Mean),color="red",position = position_dodge(width = 5))+ 
  geom_errorbar(data=Plot_data, aes(x = Temperature_numeric_C,ymax= Max_CI, ymin =Min_CI, group=Site), position = position_dodge(width = 5),color="red", width = 1, inherit.aes = FALSE) + 
  
  facet_grid(cols=vars(Direction))+
  scale_linetype_manual(values = shapes) +
  theme_classic() + labs(x = "Temperature (°C)", y= "SDNN (ms)")+
  theme(legend.position = "top") + coord_cartesian(ylim = c(0,400))


emm_tem <- emmeans(
  Model_SDNN,
  ~ Temperature | Site * Direction, type = "response")
Values_tem <- contrast(emm_tem, "revpairwise", adjust = "bonferroni") |> confint()
Values_tem <- contrast(emm_tem, "revpairwise", adjust = "bonferroni") 
summary(Values_tem, infer = TRUE)

emm_site <- emmeans(
  Model_SDNN, 
  ~ Site | Direction * Temperature, type = "response")
Values_site <- contrast(emm_site, "revpairwise", adjust = "bonferroni") |> confint()
Values_site <- contrast(emm_site, "revpairwise", adjust = "bonferroni") 
summary(Values_site, infer = TRUE)

emm_grad <- emmeans(
  Model_SDNN,
  ~ Direction | Site * Temperature, type = "response"
)
contrast(emm_grad, "pairwise", adjust = "bonferroni") |> confint()
Values_grad <- contrast(emm_grad, "revpairwise", adjust = "bonferroni") 
summary(Values_grad, infer = TRUE)



#pNN50

full_model <- glmmTMB(pNN50 ~  Temperature * Site  * Direction * Body_mass_g  + (1 | Snail),data=data_exp_1, family = gaussian())

qqnorm(resid(full_model))
qqline(resid(full_model))

Model_res <- simulateResiduals(full_model, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

var_1 <-update(full_model, dispformula = ~Temperature + Direction)

Model_res <- simulateResiduals(var_1, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)


var_2 <-update(full_model, dispformula = ~ Temperature * Direction)

Model_res <- simulateResiduals(var_2, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)


AIC(full_model, var_1, var_2)
anova(full_model, var_1, var_2)

Model_1 <- glmmTMB(pNN50 ~  Temperature * Site  * Direction * Body_mass_g  + (1 | Snail ) ,data=data_exp_1, dispformula = ~Temperature + Direction, family = gaussian())
Model_2 <- glmmTMB(pNN50 ~  Temperature * Site  * Direction + Body_mass_g  + (1 | Snail ) ,data=data_exp_1, dispformula = ~Temperature + Direction, family = gaussian())
Model_3 <- glmmTMB(pNN50~  (Temperature + Site + Direction)^2 + Body_mass_g + (1 | Snail),data = data_exp_1,dispformula = ~Temperature + Direction, family = gaussian())
Model_4 <- glmmTMB(pNN50 ~  Temperature + Site + Direction+ Body_mass_g + (1 | Snail),data = data_exp_1, dispformula = ~Temperature + Direction, family = gaussian())

Model_res <- simulateResiduals(Model_1, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_res <- simulateResiduals(Model_2, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

Model_res <- simulateResiduals(Model_3, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)


Model_res <- simulateResiduals(Model_4, n=2000)
plot(Model_res)
testCategorical(Model_res, data_exp_1$Temperature)
testCategorical(Model_res, data_exp_1$Site)
testCategorical(Model_res, data_exp_1$Direction)

AIC(Model_1, Model_2, Model_3, Model_4)
anova(Model_1, Model_2, Model_3, Model_4)

Model_pNN50 <- glmmTMB(pNN50 ~  Temperature + Site + Direction+ Body_mass_g + (1 | Snail),data = data_exp_1, dispformula = ~Temperature + Direction, family = gaussian())


data <-  expand.grid(Temperature = factor(levels(data_exp_1$Temperature), levels=levels(data_exp_1$Temperature)),Site = factor(levels(data_exp_1$Site), levels=levels(data_exp_1$Site)), Direction = factor(levels(data_exp_1$Direction), levels=levels(data_exp_1$Direction)),   Body_mass_g = mean(data_exp_1$Body_mass_g, na.rm = TRUE))
predictions <- predict(Model_pNN50, newdata = data, re.form = ~0, se.fit=TRUE) 

Plot_data <- data |>  data.frame(
  Mean = predictions$fit,
  SE = predictions$se.fit) |> dplyr::mutate(
    Max_CI = Mean + 1.96 * SE, 
    Min_CI = Mean - 1.96 * SE, 
    Temperature = factor(Temperature, levels = c("3.5C","22C","40C")),
    Temperature_numeric_C = as.numeric(
      sub("C", "", as.character(Temperature)))) |> mutate(Mean = Mean, Max_CI=Max_CI, Min_CI=Min_CI)

shapes = c("solid","dashed")
ggplot(data_exp_1, aes(x=Temperature_numeric_C,color="black", y=pNN50)) + 
  geom_boxplot(aes(linetype = Site, group=interaction(Temperature,Site)), color = "black",position = position_dodge(width = 5), width = 4)+
  geom_point(aes(group=interaction(Temperature,Site)),color = "black",position= position_jitterdodge(jitter.width = 3, dodge.width = 5), alpha = 4, size=0.3)+
  geom_smooth(aes(linetype = Site, group = Site),color = "black")+
  geom_point(data =Plot_data, aes(x=Temperature_numeric_C, group= Site,y=Mean),color="red",position = position_dodge(width = 5))+ 
  geom_errorbar(data=Plot_data, aes(x = Temperature_numeric_C,ymax= Max_CI, ymin =Min_CI, group=Site), position = position_dodge(width = 5),color="red", width = 1, inherit.aes = FALSE) + 
  
  facet_grid(cols=vars(Direction))+
  scale_linetype_manual(values = shapes) +
  theme_classic() + labs(x = "Temperature (°C)", y= "pNN50 (%)")+
  theme(legend.position = "top")


emm_tem <- emmeans(
  Model_pNN50,
  ~ Temperature | Site * Direction, type = "response")
Values_tem <- contrast(emm_tem, "revpairwise", adjust = "bonferroni") |> confint()
Values_tem <- contrast(emm_tem, "revpairwise", adjust = "bonferroni") 
summary(Values_tem, infer = TRUE)

emm_site <- emmeans(
  Model_pNN50, 
  ~ Site | Direction * Temperature, type = "response")
Values_site <- contrast(emm_site, "revpairwise", adjust = "bonferroni") |> confint()
Values_site <- contrast(emm_site, "revpairwise", adjust = "bonferroni") 
summary(Values_site, infer = TRUE)

emm_grad <- emmeans(
  Model_pNN50,
  ~ Direction | Site * Temperature, type = "response"
)
contrast(emm_grad, "pairwise", adjust = "bonferroni") |> confint()
Values_grad <- contrast(emm_grad, "revpairwise", adjust = "bonferroni") 
summary(Values_grad, infer = TRUE)

##Temperature vs mass
Model_1_mass <- glmmTMB(MeanHR ~ Temperature + Body_mass_g + (1 | Snail), data = data_exp_1, family = gaussian())
res <- simulateResiduals(Model_1_mass, n =2000)
plot(res)
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



######################
#Exp_2
######################
Exp_2_HR <- read_xlsx("C:/Users/jandr/Downloads/Data_Exp_2.xlsx", sheet="HR")
Exp_2_HR <- Exp_2_HR |>  mutate(Direction = factor(Direction))
Exp_2_HR$Elevation <- factor(as.numeric(as.character(Exp_2_HR$Elevation),
                                        levels = c("310","910", "1560","2300", "2560", "3010")))

Exp_2_Sensor <- read_xlsx("C:/Users/jandr/Downloads/Data_Exp_2.xlsx", sheet="Sensors")

plot_1 <-  ggplot(Exp_2_Sensor, aes(x= Time_s, y=Pressure_mmHg))+ 
  geom_point(size = 0.3, alpha=0.4) + geom_line(size = 0.3, alpha=0.4)+
  xlab("Time (s)") + ylab("Atmospheric pressure (mmHg)") +
  geom_vline(xintercept = 2630,, linetype = "dashed")+
  geom_vline(xintercept = 4260,, linetype = "dashed")+
  geom_vline(xintercept = 6375,, linetype = "dashed")+
  annotate("text", x = 300, y = 100*7.7, label = "310 masl", size = 3.5, color = "black", fontface = "bold")+ 
  annotate("text", x = 2930, y = 94*7.7, label = "910 masl", size = 3.5, color = "black", fontface = "bold")+ 
  annotate("text", x = 4560, y = 88*7.7, label = "1560  masl", size = 3.5, color = "black", fontface = "bold") +
  annotate("text", x = 6800, y = 80*7.7, label = "2300 masl", size = 3.5, color = "black", fontface = "bold")  + 
  theme_classic() 

plot_2 <-ggplot(Exp_2_Sensor, aes(x= Time_s, y=`Temperature_°C` ))+ 
  geom_point(size = 0.3, alpha=0.4) +
  geom_line(size = 0.3, alpha=0.4)  +
  
  xlab("Time (s)") + ylab("Temperature") + 
  geom_vline(xintercept = 2630,, linetype = "dashed")+
  geom_vline(xintercept = 4260,, linetype = "dashed")+
  geom_vline(xintercept = 6375,, linetype = "dashed")+
  
  annotate("text", x = 300, y =35, label = "310 masl", size = 3.5, color = "black", fontface = "bold")+ 
  annotate("text", x = 2930, y =39, label = "910 masl", size = 3.5, color = "black", fontface = "bold")+ 
  annotate("text", x = 4560, y = 38, label = "1560  masl", size = 3.5, color = "black", fontface = "bold") +
  annotate("text", x = 6800, y = 30, label = "2300 masl", size = 3.5, color = "black", fontface = "bold")  + 
  theme_classic() 

plots <- c(plot_1,plot_2)
grid.arrange( grobs = plots, ncols=1)

Exp_2_HR$Elevation <-  factor(as.numeric(Exp_2_HR$Elevation))

ggplot(Exp_2_HR,
       aes(x = Elevation,
           y = HR)) +
  geom_boxplot(
    aes(group = interaction(Elevation, Direction),
        linetype = Direction,  color = Direction),
    position = position_dodge2(padding = 0.4),  size = 0.6, width = 0.3,
    outlier.shape = NA
  ) +
  geom_point(
    aes(group = interaction(Elevation, Direction)), position = position_jitterdodge(dodge.width = 0.3, jitter.width = 0.1), size = 0.8, alpha = 0.8
  ) + geom_smooth(
    aes(group = Direction,
        linetype = Direction, color = Direction),
    se = TRUE,
    #color = "black",
    fill = "grey70",
    size = 0.8,
    fullrange = TRUE
  ) +  xlab("Elevation (masl)") +
  ylab("Heart Rate (BPM)")+
  theme_classic() +
  theme(legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggplot(Exp_2_HR, aes(x=(HR), color = Direction)) + geom_density()+ theme_classic()+ facet_grid(cols = vars(Elevation))

Linear_model_1 <- glmmTMB(log(HR) ~ Elevation * Direction,
                          data = Exp_2_HR)


qqnorm(resid(Linear_model_1))
qqline(resid(Linear_model_1))

res_linear_model <- simulateResiduals(Linear_model_1, n=2000)

testCategorical(res_linear_model, Exp_2_HR$Direction)
testCategorical(res_linear_model, Exp_2_HR$Elevation)

Linear_model_1 <- glmmTMB(log(HR) ~ Elevation * Direction,
                          data = Exp_2_HR)
Linear_model_2 <- glmmTMB(log(HR) ~ Elevation + Direction,
                          data = Exp_2_HR)


res_linear_model <- simulateResiduals(Linear_model_1, n=2000)
testCategorical(res_linear_model, Exp_2_HR$Direction)
testCategorical(res_linear_model, Exp_2_HR$Elevation)
plot(res_linear_model)

res_linear_model <- simulateResiduals(Linear_model_2, n=2000)
testCategorical(res_linear_model, Exp_2_HR$Direction)
testCategorical(res_linear_model, Exp_2_HR$Elevation)
plot(res_linear_model)

AIC(Linear_model_1, Linear_model_2)
anova(Linear_model_1, Linear_model_2)

Model_HR <- glmmTMB(log(HR) ~ Elevation * Direction,
                          data = Exp_2_HR)

emm_elevation <- emmeans(Model_HR, ~ Elevation | Direction, type = "response")
contrast(emm_elevation, "revpairwise", adjust = "bonferroni") |> confint()
Values_grad <- contrast(emm_elevation, "revpairwise", adjust = "bonferroni") 
summary(Values_grad)

emm_Direction <- emmeans(Model_HR, ~ Direction | Elevation, type = "response")
contrast(emm_Direction, "revpairwise", adjust = "bonferroni") |> confint()
Values_grad <- contrast(emm_Direction, "revpairwise", adjust = "bonferroni") 
summary(Values_grad)

##########Sensor_data_exp_1
data_sensors_exp1 <- read_xlsx("C:/Users/jandr/Downloads/Data_Exp_1.xlsx", sheet="Sensors")
summary_Data <- data_sensors_exp1 |> select(-contains("Time")) |> summarise(across(where(is.numeric),list(Mean = ~mean(.x, na.rm = TRUE), SD =~sd(.x,na.rm = TRUE)))) 
summary_Data <- summary_Data |>  pivot_longer(cols =everything(),names_to = c("Variable", ".value"), names_pattern = "^(.*)_(Mean|SD)$") 
View(summary_Data)
