library(lme4)
df %>% 
  mutate(
    sex = if_else(sex == "Male",1,0),
    redo = if_else(redo == "No",0,1),
    emerg = if_else(emerg == "Elective",0,1)
  )




heart_df %>% head(10)
glimpse(heart_df)

heart_df <-  heart_df %>% 
  mutate(across(
    sex:type,
    as_factor
  )) %>% 
  mutate(log_lvmi = log(lvmi))

glimpse(heart_df)

model1 <- heart_df %>% 
  lmer(log_lvmi ~ time + age + sex + redo + emerg + dm + type + time:type +
         (time|id),REML = FALSE, data = .)

summary(model1)


model2 <- heart_df %>% 
  lmer(log_lvmi ~ time + age + sex + redo + emerg + dm + type  +
         (time|id),REML = FALSE, data = .)

summary(model2)

anova(model1,model2)

VarCorr(model2)
as_tibble(VarCorr(model2))
confint(model2,method = "boot", nsim = 2000)

model2_summ <- summary(model2)
model2_summ$residuals
model_resid <- model2_summ$residuals

resids <- as_tibble(model_resid) %>% 
  mutate(residuals = value) %>% 
  select(residuals)

preds <- predict(model2) %>% 
  as_tibble() %>% 
  mutate(preds = value) %>% 
  select(preds)

model2_diags <- cbind(preds,resids)


model2_diags %>% 
  ggplot(aes(sample = residuals))+
  stat_qq()+
  stat_qq_line()+
  labs(
    title = "Q-Q Plot of Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )+
  theme(plot.title = element_text(hjust = 0.5, size = 14, fac="bold"))


model2_diags %>% 
  ggplot(aes(x = preds, y = residuals))+
  geom_point(alpha = 0.3)+
  geom_hline(yintercept = 0,linetype = "dashed",color = "red",linewidth = 0.7)+
  labs(x = "Fitted", y = "Residuals")


hist(heart_df$time)
hist(sqrt(heart_df$time))

model3 <- heart_df %>% 
  lmer(log_lvmi ~  sqrt(time ) + age + sex + redo + emerg + dm + type  +
         (time|id),REML = FALSE, data = .)

summary(model3)
anova(model2,model3)


model3_summ <- summary(model3)

model3_resid <- model3_summ$residuals

resids <- as_tibble(model3_resid) %>% 
  mutate(residuals = value) %>% 
  select(residuals)

preds <- predict(model3) %>% 
  as_tibble() %>% 
  mutate(preds = value) %>% 
  select(preds)

model3_diags <- cbind(preds,resids)

model3_diags %>% 
  ggplot(aes(x = preds, y = residuals))+
  geom_point(alpha = 0.3)+
  geom_hline(yintercept = 0,linetype = "dashed",color = "red",linewidth = 0.7)+
  labs(x = "Fitted", y = "Residuals")


model3_diags %>% 
  ggplot(aes(sample = residuals))+
  stat_qq()+
  stat_qq_line()+
  labs(
    title = "Q-Q Plot of Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )+
  theme(plot.title = element_text(hjust = 0.5, size = 14, fac="bold"))




library(lmtest)

lrtest(model2,model3)
lrtest(model,model2)








heart_df %>% 
  ggplot(aes(sample = time))+
  stat_qq()+
  stat_qq_line()

heart_df %>% 
  ggplot(aes(sample = sqrt(time)))+
  stat_qq()+
  stat_qq_line()





df |> 
  ggplot()+
  geom_line(aes(group = id,x =time, y= lvmi,color = status),alpha = 0.3)+
  geom_smooth(data = lvmi_summary_status,aes(group = status, x= time, y = mean_lvmi,color = status),se = FALSE)+
  labs(x ="Years Since Valve Replacement",
       y = "Left Ventricular Mass Index",
       title = " Trajectory of LVMI Regression Post Aortic Valve Replacement Surgery by Event",
       color = "Event Status")+
  theme(legend.position = c(1,1),legend.justification = c(1,1),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))



df |> 
  ggplot()+
  geom_line(aes(group = id,x =time, y= lvmi,color = type),alpha = 0.2)+
  geom_smooth(data = lvmi_summary_type,aes(group = type, x= time, y = mean_lvmi,color = type),se = FALSE)+
  labs(x ="Years Since Valve Replacement",
       y = "Left Ventricular Mass Index",
       title = " Trajectory of LVMI Regression by Type of Valve",
       color = "Valve Type")+
  theme(legend.position = c(1,1),legend.justification = c(1,1),
        plot.title = element_text(hjust = 0.5, size = 14, fac="bold"))


heart_df %>% 
  ggplot(aes(sample = log(lvmi)))+
  stat_qq(color = "steelblue")+
  stat_qq_line()+
  labs(
    title = "Q-Q Plot of LVMI",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )+
  theme( plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))



heart_df %>% 
  ggplot(aes(sample = lvmi))+
  stat_qq(color = "steelblue")+
  stat_qq_line()+
  labs(
    title = "Q-Q Plot of LVMI",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )+
  theme( plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
         axis.title = element_text(size = 12),
         axis.text = element_text(size = 10))

plot(model)
qqnorm(residuals(model))
qqline(residuals(model))


qqnorm(residuals(model, type = "pearson"))
qqline(residuals(model, type = "pearson"))


tidy(model)







lvmi_summary_type$type_col = if_else(lvmi_summary_type$type == "Porcine Tissue","red","blue")
lvmi_summary_type
df$type_col = if_else(df$type == "Porcine Tissue","red","blue")


df |> 
  ggplot()+
  geom_line(aes(group = id,x =time, y= lvmi,color = type_col),alpha = 0.2)+
  geom_smooth(data = lvmi_summary_type,aes(group = type, x= time, y = mean_lvmi,color = type_col),se = FALSE)+
  labs(x ="Years Since Valve Replacement",
       y = "Left Ventricular Mass Index",
       title = " Trajectory of LVMI Regression by Type of Valve",
       color = "Valve Type")+
  theme(legend.position = c(1,1),legend.justification = c(1,1),
        plot.title = element_text(hjust = 0.5, size = 14, fac="bold"))


model4_diags %>% head()


df |> 
  ggplot()+
  geom_line(aes(group = id,x =time, y= lvmi,color = type_col),alpha = 0.2)+
  geom_smooth(data = lvmi_summary_type,aes(group = type, x= time, y = mean_lvmi,color = type_col),se = FALSE)+
  labs(x ="Years Since Valve Replacement",
       y = "Left Ventricular Mass Index",
       title = " Trajectory of LVMI Regression by Type of Valve",
       color = "Valve Type")+
  theme(legend.position = c(1,1),legend.justification = c(1,1),
        plot.title = element_text(hjust = 0.5, size = 14, fac="bold"))


  

preds4 <- predict(model4)
df$preds = exp(preds4)

df %>% head()

lvmi_summary_type <- df %>% 
  group_by(type,time) %>% 
  summarise(mean_lvmi = mean(lvmi),
            mean_pred_lvmi = mean(preds)) %>% 
  mutate(type_col = if_else(type == "Porcine Tissue","red","blue"))

lvmi_summary_type %>% head()

ggplot()+
  geom_line(data = df,aes(group = id,x =time, y= preds,color = type),alpha = 0.2)+
  geom_smooth(data = lvmi_summary_type,aes( group = type,x = time, y = mean_pred_lvmi,color = type),
              se = FALSE)+
  labs(x ="Years Since Valve Replacement",
       y = " Predicted Left Ventricular Mass Index",
       title = " Trajectory of Predicted LVMI Regression by Type of Valve",
       color = "Valve Type")+
  theme(legend.position = c(1,1),legend.justification = c(1,1),
        plot.title = element_text(hjust = 0.5, size = 14, fac="bold"))


summary(model4)


round(matrix(summary(model4)$vcov,ncol= 6),5)
            

length(fixef(model4))

model4.null <-  heart_df %>% 
  lmer(log_lvmi ~ sqrt(time) + age + sex + redo + emerg + dm + 
         (sqrt(time)|id),REML = FALSE, data = .)

summary(model4.null)


lvmi_summary_type


ggplot()+
  geom_smooth(data = lvmi_summary_type,aes(group = type, x= sqrt_time, y = mean_lvmi,color = type,linetype = type),se = FALSE)+
  geom_smooth(data = lvmi_summary_type,aes(group= type, x = sqrt_time, y = mean_pred_lvmi,color = type,linetype = type),se = FALSE)+
  labs(x ="Years Since Valve Replacement",
       y = "Left Ventricular Mass Index",
       title ="LVMI Regression by Type of Valve: Observed vs Predicted Trajectory",
       color = "Valve Type",
       linetype = "Valve Type")+
  theme(legend.position = c(1,1),legend.justification = c(1,1),
        plot.title = element_text(hjust = 0.5, size = 14, fac="bold"))



lvmi_summary_type <- df %>% 
  group_by(type,sqrttime) %>% 
  summarise(mean_lvmi = mean(lvmi),
            mean_pred_lvmi = mean(preds)) %>% 
  mutate(type_col = if_else(type == "Porcine Tissue","red","blue"))


V_robust = vcovCR(model4, type="CR1",form = "sandwich")

V_robust

 mod4_sum <- summary(model4)
names(mod4_sum)

library(patchwork)

q <- heart_df %>% 
  ggplot(aes(sample = lvmi))+
  stat_qq()+
  stat_qq_line()+
  labs(
    title = "Q-Q Plot of LVMI",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"))

h <- heart_df %>% 
  ggplot(aes(x = lvmi))+
  geom_histogram(fill = "steelblue",color = "white")+
  labs(x = "Left Ventricular Mass Index",
       y = "Frequency",
       title = "Distribution of LVMI")+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold") )

q + h



q1 <- heart_df %>% 
  ggplot(aes(sample = log(lvmi)))+
  stat_qq()+
  stat_qq_line()+
  labs(
    title = " LOG-TRANSFORMED LVMI",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"))

h1 <- heart_df %>% 
  ggplot(aes(x = log(lvmi)))+
  geom_histogram(fill = "steelblue",color = "white")+
  labs(x = "Left Ventricular Mass Index",
       y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"))

q1 + h1
                                  



tbl_regression(model1)

q2 <- heart_df %>% 
  ggplot(aes(sample = time ))+
  stat_qq()+
  stat_qq_line()+
  labs(
    title = "Q-Q Plot of Time",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"))


h2 <- heart_df %>% 
  ggplot(aes(x = time))+
  geom_histogram(fill = "steelblue",color = "white")+
  labs(x = "Left Ventricular Mass Index",
       y = "Frequency",
       title = "Distribution of Time")+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"))

q2+ h2

                

q3 <- heart_df %>% 
  ggplot(aes(sample = sqrt(time)))+
  stat_qq()+
  stat_qq_line()+
  labs(
    title = "Q-Q Plot",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold"))

h3 <- heart_df %>% 
  ggplot(aes(x = sqrt(time)))+
  geom_histogram(fill = "steelblue",color = "white", bins = 13)+
  labs(x = "Left Ventricular Mass Index",
       y = "Frequency",
       title = "Square root Transformed Time")+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face="bold") )

q3+ h3

lrtest(model1,model4)

















