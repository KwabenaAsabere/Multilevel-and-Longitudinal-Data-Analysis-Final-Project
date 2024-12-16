

df_model <- model.null %>% 
  predict(se = TRUE) %>% 
   as_tibble() %>% 
  mutate(id = df$id,
    preds = exp(fit),
    lower_ci = exp(fit - 1.96 * se.fit),
    upper_ci = exp(fit + 1.96 * se.fit),
    lvmi = df$lvmi,
    time = df$time
  ) %>% 
  select(id,time,lvmi,preds,lower_ci,upper_ci,everything())


ggplot(data=,aes(x=time, y=fit)) +
  geom_line() +
  geom_abline(intercept=0,slope=0,col="red") +
  geom_line(aes(x=time,y=ci_l),col="blue") +
  geom_line(aes(x=time,y=ci_u),col="blue")+
  labs(x = "Time in Years",
       y = "Mean LVMI Difference")
 
  
df_model %>% 
  ggplot(aes(x = time, y= preds,group = id))+
  geom_line()+
  geom_abline(intercept=0,slope=0,col="red") +
  geom_line(aes(x = time,y = lower_ci),color = "blue")+
  geom_line(aes(x = time, y =upper_ci),color = "blue")+
  labs(x = "Time in Years",
       y = "Mean LVMI Difference")



df_model_summary <- df_model %>% 
  group_by(time) %>% 
  summarise(mean_lvmi = mean(lvmi),
            mean_pred_lvmi = mean(preds)) 







df |> 
  ggplot()+
  geom_line(aes(group = id,x =time, y= lvmi),alpha = 0.2)+
  geom_smooth(data = lvmi_summary_type,aes(x= time, y = mean_lvmi),se = FALSE,color = "red")+
  geom_smooth(data = lvmi_summary_type,aes( x = time, y = mean_pred_lvmi),se = FALSE,color = "blue")+
  labs(x ="Years Since Valve Replacement",
       y = "Left Ventricular Mass Index",
       title ="LVMI Regression by Type of Valve: Observed vs Predicted Trajectory")+
  theme(legend.position = c(1,1),legend.justification = c(1,1),
        plot.title = element_text(hjust = 0.5, size = 14, fac="bold"))

library(ggplot2)

df_model %>% 
  ggplot() +
  geom_line(aes(group = id, x = time, y = lvmi), alpha = 0.2) +
  geom_smooth(data = lvmi_summary_type, aes(x = time, y = mean_lvmi, color = "Observed"), se = FALSE) +
  geom_smooth(data = lvmi_summary_type, aes(x = time, y = mean_pred_lvmi, color = "Predicted"), se = FALSE) +
  labs(
    x = "Years Since Valve Replacement",
    y = "Left Ventricular Mass Index",
    title = "LVMI Regression by Type of Valve: Observed vs Predicted Trajectory",
    color = "Trajectory Type"
  ) +
  theme(
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )



df_model_pop <- model_interaction %>% 
  predict(se = TRUE,re.form = NA) %>% 
  as_tibble() %>% 
  mutate(id = df$id,
         preds = exp(fit),
         lower_ci = exp(fit - 1.96 * se.fit),
         upper_ci = exp(fit + 1.96 * se.fit),
         lvmi = df$lvmi,
         time = df$time
  ) %>% 
  select(id,time,lvmi,preds,lower_ci,upper_ci,everything())

df_model_pop %>% 
  ggplot(aes(x = time, y= preds))+
  geom_line()+
  geom_abline(intercept=0,slope=0,col="red") +
  geom_smooth(aes(x = time,y = lower_ci),color = "blue")+
  geom_smooth(aes(x = time, y =upper_ci),color = "blue")+
  labs(x = "Time in Years",
       y = "Mean LVMI Difference")



df_model_pop %>%
  ggplot(aes(x = time, y = preds)) +
  geom_line(color = "black") +
  geom_abline(intercept = 0, slope = 0, col = "red") +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.2) +
  labs(
    x = "Time in Years",
    y = "Mean LVMI Difference"
  )





# Manual Method
manual_fit <- as.vector(A %*% b)

# Predict Function Method
predict_fit <- predict(model_interaction, se = FALSE)
all.equal(manual_fit, predict_fit)


df_model_pop <- model_interaction %>% 
  predict(se = TRUE,re.form = NA) %>% 
  as_tibble() %>% 
  mutate(id = df$id,
         preds = exp(fit),
         lower_ci = exp(fit - 1.96 * se.fit),
         upper_ci = exp(fit + 1.96 * se.fit),
         lvmi = df$lvmi,
         time = df$time
  ) %>% 
  select(id,time,lvmi,preds,lower_ci,upper_ci,everything())


new_data <- expand.grid(
  time = seq(min(0, max(df$time), length.out = 100),  
  type = unique(df$type)                                  
)%>% 
  mutate(age = mean(df$age,na.rm = TRUE),
         sex = "Male",
         redo = "No",
         emerg = "Elective",
         dm = "No")


vars_mode <- function(vars){
  df %>% 
    count({{vars}}) %>% 
    filter(n == max(n))
}

df %>% select(sex,redo,emerg,dm,type) %>% 
  map_dfr(vars_mode)


df_model_pop <- model_interaction %>% 
  predict(se = TRUE,re.form = NA,newdata = new_data) %>% 
  as_tibble() %>% 
  mutate(
         preds = exp(fit),
         lower_ci = exp(fit - 1.96 * se.fit),
         upper_ci = exp(fit + 1.96 * se.fit))

df_predicted <- bind_cols(new_data,df_model_pop)

df_predicted %>% 
  ggplot(aes(x = time, y= preds))+
  geom_line()+
  geom_abline(intercept=0,slope=0,col="red") +
  geom_line(aes(x = time,y = lower_ci))+
  geom_line(aes(x = time, y =upper_ci))+
  labs(x = "Time in Years",
       y = "Mean LVMI Difference")




null_data <- tibble(
  time = seq(0, max(df$time), length.out = 100),
    age = mean(df$age,na.rm = TRUE),
           sex = "Male",
           redo = "No",
           emerg = "Elective",
           dm = "No",
           type = "Porcine Tissue")



df_model_pop <- model.null %>% 
  predict(se = TRUE,re.form = NA,newdata = null_data) %>% 
  as_tibble() %>% 
  mutate(
    preds = exp(fit),
    lower_ci = exp(fit - 1.96 * se.fit),
    upper_ci = exp(fit + 1.96 * se.fit))

df_predicted <- bind_cols(null_data,df_model_pop)


df_predicted %>% 
  ggplot(aes(x = time, y= preds))+
  geom_line()+
  geom_abline(intercept=0,slope=0,col="red") +
  geom_line(aes(x = time,y = lower_ci))+
  geom_line(aes(x = time, y =upper_ci))+
  labs(x = "Time in Years",
       y = "Mean LVMI Difference")





df_predicted %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = preds), color = "blue", size = 1) +
  geom_line(aes(y = lower_ci), color = "red", linetype = "dashed",size = 0.8) +
  geom_line(aes(y = upper_ci), color = "red", linetype = "dashed",size = 0.8) +
  geom_abline(intercept = 0, slope = 0, color = "red") +
  labs(x = "Time in Years",
       y = "Mean LVMI Difference",
       title = "Predicted LVMI with 95% CI")+

theme(
  legend.position = c(1, 1),
  legend.justification = c(1, 1),
  plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
)

range(df$age)
