library(tidyverse)
library(haven)
library(finalfit)
library(gtsummary)
heart_df <- read.table("aortic_valve.dat",header = TRUE)

heart_df %>% 
  ggplot(aes(x =futime))+
  geom_histogram(fill = "steelblue",color = "black",bins = 20)+
  labs( x= "Total follow-up time from date of surgery (years)",
        y = "Frequency",
        title = "Distribution of Total Follow-up Time")
heart_df %>% head()

heart_df %>% 
  summarise(subjects = n_distinct(id))


df <- heart_df %>% 
  mutate(
    age = age %>% ff_label("Age(years)"),
    sex = if_else(sex ==0,"Male","Female") %>% 
      fct_relevel("Female") %>% 
      ff_label("Sex"),
    redo = if_else(redo ==0,"No","Yes") %>% 
      fct_relevel("No") %>% 
      ff_label("Previous Cardiac Surgery"),
    emerg = if_else(emerg == 0,"No","Yes") %>% 
      fct_relevel("No") %>% 
      ff_label("Operative Urgency"),
    dm = if_else(dm ==0,"No","Yes") %>% 
      fct_relevel("No") %>% 
      ff_label("Preoperative Diabetes"),
    type = if_else(type ==1,"Human Tissue","Porcine Tissue") %>% 
      fct_relevel("Porcine Tissue") %>% 
      ff_label("Type of Valve"),
    lvmi =lvmi %>%  ff_label("LVMI"),
    futime = futime %>% ff_label("Follow-up Time(years)")
  )


df %>% select(-c(time,id,status)) %>% 
  tbl_summary(
    statistic =
      list(all_categorical() ~ "{n} ({p}%)",
           all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_categorical() ~ 0,
                  all_continuous() ~ 0),
  ) %>%
  #add_overall() %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_footnote(update = all_stat_cols() ~
      "*mean(standard deviation) for continuous; n(%) for categorical*")


df %>% 
  ggplot(aes())


n <- length(unique(df$id))

lvmi_summary_time <- df |> 
  group_by(time) |> 
  summarise(
    avg_lvmi = mean(lvmi,na.rm = TRUE),
    sd_lvmi = sd(lvmi,na.rm = TRUE),
    med_lvmi = median(lvmi,na.rm = TRUE),
    q75_lvmi = quantile(lvmi,0.75),
    q25_lvmi = quantile(lvmi,0.25)
  )

lvmi_summary_time
head(df)

lvmi_summary_id <- df |> 
  group_by(id) |> 
  summarise(
    avg_lvmi = mean(lvmi,na.rm = TRUE),
    sd_lvmi = sd(lvmi,na.rm = TRUE),
    med_lvmi = median(lvmi,na.rm = TRUE),
    q75_lvmi = quantile(lvmi,0.75),
    q25_lvmi = quantile(lvmi,0.25)
  )
lvmi_summary_id

lvmi_summary_time |> 
  ggplot()+
  geom_point(aes(x = time,y = avg_lvmi),color = "red")+
  geom_line(aes(x = time,y = avg_lvmi),color = "red",linewidth = 1)+
  geom_pointrange(aes(x = time,y = avg_lvmi,ymin = avg_lvmi - 1.96*sd_lvmi/sqrt(n),
                      ymax = avg_lvmi + 1.96*sd_lvmi/sqrt(n)))+
  theme(axis.text = element_text(size = 10))+
  xlab("Visit")+
  ylab("SF-36 Mental")+
  ggtitle("Average SF-36-Mental vs Visit")


df |> 
  ggplot()+
  geom_line(aes(group = id,x =time, y= lvmi),color = "grey",alpha = 0.8)+
  theme_classic()+
  xlab("Time")+
  ylab("Left Ventricular Mass Index")
  

df %>% group_by(id,time) %>% 
  mutate(mean_lvmi = mean(lvmi,na.rm = TRUE)) %>% 
  ungroup %>% 
  ggplot()+
  geom_line(aes(x = time, y= lvmi,group =id))+
  geom_line(data = lvmi_summary_time,aes(x = time,y = avg_lvmi),color = "red")
  


lvmi_summary_id


df |> 
  ggplot()+
  geom_line(aes(group = id,x =time, y= lvmi),color = "grey",alpha = 1)+
  geom_smooth(data = lvmi_summary,aes(group = status, x= time, y = mean_lvmi,color = status),se = FALSE)+
  theme_classic()+
  xlab("Years Since Valve Replacement")+
  ylab("Left Ventricular Mass Index")


df %>% select(-c(time,id)) %>% 
  tbl_summary(by = type,
              statistic =
                list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_categorical() ~ 0,
                            all_continuous() ~ 0)) %>%
  add_overall() %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_spanning_header(
    update = all_stat_cols() ~ "**Type of Valve**"
  ) %>% 
  modify_footnote(update = all_stat_cols() ~
                    "*mean(standard deviation) for continuous; n(%) for categorical*")


head(df)




lvmi_summary_time <-df  %>% 
  group_by(time) %>%  
  summarise(mean_lvmi = mean(lvmi),
            sd_lvmi = sd(lvmi),
            n = n(),
            lower_bound = mean_lvmi - 1.96*sd_lvmi/sqrt(n),
            upper_bound = mean_lvmi + 1.96*sd_lvmi/sqrt(n),
  )

head(lvmi_summary_time)

lvmi_summary_time %>% 
  ggplot()+
  #geom_point(aes( x= time, y = mean_lvmi))+
  geom_smooth(aes(x = time, y = mean_lvmi),se = FALSE)+
  geom_smooth(aes(x = time, y= lower_bound),color = "red",linetype = "dashed",se = FALSE)+
  geom_smooth(aes(x = time, y = upper_bound),color = "red",linetype = "dashed",se = FALSE)











