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

















