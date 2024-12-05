library(tidyverse)
library(nlme)
library(broom)
library(finalfit)
library(gtsummary)
library(lmtest)
library(lme4)
library(clubSandwich)
library(patchwork)
library(splines)
theme_set(theme_bw())


df <- heart_df %>% 
  mutate(
    age = age %>% ff_label("Age(years)"),
    sex = if_else(sex ==0,"Male","Female") %>% 
      fct_relevel("Female") %>% 
      ff_label("Sex"),
    redo = if_else(redo ==0,"No","Yes") %>% 
      fct_relevel("No") %>% 
      ff_label("Previous Cardiac Surgery"),
    emerg = if_else(emerg == 0,"Elective","Emergent") %>% 
      fct_relevel("Elective") %>% 
      ff_label("Operative Urgency"),
    dm = if_else(dm ==0,"No","Yes") %>% 
      fct_relevel("No") %>% 
      ff_label("Preoperative Diabetes"),
    type = if_else(type ==1,"Human Tissue","Porcine Tissue") %>% 
      fct_relevel("Porcine Tissue") %>% 
      ff_label("Type of Valve"),
    lvmi =lvmi %>%  ff_label("LVMI"),
    futime = futime %>% ff_label("Follow-up Time(years)"),
    status = if_else(status == 0,"Censored","Death") %>% 
      fct_relevel("Censored") %>% 
      ff_label("Status")
  )























































































