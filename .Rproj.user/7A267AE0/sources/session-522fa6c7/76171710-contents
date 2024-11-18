help(heart.valve)
library(joineR)

tbl_summary(
  statistic =
    list(all_categorical() ~ "{n} ({p}%)",
         all_continuous() ~ "{mean} ({sd})"),
  digits = list(all_categorical() ~ 0,
                all_continuous() ~ 0),
) %>%
  add_overall() %>%
  bold_labels() %>%
  italicize_levels() %>%
  %>%
  modify_footnote(
    update = all_stat_cols() ~
      "*mean(standard deviation) for continuous; n(%) for categorical;
  )

