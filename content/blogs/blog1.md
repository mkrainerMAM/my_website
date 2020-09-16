---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: ""
draft: false
image: challenge2.png
keywords: ""
slug: ipsum
title: Ipsum
---

Covid death rate % by age group, sex, and whether the patient had co-morbidities or not:

```{r, covid_death_rate2, fig6, fig.width = 12, fig.height=7}
#removing missing and unknown values
val_to_remove=c("Missing", "Unknown", "Other", NA)

death_rate_medcond <- covid_data %>% 
  select(medcond_yn, death_yn, sex, age_group) %>% 
  #removing missing and unknown values
  filter(!(death_yn %in% val_to_remove) ,
         !(medcond_yn %in% val_to_remove), 
         !(sex %in% val_to_remove),
         !(age_group %in% val_to_remove),
  ) %>%
  #assigning more meaningful names to variable medcond_yn
  mutate(death_bool = ifelse(death_yn=="Yes", 1, 0), 
         co_mor= ifelse(medcond_yn=="Yes", "With comorbidities", "Without comorbidities")) %>% 
  group_by(age_group, sex, co_mor) %>% 
  summarise(death_r = prop(death_bool))

ggplot(death_rate_medcond, aes(x=death_r, y=age_group)) +
  geom_col(fill="#1b5596", alpha=0.6) +
  #faceting by sex and presence of co-morbidities
  facet_grid(rows= vars(co_mor), cols= vars(sex))+
  scale_x_continuous(labels = scales::percent)+
  theme_bw()+
  geom_text(aes(label=round(death_r*100, 2)), position=position_dodge(width=0.8), hjust=-0.05, size=3)+
  labs (
    title = "Covid death % by age group, sex and presence of co-morbidities",
    caption="Source: CDC"
  )+
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
  )



```

Covid death rate % by age group, sex, and whether the patient was admited to Intensive Care Unit (ICU) or not:
```{r, covid_death_rate2, fig7, fig.width = 12, fig.height=7}

val_to_remove=c("Missing", "Unknown", "Other", NA)

death_rate_icu <- covid_data %>% 
  select(icu_yn, death_yn, sex, age_group) %>% 
  #removing missing and unknown values
  filter(!(death_yn %in% val_to_remove) ,
         !(icu_yn %in% val_to_remove), 
         !(sex %in% val_to_remove),
         !(age_group %in% val_to_remove),
  ) %>%
  #assigning more meaningful names to variable icu_yn
  mutate(death_bool = ifelse(death_yn=="Yes", 1, 0), 
         icu= ifelse(icu_yn=="Yes", "Admitted to ICU", "No ICU")) %>% 
  group_by(age_group, sex, icu) %>% 
  summarise(death_r = prop(death_bool))

ggplot(death_rate_icu, aes(x=death_r, y=age_group)) +
  geom_col(fill="#f2695c", alpha=0.7) +
  #faceting by sex and whether patient was admitted to the ICU
  facet_grid(rows= vars(icu), cols= vars(sex))+
  scale_x_continuous(labels = scales::percent)+
  theme_bw()+
  geom_text(aes(label=round(death_r*100, 2)), position=position_dodge(width=0.8), hjust=-0.05, size=2)+
  labs (
    title = "Covid death % by age group, sex and whether patient was admitted to ICU",
    caption="Source: CDC"
  )+
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
  )


