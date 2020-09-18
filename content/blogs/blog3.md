---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description: This project shows the death rate per age group by contrasting female and male
draft: false
image:  covid_death_rate.png
image: covid_death_rate_comorbidities.png
keywords: ""
slug: tempus
title: Covid
---

After having imported and looked at the date, I discovered many different forms of unknowns and decided to clear the dataframe by removing those. 

```{r, covid_death_rate2, fig7, fig.width = 12, fig.height=7}

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
  
```  
I am now plotting the graph using ggplot.

```
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
