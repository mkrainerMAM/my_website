---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description: Nullam et orci eu lorem consequat tincidunt vivamus et sagittis magna sed nunc rhoncus condimentum sem. In efficitur ligula tate urna. Maecenas massa sed magna lacinia magna pellentesque lorem ipsum dolor. Nullam et orci eu lorem consequat tincidunt. Vivamus et sagittis tempus.
draft: false
image:  covid_death_rate.png
image: covid_death_rate_comorbidities.png
keywords: ""
slug: tempus
title: Tempus
---
CA_contributors_2016 <- vroom::vroom(here::here("data","CA_contributors_2016.csv"))
zipcodes_2016 <- vroom::vroom(here::here("data","zip_code_database.csv"))
```

As we have seen that the zipcodes in the zipcodes file are stored as characters. We first had to transform them into doubles to match the format of the zipcodes in the CA_contributors. In order to match zipcodes to the cities and so combine both datasets, we used left_join and called the final dataset 'mix'.

We now filtered for Trump and Clinton respectively and made a list of the top 10 cities that donated the most money.

```{r, Summary_Clinton}
library(scales)
library(patchwork)

#changing zip codes to double and joining with CA_contributors_2016 dataset
zipcodes_2016 <- zipcodes_2016 %>%
    mutate(zip=as.double(zip))
mix <- left_join( CA_contributors_2016, zipcodes_2016, by="zip")

#Summarising contribution amounts for Hillary Clinton
CA_Clinton <- mix %>%
      filter(cand_nm=="Clinton, Hillary Rodham")%>%
      group_by(primary_city) %>%
      summarise(totalAmount=sum(contb_receipt_amt)) %>%
      arrange(desc(totalAmount))%>%
      mutate(primary_city = fct_reorder(primary_city, totalAmount)) %>%
      head(10)
      
  CA_Clinton
``` 
 
```{r, Summary_Trump}  

#Summarising contribution amounts for Hillary Clinton
  CA_Trump <- mix %>%
      filter(cand_nm=="Trump, Donald J.")%>%
      group_by(primary_city) %>%
      summarise(totalAmount=sum(contb_receipt_amt)) %>%
      arrange(desc(totalAmount))%>%
      mutate(primary_city = fct_reorder(primary_city, totalAmount)) %>%
      head(10)
      
  CA_Trump
```  
We now plotted both graphs of Trump and Clinton through the patchwork library next to each other. To replicate the design, we used the theme_bw. 

```{r, fig3, fig.height = 5, fig.width = 10}

#Creating a bar plot for Clinton in descending order of contribution raised 
p1 <- ggplot(CA_Clinton, aes(x=totalAmount, y=primary_city)) + 
  geom_col(fill="#3182bd", size=0.30)  + 
  theme_bw() +
  facet_grid(~"Clinton, Hillary Rodham")+
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank()) + 
  scale_x_continuous(labels=dollar)

#Creating a bar plot for Trump in descending order of contribution raised  
p2 <- ggplot(CA_Trump, aes(x=totalAmount, y=primary_city)) + 
  geom_col(fill="#d73027", size=0.30) + 
  theme_bw() +
  facet_grid(~"Trump, Donald J.")+
  theme (axis.title.x = element_blank(), 
        axis.title.y = element_blank()) + 
  scale_x_continuous(labels=dollar)

#Using the patchwork library to display the plots side by side
wrap_plots(p1,p2) + labs(x="Amount raised") + theme(axis.title.x = element_text(face="bold", hjust=-0.41)) + plot_annotation("Where did candidates raise most money?") 

```

As we want to find the top 10 candidates who received the most money, we created the dataset cand_top. As a second step, we wanted to find the top 10 cities per candidate who received the most money. In order to do so we had to filter for these top 10 candidates in the mix dataset and save this result in the dataset top10. This dataset now had to be modified to only select the top 10 cities per candidate which we then plotted through ggplot. 

```{r, fig4, fig.height = 8, fig.width = 18}

library(tidytext)

#creating a tibble containing the top 10 candidates by total contribution amount 
cand_top <- mix %>%
  group_by(cand_nm) %>%
  summarise(total_contributions = sum(contb_receipt_amt)) %>%
  arrange(desc(total_contributions))  %>%
  head(10)


#grouping by primary city and candidate and then reordering within categories
top10 <-mix %>%
    filter(cand_nm %in% cand_top$cand_nm)%>%
    group_by(primary_city, cand_nm) %>%
    summarise(totalAmount =sum(contb_receipt_amt)) %>% 
    group_by(cand_nm) %>%
    top_n(10) %>% 
    ungroup %>% 
    #using reorder_within to reorder each category by total amount 
    mutate(cand_nm = as.factor(cand_nm),
           primary_city = reorder_within(primary_city, totalAmount, cand_nm)) 


    ggplot(top10, aes(primary_city, totalAmount)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~cand_nm, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    theme_bw()+
    scale_y_continuous(expand = c(0,0), labels=dollar) + #adding currency unit - dollars
    theme (axis.title.x = element_blank(), 
        axis.title.y = element_blank())  

