---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: This project shows the percentage of people per party who voted pro Brexit
draft: false
keywords: ""
slug: magna
title: Brexit
image: brexit.png
---

During this coding project the aim was to compare the four major UK parties Conservative,
Labour, UKIP and Liberal Democrats accorind to the percetage that voted for leaving the EU. To visualise the results, I decided to use a scatterplot.

First I wanted to have a look at the dataset provided and therefore used the skim and summary function of R.

```{r}

brexit_results <- vroom::vroom(here::here("data", "brexit_results.csv"))
skim(brexit_results)
summary(brexit_results)

```
As the dataframe had four different columns for each party, I decided to create a new dataframe that has a column called party and includes all four parties.

```{r}
#adding column called party to link all parties 
brexit_long <- gather(brexit_results, party, percentage, con_2015:ukip_2015, factor_key=TRUE)
```

In this part I plotted the actual graph using the ggplot2 package of R. I used the official color of the parties and customized the graph.

```{r}

ggplot(brexit_long, aes(x = percentage ,y=leave_share, color = party))+
  geom_point(size = 2, alpha=0.5) +
  geom_smooth(method = lm) +
  labs(title = "How political affiliation translated to Brexit Voting", 
       x = "Party % in the UK 2015 general election", 
       y = "Leave % in the 2016 Brexit referendum") +
  scale_x_discrete(expand=c(0.05,0.05),limits=c(0 ,20, 40, 60, 80))+
  scale_color_manual(values = c("#0087DC","#DC241f","#FDBB30","#EFE600"), name = "", labels = c("Conservative", "Labour", "Lib Dems", "UKIP") ) +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal", plot.title =element_text(face="bold")) 
  

```