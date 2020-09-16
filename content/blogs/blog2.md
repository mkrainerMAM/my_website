---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Lorem Etiam Nullam
draft: false
keywords: ""
slug: magna
title: Brexit
image: brexit.png
---



We will first have a look at the Brexit dataframe.


```{r brexit_challenge, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "brexit.png"), error = FALSE)
```
We will now replicate the graph from above.

```{r}
#importing dataframe brexit
library(skimr)
library(ggplot2)
brexit_results <- vroom::vroom(here::here("data", "brexit_results.csv"))
skim(brexit_results)
summary(brexit_results)



#adding column called party to link all parties 
brexit_long <- gather(brexit_results, party, percentage, con_2015:ukip_2015, factor_key=TRUE)

#building our graph with ggplot
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