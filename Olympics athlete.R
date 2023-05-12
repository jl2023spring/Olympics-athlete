library(tidyverse)

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

olympic_gymnasts <- olympics %>% 
  filter(!is.na(age)) %>%             # only keep athletes with known age
  filter(sport == "Gymnastics") %>%   # keep only gymnasts
  mutate(
    medalist = case_when(             # add column for success in medaling
      is.na(medal) ~ FALSE,           # NA values go to FALSE
      !is.na(medal) ~ TRUE            # non-NA values (Gold, Silver, Bronze) go to TRUE
    )
  )
head(olympic_gymnasts)



# First we plot the gymnast's age distribution as violins.
ggplot(olympic_gymnasts,aes(sex, age, fill = medalist))+
  geom_violin()+
  scale_x_discrete(
    name = '',
    labels = c('Female','Male')
  )+
  scale_y_continuous(
    name = 'Age'
  )+
  scale_fill_discrete(
    name = NULL,
    labels = c('did not medal', 'medaled')
  )+
  ggtitle("Gymnast's age distribution")+
  theme_bw(12)


# Then we plot the age distribution over the years(using the column 'games' which is a discrete variable) as boxplot.
q<-ggplot(olympic_gymnasts, aes(games, age, fill=sex))+
  geom_boxplot()+
  scale_x_discrete(name = '')+
  scale_fill_discrete(name = '',labels = c('Female','Male'))+
  facet_wrap(~ medalist, 
             nrow = 2,
             # use `as_labeller` to rename the labels
             labeller = as_labeller(c(`TRUE` = "Medaled", `FALSE` = "Did not medal"))
  )
# rotate x axis labels to be vertical
q+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Gymnast's age distribution change over the years")