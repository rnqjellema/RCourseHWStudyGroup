# clear the workspace ----
rm(list =ls())

# load packages
library(fansi)
library(tidyverse)
library(janitor)
library(here)
library(ggbeeswarm)

# load data
load("~/Documents/GitHub/RCourseHWStudyGroup/data/Example_LongFormatHashed.RData")

# clean data
long_data <- clean_names(Example_LongFormat)
view(long_data)

#plot data -----
long_data %>%
  na.omit() %>%
  ggplot(aes(x=geslacht, y=vas_pijn_gemiddeld_1, )) 
+ geom_point()

long_data %>%
  ggplot(aes(x=geslacht, y=vas_pijn_gemiddeld_1, )) 
+ geom_jitter()

long_data %>%
  ggplot(aes(x=geslacht, y=vas_pijn_gemiddeld_1, )) 
+ geom_quasirandom()

long_data %>%
  na.omit() %>%
  ggplot(aes(x=rounddescription, y=vas_pijn_gemiddeld_1, color=geslacht )) + 
  geom_jitter() + 
  coord_flip()

# facet wrap plot and filter------
long_data %>%
  na.omit() %>%
  filter (vas_pijn_gemiddeld_1 < 100)%>%
  filter (vas_pijn_gemiddeld_1 > 0) %>%
  ggplot(aes(x=geslacht, y=vas_pijn_gemiddeld_1, color = geslacht)) +
  geom_jitter()+
  facet_wrap(~rounddescription)

# Save plot bij export or by
ggsave('ggplot.png')

# plot with bars of stderr -------
long_data %>%
  na.omit() %>%
  group_by(geslacht)%>%
  summarise(mean = mean(vas_pijn_gemiddeld_1),
            sd= sd(vas_pijn_gemiddeld_1),
            n= n(),
            stderr= sd/sqrt(n))%>%
  ggplot(aes(x=geslacht, y=mean))+
  geom_col()+
  coord_flip()+
  geom_errorbar(aes(x=geslacht , ymin= mean-stderr, ymax= mean + stderr))

# correlations and scatterplots-------
long_data %>%
  na.omit %>%
  ggplot(aes(x=vas_pijn_gemiddeld_1,y=vas_functie_1, color=geslacht))+
  geom_point()+
  geom_smooth()

# theme's
long_data %>%
  na.omit %>%
  ggplot(aes(x=vas_pijn_gemiddeld_1,y=vas_functie_1, color=geslacht))+
  geom_point()+
  geom_smooth()+
  theme_minimal()

# adding title, subtitle, caption and axis.---------
long_data %>%
  na.omit %>%
  ggplot(aes(x=vas_pijn_gemiddeld_1,y=vas_functie_1, color=geslacht))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  labs(title = "relationship between pain and hand function",
       x= "Vas pijn (0-10)",
       y= "Vas Functie (0-10)")

  
  
  