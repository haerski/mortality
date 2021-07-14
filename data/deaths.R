library(tidyverse)
deaths <- read_csv("covid_deaths_usafacts.csv")
deaths <- deaths %>% select(1,5:542) %>% pivot_longer(2:539,names_to= "dates",values_to = "deaths")
deaths$dates <- parse_date(deaths$dates)

deaths <- deaths %>% filter(countyFIPS!=0)

deaths %>% mutate(countyFIPS=as.character(countyFIPS)) %>% mutate(countyFIPS=str_pad(countyFIPS,5,pad="0")) %>% 
  mutate(state=str_sub(countyFIPS,1,2),county=str_sub(countyFIPS,3,5)) %>% 
  select(-countyFIPS)


deaths %>% filter(countyFIPS==20045) %>% 
  ggplot(aes(x=dates,y=deaths))+geom_line()

