@@ -0,0 +1,207 @@
  ```{r}
library(tidyverse)
library(tidymodels)
library(feather)
library(tidymodels)
library(ggplot2)
```

```{r}
# Helper packages
library(readr)       # for importing data
library(vip)         # for variable importance plots

# Load it instead of reprocessing
per <- read_feather("all_persons.feather")

# This is how much we expect to pay for each client in a year
exp <-
  per %>%
  group_by(client) %>%
  summarize(expected = sum(qx * FaceAmt), zip3 = first(zip3))

# This is how much we payed for each client in 2019
act_2019 <-
  per %>%
  filter(year == 2019) %>%
  group_by(client) %>%
  summarize(actual2019 = sum(FaceAmt))
act_2020 <-
  per %>%
  filter(year == 2020) %>%
  group_by(client) %>%
  summarize(actual2020 = sum(FaceAmt))
act_2021 <-
  per %>%
  filter(year == 2021) %>%
  group_by(client) %>%
  summarize(actual2021 = sum(FaceAmt))


exp <-
  exp %>%
  left_join(act_2019) %>%
  left_join(act_2020) %>%
  left_join(act_2021) %>%
  replace_na(list(actual2019 = 0)) %>%
  replace_na(list(actual2020 = 0)) %>%
  replace_na(list(actual2021 = 0)) %>%
  mutate(AE2019 = actual2019 / expected) %>%
  mutate(AE2020 = actual2020 / expected) %>%
  mutate(AE2021 = actual2021 / ((7 / 12) * expected))

exp
```

```{r}
other_data <-
  read_feather("data.feather") %>%
  rename(covid_deaths = `Deaths involving COVID-19`, svi = `Social Vulnerability Index (SVI)`, cvac = `CVAC level of concern for vaccination rollout`) %>%
  mutate(density = POP / AREALAND, AREALAND = NULL)

data <-
  exp %>%
  left_join(other_data) %>%
  mutate(zip3 = factor(zip3)) %>%
  mutate(Y = factor(AE2020 > 1.1)) %>%
  mutate( Z= factor(AE2021>1.1)) %>%
  mutate(X= factor(AE2019 > 1.1))

glimpse(data)

data %>%
  count(Y)
```


# other_data contains predictors per zip3
# data contains predictors and the target variable as well (its actual value)

```{r}
# One nice thing to do would be to visualize the target variable for some clients in terms of some of predictors (both are discrete)


ggplot(data, aes(x=client, y=X)) +
  geom_count(size=0.00001)

ggplot(data, aes(x=client, y=Y)) +
  geom_count(size=0.00001)

ggplot(data, aes(x=client, y=Z)) +
  geom_count(size=0.00001)

# The comparison of these three is particularly interesting
```

```{r}
# One variable is discrete, the other is continuous
ggplot(data, aes(x=client, y=AE2019)) +
  geom_col()

ggplot(data, aes(x=client, y=AE2020)) +
  geom_col()

ggplot(data, aes(x=client, y=AE2021)) +
  geom_col()

```

```{r}
# Not sure this is useful
ggplot(data, aes(x=AE2019, y=AE2020)) +
  geom_smooth(method=lm)

ggplot(data, aes(x=AE2019, y=AE2021))+
  geom_smooth(method=lm)

ggplot(data, aes(x=AE2021, y=AE2019))+
  geom_smooth(method=lm)
```

```{r}
ggplot(data, aes(X))+
  geom_bar() +
  labs(x="Adverse Mortality 2019")

ggplot(data, aes(Y))+
  geom_bar() +
  labs(x="Adverse Mortality 2020")


ggplot(data, aes(Z))+
  geom_bar(position="stack") +
  labs(x="Adeverse Mortality 2021")

```

```{r}
ggplot(act_2019, aes(actual2019))+
  geom_histogram()

ggplot(act_2020, aes(actual2020))+
  geom_histogram()

ggplot(act_2021, aes(actual2021))+
  geom_histogram()
```

```{r}
ggplot(data, aes(AE2019))+
  geom_density()

ggplot(data, aes(AE2020))+
  geom_density()

ggplot(data, aes(AE2021))+
  geom_density()
```

```{r}
dat <- data %>% 
  
  #Plot.
  ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5)

```
```{r}
ggplot() + 
  geom_point(data=data, aes(x=client, y=actual2019), colour="yellow")+
  geom_point(data=data, aes(x=client, y=actual2020), colour="deepskyblue")+
  #geom_point(data=data, aes(x=client, y=actual2021), color="black") +
  scale_y_continuous(name="Actual Claims 2019 vs 2020", limits=c(0, 2e+08))
```
```{r}
ggplot() + 
  geom_bar(data=data, aes(x=actual2019), colour="yellow")+
  geom_bar(data=data, aes(x=actual2020), colour="blue")
#geom_point(data=data, aes(x=client, y=actual2021), color="black") +
#  scale_y_continuous(name="Actual Claims", limits=c(0, 2e+08))
```

```{r}
ggplot() + 
  geom_point(data=data, aes(x=client, y=actual2019), colour="yellow")+
  geom_point(data=data, aes(x=client, y=expected), colour="deepskyblue")+
  #geom_point(data=data, aes(x=client, y=actual2021), color="black") +
  scale_y_continuous(name="Actual vs Expected Claims in 2019")
```
```{r}
ggplot() + 
  geom_point(data=data, aes(x=client, y=actual2020), colour="yellow")+
  geom_point(data=data, aes(x=client, y=expected), colour="deepskyblue")+
  #geom_point(data=data, aes(x=client, y=actual2021), color="black") +
  scale_y_continuous(name="Actual vs Expected Claims in 2020", limits=c(0, 2e+08))
```

```{r}
# poverty level in the zip codes where the companies are located
ggplot(data, aes(x=client, y=PCTPOVALL_2019))+
  geom_col()

ggplot(data, aes(x=client, y=covid_deaths))+
  geom_col()
```

```{r}

```