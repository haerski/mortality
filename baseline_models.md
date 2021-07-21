# Baseline model

The goal of this documents is to compare some tuned and untuned models on a "baseline" dataset. We want to predict whether AE in 2020 goes above 3. We are only using 2019 data here! Next steps will be adding time-dependent data (e.g. COVID deaths per week per zipcode).

## Common data

```r
library(tidyverse)
library(tidymodels)
library(themis)
library(feather)
library(magrittr)
library(skimr)
library(vip)
per <- read_feather("data/simulation_data/all_persons.feather")
```

Compute some summary statistic for each client.

```r
clients <-
  per %>%
  group_by(client) %>%
  summarize(
    zip3 = first(zip3),
    size = n(),
    volume = sum(FaceAmt),
    avg_qx = mean(qx),
    avg_age = mean(Age),
    per_male = sum(Sex == "Male") / size,
    per_blue_collar = sum(collar == "blue") / size,
    expected = sum(qx * FaceAmt),
    actual_2020 = sum(FaceAmt[year == 2020], na.rm = TRUE),
    ae_2020 = actual_2020 / expected,
    adverse = as_factor(if_else(ae_2020 > 3, "ae > 3", "ae < 3"))
  ) %>%
  relocate(adverse, ae_2020, .after = zip3) %>%
  mutate(adverse = fct_relevel(adverse, c("ae > 3", "ae < 3")))
```

We can add some demographic information based on zip3.

```r
zip_data <-
  read_feather("data/data.feather") %>%
  mutate(
    density = POP / AREALAND,
    AREALAND = NULL,
    AREA = NULL,
    HU = NULL,
    vaccinated = NULL,
    per_lib = NULL,
    per_green = NULL,
    per_other = NULL,
    per_rep = NULL,
    unempl_2020 = NULL,
    deaths_covid = NULL,
    deaths_all = NULL
  ) %>%
  rename(
    unemp = unempl_2019,
    hes_uns = hes_unsure,
    str_hes = strong_hes,
    income = Median_Household_Income_2019
  )
```
There seems to be some clients with some zip codes that we cannot deal with. These are the ones

```r
clients %>%
  anti_join(zip_data, by = "zip3") %>%
  select(zip3)
```

```
## # A tibble: 6 x 1
##   zip3 
##   <chr>
## 1 969  
## 2 093  
## 3 732  
## 4 872  
## 5 004  
## 6 202
```
These correspond to the following areas

ZIP3 | Area       |
-----|------------|
969  | Guam, Palau, Federated States of Micronesia, Northern Mariana Islands, Marshall Islands |
093  | Military bases in Iraq and Afghanistan |
732  | Not in use |
872  | Not in use |
004  | Not in use |
202  | Washington DC, Government 1 |

We ignore clients with these zip codes. There are also two clients in DC for which we're missing election data. We will ignore those as well.

```r
clients %<>%
  inner_join(zip_data, by = "zip3") %>%
  drop_na()
```

We now have our full dataset. Behold!

```r
skim(clients)
```


Table: Data summary

|                         |        |
|:------------------------|:-------|
|Name                     |clients |
|Number of rows           |492     |
|Number of columns        |29      |
|_______________________  |        |
|Column type frequency:   |        |
|character                |2       |
|factor                   |1       |
|numeric                  |26      |
|________________________ |        |
|Group variables          |None    |


**Variable type: character**

|skim_variable | n_missing| complete_rate| min| max| empty| n_unique| whitespace|
|:-------------|---------:|-------------:|---:|---:|-----:|--------:|----------:|
|client        |         0|             1|   1|   3|     0|      492|          0|
|zip3          |         0|             1|   3|   3|     0|      222|          0|


**Variable type: factor**

|skim_variable | n_missing| complete_rate|ordered | n_unique|top_counts         |
|:-------------|---------:|-------------:|:-------|--------:|:------------------|
|adverse       |         0|             1|FALSE   |        2|ae : 367, ae : 125 |


**Variable type: numeric**

|skim_variable   | n_missing| complete_rate|         mean|           sd|         p0|          p25|          p50|          p75|         p100|hist  |
|:---------------|---------:|-------------:|------------:|------------:|----------:|------------:|------------:|------------:|------------:|:-----|
|ae_2020         |         0|             1|        15.08|        27.46|       0.00|         2.98|         6.46|        14.09| 2.329000e+02|▇▁▁▁▁ |
|size            |         0|             1|      2783.93|      2368.37|      50.00|      1026.25|      2120.50|      3969.50| 1.427000e+04|▇▃▁▁▁ |
|volume          |         0|             1| 432951568.75| 406654465.55| 6235075.00| 151797887.50| 335064812.50| 587443900.00| 4.350904e+09|▇▁▁▁▁ |
|avg_qx          |         0|             1|         0.00|         0.00|       0.00|         0.00|         0.00|         0.00| 0.000000e+00|▁▇▇▂▁ |
|avg_age         |         0|             1|        41.56|         2.05|      37.68|        40.12|        41.11|        42.49| 4.865000e+01|▃▇▃▁▁ |
|per_male        |         0|             1|         0.57|         0.10|       0.22|         0.50|         0.56|         0.64| 8.900000e-01|▁▃▇▅▁ |
|per_blue_collar |         0|             1|         1.00|         0.00|       1.00|         1.00|         1.00|         1.00| 1.000000e+00|▁▁▇▁▁ |
|expected        |         0|             1|   1076643.68|   1056824.25|   11604.50|    350329.91|    831644.68|   1439648.44| 1.189955e+07|▇▁▁▁▁ |
|actual_2020     |         0|             1|  15432043.75|  44771997.48|       0.00|   1963387.50|   4525375.00|  13923600.00| 5.232112e+08|▇▁▁▁▁ |
|nohs            |         0|             1|        11.20|         3.75|       4.00|         8.44|        10.78|        12.90| 2.165000e+01|▂▇▅▁▂ |
|hs              |         0|             1|        23.75|         7.02|      12.10|        18.90|        23.10|        27.50| 4.680000e+01|▅▇▅▂▁ |
|college         |         0|             1|        28.45|         4.83|      13.50|        25.60|        28.58|        31.56| 3.980000e+01|▁▃▇▆▂ |
|bachelor        |         0|             1|        36.62|        10.42|      14.07|        30.00|        35.36|        43.04| 6.130000e+01|▂▇▇▆▂ |
|R_birth         |         0|             1|        11.31|         1.17|       8.30|        10.50|        11.20|        12.00| 1.551000e+01|▁▇▇▂▁ |
|R_death         |         0|             1|         8.13|         1.87|       4.69|         6.80|         7.59|         9.13| 1.401000e+01|▃▇▃▂▁ |
|unemp           |         0|             1|         3.43|         0.87|       2.10|         2.80|         3.31|         3.89| 6.690000e+00|▆▇▃▁▁ |
|poverty         |         0|             1|        10.98|         3.28|       5.04|         8.73|        10.56|        13.30| 2.577000e+01|▆▇▃▁▁ |
|per_dem         |         0|             1|         0.57|         0.16|       0.16|         0.46|         0.58|         0.71| 8.600000e-01|▂▅▇▇▆ |
|hes             |         0|             1|         0.09|         0.04|       0.04|         0.06|         0.07|         0.11| 2.600000e-01|▇▃▂▁▁ |
|hes_uns         |         0|             1|         0.13|         0.05|       0.06|         0.10|         0.12|         0.17| 3.100000e-01|▇▆▅▁▁ |
|str_hes         |         0|             1|         0.05|         0.03|       0.02|         0.03|         0.04|         0.07| 1.800000e-01|▇▅▂▁▁ |
|svi             |         0|             1|         0.46|         0.19|       0.04|         0.33|         0.44|         0.59| 9.200000e-01|▂▆▇▃▂ |
|cvac            |         0|             1|         0.42|         0.21|       0.02|         0.24|         0.41|         0.53| 9.400000e-01|▃▅▇▃▁ |
|income          |         0|             1|     79056.02|     23916.16|   38621.49|     62130.76|     73570.69|     85137.47| 1.352340e+05|▃▇▅▂▂ |
|POP             |         0|             1|    785665.87|    558640.36|   33245.00|    346048.00|    771280.00|    974040.00| 2.906700e+06|▇▇▂▁▁ |
|density         |         0|             1|         0.00|         0.00|       0.00|         0.00|         0.00|         0.00| 3.000000e-02|▇▁▁▁▁ |

## Workflow set
We'll evaluate models using a workflow set. To make our life easier, we will remove some variables and use a formula instead of a recipe.

```r
clients <-
  clients %>%
  select(-client, -zip3, -ae_2020, -actual_2020)
```

We now gather our recipes and models.

```r
centered_rec <-
  recipe(adverse ~ ., data = clients) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors(), -all_nominal())
basic_rec <-
  recipe(adverse ~ ., data = clients) %>%
  step_zv(all_predictors())

log_spec <-
  logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")
tuned_log_spec <-
  logistic_reg(penalty = 0.00118) %>%
  set_engine("glmnet") %>%
  set_mode("classification")
forest_spec <-
  rand_forest(trees = 1000) %>%
  set_mode("classification") %>%
  set_engine("ranger", num.threads = 8, importance = "impurity", seed = 123)
tuned_forest_spec <-
  rand_forest(trees = 1000, mtry = 12, min_n = 21) %>%
  set_mode("classification") %>%
  set_engine("ranger", num.threads = 8, importance = "impurity", seed = 123)
# Samara's models
sln_spec <-
  mlp() %>%
  set_engine("nnet") %>%
  set_mode("classification")
svm_rbf_spec <-
  svm_rbf() %>%
  set_engine("kernlab") %>%
  set_mode("classification")
svm_poly_spec <-
  svm_poly() %>%
  set_engine("kernlab") %>%
  set_mode("classification")
knn_spec <-
  nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

models <- list(log = log_spec,
               log_tuned = tuned_log_spec,
               forest = forest_spec,
               forest_tuned = tuned_forest_spec,
               neural = sln_spec,
               svm_rbf = svm_rbf_spec,
               svm_poly = svm_poly_spec,
               knn_spec = knn_spec)
recipes <- list("normalized" = centered_rec,
                "basic" = basic_rec)
wflows <- workflow_set(recipes, models)
```

Data splitting

```r
set.seed(30308)
init <- initial_split(clients, strata = adverse)
set.seed(30308)
crossval <- vfold_cv(training(init), strata = adverse)
```

Fit all models and estimate metrics using 10-fold cross-validation. We're not performing any tuning here (although we could do that very easily!!!).

```r
fit_wflows <-
  wflows %>%
  workflow_map(fn = "fit_resamples",
               seed = 30332,
               resamples = crossval,
               verbose = TRUE)
```

```
## i  1 of 16 resampling: normalized_log
```

```
## ✔  1 of 16 resampling: normalized_log (4.5s)
```

```
## i  2 of 16 resampling: normalized_log_tuned
```

```
## ✔  2 of 16 resampling: normalized_log_tuned (4.8s)
```

```
## i  3 of 16 resampling: normalized_forest
```

```
## ✔  3 of 16 resampling: normalized_forest (5.8s)
```

```
## i  4 of 16 resampling: normalized_forest_tuned
```

```
## ✔  4 of 16 resampling: normalized_forest_tuned (6s)
```

```
## i  5 of 16 resampling: normalized_neural
```

```
## ✔  5 of 16 resampling: normalized_neural (4.6s)
```

```
## i  6 of 16 resampling: normalized_svm_rbf
```

```
## ✔  6 of 16 resampling: normalized_svm_rbf (4.7s)
```

```
## i  7 of 16 resampling: normalized_svm_poly
```

```
## ✔  7 of 16 resampling: normalized_svm_poly (4.9s)
```

```
## i  8 of 16 resampling: normalized_knn_spec
```

```
## ✔  8 of 16 resampling: normalized_knn_spec (4.6s)
```

```
## i  9 of 16 resampling: basic_log
```

```
## ✔  9 of 16 resampling: basic_log (4.1s)
```

```
## i 10 of 16 resampling: basic_log_tuned
```

```
## ✔ 10 of 16 resampling: basic_log_tuned (4.5s)
```

```
## i 11 of 16 resampling: basic_forest
```

```
## ✔ 11 of 16 resampling: basic_forest (5.5s)
```

```
## i 12 of 16 resampling: basic_forest_tuned
```

```
## ✔ 12 of 16 resampling: basic_forest_tuned (5.7s)
```

```
## i 13 of 16 resampling: basic_neural
```

```
## ✔ 13 of 16 resampling: basic_neural (4.2s)
```

```
## i 14 of 16 resampling: basic_svm_rbf
```

```
## ✔ 14 of 16 resampling: basic_svm_rbf (5.2s)
```

```
## i 15 of 16 resampling: basic_svm_poly
```

```
## ✔ 15 of 16 resampling: basic_svm_poly (5.4s)
```

```
## i 16 of 16 resampling: basic_knn_spec
```

```
## ✔ 16 of 16 resampling: basic_knn_spec (4.5s)
```

Comparing our metrics for the models (unfortunately I couldn't figure out how to show which recipe was picked...)

```r
autoplot(fit_wflows, metric = "roc_auc")
```

![plot of chunk rank_baseline_models](figure/rank_baseline_models-1.png)

```r
fit_wflows %>% collect_metrics()
```

```
## # A tibble: 32 x 9
##    wflow_id  .config  preproc model .metric .estimator  mean     n std_err
##    <chr>     <chr>    <chr>   <chr> <chr>   <chr>      <dbl> <int>   <dbl>
##  1 normaliz… Preproc… recipe  logi… accura… binary     0.774    10  0.0131
##  2 normaliz… Preproc… recipe  logi… roc_auc binary     0.722    10  0.0306
##  3 normaliz… Preproc… recipe  logi… accura… binary     0.769    10  0.0142
##  4 normaliz… Preproc… recipe  logi… roc_auc binary     0.738    10  0.0340
##  5 normaliz… Preproc… recipe  rand… accura… binary     0.853    10  0.0163
##  6 normaliz… Preproc… recipe  rand… roc_auc binary     0.876    10  0.0199
##  7 normaliz… Preproc… recipe  rand… accura… binary     0.858    10  0.0172
##  8 normaliz… Preproc… recipe  rand… roc_auc binary     0.867    10  0.0190
##  9 normaliz… Preproc… recipe  mlp   accura… binary     0.731    10  0.0128
## 10 normaliz… Preproc… recipe  mlp   roc_auc binary     0.695    10  0.0273
## # … with 22 more rows
```

Looks like there is no downside to normalizing. Perhaps it should be the default? Also looks like the machine learning models (svn and neural net) could benefit from some tuning.

Here are the models ranked by `roc_auc`

```r
fit_wflows %>% rank_results("roc_auc") %>% select(wflow_id) %>% unique()
```

```
## # A tibble: 16 x 1
##    wflow_id               
##    <chr>                  
##  1 basic_forest           
##  2 normalized_forest      
##  3 basic_forest_tuned     
##  4 normalized_forest_tuned
##  5 basic_svm_rbf          
##  6 normalized_svm_rbf     
##  7 basic_log_tuned        
##  8 normalized_log_tuned   
##  9 basic_knn_spec         
## 10 normalized_knn_spec    
## 11 normalized_log         
## 12 basic_log              
## 13 basic_svm_poly         
## 14 normalized_svm_poly    
## 15 normalized_neural      
## 16 basic_neural
```

