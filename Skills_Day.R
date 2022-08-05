################################################################################
## Skills Day- 5/8/2022 ########################################################
################################################################################

pacman::p_load(tidyverse, modeltime, timetk, lubridate,tidymodels,plotly)
values <- c("#EB0050", "#143C75", "#4BB9A0", "#ECBF08")
#### Step 1, retrieve some numbers to perform forecasting for: ####

Forecasting_df %>%
  as_tibble() %>%
  select(WEEK_NO, DATE_KEY, REGISTRATIONS, REGISTRATION_FORECAST) -> DN_numbs

## Some formatting and unique valriable selection:

DN_numbs %>% 
  group_by(WEEK_NO) %>% 
  summarise(DATE_KEY = max(DATE_KEY),
            REGISTRATIONS = as.numeric(REGISTRATIONS),
            REGISTRATION_FORECAST = as.numeric(REGISTRATION_FORECAST)) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(DATE_KEY = date(DATE_KEY))-> DN_numbs

## Plot the general trend to pick out any patterns:

DN_numbs %>% 
select(DATE_KEY, REGISTRATIONS) %>% 
  set_names(c("date","value")) -> dn_numbs_tbl

dn_numbs_tbl %>% 
  plot_time_series(date,value,.interactive = TRUE)

## Look at a subset of the data:
dn_numbs_tbl %>% 
  filter(date >='2021-04-01'& date <= '2022-08-06') -> dn_numbs_tbl

dn_numbs_tbl %>% 
  plot_time_series(date,value,.interactive = TRUE)

#### Step 2: Set up training and testing sets: ####

## We can set up the plsits using time_series_split()
## Setting assess as 3 months, to use the last 3 months 
## as the testing set:

splits <-  dn_numbs_tbl %>% 
  time_series_split(assess = "3 months", cumulative = TRUE,date_var = date) # Cummulative true to use all previous data as training

# Visualise the splits:

splits %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan()

#### Step 3 Modelling: ####

## Model 1: Auto Arima
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splits))

## Auto Arima summary:
model_fit_arima

## Model 2: Prophet Regression

model_fit_prophet <- prophet_reg() %>%
  set_engine("prophet", yearly.seasonality = TRUE) %>%
  fit(value ~ date, training(splits))

## Prophet Model Summary:

model_fit_prophet
  

#### Machine Learning Approach ####

## More complex than auto models, and require the setting up of workflows.
## The general approach has the following 3 steps:

## Creating preprocessing recipe
## We can use recipe() to add some variables that may be useful from a time series 
## point of view:

recipe_spec <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_fourier(date, period = 365, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()

## Now that we have this, let's set up machine learning pipelines.

## Machine Learning Model 1: Elastic NET

model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

## Make a fitted workflow:

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))

## Machine Learning Model 2: Random Forest

model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
  set_engine("randomForest")

workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))

#### New School, Hybrid Models ####

## Prophet Boost

model_spec_prophet_boost <- prophet_boost() %>%
  set_engine("prophet_xgboost", yearly.seasonality = TRUE) 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost

### Model time table to assess performance:

model_table <- modeltime_table(
  model_fit_arima, 
  model_fit_prophet,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_prophet_boost
) 
model_table

## The table helps us to keep track of the models with ID.

#### Calibration ####
## Lets us get confidence intervals, etc. 

calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))
calibration_table

## Forecasting on the test data:

calibration_table %>%
  modeltime_forecast(actual_data = dn_numbs_tbl) %>%
  plot_modeltime_forecast(.interactive = TRUE)

## Accuracy (Test Set)
calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## A word on metrics 

#### Refit and Forecast Forward ####
## Retrain on the full dataset
## 

calibration_table %>%
# Remove ARIMA model with low accuracy
  filter(!.model_id %in% c(2,3)) %>%
  
# Refit and Forecast Forward
  modeltime_refit(dn_numbs_tbl) %>%
  modeltime_forecast(h = "12 months", actual_data = dn_numbs_tbl) %>%
  plot_modeltime_forecast(.interactive = TRUE)

### Get the information for the predicted registrations,
### Ultimately for combination of to as much forecast as possible:

calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(!.model_id %in% c(2,3)) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(dn_numbs_tbl) %>%
  modeltime_forecast(h = "12 months", actual_data = dn_numbs_tbl) %>% 
  select(.model_desc,.index,.value) -> Model_preds

Model_preds %>% 
  rbind(to_bind) -> Comparison_DF

# View(Comparison_DF)

Comparison_DF %>% 
  group_by(.model_desc) %>% 
  filter(.index >= '2022-08-13') -> to_plot

to_plot  %>% 
  rename(Date=.index,Registrations=.value,Model=.model_desc) %>% 
  ggplot(aes(Date,Registrations,color=Model))+
  geom_line(size =1)+
  scale_x_date(date_breaks = "4 weeks")+
  scale_y_continuous(labels = scales::comma_format())+
  ylab("Registrations")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_color_manual(values = values) -> p

plotly::ggplotly(p)  %>% 
  layout(legend = list(orientation = 'h'))

to_plot %>% 
  pivot_wider(names_from = ".model_desc",
              values_from = ".value") -> cal_variance


cal_variance %>%
  mutate(
    Forecast_RF = RANDOMFOREST - FORECAST,
    Forecast_AR = `UPDATE: ARIMA(1,1,2)` - FORECAST,
    Forecast_XGB = `PROPHET W/ XGBOOST ERRORS` - FORECAST
  ) %>%
  select(.index, starts_with("Forecast")) %>%
  pivot_longer(cols = FORECAST:Forecast_XGB) %>%
  filter(name != "FORECAST" & .index <= '2023-03-04') %>%
  mutate(flag = if_else(value < 0, "Under Forecasting", "Over Forecasting")) %>%
  ggplot(aes(.index, value, color = name)) +
  geom_line() +
  facet_wrap( ~ flag) +
  scale_x_date(date_breaks = "4 weeks")


cal_variance %>%
  mutate(
    Forecast_RF = RANDOMFOREST - FORECAST,
    Forecast_AR = `UPDATE: ARIMA(1,1,2)` - FORECAST,
    Forecast_XGB = `PROPHET W/ XGBOOST ERRORS` - FORECAST
  ) %>%
  select(.index, starts_with("Forecast")) %>%
  pivot_longer(cols = FORECAST:Forecast_XGB) %>%
  filter(name != "FORECAST" & .index <= '2023-03-04') %>%
  mutate(flag = if_else(value < 0, "Under Forecasting", "Over Forecasting")) %>% 
  mutate_if(is.numeric, abs) %>% 
  group_by(name,flag) %>% 
  summarise(mean_variance=mean(value),
            sd=sd(value))## Do buble of this:
  



  












