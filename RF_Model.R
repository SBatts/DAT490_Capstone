## necessary packages
install.packages("randomForest")
install.packages("caret")
install.packages("tidymodels")
install.packages("xtable")

library(randomForest)
library(caret)
library(dplyr)
library(tidymodels)
library(tibble)
library(sf)
library(ggplot2)
library(stringr)
library(tidyr)
library(xtable)
library(ggcorrplot)

## set directory
setwd("C:/Users/sydne/OneDrive/Desktop/School/DAT 490/")

## read in full & trimmed data
df = read.csv("Data/merged_data.csv")
short_df = read.csv("Data/short_data.csv")

## shapefile (for later)
counties = st_read("Data/tl_2023_us_county.shp")
states = st_read("Data/us_states.shp")

## convert region to factor
short_df$SUB_REGION = as.factor(short_df$SUB_REGION)

## for reproducibility 
set.seed(123)

## split train/test
train_index = createDataPartition(short_df$avg_mor, p = 0.8, list = FALSE)
train_df = short_df[train_index, ]
test_df = short_df[-train_index, ]

## train RF model
mod1 = randomForest(avg_mor ~ ., ntree = 1000, importance = TRUE,
                    data = train_df %>% select(-FIPS, -COUNTY, -SUB_REGION))

## make predictions 
pred1 = predict(mod1, newdata = test_df)

## evaluate performance
rmse1 = RMSE(pred1, test_df$avg_mor)
r21 = R2(pred1, test_df$avg_mor)

## feature importance 
imp_vals1 = importance(mod1)
imp1 = data.frame(Feature = rownames(imp_vals1), 
                  IncMSE = imp_vals1[, "%IncMSE"])%>% 
  arrange(desc(IncMSE))
print(imp1)


## trimmed rf model (remove smallest imp)
short_df2 = subset(short_df, select = -c(EP_SNGPNT, EP_MUNIT, EP_HBURD, 
                                         facility_count, EP_CROWD))

## for reproducibility
set.seed(123)

## split train/test
train_index = createDataPartition(short_df2$avg_mor, p = 0.8, list = FALSE)
train_df = short_df2[train_index, ]
test_df = short_df2[-train_index, ]

## train trimmed RF model
mod2 = randomForest(avg_mor ~ ., ntree = 1000, importance = TRUE,
                    data = train_df %>% select(-FIPS, -COUNTY, -SUB_REGION))

## make predictions 
pred2 = predict(mod2, newdata = test_df)

## evaluate performance
rmse2 = RMSE(pred2, test_df$avg_mor)
r22 = R2(pred2, test_df$avg_mor)

## feature importance 
imp_vals2 = importance(mod2)
imp2 = data.frame(Feature = rownames(imp_vals2), 
                  IncMSE = imp_vals2[, "%IncMSE"])%>% 
  arrange(desc(IncMSE))
print(imp2)


## trimmed rf model #2 (removed SVI themes)
short_df3 = subset(short_df2, select = -c(RPL_THEME1, RPL_THEME2, RPL_THEME3,
                                          RPL_THEME4, RPL_THEMES))

## for reproducibility
set.seed(123)

## split train/test
train_index = createDataPartition(short_df3$avg_mor, p = 0.8, list = FALSE)
train_df = short_df3[train_index, ]
test_df = short_df3[-train_index, ]

## train trimmed RF model
mod3 = randomForest(avg_mor ~ ., ntree = 1000, importance = TRUE,
                    data = train_df %>% select(-FIPS, -COUNTY, -SUB_REGION))

## make predictions 
pred3 = predict(mod3, newdata = test_df)

## evaluate performance
rmse3 = RMSE(pred3, test_df$avg_mor)
r23 = R2(pred3, test_df$avg_mor)

## feature importance 
imp_vals3 = importance(mod3)
imp3 = data.frame(Feature = rownames(imp_vals3), 
                  IncMSE = imp_vals3[, "%IncMSE"])%>% 
  arrange(desc(IncMSE))
print(imp3)


## 4th model (drop lag features)
short_df4 = subset(short_df3, select = -c(lag_avg_mor, lag_density))

## for reproducibility
set.seed(123)

## split train/test
train_index = createDataPartition(short_df4$avg_mor, p = 0.8, list = FALSE)
train_df = short_df4[train_index, ]
test_df = short_df4[-train_index, ]

## train trimmed RF model
mod4 = randomForest(avg_mor ~ ., ntree = 1000, importance = TRUE,
                    data = train_df %>% select(-FIPS, -COUNTY, -SUB_REGION))

## make predictions 
pred4 = predict(mod4, newdata = test_df)

## evaluate performance
rmse4 = RMSE(pred4, test_df$avg_mor)
r24 = R2(pred4, test_df$avg_mor)

## feature importance 
imp_vals4 = importance(mod4)
imp4 = data.frame(Feature = rownames(imp_vals4), 
                  IncMSE = imp_vals4[, "%IncMSE"])%>% 
  arrange(desc(IncMSE))
print(imp4)


## check importance across existing models
imp1 = imp1 %>% rename(IncMSE1 = IncMSE)
imp2 = imp2 %>% rename(IncMSE2 = IncMSE)
imp3 = imp3 %>% rename(IncMSE3 = IncMSE)
imp4 = imp4 %>% rename(IncMSE4 = IncMSE)

## join tables
imp_all = imp1 %>%
  full_join(imp2, by = "Feature") %>%
  full_join(imp3, by = "Feature") %>%
  full_join(imp4, by = "Feature")

## calculate averages
imp_all = imp_all %>%
  mutate(Avg_IncMSE = rowMeans(across(starts_with("IncMSE")), na.rm = TRUE),
         Avg_IncMSE = ifelse(is.nan(Avg_IncMSE), NA, Avg_IncMSE),
         Count = rowSums(!is.na(across(starts_with("IncMSE"))))) %>%
  arrange(desc(Avg_IncMSE))
print(imp_all)


## trimmed for top 10 
short_df5 = subset(short_df, select = c(FIPS, COUNTY, SUB_REGION, avg_mor, 
                                        lag_avg_mor, EP_DISABL, EP_MINRTY,
                                        pop_density, service_diversity, 
                                        EP_UNEMP, EP_NOHSDP, EP_UNINSUR,
                                        EP_POV150, facilities_per_10k))

## for reproducibility
set.seed(123)

## split train/test
train_index = createDataPartition(short_df5$avg_mor, p = 0.8, list = FALSE)
train_df = short_df5[train_index, ]
test_df = short_df5[-train_index, ]

## train trimmed RF model
mod5 = randomForest(avg_mor ~ ., ntree = 1000, importance = TRUE,
                    data = train_df %>% select(-FIPS, -COUNTY, -SUB_REGION))

## make predictions 
pred5 = predict(mod5, newdata = test_df)

## evaluate performance
rmse5 = RMSE(pred5, test_df$avg_mor)
r25 = R2(pred5, test_df$avg_mor)

## feature importance 
imp_vals5 = importance(mod5)
imp5 = data.frame(Feature = rownames(imp_vals5), 
                  IncMSE = imp_vals5[, "%IncMSE"])%>% 
  arrange(desc(IncMSE))
print(imp5)

## cross-validation
cv_ctrl = trainControl(method = "cv", number = 5,
  savePredictions = "final", verboseIter = TRUE)

# RF with cross validation
modcv = train(avg_mor ~ ., data = short_df5 %>% 
                select(-FIPS, -COUNTY, -SUB_REGION),
              method = "rf", ntree = 100, importance = TRUE, 
              trControl = cv_ctrl)

# View model performance
print(modcv)



## cross-validation
cv_ctrl = trainControl(method = "cv", number = 5,
                       savePredictions = "final", verboseIter = TRUE)

# RF with cross validation
modcv = train(avg_mor ~ ., data = short_df5 %>% 
                select(-FIPS, -COUNTY, -SUB_REGION),
              method = "rf", ntree = 100, importance = TRUE, 
              trControl = cv_ctrl)

# View model performance
print(modcv)


## model with mtry = 6
final_rf = randomForest(avg_mor ~ ., data = short_df5 %>% 
                          select(-FIPS, -COUNTY, -SUB_REGION),
                        ntree = 1000, mtry = 6, importance = TRUE)

## final predictions
short_df5$pred_avg_mor = predict(final_rf, newdata = short_df5)

## residuals 
short_df5$residual = short_df5$avg_mor - short_df5$pred_avg_mor

## table of rsme/r2
model_metrics = tibble(Model = c("mod1", "mod2", "mod3", "mod4", 
                                 "mod5"),
                       RMSE = c(rmse1, rmse2, rmse3, rmse4, rmse5),
                       R2 = c(r21, r22, r23, r24, r25))

## new 5 digit FIPS code for merging
counties$FIPS = paste(counties$STATEFP, counties$COUNTYFP, sep = "")

## make FIPS 5 digits in frames where leading zero is missing
short_df5$FIPS = str_pad(as.character(short_df5$FIPS), width = 5, 
                         side = "left", pad = "0")

## merge for mapping
counties_merged = counties %>%
  left_join(short_df5, by = "FIPS")

## clean up 
counties_merged = subset(counties_merged, select = -c(CSAFP, CBSAFP, METDIVFP))
counties_merged = st_transform(counties_merged, crs = 4326)
counties = st_transform(counties, crs = 4326)
states = st_transform(states,crs = 4326)

## predicted mortality map
ggplot(counties_merged) +
  geom_sf(aes(fill = pred_avg_mor), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(title = "Predicted Average Mortality by County",
       fill = "Predicted Rate") +
  theme_minimal()

## continental separate from HI and AK (for mapping)
counties_conus = counties_merged %>% filter(!STATEFP %in% c("02", "15"))
counties_ak = counties_merged %>% filter(STATEFP == "02")
counties_hi = counties_merged %>% filter(STATEFP == "15")
states_conus = states %>% filter(!STATE_FIPS %in% c("02", "15"))

## actual mortality
ggplot(counties_conus) +
  geom_sf(aes(fill = avg_mor), color = NA) +
  geom_sf(data = states_conus, fill = NA, color = "grey", size = 0.2) +
  scale_fill_viridis_c(option = "viridis") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  labs(title = "Actual Drug-Related Mortality by County",
       fill = "Per 100,000") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16))

## predicted mortality 
ggplot(counties_conus) +
  geom_sf(aes(fill = pred_avg_mor), color = NA) +
  geom_sf(data = states_conus, fill = NA, color = "grey", size = 0.2) +
  scale_fill_viridis_c(option = "viridis") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  labs(title = "Predicted Drug-Related Mortality by County",
       fill = "Per 100,000") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16))

## residuals
ggplot(counties_conus) +
  geom_sf(aes(fill = residual), color = "grey85", size = 0.1) +
  geom_sf(data = states_conus, fill = NA, color = "black", size = 0.2) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, na.value = "grey90", name = "Residual") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  labs(title = "Model Residuals by County in Contiguous US",
       caption = "Residuals  (Actual - Predicted)",
       fill = "Residuals") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.caption = element_text(hjust = 0.5))

## facilities per 10K map
ggplot(counties_conus) +
  geom_sf(aes(fill = facilities_per_10k), color = NA) +
  geom_sf(data = states_conus, fill = NA, color = "grey", size = 0.2) +
  geom_sf_text(data = states, aes(label = STATE_ABBR), color = 'white',
               size = 2, fontface = 'bold', inherit.aes = FALSE) +
  scale_fill_viridis_c(option = "viridis") +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  labs(title = "Substance Abuse Treatment Density by County",
       fill = "Per 10K Persons") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16))

## RMSE line plot comparison
ggplot(model_metrics, aes(x = Model, y = RMSE, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  theme_minimal() +
  labs(title = "RMSE Across Models", y = "RMSE")

## R2 line plot comparison
ggplot(model_metrics, aes(x = Model, y = R2, group = 1)) +
  geom_line(color = "darkgreen") +
  geom_point(size = 3, color = "darkgreen") +
  theme_minimal() +
  labs(title = "R² Across Models", y = expression(R^2))

## R2/RMSE
model_long = model_metrics %>%
  pivot_longer(cols = c(RMSE, R2), names_to = "Metric", values_to = "Value")

## plot R2/RMSE
ggplot(model_long, aes(x = Model, y = Value, group = Metric, color = Metric)) +
  geom_line() +
  geom_point(size = 3) +
  facet_wrap(~Metric, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(title = "Model Performance Across Metrics")

## for aesthetics
new_labels = c(
  "avg_mor" = "Average Mortality",
  "lag_avg_mor" = "Lagged Mortality",
  "EP_DISABL" = "Disability %",
  "pop_density" = "Population Density",
  "service_diversity" = "Service Diversity",
  "E_TOTPOP" = "Total Population",
  "EP_UNEMP" = "Unemployment %",
  "EP_MINRTY" = "Minority %", 
  "EP_NOHSDP" = "No High School Diploma %",
  "EP_UNINSUR" = "Uninsured %",
  "EP_LIMENG" = "Limited English Proficiency",
  "facilities_per_10k" = "Facilties per 10K Persons",
  "EP_POV150" = "Poverty %")

## for plotting
top_features = imp5 %>%
  mutate(Feature = factor(Feature, 
                          levels = names(new_labels), 
                          labels = new_labels))

## feature importance
ggplot(top_features, aes(x = reorder(Feature, IncMSE), y = IncMSE)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Feature Importances",
       x = "Feature",
       y = "% Increase in MSE if Removed") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))

## tables
table1 = xtable(imp1)
table2 = xtable(imp2)
table3 = xtable(imp3)
table4 = xtable(imp4)
table5 = xtable(imp5)
table6 = xtable(model_metrics)
print(table1, file = "imp1_table.tex", include.rownames = FALSE)
print(table2, file = "imp2_table.tex", include.rownames = FALSE)
print(table3, file = "imp3_table.tex", include.rownames = FALSE)
print(table4, file = "imp4_table.tex", include.rownames = FALSE)
print(table5, file = "imp5_table.tex", include.rownames = FALSE)
print(table6, file = "metric_table.tex", include.rownames = FALSE)

## histogram of mortality
ggplot(data = short_df5,aes(x = avg_mor)) +
  geom_histogram (color="black", fill="slateblue") +
  xlab("Drug-Related Mortality (per 100,000)") +
  ylab("Counts") +
  ggtitle("Distribution of Annual Drug-Related Deaths") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

## correlation for plotting
correlations = cor(short_df5 %>% 
                     select(-FIPS, -COUNTY, -SUB_REGION), 
                   use = "complete.obs")

## only avg_mor correlations
cor_target = correlations[, "avg_mor"]
cor_target = cor_target[-which(names(cor_target) == "avg_mor")]

cor_df = data.frame(Feature = names(cor_target),
                    Correlation = cor_target)

## for labelling
cor_df$Feature = factor(cor_df$Feature, 
                        levels = names(new_labels), 
                        labels = new_labels)

## plot
ggplot(cor_df, aes(x = reorder(Feature, Correlation), y = Correlation)) +
  geom_col(fill = "slateblue") +
  coord_flip() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Correlation with Average Mortality",
       y = "Pearson Correlation",
       x = "Feature") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))