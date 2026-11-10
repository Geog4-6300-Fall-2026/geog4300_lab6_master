library(tidymodels)
library(spatialsample)
library(sf)
library(tidyverse)

# Predict land surface temperature (lst_c) for block group centroids with tidymodels
# Compares an OLS baseline to a random forest using spatial cross-validation.
#
#   install.packages(c("tidymodels", "spatialsample", "ranger", "vip", "sf"))

set.seed(42)
tidymodels_prefer()

# ---- Parameters -------------------------------------------------------------
data_file       <- "data/gee_out_bg/bg_centroids_covariates.csv"
include_indices <- TRUE    # FALSE drops ndvi/ndbi/ndwi (landscape-only model)

# ---- Step 1: Load and prepare data ------------------------------------------
dat <- read_csv(data_file, show_col_types = FALSE) |>
  filter(!is.na(lst_c), !is.na(nlcd_class)) |>
  mutate(
    nlcd_class = factor(nlcd_class),
    # Aspect is circular, so convert to northness/eastness
    northness = cos(aspect * pi / 180),
    eastness  = sin(aspect * pi / 180)
  ) %>%
  sample_n(30000)

predictors <- c("impervious_pct", "tree_cover_pct", "ndwi",
                "ndvi","elevation", "slope",
                "northness", "eastness", "prism_ppt_mm",
                "lat")

# Spatial object so splits and folds are based on location
dat_sf <- st_as_sf(dat, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# ---- Step 2: Spatial train/test split and spatial CV folds ------------------
# Held-out test blocks and CV folds are spatial blocks, so nearby block groups
# never end up on both sides of a split (random splits would inflate accuracy).
outer <- spatial_block_cv(dat_sf, v = 5)   # works in all spatialsample versions
split <- outer$splits[[1]]                 # first fold's blocks become the test set (~20%)
train <- training(split)
test  <- testing(split)
folds <- spatial_block_cv(train, v = 5)



# ---- Step 3: Recipe and models ----------------------------------------------
rec <- recipe(as.formula(paste("lst_c ~", paste(predictors, collapse = " + "))),
              data = train) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

lm_spec <- linear_reg() |> set_engine("lm")

rf_spec <- rand_forest(trees = 300, mtry = 7, min_n = 10) |>
  set_engine("ranger", importance = "permutation") |>
  set_mode("regression")

lm_wf <- workflow(rec, lm_spec)
rf_wf <- workflow(rec, rf_spec)

# ---- Step 4: Cross-validate (OLS baseline) and tune (random forest) ---------
metrics <- metric_set(rmse, rsq, mae)

lm_res <- fit_resamples(lm_wf, folds, metrics = metrics)

rf_res <- tune_grid(rf_wf, folds, grid = 10, metrics = metrics)

bind_rows(
  collect_metrics(lm_res) |> mutate(model = "OLS"),
  show_best(rf_res, metric = "rmse", n = 1) |> mutate(model = "Random forest")
) |>
  select(model, .metric, mean, std_err) |>
  print()

# ---- Step 5: Final fit and test-set performance -----------------------------
best_rf   <- select_best(rf_res, metric = "rmse")
final_wf  <- finalize_workflow(rf_wf, best_rf)
final_fit <- last_fit(final_wf, split, metrics = metrics)

collect_metrics(final_fit) |> print()

# ---- Step 6: Variable importance --------------------------------------------
imp <- extract_fit_engine(final_fit)$variable.importance

tibble(variable = names(imp), importance = imp) |>
  slice_max(importance, n = 15) |>
  ggplot(aes(importance, reorder(variable, importance))) +
  geom_col() +
  labs(x = "Permutation importance (increase in MSE)", y = NULL)


# ---- Step 7: Observed vs predicted and residual map -------------------------
preds <- predict(extract_workflow(final_fit), test) |>
  bind_cols(st_drop_geometry(test) |> select(GISJOIN, lon, lat, lst_c)) |>
  mutate(resid = lst_c - .pred)

ggplot(preds, aes(.pred, lst_c)) +
  geom_point(alpha = 0.3) +
  geom_abline(color = "red") +
  coord_obs_pred() +
  labs(x = "Predicted LST (°C)", y = "Observed LST (°C)")

ggplot(preds, aes(lon, lat, color = resid)) +
  geom_point(size = 0.8) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  coord_quickmap() +
  labs(color = "Residual (°C)", title = "Test-set residuals (observed - predicted)")
