# On-Net Data Center Characteristics, Customer Data and Model
# Version 5.4-shinylive — browser-hosted; workbook supplied by the user at runtime.
# The Excel workbook is never bundled with the published static site.
# Annual revenue is the existing Aug 2025-Jul 2026 sum; it is not multiplied by 12.
# Billed percentage uses the workbook's normalized unique-customer denominator.
# Matching, source eligibility, browser-model training and 4MC calculations are retained.
# Site summary is the last matched snapshot; re-match after changing inputs.

required_packages <- c(
  "shiny", "DT", "readxl", "dplyr", "tidyr", "readr", "tibble",
  "ggplot2", "scales"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install before running: ", paste(missing_packages, collapse = ", "))
}

library(shiny)
library(DT)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(ggplot2)
library(scales)

options(scipen = 999, shiny.maxRequestSize = 150 * 1024^2)
set.seed(123)

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
workbook_placeholder <- "Choose an Excel workbook (.xlsx or .xls)"
default_sheet_name <- "main"
target_column <- "Total_MRR_12M"
id_column <- "GLM_ID"
construction_year_column <- "Year_Build"
minimum_revenue <- 1000
excluded_dc_segment <- "6"
historical_onnet_count <- 25
capacity_source_column <- "Bandwidth"
capex_column <- "CAPEX"
historical_tam_column <- "TAM_(2030)"
quote_text_column <- "BandwidthQuoting_Last12Months_(>= 10G)"
revenue_window <- "Aug 2025-Jul 2026"
valid_segments <- as.character(1:5)
numeric_predictors <- c("SQ_foot", "Capacity", "CAPEX")
categorical_predictors <- c("state", "city", "urban_rural", "DC_Segments")
predictor_columns <- c(numeric_predictors, categorical_predictors)
required_columns <- c(
  "GLM_ID", "is_on_net", "Total_MRR_12M", "SQ_foot", "Bandwidth", "CAPEX",
  "Year_Build", "state", "city", "DC_Segments", "derived_tenant_count",
  "Unique_Billing_Customer_Count_12M"
)
optional_columns <- c(
  "address", "DC_Operator", "urban_rural", "TAM_(2030)",
  "Tenant_Norm_Unique_UC_Count", "Unique_Billing_Customer_Count_3M",
  "QuoteCount_Last_12Months", quote_text_column, "Maturity_Score", "NaaSEnabled"
)

browser_model_lambda <- 1

# Static 4MC assumptions
projection_years <- 2026:2035
sf_bw_base <- 0.00123785423992463
compute_density_growth <- 0.10
overprovisioning_factor <- 2
price_400g_2026 <- 5000
price_decline <- 0.02
first_mover_win_rate <- 0.65
capex_constant <- 830000
wacc <- 0.08

four_mc_assumptions_data <- function() {
  tibble(
    Category = c(
      "Projection", "Projection", "Capacity Ramp", "Capacity Ramp",
      "Capacity Ramp", "Bandwidth", "Bandwidth", "Bandwidth",
      "Pricing", "Pricing", "Market Share", "CAPEX", "Financial"
    ),
    Value = c(
      as.character(min(projection_years)), as.character(max(projection_years)),
      percent(0.33, accuracy = 0.1), percent(0.65, accuracy = 0.1),
      percent(0.95, accuracy = 0.1),
      format(sf_bw_base, scientific = FALSE, digits = 16),
      percent(compute_density_growth, accuracy = 0.1),
      paste0(format(overprovisioning_factor, nsmall = 1), "x"),
      dollar(price_400g_2026, accuracy = 1),
      percent(price_decline, accuracy = 0.1),
      percent(first_mover_win_rate, accuracy = 0.1),
      dollar(capex_constant, accuracy = 1), percent(wacc, accuracy = 0.1)
    ),
    Units = c(
      "year", "year", "%", "%", "%", "bandwidth / sq ft",
      "% / year", "x", "$ / month", "% / year", "%", "$", "%"
    ),
    Assumption = c(
      "Projection Start Year", "Projection End Year", "Construction Year CRT",
      "Year After Construction CRT", "Following Years CRT",
      "SF-BW Conversion Factor (2026)", "Compute Density Increase",
      "Overprovisioning Factor", "400G Monthly Price (2026)",
      "Annual Price Decline", "First Mover Win Rate", "CAPEX Constant", "WACC"
    ),
    `Documentation Reference` = c(
      "Method step 2", "Method step 2", "Method step 3",
      "Original documentation", "Method step 3", "Method step 4",
      "Method steps 5-6", "Method step 10", "Method step 11",
      "Method step 11", "Method step 14", "Method step 17", "Method step 20"
    ),
    Status = rep("Static", 13)
  )
}

normalize_segment <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- sub("^SEGMENT\\s*", "", x)
  x <- sub("\\.0+$", "", x)
  trimws(x)
}

safe_rank <- function(reference, value) {
  reference <- reference[is.finite(reference)]
  if (!length(reference)) return(NA_integer_)
  sum(reference > value) + 1L
}

safe_percentile <- function(reference, value) {
  reference <- reference[is.finite(reference)]
  if (length(reference) < 2) return(NA_real_)
  as.numeric(ecdf(reference)(value))
}

future_bandwidth_growth <- function(current_revenue, start_year, horizon_years = 5,
                                    bandwidth_growth_rate = 0.10, revenue_capture_rate = 0.50) {
  values <- c(current_revenue, start_year, horizon_years, bandwidth_growth_rate, revenue_capture_rate)
  if (any(!is.finite(values)) || current_revenue < 0 || horizon_years < 1 ||
      bandwidth_growth_rate < 0 || revenue_capture_rate < 0 || revenue_capture_rate > 1) return(tibble())
  year_index <- 0:as.integer(horizon_years)
  bandwidth_index <- (1 + bandwidth_growth_rate) ^ year_index
  revenue_index <- 1 + revenue_capture_rate * (bandwidth_index - 1)
  tibble(Year = as.integer(start_year) + year_index, Year_From_Start = year_index,
         Bandwidth_Demand_Index = bandwidth_index, Revenue_Capture_Rate = revenue_capture_rate,
         Revenue_Growth_Index = revenue_index,
         Future_Revenue_Opportunity = current_revenue * revenue_index,
         Incremental_Revenue_Opportunity = current_revenue * (revenue_index - 1))
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

safe_median <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  median(x)
}

nonnegative_values <- function(x) {
  x[!is.finite(x) | x < 0] <- NA_real_
  x
}

billed_percentage <- function(numerator, denominator) {
  result <- rep(NA_real_, length(numerator))
  valid <- is.finite(numerator) & numerator >= 0 &
    is.finite(denominator) & denominator > 0
  result[valid] <- numerator[valid] / denominator[valid]
  result[!is.finite(result)] <- NA_real_
  result
}

matched_measures <- function(sites) {
  normalized <- sites$Tenant_Norm_Unique_UC_Count
  normalized[!is.finite(normalized) | normalized <= 0] <- NA_real_
  values <- list(
    `Facility tenant estimate` = nonnegative_values(sites$derived_tenant_count),
    `Normalized unique customers (base)` = normalized,
    `Unique billed customers (12M)` = nonnegative_values(sites$Unique_Billing_Customer_Count_12M),
    `% billed (normalized base)` = sites$Billed_Percent_Normalized,
    `Annual revenue` = nonnegative_values(sites$Total_MRR_12M)
  )
  tibble(Measure = names(values),
         Average = vapply(values, safe_mean, numeric(1)),
         Valid_GLMS = vapply(values, function(x) sum(is.finite(x)), integer(1)),
         Matched_GLMS = rep(nrow(sites), length(values)))
}

fmt_dollar <- function(x) ifelse(is.finite(x), dollar(x, accuracy = 1), "Unavailable")
fmt_percent <- function(x) ifelse(is.finite(x), percent(x, accuracy = 0.1), "Unavailable")

documented_npv <- function(cash_flows, discount_rate = 0.08) {
  cash_flows <- as.numeric(cash_flows)
  if (!length(cash_flows) || any(!is.finite(cash_flows)) ||
      !is.finite(discount_rate)) return(NA_real_)
  sum(cash_flows / ((1 + discount_rate) ^ seq_along(cash_flows)))
}

safe_irr <- function(cash_flows) {
  cash_flows <- as.numeric(cash_flows)
  if (length(cash_flows) < 2 || any(!is.finite(cash_flows)) ||
      !any(cash_flows < 0) || !any(cash_flows > 0)) return(NA_real_)
  f <- function(rate) sum(cash_flows / ((1 + rate) ^ (seq_along(cash_flows) - 1)))
  lower <- -0.9999
  upper <- 10
  lower_npv <- f(lower)
  upper_npv <- f(upper)
  while (is.finite(lower_npv) && is.finite(upper_npv) &&
         sign(lower_npv) == sign(upper_npv) && upper < 1000) {
    upper <- upper * 2
    upper_npv <- f(upper)
  }
  if (!is.finite(lower_npv) || !is.finite(upper_npv) ||
      sign(lower_npv) == sign(upper_npv)) return(NA_real_)
  tryCatch(uniroot(f, interval = c(lower, upper))$root,
           error = function(e) NA_real_)
}

calculate_4mc_financials <- function(
    construction_year, square_footage, variable_capex,
    projection_years = 2026:2035,
    sf_bw_base = 0.00123785423992463,
    compute_density_growth = 0.10,
    overprovisioning_factor = 2,
    price_400g_2026 = 5000,
    price_decline = 0.02,
    first_mover_win_rate = 0.65,
    capex_constant = 830000,
    wacc = 0.08
) {
  construction_year <- as.numeric(construction_year)
  square_footage <- as.numeric(square_footage)
  variable_capex <- as.numeric(variable_capex)
  if (!all(is.finite(c(construction_year, square_footage, variable_capex)))) {
    return(list(
      projection = tibble(), tam_2030 = NA_real_, tam_10_year = NA_real_,
      gross_revenue_10_year = NA_real_, som_10_year = NA_real_,
      npv = NA_real_, irr = NA_real_, four_mc_capex = NA_real_,
      cash_flows = numeric()
    ))
  }
  
  four_mc_capex <- abs(capex_constant + variable_capex)
  first_mover_flag <- ifelse(construction_year >= min(projection_years), 1, 0)
  
  projection <- tibble(Year = projection_years) %>%
    mutate(
      Active_SQFT = ifelse(Year >= construction_year, square_footage, 0),
      CRT = case_when(
        Active_SQFT == 0 ~ 0,
        Year == construction_year ~ 0.33,
        Year == construction_year + 1 ~ 0.65,
        TRUE ~ 0.95
      ),
      SFBW = sf_bw_base *
        ((1 + compute_density_growth) ^ (Year - min(projection_years))),
      Total_Conversion_Factor = CRT * SFBW,
      Egress_Bandwidth = Active_SQFT * Total_Conversion_Factor,
      Raw_400G_Waves = Egress_Bandwidth / 400,
      Waves_Demanded = ceiling(Egress_Bandwidth / 400),
      Waves_Required = Waves_Demanded * overprovisioning_factor,
      Monthly_400G_Price = price_400g_2026 *
        ((1 - price_decline) ^ (Year - min(projection_years))),
      Annual_TAM = Monthly_400G_Price * Waves_Required * 12,
      First_Mover_Flag = first_mover_flag,
      Market_Share = First_Mover_Flag * first_mover_win_rate,
      Revenue_Share = Annual_TAM * Market_Share
    )
  
  documentation_som <- projection$Revenue_Share
  
  if (construction_year == min(projection_years)) {
    
    # 2026 build:
    # CAPEX lives one period before 2026 SOM
    
    projection$CAPEX_Cash_Flow <- rep(
      0,
      length(projection_years)
    )
    
    projection$SOM_Lit_400G <- documentation_som
    
    cash_flows <- c(
      -four_mc_capex,
      documentation_som
    )
    
  } else {
    
    # Non‑2026 build:
    # SOM 2026 = CAPEX only
    
    projection$CAPEX_Cash_Flow <- c(
      -four_mc_capex,
      rep(0, length(projection_years) - 1)
    )
    
    documentation_som[1] <- -four_mc_capex
    
    projection$SOM_Lit_400G <- documentation_som
    
    cash_flows <- documentation_som
  }
  
  tam_2030 <- projection %>% filter(Year == 2030) %>% pull(Annual_TAM)
  if (!length(tam_2030)) tam_2030 <- NA_real_
  
  list(
    projection = projection,
    tam_2030 = tam_2030,
    tam_10_year = sum(projection$Annual_TAM, na.rm = TRUE),
    gross_revenue_10_year = sum(projection$Revenue_Share, na.rm = TRUE),
    som_10_year = sum(projection$SOM_Lit_400G, na.rm = TRUE),
    npv = documented_npv(cash_flows, wacc),
    irr = safe_irr(cash_flows),
    four_mc_capex = four_mc_capex,
    cash_flows = cash_flows
  )
}
# -----------------------------------------------------------------------------
# Cleaning, quoting and browser-compatible historical model
# -----------------------------------------------------------------------------
clean_number <- function(x) {
  suppressWarnings(parse_number(as.character(x),
                                na = c("", "NA", "N/A", "#N/A", "NULL", "-", "NF")))
}

clean_text <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x %in% c("", "NA", "N/A", "#N/A", "NULL", "NF")] <- "UNKNOWN"
  x
}

parse_build_year <- function(x) {
  text <- trimws(as.character(x))
  result <- rep(NA_real_, length(text))
  valid <- !is.na(text) & grepl("^[12][0-9]{3}(\\.0+)?$", text)
  result[valid] <- as.numeric(text[valid])
  result[!is.na(text) & text == "Baseline (2025)"] <- 2025
  result
}

quote_tokens <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(trimws(x))) return(character())
  tokens <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  unique(tokens[nzchar(tokens) & !toupper(tokens) %in% c("NA", "N/A", "#N/A", "NULL")])
}

classify_quote <- function(tokens) {
  text <- toupper(trimws(tokens))
  legacy <- grepl("TDM|LEGACY|(^|[^A-Z0-9])(DS1|DS3|OC[ -]?[0-9]+)([^A-Z0-9]|$)", text)
  speed <- suppressWarnings(as.numeric(sub("^([0-9]+(?:\\.[0-9]+)?)G\\s+.*$", "\\1", text, perl = TRUE)))
  modern <- grepl("^[0-9]+(?:\\.[0-9]+)?G\\s+(IP|ETHERNET|WAVELENGTHS?)$", text, perl = TRUE) &
    is.finite(speed) & speed >= 10 & !legacy
  result <- rep("Unclassified / other (not approved growth scope)", length(tokens))
  result[grepl("VPN|SECURITY|CYBERSECURITY|SD[- ]?WAN", text)] <-
    "Separate unclassified VPN / security / SD-WAN"
  result[modern] <- "Provisional modern >=10G IP / Ethernet / Wavelength"
  result[legacy] <- "Excluded TDM / legacy"
  result
}

quote_catalogue <- function(profile) {
  empty <- tibble(Category = character(), Classification = character(), Site_Mentions = integer())
  if (!nrow(profile)) return(empty)
  detail <- lapply(seq_len(nrow(profile)), function(i) {
    tokens <- profile$Quote_Tokens[[i]]
    tibble(GLM_ID = rep(profile$GLM_ID[[i]], length(tokens)), Category = tokens)
  }) %>% bind_rows()
  if (!nrow(detail)) return(empty)
  detail %>% distinct(GLM_ID, Category) %>% count(Category, name = "Site_Mentions") %>%
    mutate(Classification = classify_quote(Category)) %>%
    select(Category, Classification, Site_Mentions) %>% arrange(desc(Site_Mentions), Category)
}

fit_encoder <- function(df) {
  list(
    medians = setNames(vapply(numeric_predictors, function(name) {
      value <- safe_median(df[[name]])
      if (is.finite(value)) value else 0
    }, numeric(1)), numeric_predictors),
    levels = setNames(lapply(categorical_predictors, function(name) sort(unique(df[[name]]))),
                      categorical_predictors)
  )
}

encode_features <- function(df, encoder) {
  columns <- lapply(numeric_predictors, function(name) {
    value <- as.numeric(df[[name]])
    value[!is.finite(value)] <- encoder$medians[[name]]
    matrix(value, ncol = 1, dimnames = list(NULL, name))
  })
  for (name in categorical_predictors) {
    for (level in encoder$levels[[name]]) {
      columns[[length(columns) + 1L]] <- matrix(as.numeric(df[[name]] == level),
                                                ncol = 1, dimnames = list(NULL, paste(name, level, sep = "_")))
    }
  }
  x <- do.call(cbind, columns)
  colnames(x) <- make.names(colnames(x), unique = TRUE)
  x
}

fit_browser_model <- function(x, y, lambda = browser_model_lambda) {
  x <- as.matrix(x)
  center <- colMeans(x)
  scale <- apply(x, 2, stats::sd)
  scale[!is.finite(scale) | scale == 0] <- 1
  standardized <- sweep(sweep(x, 2, center, "-"), 2, scale, "/")
  design <- cbind(`(Intercept)` = 1, standardized)
  penalty <- diag(ncol(design))
  penalty[1, 1] <- 0
  coefficients <- tryCatch(
    solve(crossprod(design) + lambda * penalty, crossprod(design, y)),
    error = function(e) qr.solve(crossprod(design) + lambda * penalty, crossprod(design, y))
  )
  list(coefficients = as.numeric(coefficients), center = center, scale = scale,
       feature_names = colnames(x), lambda = lambda)
}

predict_browser_model <- function(model, x) {
  x <- as.matrix(x)
  missing <- setdiff(model$feature_names, colnames(x))
  if (length(missing)) {
    padding <- matrix(0, nrow = nrow(x), ncol = length(missing),
                      dimnames = list(NULL, missing))
    x <- cbind(x, padding)
  }
  x <- x[, model$feature_names, drop = FALSE]
  standardized <- sweep(sweep(x, 2, model$center, "-"), 2, model$scale, "/")
  as.numeric(cbind(1, standardized) %*% model$coefficients)
}

browser_model_importance <- function(model) {
  importance <- abs(model$coefficients[-1])
  if (!length(importance)) return(data.frame())
  total <- sum(importance)
  data.frame(
    Feature = model$feature_names,
    Gain = if (is.finite(total) && total > 0) importance / total else importance,
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(dplyr::desc(Gain))
}

train_secondary_model <- function(profile, progress) {
  training <- profile %>% filter(is.finite(Total_MRR_12M), Total_MRR_12M >= minimum_revenue)
  empty <- list(
    model = NULL, encoder = NULL, eligible_rows = nrow(training), final_nrounds = NA_integer_,
    model_message = "Browser regression unavailable: at least 20 eligible rows are required.",
    historical_scores = tibble(GLM_ID = character(), Actual_Revenue = numeric(), Predicted_Revenue = numeric()),
    test_results = tibble(Actual_Revenue = numeric(), Predicted_Revenue = numeric()),
    cv_results = tibble(Fold = integer(), RMSE = numeric(), MAE = numeric(), R2 = numeric()),
    importance_matrix = data.frame(), holdout_rmse = NA_real_, holdout_mae = NA_real_, holdout_r2 = NA_real_
  )
  if (nrow(training) < 20L) return(empty)
  tryCatch({
    encoder <- fit_encoder(training)
    x <- encode_features(training, encoder)
    y <- log1p(training$Total_MRR_12M)
    metrics <- function(a, p) {
      denominator <- sum((a - mean(a))^2)
      c(RMSE = sqrt(mean((a - p)^2)), MAE = mean(abs(a - p)),
        R2 = if (denominator > 0) 1 - sum((a - p)^2) / denominator else NA_real_)
    }
    set.seed(123)
    train_index <- sample(seq_len(nrow(x)), floor(0.8 * nrow(x)))
    test_index <- setdiff(seq_len(nrow(x)), train_index)
    progress(0.28, "Training browser-compatible historical revenue model")
    validation_model <- fit_browser_model(x[train_index, , drop = FALSE], y[train_index])
    predictions <- pmax(expm1(predict_browser_model(validation_model, x[test_index, , drop = FALSE])), 0)
    actual <- expm1(y[test_index])
    holdout <- metrics(actual, predictions)
    set.seed(123)
    folds <- sample(rep(seq_len(5L), length.out = nrow(x)))
    cv <- lapply(seq_len(5L), function(i) {
      progress(0.35 + i * 0.10, paste("Browser model CV fold", i))
      a <- which(folds != i)
      b <- which(folds == i)
      fit <- fit_browser_model(x[a, , drop = FALSE], y[a])
      m <- metrics(expm1(y[b]), pmax(expm1(predict_browser_model(fit, x[b, , drop = FALSE])), 0))
      tibble(Fold = i, RMSE = m[["RMSE"]], MAE = m[["MAE"]], R2 = m[["R2"]])
    }) %>% bind_rows()
    fit <- fit_browser_model(x, y)
    fitted <- pmax(expm1(predict_browser_model(fit, x)), 0)
    list(model = fit, encoder = encoder, eligible_rows = nrow(training), final_nrounds = NA_integer_,
         model_message = paste0("Browser-compatible ridge revenue model ready (lambda = ",
                                browser_model_lambda, "); training excludes revenue below $1,000."),
         historical_scores = tibble(GLM_ID = training$GLM_ID, Actual_Revenue = training$Total_MRR_12M,
                                    Predicted_Revenue = fitted),
         test_results = tibble(Actual_Revenue = actual, Predicted_Revenue = predictions), cv_results = cv,
         importance_matrix = browser_model_importance(fit),
         holdout_rmse = holdout[["RMSE"]], holdout_mae = holdout[["MAE"]], holdout_r2 = holdout[["R2"]])
  }, error = function(e) {
    empty$model_message <- paste("Browser regression unavailable:", conditionMessage(e))
    empty
  })
}

build_model_bundle <- function(file_path, workbook_name, sheet_name, progress_callback = NULL) {
  progress <- function(value, detail) {
    if (!is.null(progress_callback)) progress_callback(value, detail)
  }
  progress(0.03, "Reading on-net facility characteristics")
  extension <- tolower(tools::file_ext(workbook_name))
  if (!extension %in% c("xlsx", "xls")) {
    stop("Upload an Excel workbook with a .xlsx or .xls extension.")
  }
  raw <- if (identical(extension, "xlsx")) {
    read_xlsx(file_path, sheet = sheet_name, .name_repair = "minimal")
  } else {
    read_xls(file_path, sheet = sheet_name, .name_repair = "minimal")
  }
  if (anyNA(names(raw)) || any(!nzchar(names(raw))) || anyDuplicated(names(raw))) {
    stop("Workbook columns must have nonempty, unique headers.")
  }
  raw_source <- raw
  missing <- setdiff(required_columns, names(raw))
  if (length(missing)) stop("Missing required columns: ", paste(missing, collapse = ", "))
  for (name in setdiff(optional_columns, names(raw))) raw[[name]] <- NA
  numeric_columns <- c("Total_MRR_12M", "SQ_foot", "Bandwidth", "CAPEX", "TAM_(2030)",
                       "derived_tenant_count", "Tenant_Norm_Unique_UC_Count", "Unique_Billing_Customer_Count_12M",
                       "Unique_Billing_Customer_Count_3M", "QuoteCount_Last_12Months", "Maturity_Score")
  profile <- raw %>%
    select(all_of(unique(c(required_columns, optional_columns)))) %>%
    mutate(across(all_of(numeric_columns), clean_number),
           Year_Build_Raw = as.character(Year_Build), Year_Build = parse_build_year(Year_Build),
           across(all_of(c("GLM_ID", "address", "DC_Operator", "urban_rural", "NaaSEnabled")), clean_text),
           state = toupper(clean_text(state)), city = toupper(clean_text(city)),
           urban_rural = toupper(urban_rural), DC_Segments = normalize_segment(DC_Segments),
           is_on_net = toupper(trimws(as.character(is_on_net))),
           Capacity = Bandwidth, Historical_TAM_2030 = .data[[historical_tam_column]],
           Quote_Raw = as.character(.data[[quote_text_column]])) %>%
    filter(is_on_net == "Y", GLM_ID != "UNKNOWN")
  duplicates <- sum(duplicated(profile$GLM_ID))
  profile <- profile %>% distinct(GLM_ID, .keep_all = TRUE)
  profile$Quote_Tokens <- lapply(profile$Quote_Raw, quote_tokens)
  profile$Count_Conflict <- with(profile,
                                 is.finite(derived_tenant_count) & is.finite(Unique_Billing_Customer_Count_12M) &
                                   Unique_Billing_Customer_Count_12M > derived_tenant_count)
  profile$Billed_Percent_Normalized <- billed_percentage(
    profile$Unique_Billing_Customer_Count_12M, profile$Tenant_Norm_Unique_UC_Count)
  profile$Normalized_Base_Conflict <- is.finite(profile$Billed_Percent_Normalized) &
    profile$Billed_Percent_Normalized > 1
  all_onnet <- profile
  profile <- profile %>%
    filter(
      DC_Segments %in% valid_segments,
      is.finite(Total_MRR_12M),
      Total_MRR_12M >= minimum_revenue
    )
  
  full_onnet <- profile
  progress(0.12, "Calculating historical financials")
  if (nrow(profile)) {
    financials <- lapply(seq_len(nrow(profile)), function(i) {
      f <- calculate_4mc_financials(profile$Year_Build[[i]], profile$SQ_foot[[i]], profile$CAPEX[[i]])
      tibble(GLM_ID = profile$GLM_ID[[i]], Calculated_Historical_TAM_2030 = f$tam_2030,
             Calculated_Historical_TAM_10_Year = f$tam_10_year,
             Calculated_Historical_Gross_Revenue_10_Year = f$gross_revenue_10_year,
             Calculated_Historical_SOM_10_Year = f$som_10_year, Calculated_Historical_NPV = f$npv,
             Calculated_Historical_IRR = f$irr, Calculated_Historical_4MC_CAPEX = f$four_mc_capex)
    }) %>% bind_rows()
    profile <- left_join(profile, financials, by = "GLM_ID")
  } else {
    for (name in c("Calculated_Historical_TAM_2030", "Calculated_Historical_TAM_10_Year",
                   "Calculated_Historical_Gross_Revenue_10_Year", "Calculated_Historical_SOM_10_Year",
                   "Calculated_Historical_NPV", "Calculated_Historical_IRR", "Calculated_Historical_CAPEX")) {
      profile[[name]] <- numeric()
    }
  }
  secondary <- train_secondary_model(profile, progress)
  progress(1, "On-net profile ready")
  c(list(raw_rows = nrow(raw_source), onnet_rows = nrow(all_onnet),
         excluded_segment_rows = sum(!all_onnet$DC_Segments %in% valid_segments),
         duplicate_rows = duplicates, raw_source = raw_source,
         raw_glm_keys = clean_text(raw_source[[id_column]]), all_onnet_profile = all_onnet,
         full_onnet_profile = full_onnet, historical_profile = profile,
         quote_catalogue = quote_catalogue(profile)), secondary)
}

# -----------------------------------------------------------------------------
# Characteristic matching and transparent size-adjusted historical anchors
# -----------------------------------------------------------------------------
select_comparables <- function(profile, new_dc) {
  segment <- new_dc$DC_Segments[[1]]
  state <- new_dc$state[[1]]
  city <- new_dc$city[[1]]
  segment_pool <- profile %>% filter(DC_Segments == segment)
  state_pool <- segment_pool %>% filter(state != "UNKNOWN", .data$state == .env$state)
  city_pool <- state_pool %>% filter(city != "UNKNOWN", .data$city == .env$city)
  if (nrow(city_pool)) {
    pool <- city_pool; scope <- "City + state + segment"
  } else if (nrow(state_pool)) {
    pool <- state_pool; scope <- "State + segment"
  } else {
    pool <- segment_pool; scope <- "National segment"
  }
  sqft <- new_dc$SQ_foot[[1]]
  capacity <- new_dc$Capacity[[1]]
  urban <- new_dc$urban_rural[[1]]
  candidates <- pool %>% mutate(
    Size_Ratio = ifelse(is.finite(SQ_foot) & SQ_foot > 0, SQ_foot / sqft, NA_real_),
    SQFT_Distance = ifelse(is.finite(Size_Ratio), abs(log(Size_Ratio)), 2),
    Capacity_Distance = ifelse(is.finite(Capacity) & Capacity > 0 & is.finite(capacity) & capacity > 0,
                               abs(log(Capacity / capacity)), 1),
    Urban_Rural_Penalty = ifelse(urban_rural == "UNKNOWN" | urban == "UNKNOWN", 0.125,
                                 ifelse(urban_rural == urban, 0, 0.25)),
    Comparable_Score = SQFT_Distance + 0.25 * Capacity_Distance + Urban_Rural_Penalty) %>%
    arrange(Comparable_Score, GLM_ID)
  selected <- candidates %>% slice_head(n = historical_onnet_count) %>% mutate(Comparable_Rank = row_number())
  list(sites = selected, scope = scope, candidate_count = nrow(candidates),
       scope_counts = tibble(Scope = c("City + state + segment", "State + segment", "National segment"),
                             Sites = c(nrow(city_pool), nrow(state_pool), nrow(segment_pool))))
}

size_adjusted_anchors <- function(sites, target_sqft) {
  
  size_ok <-
    is.finite(sites$SQ_foot) &
    sites$SQ_foot > 0 &
    sites$SQ_foot >= 0.5 * target_sqft &
    sites$SQ_foot <= 2 * target_sqft
  
  revenue_ok <-
    size_ok &
    is.finite(sites$Total_MRR_12M) &
    sites$Total_MRR_12M >= 0
  
  tenant_ok <-
    size_ok &
    is.finite(sites$derived_tenant_count) &
    sites$derived_tenant_count >= 0
  
  customer_ok <-
    tenant_ok &
    is.finite(sites$Unique_Billing_Customer_Count_12M) &
    sites$Unique_Billing_Customer_Count_12M >= 0 &
    sites$Unique_Billing_Customer_Count_12M <=
    sites$derived_tenant_count
  
  revenue_scaled <- ifelse(
    revenue_ok,
    target_sqft *
      sites$Total_MRR_12M /
      sites$SQ_foot,
    NA_real_
  )
  
  tenants_scaled <- ifelse(
    tenant_ok,
    target_sqft *
      sites$derived_tenant_count /
      sites$SQ_foot,
    NA_real_
  )
  
  estimated_tenants <- safe_mean(tenants_scaled)
  
  #
  # IMPORTANT:
  # Use the same logic displayed in the benchmark cards
  #
  historical_average_tenants <-
    safe_mean(sites$derived_tenant_count)
  
  historical_average_customers <-
    safe_mean(sites$Unique_Billing_Customer_Count_12M)
  
  historical_penetration <-
    historical_average_customers /
    historical_average_tenants
  
  historical_penetration <-
    max(
      0,
      min(1, historical_penetration)
    )
  
  estimated_customers <-
    estimated_tenants *
    historical_penetration
  
  blended_ok <-
    revenue_ok &
    is.finite(sites$Unique_Billing_Customer_Count_12M) &
    sites$Unique_Billing_Customer_Count_12M > 0
  
  blended <- ifelse(
    blended_ok,
    sites$Total_MRR_12M /
      sites$Unique_Billing_Customer_Count_12M,
    NA_real_
  )
  
  trace <- tibble(
    GLM_ID = sites$GLM_ID,
    
    Square_Footage = sites$SQ_foot,
    
    Size_Within_0_5x_to_2x =
      size_ok,
    
    Observed_12M_Revenue =
      sites$Total_MRR_12M,
    
    Derived_Facility_Tenants =
      sites$derived_tenant_count,
    
    Billed_Customers_12M =
      sites$Unique_Billing_Customer_Count_12M,
    
    Historical_Penetration =
      historical_penetration,
    
    Count_Conflict =
      sites$Count_Conflict,
    
    Size_Adjusted_Revenue =
      revenue_scaled,
    
    Size_Adjusted_Tenants =
      tenants_scaled
  )
  
  list(
    revenue =
      safe_mean(revenue_scaled),
    
    tenants =
      estimated_tenants,
    
    customers =
      estimated_customers,
    
    historical_penetration =
      historical_penetration,
    
    blended_annual_per_customer =
      safe_mean(blended),
    
    revenue_n =
      sum(revenue_ok),
    
    tenants_n =
      sum(tenant_ok),
    
    customers_n =
      sum(customer_ok),
    
    blended_n =
      sum(blended_ok),
    
    trace =
      trace
  )
}
score_new_dc <- function(bundle, new_dc) {
  clean <- new_dc %>% mutate(Year_Build = parse_build_year(Year_Build),
                             across(all_of(numeric_predictors), clean_number), state = toupper(clean_text(state)),
                             city = toupper(clean_text(city)), urban_rural = toupper(clean_text(urban_rural)),
                             GLM_ID = clean_text(GLM_ID), DC_Segments = normalize_segment(DC_Segments))
  if (nrow(clean) != 1L || !clean$DC_Segments[[1]] %in% valid_segments) stop("Choose a valid segment 1-5.")
  if (!is.finite(clean$SQ_foot[[1]]) || clean$SQ_foot[[1]] <= 0) stop("Square footage must be greater than zero.")
  if (is.finite(clean$CAPEX[[1]]) && clean$CAPEX[[1]] < 0) stop("CAPEX input cannot be negative.")
  if (is.finite(clean$Capacity[[1]]) && clean$Capacity[[1]] < 0) stop("Capacity cannot be negative.")
  matches <- select_comparables(bundle$historical_profile, clean)
  sites <- matches$sites
  anchors <- size_adjusted_anchors(sites, clean$SQ_foot[[1]])
  predicted <- NA_real_
  unknown_levels <- character()
  if (!is.null(bundle$model)) {
    unknown_levels <- categorical_predictors[vapply(categorical_predictors,
                                                    function(name) !clean[[name]][[1]] %in% bundle$encoder$levels[[name]], logical(1))]
    predicted <- tryCatch(
      as.numeric(pmax(expm1(predict_browser_model(bundle$model, encode_features(clean, bundle$encoder))), 0)),
      error = function(e) NA_real_
    )
  }
  errors <- sites %>% left_join(bundle$historical_scores, by = "GLM_ID") %>%
    mutate(Error_Pct = abs(Actual_Revenue - Predicted_Revenue) / pmax(Actual_Revenue, 1))
  error <- safe_median(errors$Error_Pct)
  f <- calculate_4mc_financials(clean$Year_Build[[1]], clean$SQ_foot[[1]], clean$CAPEX[[1]])
  source_rows <- match(sites$GLM_ID, bundle$raw_glm_keys)
  if (anyNA(source_rows)) stop("Matched GLM has no original workbook record.")
  list(input = clean, comparable_facilities = sites,
       matched_raw = bundle$raw_source[source_rows, , drop = FALSE],
       comparable_scope = matches$scope,
       comparable_scope_counts = matches$scope_counts, comparable_candidate_count = matches$candidate_count,
       comparable_total = nrow(sites), anchors = anchors, quote_catalogue = quote_catalogue(sites),
       predicted_revenue = predicted, unknown_model_levels = unknown_levels, similar_glm_pct_error = error,
       error_sample = sum(is.finite(errors$Error_Pct)),
       heuristic_low = if (is.finite(error)) max(predicted * (1 - error), 0) else NA_real_,
       heuristic_high = if (is.finite(error)) predicted * (1 + error) else NA_real_,
       financials = f)
}

fmt_count <- function(x) {
  if (!is.finite(x)) "Unavailable" else format(round(x, 1), nsmall = 1, trim = TRUE)
}

input_number <- function(x) {
  if (length(x) != 1L) return(NA_real_)
  suppressWarnings(as.numeric(x))
}

# Presentation helpers: native details stay closed until explicitly expanded.
metric_card <- function(title, output_id, subtitle = NULL, class_name = "") {
  div(class = paste("metric-card", class_name), div(class = "metric-title", title),
      div(class = "metric-value", textOutput(output_id, inline = TRUE)),
      if (!is.null(subtitle)) div(class = "metric-subtitle", subtitle))
}

details_panel <- function(title, ...) {
  tags$details(class = "details-panel", tags$summary(title), div(class = "details-body", ...))
}

section_heading <- function(title, kicker = NULL) {
  div(class = "section-heading", div(
    if (!is.null(kicker)) div(class = "section-kicker", kicker), h4(title)))
}

site_input_tile <- function(label, value) {
  div(class = "input-display", div(class = "input-display-label", label),
      div(class = "input-display-value", value))
}

# -----------------------------------------------------------------------------
# Interface
# -----------------------------------------------------------------------------
ui <- fluidPage(
  class = "app-shell",
  tags$head(
    tags$style(HTML("
      body {background:#f4f6f8;color:#1f2937;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Arial,sans-serif;font-size:14px;line-height:1.5;}
      .app-shell {width:100%;padding:24px;}
      .app-header {display:flex;justify-content:space-between;align-items:center;gap:24px;margin-bottom:24px;}
      .app-title {color:#101828;font-size:30px;font-weight:800;line-height:1.2;margin:6px 0;letter-spacing:-.02em;}
      .app-subtitle {color:#667085;font-size:14px;margin-top:8px;}
      .app-kicker,.section-kicker,.sidebar-section-title {color:#667085;font-size:12px;font-weight:800;text-transform:uppercase;letter-spacing:.06em;}
      .workflow-badge {display:inline-block;background:#fff;border:1px solid #e4e7ec;border-radius:999px;padding:9px 14px;color:#475467;font-size:12px;font-weight:700;flex-shrink:0;}
      .app-layout {display:grid;grid-template-columns:320px minmax(0,1fr);gap:24px;align-items:start;}
      .main-content,.tab-content,.tab-pane,.section-panel,.user-input-card,.sidebar-panel {min-width:0;}
      .sidebar-panel,.section-panel,.user-input-card {background:#fff;border:1px solid #eaecf0;border-radius:12px;padding:18px;box-shadow:0 1px 4px rgba(16,24,40,.10);}
      .section-panel,.user-input-card {margin-bottom:18px;}
      .sidebar-section-title {margin:0 0 12px;}
      .sidebar-section + .sidebar-section,.sidebar-status {border-top:1px solid #eaecf0;margin-top:20px;padding-top:20px;}
      .sidebar-panel .form-group {margin-bottom:12px;}
      .sidebar-panel label {font-size:13px;font-weight:600;line-height:1.4;}
      .sidebar-note,.context-note {color:#667085;font-size:12px;line-height:1.5;}
      .sidebar-note {margin:8px 0 0;}
      .context-note {margin:10px 0 0;}
      .matching-summary,.scenario-description {background:#f8fafc;border:1px solid #dfe7ef;border-radius:10px;padding:15px 17px;margin:14px 0;}
      .matching-summary-title,.scenario-title {color:#101828;font-size:14px;font-weight:750;margin-bottom:8px;}
      .matching-summary ul {display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:7px 24px;margin:0;padding-left:20px;color:#475467;font-size:13px;}
      .positioning-panel {background:linear-gradient(135deg,#f8fafc,#eef4fb);border:1px solid #d8e4f0;border-radius:12px;padding:18px;margin-top:18px;}
      .positioning-grid,.growth-grid {display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px;margin-top:14px;}
      .growth-panel {background:#fff;border:1px solid #d8e4f0;border-radius:12px;padding:18px;margin-top:18px;}
      .growth-grid {grid-template-columns:repeat(4,minmax(0,1fr));}
      .growth-assumptions {background:#f8fafc;border:1px solid #e4e7ec;border-radius:10px;padding:14px;margin:14px 0;}
      .scenario-buttons .radio-inline {background:#fff;border:1px solid #d0d5dd;border-radius:8px;padding:10px 14px 10px 34px;margin:4px 8px 4px 0;font-weight:650;}
      .scenario-buttons .radio-inline:has(input:checked) {background:#f4f0ff;border-color:#7f56d9;color:#6941c6;}
      .sidebar-status .note {margin:0;font-size:12px;}
      .sidebar-panel .shiny-input-container {width:100%;max-width:100%;min-width:0;}
      .form-control {border-color:#d0d5dd;border-radius:8px;height:38px;box-shadow:none;color:#1f2937;}
      .form-control:focus {border-color:#7f56d9;box-shadow:0 0 0 3px rgba(127,86,217,.12);}
      .selectize-input {border-color:#d0d5dd;border-radius:8px;min-height:38px;padding:8px 12px;box-shadow:none;}
      .selectize-input.focus {border-color:#7f56d9;box-shadow:0 0 0 3px rgba(127,86,217,.12);}
      .selectize-input .item {overflow-wrap:anywhere;}
      .btn {border-radius:8px;padding:9px 14px;font-weight:600;}
      .sidebar-panel > .sidebar-section > .action-button {width:100%;}
      .sidebar-panel .btn-primary {background:#294f7a;border-color:#294f7a;}
      .sidebar-panel .btn-primary:hover,.sidebar-panel .btn-primary:focus {background:#203f62;border-color:#203f62;}
      .sidebar-panel .btn-success {background:#7f56d9;border-color:#7f56d9;}
      .sidebar-panel .btn-success:hover,.sidebar-panel .btn-success:focus {background:#6941c6;border-color:#6941c6;}
      .sidebar-panel .input-group-btn .btn {padding:8px 10px;}
      .nav-tabs {display:flex;flex-wrap:wrap;gap:6px;padding:6px;margin:0 0 18px;border:1px solid #eaecf0;border-radius:12px;background:#fff;box-shadow:0 1px 4px rgba(16,24,40,.08);}
      .nav-tabs:before,.nav-tabs:after {display:none;}
      .nav-tabs > li {float:none;margin:0;}
      .nav-tabs > li > a {color:#475467;font-weight:700;font-size:13px;border:0;border-radius:8px;margin:0;padding:11px 14px;}
      .nav-tabs > li > a:hover {border:0;background:#f2f4f7;color:#344054;}
      .nav-tabs > li.active > a,.nav-tabs > li.active > a:hover,.nav-tabs > li.active > a:focus {border:0;background:#f4f0ff;color:#6941c6;}
      .section-heading {display:flex;justify-content:space-between;align-items:flex-start;gap:16px;margin-bottom:14px;}
      .section-heading h4 {color:#101828;font-size:18px;font-weight:750;line-height:1.3;margin:4px 0 0;}
      .details-body h4 {color:#101828;font-size:16px;font-weight:700;margin:18px 0 12px;}
      .metric-grid {display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:14px;margin:16px 0;}
      .customer-metric-grid {grid-template-columns:repeat(4,minmax(0,1fr));}
      .metric-card {min-width:0;min-height:125px;background:#fff;border:1px solid #eaecf0;border-left:6px solid #0f6cbd;border-radius:12px;padding:18px;box-shadow:0 1px 4px rgba(16,24,40,.08);}
      .metric-title {color:#667085;font-size:12px;font-weight:700;text-transform:uppercase;letter-spacing:.04em;}
      .metric-value {color:#101828;font-size:27px;font-weight:800;line-height:1.15;margin-top:8px;overflow-wrap:anywhere;}
      .metric-subtitle {color:#667085;font-size:12px;line-height:1.5;margin-top:7px;}
      .hero-card {border-left-color:#7f56d9;background:linear-gradient(135deg,#fff,#f4f0ff);}
      .range-card {border-left-color:#0f6cbd;background:linear-gradient(135deg,#fff,#eef6ff);}
      .comparable-card {border-left-color:#12b76a;background:linear-gradient(135deg,#fff,#ecfdf3);}
      .finance-card {border-left-color:#f79009;background:linear-gradient(135deg,#fff,#fffaeb);}
      .scope-badge {display:inline-block;border-radius:999px;background:#e8f1fb;color:#0f548c;padding:6px 11px;font-weight:700;font-size:12px;overflow-wrap:anywhere;}
      .user-input-header {display:flex;justify-content:space-between;align-items:flex-start;gap:18px;padding-bottom:14px;border-bottom:1px solid #eaecf0;}
      .user-input-id {color:#101828;font-size:24px;font-weight:800;line-height:1.25;overflow-wrap:anywhere;}
      .user-input-location {color:#667085;font-size:15px;margin-top:4px;overflow-wrap:anywhere;}
      .segment-pill {background:#f4f0ff;color:#6941c6;border-radius:999px;padding:7px 12px;font-size:12px;font-weight:800;flex-shrink:0;}
      .user-input-grid {display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:14px;margin-top:16px;}
      .input-display {min-width:0;background:#f9fafb;border:1px solid #eaecf0;border-radius:10px;padding:14px;}
      .input-display-label {color:#667085;font-size:11px;font-weight:800;text-transform:uppercase;letter-spacing:.05em;}
      .input-display-value {color:#101828;font-size:19px;font-weight:750;line-height:1.3;margin-top:5px;overflow-wrap:anywhere;}
      .empty-state {background:#f9fafb;border:1px dashed #d0d5dd;border-radius:10px;padding:20px;color:#667085;}
      .empty-state strong {display:block;color:#344054;font-size:16px;margin-bottom:6px;}
      .plot-panel {min-width:0;background:#fff;border:1px solid #eaecf0;border-radius:12px;padding:18px;margin-top:18px;}
      .shiny-plot-output {max-width:100%;}
      .note,.warning {border-radius:8px;padding:11px 12px;margin:12px 0;font-size:13px;line-height:1.5;overflow-wrap:anywhere;}
      .note {background:#eef4fb;border-left:3px solid #0f6cbd;}
      .warning {background:#fffaeb;border-left:3px solid #f79009;color:#7a4700;}
      .warning ul {margin:0;padding-left:18px;}
      .details-panel {min-width:0;background:#fff;border:1px solid #eaecf0;border-radius:12px;margin:12px 0;box-shadow:0 1px 3px rgba(16,24,40,.05);}
      .details-panel > summary {cursor:pointer;padding:15px 18px;font-size:13px;font-weight:700;color:#344054;overflow-wrap:anywhere;border-radius:12px;}
      .details-panel > summary::marker {color:#7f56d9;}
      .details-panel > summary:hover {background:#f9fafb;}
      .details-panel[open] > summary {border-bottom:1px solid #eaecf0;border-radius:12px 12px 0 0;}
      .details-body {padding:16px 18px 18px;min-width:0;}
      .details-body p,.details-body li {font-size:13px;line-height:1.5;}
      .sidebar-panel .details-panel {box-shadow:none;margin:12px 0;}
      .sidebar-panel .details-panel > summary {font-size:12px;padding:11px 12px;}
      .sidebar-panel .details-body {padding:12px;}
      .reference-grid {display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px;margin:12px 0;}
      .reference-grid .metric-card {min-height:0;border-left:1px solid #eaecf0;padding:14px;background:#f9fafb;box-shadow:none;}
      .reference-grid .metric-value {font-size:20px;}
      table.dataTable {font-size:13px;border-collapse:separate !important;border-spacing:0;}
      table.dataTable thead th {background:#294f7a !important;color:#fff !important;border-bottom:0 !important;padding:12px 10px;vertical-align:middle;}
      table.dataTable thead th.sorting,table.dataTable thead th.sorting_asc,table.dataTable thead th.sorting_desc {padding-right:24px;}
      #comparable_compact table.dataTable thead th {padding-right:18px;}
      table.dataTable tbody td {padding:10px;vertical-align:middle;border-color:#eaecf0;}
      table.dataTable tbody tr:hover {background:#f4f7fb;}
      table.dataTable thead .form-control {color:#1f2937;background:#fff;}
      .compact-table-wrap {max-width:100%;min-width:0;overflow-x:auto;-webkit-overflow-scrolling:touch;}
      #comparable_compact table.dataTable {width:100% !important;min-width:1500px;table-layout:fixed;font-size:12px;}
      #comparable_compact table.dataTable th,#comparable_compact table.dataTable td {white-space:normal;overflow-wrap:anywhere;padding:10px 8px;}
      .dataTables_wrapper {min-width:0;font-size:12px;}
      .dataTables_wrapper .dataTables_filter {margin-bottom:12px;}
      .dataTables_wrapper .dataTables_filter input {border:1px solid #d0d5dd;border-radius:8px;padding:6px 8px;max-width:100%;}
      .dataTables_wrapper .dataTables_info {color:#667085;padding-top:12px;}
      .dataTables_wrapper .dataTables_paginate {padding-top:10px;}
      .dataTables_wrapper .dataTables_scroll {max-width:100%;}
      .dataTables_wrapper .dataTables_scrollBody {overscroll-behavior-x:contain;}
      a:focus-visible,button:focus-visible,input:focus-visible,select:focus-visible,summary:focus-visible {outline:2px solid #7f56d9;outline-offset:3px;}
      .skip-link {position:absolute;left:24px;top:-100px;background:#fff;color:#6941c6;padding:10px 14px;border-radius:8px;z-index:1000;}
      .skip-link:focus {top:8px;}
      @media(min-width:1600px) {.app-shell {padding:28px 32px;} .app-layout {grid-template-columns:340px minmax(0,1fr);gap:28px;}}
      @media(max-width:1399px) {.customer-metric-grid {grid-template-columns:repeat(2,minmax(0,1fr));}}
      @media(max-width:1199px) and (min-width:1100px) {.user-input-grid {grid-template-columns:repeat(2,minmax(0,1fr));} .metric-card {padding:14px;} .metric-value {font-size:24px;}}
      @media(max-width:1099px) {
        .app-layout {grid-template-columns:minmax(0,1fr);gap:18px;}
        .sidebar-panel {display:grid;grid-template-columns:minmax(0,.9fr) minmax(0,1.3fr);gap:20px;}
        .sidebar-section + .sidebar-section {border:0;margin:0;padding:0;}
        .sidebar-status {grid-column:1 / -1;margin:0;padding-top:16px;}
        .app-header {align-items:flex-start;}
        .workflow-badge {display:none;}
      }
      @media(max-width:767px) {
        .app-shell {padding:16px;}
        .app-title {font-size:26px;}
        .app-header {margin-bottom:18px;}
        .sidebar-panel {display:block;}
        .sidebar-section + .sidebar-section,.sidebar-status {border-top:1px solid #eaecf0;margin-top:18px;padding-top:18px;}
        .user-input-grid {grid-template-columns:repeat(2,minmax(0,1fr));gap:10px;}
        .metric-grid,.customer-metric-grid {grid-template-columns:1fr;gap:12px;}
        .metric-card {min-height:0;}
        .reference-grid {grid-template-columns:1fr;}
        .section-panel,.user-input-card {padding:16px;}
        .plot-panel {padding:12px;}
        .user-input-header {flex-wrap:wrap;gap:10px;}
        .nav-tabs > li {flex:1 1 calc(50% - 6px);text-align:center;}
        .nav-tabs > li > a {padding:10px 8px;font-size:12px;}
        .matching-summary ul,.positioning-grid,.growth-grid {grid-template-columns:1fr;}
      }
      @media(max-width:399px) {
        .app-shell {padding:12px;}
        .sidebar-panel,.section-panel,.user-input-card {padding:14px;}
        .user-input-grid {grid-template-columns:1fr;}
        .details-panel > summary {padding:13px 14px;}
        .details-body {padding:14px;}
      }
    ")),
    tags$script(HTML("
      (function () {
        function adjustTables() {
          if (window.jQuery && jQuery.fn.dataTable) {
            var tables = jQuery.fn.dataTable.tables({visible: true, api: true});
            tables.columns.adjust();
          }
        }
        function afterReveal() {
          window.requestAnimationFrame(function () {
            window.requestAnimationFrame(function () {
              adjustTables();
              window.dispatchEvent(new Event('resize'));
            });
          });
        }
        // Resize listeners only adjust tables: dispatching another resize here would loop.
        var resizeFrame;
        window.addEventListener('resize', function () {
          if (resizeFrame) window.cancelAnimationFrame(resizeFrame);
          resizeFrame = window.requestAnimationFrame(function () {
            resizeFrame = null;
            adjustTables();
          });
        });
        // Native details do not emit Bootstrap's shown event; notify Shiny on opening.
        document.addEventListener('toggle', function (event) {
          if (event.target.tagName === 'DETAILS' && event.target.open) afterReveal();
        }, true);
        if (window.jQuery) {
          jQuery(document).on('shown.bs.tab', afterReveal);
          jQuery(document).on('shiny:value init.dt', function () {
            window.requestAnimationFrame(adjustTables);
          });
        }
      })();
    "))
  ),
  tags$a(href = "#main-content", class = "skip-link", "Skip to results"),
  div(class = "app-header",
      div(div(class = "app-kicker", "In Progress"),
          h1(class = "app-title", "On-Net Data Center Modeling"),
          div(class = "app-subtitle", "Comparable facilities, revenue estimates, bandwidth growth scenarios, and financial analysis")),
      div(class = "workflow-badge", "Match → Benchmark → Evaluate")),
  div(class = "app-layout",
      tags$aside(class = "sidebar-panel", `aria-label` = "Workbook and proposed-site inputs",
                 div(class = "sidebar-section",
                     h4(class = "sidebar-section-title", "01 / Workbook"),
                     fileInput("model_file", "Modeling workbook", accept = c(".xlsx", ".xls"), placeholder = workbook_placeholder),
                     p(class = "sidebar-note", "The workbook is processed only in this browser session and is not part of the published site."),
                     textInput("sheet_name", "Sheet", default_sheet_name),
                     actionButton("load_model", "Load workbook", class = "btn-primary"),
                     uiOutput("load_status")),
                 div(class = "sidebar-section",
                     h4(class = "sidebar-section-title", "02 / Proposed site"),
                     textInput("glm_id", "GLM ID", "NEW_TX_DC"),
                     fluidRow(column(6, numericInput("sq_foot", "Size (sqft)", 100000, min = 1)),
                              column(6, numericInput("year_build", "Build year", 2028, min = 2026, step = 1))),
                     fluidRow(column(7, textInput("city", "City", "DALLAS")), column(5, textInput("state", "State", "TX"))),
                     selectInput("dc_segment", "Segment", paste("Segment", 1:5), "Segment 2"),
                     numericInput("capacity", "Capacity (MW)", 30, min = 0),
                     selectInput("urban_rural", "Urban / rural", c("URBAN", "URBAN AREA", "RURAL", "UNKNOWN"), "URBAN"),
                     numericInput("capex", "CAPEX ($; financials / revenue model only)", 500000, min = 0),
                     actionButton("score_dc", "Match proposed site", class = "btn-success"),
                     p(class = "sidebar-note", "Re-match after changing inputs. CAPEX does not affect matching."))
      ),
      tags$main(id = "main-content", class = "main-content", tabindex = "-1", tabsetPanel(
        tabPanel("Overview",
                 div(class="section-panel",section_heading("Comparable Datacenter Profile","HISTORICAL BENCHMARKS"),
                     uiOutput("loaded_workbook_context"),uiOutput("comparable_scope"),uiOutput("matching_explanation"),uiOutput("match_warnings"),
                     p(class="context-note","The metrics below summarize similar valid on-net data centers identified as the closest matches to the proposed site. These values provide historical context and are not predictions."),
                     div(class="metric-grid customer-metric-grid",
                         metric_card(
                           "Average DC tenants",
                           "average_tenants",
                           "Typical tenant count among similar on-net data centers.",
                           "hero-card"
                         ),
                         
                         metric_card(
                           "Average billed customers",
                           "average_customers",
                           "Typical customer count among similar on-net data centers.",
                           "comparable-card"
                         ),
                         
                         metric_card(
                           "Average annual revenue",
                           "average_revenue",
                           "Typical annual revenue (all products) among similar on-net data centers.",
                           "range-card"
                         ),
                         
                         metric_card(
                           "Observed revenue range",
                           "historical_revenue_range",
                           "Revenue range observed across similar on-net data centers.",
                           "range-card"
                         ))),
                 div(class="section-panel",section_heading("Projected Datacenter Metrics","PROPOSED Datacenter OUTLOOK"),
                     
                     div(class="metric-grid customer-metric-grid",
                         metric_card(
                           "Estimated DC tenants",
                           "overview_estimated_tenants",
                           "Expected tenant count for the proposed site.",
                           "hero-card"
                         ),
                         
                         metric_card(
                           "Estimated billed customers",
                           "overview_estimated_customers",
                           "Expected customer count for the proposed site.",
                           "comparable-card"
                         ),
                         
                         metric_card(
                           "Predicted annual revenue",
                           "overview_predicted_revenue",
                           "Expected annual revenue based on similar on-net data centers.",
                           "range-card"
                         ),
                         
                         metric_card(
                           "Predicted revenue range",
                           "overview_revenue_range",
                           "Likely revenue range based on similar on-net data centers.",
                           "range-card"
                         )),
                     
                     div(class="positioning-panel",section_heading("DC positioning","Predicted revenue compared with similar on-net data centers"),
                         div(class="positioning-grid",
                             metric_card("Revenue rank vs matched GLMs","position_revenue_rank_matched","Predicted revenue versus selected matches.","comparable-card"),
                             metric_card("Revenue rank vs historical portfolio","position_revenue_rank_portfolio","Predicted revenue versus all eligible historical GLMs.","range-card")),
                         uiOutput("positioning_context"),
                         fluidRow(column(6,selectInput("revenue_peer_scope","Revenue peer group",c("Entire historical portfolio"="portfolio","Matched GLMs only"="matched"),"portfolio")),
                                  column(6,div(style="padding-top:25px;",actionButton("show_revenue_peers","View Revenue Peers",class="btn-primary")))),
                         conditionalPanel("input.show_revenue_peers % 2 === 1",div(class="details-panel",div(class="details-body",h4("Revenue peers around the proposed site"),DTOutput("revenue_peers_table"))))),
                     div(class="growth-panel",section_heading("Future bandwidth growth opportunity","Greater demand and service consumption over time"),
                         p(class="context-note","This scenario starts with current predicted annual revenue and estimates how the opportunity could expand as bandwidth demand grows. It does not assume additional billed customers or a specific future product mix."),
                         div(class="scenario-buttons",
                             radioButtons("growth_scenario","Planning scenario",
                                          choices=c("No Growth"="no_growth","Demand Grows, Revenue Doesn't"="demand_no_revenue","Partial Monetization"="partial","Full Monetization"="full"),
                                          selected="partial",inline=TRUE)),
                         uiOutput("growth_scenario_description"),
                         div(class="growth-assumptions",fluidRow(
                           column(4,numericInput("growth_horizon_years","Forecast horizon (years)",5,min=1,max=10,step=1)),
                           column(4,numericInput("bandwidth_growth_rate","Annual bandwidth-demand growth (%)",10,min=0,max=100,step=1)),
                           column(4,sliderInput("revenue_capture_rate","Revenue capture of bandwidth growth",min=0,max=100,value=50,step=5,post="%"))),
                           p(class="context-note","Preset buttons populate the technical controls. You can then override any technical assumption manually.")),
                         div(class="growth-grid",
                             metric_card("Current revenue benchmark","growth_current_revenue","Current browser-model predicted annual revenue.","range-card"),
                             metric_card("Future revenue opportunity","growth_future_revenue","Opportunity at the end of the selected horizon.","hero-card"),
                             metric_card("Incremental opportunity","growth_incremental_revenue","Future opportunity minus current benchmark.","comparable-card"),
                             metric_card("Growth multiple","growth_multiple","Future opportunity divided by current benchmark.","comparable-card")),
                         div(class="plot-panel",plotOutput("future_growth_plot",height=360)),
                         details_panel("Annual bandwidth-demand growth outlook",DTOutput("future_growth_table"),
                                       p(class="context-note","Future revenue opportunity = current predicted revenue × [1 + capture rate × (bandwidth-demand index − 1)]. This is an assumption-driven opportunity view, not a committed forecast.")))
                 )
        ),
        tabPanel("Matched GLMs",
                 div(class = "section-panel", section_heading("Matched historical facilities", "Closest on-net facilities ranked by physical similarity"),
                     uiOutput("comparable_scope_detail"), uiOutput("match_warnings_detail"),
                     p(class = "context-note", "Annual revenue: Aug 2025-Jul 2026; all products. Customer counts are unique per GLM, not across sites. Quotes are not sales."),
                     div(class = "compact-table-wrap", DTOutput("comparable_compact")),
                     uiOutput("matched_average_summary"),
                     p(class = "context-note", "% billed uses the normalized base, not facility tenants. Click a row to see its complete workbook record.")),
                 div(class = "section-panel", section_heading("Selected GLM: full workbook record"),
                     selectInput("detail_glm", "GLM", choices = character()),
                     DTOutput("selected_glm_record")),
                 details_panel("All original workbook fields & CSV", DTOutput("all_fields_table"),
                               downloadButton("download_matched_rows", "Download matched workbook rows (CSV)"),
                               p(class = "context-note", "Original headers and values retained, including raw quote text. The CSV includes all original fields, without replacing source percentages.")),
                 details_panel("Quoted offerings & raw quote information",
                               DTOutput("quote_table"), DTOutput("quote_site_table"),
                               p(class = "context-note", "Site mentions are not product-level quote counts or purchases. Blank quote detail means unknown."))
        ),
        tabPanel("Financials",
                 div(class = "section-panel", section_heading("Separate financial case", "Independent 400G assumptions"),
                     p(class = "context-note", "400G assumption-based case, not observed annual revenue. Original financial timing and assumptions retained; not used for matching."),
                     div(class = "metric-grid", metric_card("NPV", "npv_card", class_name = "finance-card"),
                         metric_card("IRR", "irr_card", class_name = "finance-card"),
                         metric_card("2030 TAM", "tam_2030_card", class_name = "finance-card"))),
                 details_panel("Additional financial summaries",
                               div(class = "reference-grid", metric_card("10-year SOM", "som_card"),
                                   metric_card("10-year gross revenue", "gross_revenue_card"), metric_card("CAPEX", "four_mc_capex_card"),
                                   metric_card("10-year TAM", "tam_10_year_card"))),
                 details_panel("Annual proposed-site projection", DTOutput("tam_table")),
                 details_panel("4MC assumptions", DTOutput("assumptions_table")),
                 details_panel("Historical site financials",
                               p("Unknown/non-numeric build years remain unavailable, not year zero. Source TAM is retained separately."),
                               DTOutput("historical_financials_table"))
        ),
        tabPanel("Model details",
                 div(class = "section-panel", section_heading("Model status & historical signal", "Browser revenue model"),
                     uiOutput("model_status"), uiOutput("secondary_status"),
                     p(class = "context-note", "Historical all-product prediction, not future revenue or customer growth."),
                     div(class = "reference-grid",
                         metric_card("Historical annual revenue signal", "predicted_revenue"),
                         metric_card("In-sample error heuristic", "heuristic_range", "Not a confidence interval."),
                         metric_card("Median absolute percentage error", "similar_error_pct", "Selected fitted sites; not cross-validation error."),
                         metric_card("CV RMSE", "cv_rmse"), metric_card("CV R-squared", "cv_r2"))),
                 details_panel("Held-out plot, CV folds & feature importance",
                               plotOutput("actual_predicted_plot", height = 300),
                               h4("Cross-validation folds"), DTOutput("cv_table"),
                               h4("Feature importance"), DTOutput("importance_table")),
                 details_panel("Matching & calculation methods",
                               p("Same source pool: confirmed on-net GLMs, segments 1–5, finite annual revenue of at least $1,000. Zero and lower revenue remain excluded by the existing filter."),
                               p("City/state/segment, then state/segment, then national segment; up to 25 closest physical matches. Small pools are not padded. Size, capacity and urban/rural determine closeness."),
                               DTOutput("scope_table"),
                               p("Business summaries use arithmetic averages of valid site values. % billed averages per-GLM ratios, not pooled counts. Unique counts cannot be deduplicated across GLMs."),
                               p("The browser-compatible ridge model uses log annual revenue, standardized encoded predictors, lambda = 1, and deterministic five-fold cross-validation. Numeric missing-value imputation and the fitted-error heuristic use medians; heuristic bounds are not calibrated intervals.")),
                 details_panel("Average samples & size-adjusted references",
                               DTOutput("average_samples_table"),
                               div(class = "reference-grid", metric_card("Average size-adjusted annual revenue", "size_anchor"),
                                   metric_card("Average size-adjusted facility tenants", "tenant_anchor"),
                                   metric_card("Average size-adjusted billed customers", "customer_anchor"),
                                   metric_card("Average annual revenue / billed customer", "blended_annual")),
                               uiOutput("anchor_notes"), DTOutput("anchor_trace")),
                 details_panel("Data quality & unavailable detail",
                               DTOutput("quality_table"), DTOutput("matched_quality_table"),
                               p("Annual revenue is Total_MRR_12M: the existing Aug 2025–Jul 2026 billing sum, never multiplied by 12. Percentages above 100% are shown and flagged, not capped."),
                               p("Purchased-product revenue, legacy split, monthly trends, quote conversion and product prices are unavailable. Recent 3M customer counts are not a growth rate; quotes are not wins."))
        )
      ))
  )
)


# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  model_bundle <- reactiveVal(NULL)
  scoring_result <- reactiveVal(NULL)
  model_message <- reactiveVal("Choose a workbook and click Load workbook.")
  load_message <- reactiveVal("Choose a workbook and click Load workbook.")
  observeEvent(input$load_model, {
    if (is.null(input$model_file)) {
      showNotification("Choose an Excel workbook before loading.", type = "error")
      return(invisible(NULL))
    }
    path <- input$model_file$datapath
    workbook_name <- input$model_file$name
    model_bundle(NULL); scoring_result(NULL)
    updateSelectInput(session, "detail_glm", choices = character(), selected = character())
    load_message("Loading workbook…")
    model_message("Loading characteristics and browser-compatible calculations…")
    tryCatch({
      bundle <- withProgress(message = "Loading on-net evidence", value = 0, {
        build_model_bundle(path, workbook_name, input$sheet_name,
                           function(value, detail) setProgress(value = value, detail = detail))
      })
      bundle$workbook_label <- workbook_name
      bundle$sheet_name <- input$sheet_name
      model_bundle(bundle)
      load_message(paste("Workbook ready:", comma(nrow(bundle$historical_profile)), "eligible GLMs. Match the site next."))
      model_message(paste("On-net profile ready.", bundle$model_message))
      showNotification("Workbook loaded. Match the proposed site next.", type = "message")
    }, error = function(e) {
      model_message(paste("Load failed:", conditionMessage(e)))
      load_message(model_message())
      showNotification(model_message(), type = "error", duration = NULL)
    })
  }, ignoreInit = TRUE)
  observeEvent(input$score_dc, {
    req(model_bundle())
    site <- tibble(GLM_ID = input$glm_id, Year_Build = input_number(input$year_build), SQ_foot = input_number(input$sq_foot),
                   Capacity = input_number(input$capacity), CAPEX = input_number(input$capex), state = input$state, city = input$city,
                   urban_rural = input$urban_rural, DC_Segments = input$dc_segment)
    tryCatch({
      result <- score_new_dc(model_bundle(), site)
      scoring_result(result)
      ids <- result$comparable_facilities$GLM_ID
      updateSelectInput(session, "detail_glm", choices = ids, selected = if (length(ids)) ids[[1]] else character())
    }, error = function(e) {
      scoring_result(NULL)
      updateSelectInput(session, "detail_glm", choices = character(), selected = character())
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  }, ignoreInit = TRUE)
  observeEvent(input$comparable_compact_rows_selected, {
    r <- scoring_result(); req(r)
    index <- input$comparable_compact_rows_selected
    if (length(index) == 1L && index >= 1L && index <= nrow(r$comparable_facilities)) {
      updateSelectInput(session, "detail_glm", selected = r$comparable_facilities$GLM_ID[[index]])
    }
  }, ignoreInit = TRUE)
  
  output$load_status <- renderUI(p(class = "sidebar-note", load_message()))
  output$model_status <- renderUI({
    b <- model_bundle()
    div(class = "note", tags$strong(model_message()), if (!is.null(b)) tagList(tags$br(),
                                                                               paste("Confirmed on-net:", comma(b$onnet_rows), "| eligible comparable GLMs:", comma(nrow(b$historical_profile))),
                                                                               tags$br(), paste("Browser-model training rows:", comma(b$eligible_rows))))
  })
  output$loaded_workbook_context <- renderUI({
    b <- model_bundle(); req(b)
    p(class = "context-note", paste("Loaded:", b$workbook_label, "| sheet:", b$sheet_name,
                                    "| eligible GLMs:", comma(nrow(b$historical_profile))))
  })
  
  scope_ui <- function() {
    r <- scoring_result(); req(r)
    span(class = "scope-badge", paste(r$comparable_scope, "|", r$comparable_total, "matched /", r$comparable_candidate_count, "candidates"))
  }
  output$comparable_scope <- renderUI(scope_ui())
  output$comparable_scope_detail <- renderUI(scope_ui())
  output$matching_explanation <- renderUI({
    r <- scoring_result(); req(r)
    div(class="matching-summary",div(class="matching-summary-title","Why these data centers were selected for comparison"),tags$ul(
      tags$li(tags$strong("Required segment: "),paste0("Segment ",r$input$DC_Segments[[1]])),
      tags$li(tags$strong("Geographic scope: "),r$comparable_scope),
      tags$li(tags$strong("Similarity: "),"square footage, capacity and urban/rural classification"),
      tags$li(tags$strong("Selection: "),paste0(r$comparable_total," of ",r$comparable_candidate_count," candidates"))))
  })
  warning_ui <- function() {
    r <- scoring_result(); req(r)
    sites <- r$comparable_facilities
    notes <- character()
    if (!nrow(sites)) notes <- c(notes, "No eligible on-net GLMs in this segment.")
    if (nrow(sites) > 0 && nrow(sites) < 5) notes <- c(notes, "Fewer than 5 matches; averages are descriptive only.")
    conflicts <- sum(sites$Normalized_Base_Conflict)
    if (conflicts) notes <- c(notes, paste(conflicts, "GLMs exceed 100% billed: billed/base counts differ; values are not capped."))
    invalid <- sum(!is.finite(sites$Billed_Percent_Normalized))
    if (invalid) notes <- c(notes, paste(invalid, "GLMs have unavailable billed percentages."))
    if (length(notes)) div(class = "warning", paste(notes, collapse = " ")) else NULL
  }
  output$match_warnings <- renderUI(warning_ui())
  output$match_warnings_detail <- renderUI(warning_ui())
  output$scope_table <- renderDT({
    r <- scoring_result(); req(r)
    datatable(r$comparable_scope_counts, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })
  averages <- reactive({ r <- scoring_result(); req(r); matched_measures(r$comparable_facilities) })
  output$average_tenants <- renderText(fmt_count(averages()$Average[[1]]))
  ###output$average_normalized_base <- renderText(fmt_count(averages()$Average[[2]]))
  output$average_customers <- renderText(fmt_count(averages()$Average[[3]]))
  #####output$average_billed_percent <- renderText(fmt_percent(averages()$Average[[4]]))
  output$average_revenue <- renderText(fmt_dollar(averages()$Average[[5]]))
  output$historical_revenue_range <- renderText({
    
    r <- scoring_result()
    req(r)
    
    revenue_values <- r$comparable_facilities$Total_MRR_12M
    
    revenue_values <- revenue_values[
      is.finite(revenue_values) &
        revenue_values >= 0
    ]
    
    if (!length(revenue_values)) {
      return("Unavailable")
    }
    
    paste(
      fmt_dollar(min(revenue_values)),
      "-",
      fmt_dollar(max(revenue_values))
    )
  })
  output$overview_estimated_tenants <- renderText({r<-scoring_result();req(r);fmt_count(r$anchors$tenants)})
  output$overview_estimated_customers <- renderText({r<-scoring_result();req(r);fmt_count(r$anchors$customers)})
  
  output$overview_predicted_revenue <- renderText({r<-scoring_result();req(r);fmt_dollar(r$predicted_revenue)})
  
  output$overview_revenue_range <- renderText({r<-scoring_result();req(r);if(is.finite(r$heuristic_low)&&is.finite(r$heuristic_high)) paste(fmt_dollar(r$heuristic_low),"-",fmt_dollar(r$heuristic_high)) else "Unavailable"})
  output$position_revenue_rank_matched <- renderText({r<-scoring_result();req(r);x<-r$comparable_facilities$Total_MRR_12M;x<-x[is.finite(x)];if(!is.finite(r$predicted_revenue)||!length(x)) "Unavailable" else paste0(safe_rank(x,r$predicted_revenue)," of ",length(x))})
  output$position_revenue_rank_portfolio <- renderText({r<-scoring_result();b<-model_bundle();req(r,b);x<-b$historical_profile$Total_MRR_12M;x<-x[is.finite(x)];if(!is.finite(r$predicted_revenue)||!length(x)) "Unavailable" else paste0(safe_rank(x,r$predicted_revenue)," of ",length(x))})
  output$positioning_context <- renderUI({r<-scoring_result();b<-model_bundle();req(r,b);div(class="note",paste("Predicted annual revenue",fmt_dollar(r$predicted_revenue),"is compared with",sum(is.finite(r$comparable_facilities$Total_MRR_12M)),"matched GLMs and",sum(is.finite(b$historical_profile$Total_MRR_12M)),"eligible historical GLMs."))})
  
  observeEvent(input$growth_scenario, {
    presets <- list(
      no_growth=list(growth=0,capture=100),
      demand_no_revenue=list(growth=10,capture=0),
      partial=list(growth=10,capture=50),
      full=list(growth=10,capture=100))
    x <- presets[[input$growth_scenario]]
    updateNumericInput(session,"bandwidth_growth_rate",value=x$growth)
    updateSliderInput(session,"revenue_capture_rate",value=x$capture)
  },ignoreInit=FALSE)
  output$growth_scenario_description <- renderUI({
    descriptions <- list(
      no_growth=list(title="No Growth",text="Bandwidth demand remains flat, so future revenue stays aligned with today's predicted revenue."),
      demand_no_revenue=list(title="Demand Grows, Revenue Doesn't",text="Bandwidth consumption increases, but none of the incremental demand is converted into additional revenue."),
      partial=list(title="Partial Monetization",text="Bandwidth demand increases and only part of the incremental demand becomes revenue. This is the default balanced scenario."),
      full=list(title="Full Monetization",text="Bandwidth demand increases and all incremental demand is reflected in revenue growth."))
    x<-descriptions[[input$growth_scenario]]
    div(class="scenario-description",div(class="scenario-title",x$title),div(x$text))
  })
  future_growth_data <- reactive({r<-scoring_result();req(r);future_bandwidth_growth(r$predicted_revenue,r$input$Year_Build[[1]],input$growth_horizon_years,input$bandwidth_growth_rate/100,input$revenue_capture_rate/100)})
  output$growth_current_revenue <- renderText({r<-scoring_result();req(r);fmt_dollar(r$predicted_revenue)})
  output$growth_future_revenue <- renderText({d<-future_growth_data();if(nrow(d)) fmt_dollar(tail(d$Future_Revenue_Opportunity,1)) else "Unavailable"})
  output$growth_incremental_revenue <- renderText({d<-future_growth_data();if(nrow(d)) fmt_dollar(tail(d$Incremental_Revenue_Opportunity,1)) else "Unavailable"})
  output$growth_multiple <- renderText({d<-future_growth_data();if(nrow(d)) paste0(round(tail(d$Revenue_Growth_Index,1),2),"x") else "Unavailable"})
  output$future_growth_plot <- renderPlot({d<-future_growth_data();validate(need(nrow(d)>0,"Valid revenue and growth assumptions are required."));ggplot(d,aes(Year,Future_Revenue_Opportunity))+geom_area(fill="#dbeafe",alpha=.65)+geom_line(linewidth=1.25,color="#0f6cbd")+geom_point(size=2.8,color="#0f6cbd")+geom_hline(yintercept=d$Future_Revenue_Opportunity[[1]],linetype=2,color="#667085")+scale_x_continuous(breaks=d$Year)+scale_y_continuous(labels=dollar)+labs(x=NULL,y="Annual revenue opportunity",title="Future bandwidth growth opportunity",subtitle="Current predicted revenue expanded by bandwidth-demand growth")+theme_minimal()})
  output$future_growth_table <- renderDT({d<-future_growth_data();validate(need(nrow(d)>0,"Valid revenue and growth assumptions are required."));display<-d%>%transmute(Year,`Year from start`=Year_From_Start,`Bandwidth-demand index`=Bandwidth_Demand_Index,`Revenue capture rate`=Revenue_Capture_Rate,`Revenue growth index`=Revenue_Growth_Index,`Future revenue opportunity`=Future_Revenue_Opportunity,`Incremental opportunity`=Incremental_Revenue_Opportunity);datatable(display,rownames=FALSE,options=list(dom="t",ordering=FALSE,scrollX=TRUE))%>%formatRound(c("Bandwidth-demand index","Revenue growth index"),2)%>%formatPercentage("Revenue capture rate",digits=0)%>%formatCurrency(c("Future revenue opportunity","Incremental opportunity"),"$",digits=0)})
  output$revenue_peers_table <- renderDT({
    r<-scoring_result();b<-model_bundle();req(r,b);validate(need(is.finite(r$predicted_revenue),"Revenue prediction unavailable."))
    ref<-if(identical(input$revenue_peer_scope,"matched")) r$comparable_facilities else b$historical_profile
    ref<-ref%>%transmute(GLM_ID,Location=paste(city,state,sep=", "),Segment=DC_Segments,`Annual Revenue`=Total_MRR_12M,`Facility Tenants`=derived_tenant_count,`Billed Customers`=Unique_Billing_Customer_Count_12M,`Size (sqft)`=SQ_foot,Capacity,`Build Year`=Year_Build,`Urban / Rural`=urban_rural,Proposed=FALSE)%>%filter(is.finite(`Annual Revenue`))
    prop<-tibble(GLM_ID=r$input$GLM_ID[[1]],Location=paste(r$input$city[[1]],r$input$state[[1]],sep=", "),Segment=r$input$DC_Segments[[1]],`Annual Revenue`=r$predicted_revenue,`Facility Tenants`=r$anchors$tenants,`Billed Customers`=r$anchors$customers,`Size (sqft)`=r$input$SQ_foot[[1]],Capacity=r$input$Capacity[[1]],`Build Year`=r$input$Year_Build[[1]],`Urban / Rural`=r$input$urban_rural[[1]],Proposed=TRUE)
    z<-bind_rows(ref,prop)%>%arrange(desc(`Annual Revenue`),GLM_ID)%>%mutate(Rank=min_rank(desc(`Annual Revenue`)));i<-which(z$Proposed);z<-z[max(1,i-10):min(nrow(z),i+10),]%>%transmute(Rank,Position=ifelse(Proposed,"PROPOSED SITE","Historical GLM"),GLM_ID,Location,Segment,`Annual Revenue`,`Facility Tenants`,`Billed Customers`,`Size (sqft)`,Capacity,`Build Year`,`Urban / Rural`)
    datatable(z,rownames=FALSE,selection="none",options=list(dom="tip",pageLength=25,ordering=FALSE,scrollX=TRUE))%>%formatCurrency("Annual Revenue","$",digits=0)%>%formatRound(c("Facility Tenants","Billed Customers","Capacity"),1)%>%formatStyle("Position",target="row",backgroundColor=styleEqual("PROPOSED SITE","#e8f1fb"),fontWeight=styleEqual("PROPOSED SITE","700"))
  })
  output$matched_average_summary <- renderUI({
    r <- scoring_result(); req(r); a <- averages()
    p(class = "context-note", paste0("Averages (", r$comparable_total, " matched GLMs): facility tenants ", fmt_count(a$Average[[1]]),
                                     " | normalized base ", fmt_count(a$Average[[2]]), " | billed customers ", fmt_count(a$Average[[3]]),
                                     " | % billed ", fmt_percent(a$Average[[4]]), " | annual revenue ", fmt_dollar(a$Average[[5]]), "."))
  })
  output$average_samples_table <- renderDT({
    a <- averages()
    display <- a %>% mutate(Average = c(fmt_count(a$Average[[1]]), fmt_count(a$Average[[2]]),
                                        fmt_count(a$Average[[3]]), fmt_percent(a$Average[[4]]), fmt_dollar(a$Average[[5]]))) %>%
      rename(`Valid GLMs (n)` = Valid_GLMS, `Matched GLMs` = Matched_GLMS)
    datatable(display, rownames = FALSE, options = list(dom = "t", ordering = FALSE, scrollX = TRUE))
  })
  output$size_anchor <- renderText({ r <- scoring_result(); req(r); fmt_dollar(r$anchors$revenue) })
  output$tenant_anchor <- renderText({ r <- scoring_result(); req(r); fmt_count(r$anchors$tenants) })
  output$customer_anchor <- renderText({ r <- scoring_result(); req(r); fmt_count(r$anchors$customers) })
  output$blended_annual <- renderText({
    r <- scoring_result(); req(r)
    paste(fmt_dollar(r$anchors$blended_annual_per_customer), "| n =", r$anchors$blended_n)
  })
  output$anchor_notes <- renderUI({
    r <- scoring_result(); req(r); a <- r$anchors
    p(class = "context-note", paste("Size-adjusted means: per-site value / sqft × proposed sqft; sizes 0.5–2× proposed.",
                                    "Samples: revenue", a$revenue_n, "| tenants", a$tenants_n, "| constrained customers", a$customers_n,
                                    ". Customer reference requires billed customers ≤ facility tenant estimate; it is not a forecast or the % billed denominator."))
  })
  output$anchor_trace <- renderDT({
    r <- scoring_result(); req(r)
    datatable(r$anchors$trace, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatCurrency(c("Observed_12M_Revenue", "Size_Adjusted_Revenue"), "$", digits = 0) %>%
      formatRound(c("Size_Adjusted_Tenants", "Size_Adjusted_Customers"), 1)
  })
  output$quote_table <- renderDT({
    r <- scoring_result(); req(r)
    validate(need(nrow(r$quote_catalogue) > 0, "No recorded quote category detail; demand is unknown."))
    datatable(r$quote_catalogue %>% select(Category, Site_Mentions) %>%
                rename(`Quoted offering` = Category, `Matched GLMs mentioning` = Site_Mentions),
              rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })
  output$quote_site_table <- renderDT({
    r <- scoring_result(); req(r)
    r$comparable_facilities %>% transmute(GLM_ID,
                                          `Raw quoted categories` = Quote_Raw,
                                          `Unique category mentions` = vapply(Quote_Tokens, paste, character(1), collapse = " | "),
                                          `Aggregate site quote count` = QuoteCount_Last_12Months) %>%
      datatable(rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$comparable_compact <- renderDT({
    r<-scoring_result();req(r);validate(need(nrow(r$comparable_facilities)>0,"No comparable GLMs in this segment."))
    short<-function(x){if(!length(x)) return("No recorded detail");paste(head(x,2),collapse=", ")}
    d<-r$comparable_facilities%>%transmute(`GLM / Site`=if_else(!is.na(DC_Operator)&nzchar(DC_Operator),paste(GLM_ID,DC_Operator,sep=" | "),GLM_ID),Location=paste(city,state,sep=", "),Segment=DC_Segments,`Match Score`=Comparable_Score,`Annual Revenue`=Total_MRR_12M,`Facility Tenants`=derived_tenant_count,`Billed Customers`=Unique_Billing_Customer_Count_12M,`Size (sqft)`=SQ_foot,Capacity,CAPEX,`Build Year`=Year_Build,`Urban / Rural`=urban_rural,`Quoted Offerings`=vapply(Quote_Tokens,short,character(1)))
    datatable(d,rownames=FALSE,class="compact stripe hover",selection="single",options=list(dom="ftip",pageLength=10,scrollX=TRUE,order=list(list(3,"asc"))))%>%formatCurrency(c("Annual Revenue","CAPEX"),"$",digits=0)%>%formatStyle("Match Score",backgroundColor=styleInterval(c(.3,.8),c("#d1fae5","#fef3c7","#fee2e2")),fontWeight="600")%>%formatRound(c("Facility Tenants","Billed Customers","Capacity"),1)%>%formatRound("Match Score",3)%>%formatRound("Size (sqft)",0)
  })
  output$selected_glm_record <- renderDT({
    r <- scoring_result(); req(r)
    validate(need(nrow(r$matched_raw) > 0, "No matched workbook records."))
    keys <- clean_text(r$matched_raw[[id_column]])
    index <- match(input$detail_glm, keys)
    req(length(index) == 1L, !is.na(index))
    values <- vapply(r$matched_raw, function(column) {
      value <- column[index]
      if (is.na(value)) "Missing" else as.character(value)
    }, character(1))
    datatable(tibble(`Workbook field` = names(r$matched_raw), `Source value` = values),
              rownames = FALSE, options = list(dom = "ft", paging = FALSE, ordering = FALSE,
                                               columnDefs = list(list(width = "40%", targets = 0))))
  })
  output$all_fields_table <- renderDT({
    r <- scoring_result(); req(r)
    validate(need(nrow(r$matched_raw) > 0, "No matched workbook records."))
    datatable(r$matched_raw, rownames = FALSE, filter = "top", selection = "none",
              options = list(pageLength = 10, scrollX = TRUE, order = list()))
  })
  output$download_matched_rows <- downloadHandler(
    filename = function() "OnNet_Matched_GLMs_All_Workbook_Fields.csv",
    content = function(file) {
      r <- scoring_result(); req(r)
      validate(need(nrow(r$matched_raw) > 0, "Match GLMs before downloading."))
      write.csv(r$matched_raw, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    })
  
  output$secondary_status <- renderUI({
    b <- model_bundle(); req(b)
    r <- scoring_result()
    tagList(div(class = "note", b$model_message), if (!is.null(r) && length(r$unknown_model_levels))
      div(class = "warning", paste("Unseen model factor fields (all trained flags zero):", paste(r$unknown_model_levels, collapse = ", "))))
  })
  output$predicted_revenue <- renderText({ r <- scoring_result(); req(r); fmt_dollar(r$predicted_revenue) })
  output$heuristic_range <- renderText({
    r <- scoring_result(); req(r)
    paste(fmt_dollar(r$heuristic_low), "-", fmt_dollar(r$heuristic_high))
  })
  output$similar_error_pct <- renderText({
    r <- scoring_result(); req(r)
    paste(fmt_percent(r$similar_glm_pct_error), "| n =", r$error_sample)
  })
  output$cv_rmse <- renderText({ b <- model_bundle(); req(b); fmt_dollar(safe_mean(b$cv_results$RMSE)) })
  output$cv_r2 <- renderText({
    b <- model_bundle(); req(b); x <- safe_mean(b$cv_results$R2)
    if (is.finite(x)) round(x, 3) else "Unavailable"
  })
  output$actual_predicted_plot <- renderPlot({
    b <- model_bundle(); req(b)
    validate(need(nrow(b$test_results) > 0, "Browser historical revenue model unavailable; physical comparables still work."))
    ggplot(b$test_results, aes(Actual_Revenue, Predicted_Revenue)) + geom_point(alpha = 0.6, color = "#0f6cbd") +
      geom_abline(slope = 1, intercept = 0, linetype = 2) + scale_x_continuous(labels = dollar) +
      scale_y_continuous(labels = dollar) + labs(x = "Actual observed 12M all-product revenue", y = "Predicted 12M all-product revenue") + theme_minimal()
  })
  output$cv_table <- renderDT({
    b <- model_bundle(); req(b)
    validate(need(nrow(b$cv_results) > 0, b$model_message))
    datatable(b$cv_results, rownames = FALSE, options = list(pageLength = 5)) %>%
      formatCurrency(c("RMSE", "MAE"), "$", digits = 0) %>% formatRound("R2", 3)
  })
  output$importance_table <- renderDT({
    b <- model_bundle(); req(b)
    validate(need(nrow(b$importance_matrix) > 0, b$model_message))
    datatable(head(as.data.frame(b$importance_matrix), 25), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$npv_card <- renderText({ r <- scoring_result(); req(r); fmt_dollar(r$financials$npv) })
  output$irr_card <- renderText({ r <- scoring_result(); req(r); fmt_percent(r$financials$irr) })
  output$som_card <- renderText({ r <- scoring_result(); req(r); fmt_dollar(r$financials$som_10_year) })
  output$gross_revenue_card <- renderText({ r <- scoring_result(); req(r); fmt_dollar(r$financials$gross_revenue_10_year) })
  output$four_mc_capex_card <- renderText({ r <- scoring_result(); req(r); fmt_dollar(r$financials$four_mc_capex) })
  output$tam_2030_card <- renderText({ r <- scoring_result(); req(r); fmt_dollar(r$financials$tam_2030) })
  output$tam_10_year_card <- renderText({ r <- scoring_result(); req(r); fmt_dollar(r$financials$tam_10_year) })
  output$tam_table <- renderDT({
    r <- scoring_result(); req(r)
    validate(need(nrow(r$financials$projection) > 0, "Financials requires known build year, size and CAPEX."))
    datatable(r$financials$projection, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatCurrency(c("Monthly_400G_Price", "Annual_TAM", "Revenue_Share", "CAPEX_Cash_Flow", "SOM_Lit_400G"), "$", digits = 0) %>%
      formatPercentage(c("CRT", "Market_Share"), digits = 1) %>%
      formatRound(c("SFBW", "Total_Conversion_Factor"), 8)
  })
  output$assumptions_table <- renderDT({
    datatable(four_mc_assumptions_data(), rownames = FALSE, options = list(dom = "t", ordering = FALSE, scrollX = TRUE))
  })
  output$historical_financials_table <- renderDT({
    b <- model_bundle(); req(b)
    b$historical_profile %>% transmute(GLM_ID, State = state, City = city, Segment = DC_Segments,
                                       `Build year (raw)` = Year_Build_Raw, `Parsed build year` = Year_Build,
                                       `Observed 12M all-product revenue` = Total_MRR_12M, `Sqft` = SQ_foot, Capacity,
                                       `CAPEX input` = CAPEX, `Source TAM 2030` = Historical_TAM_2030,
                                       `Calculated TAM 2030` = Calculated_Historical_TAM_2030,
                                       `4MC CAPEX` = Calculated_Historical_4MC_CAPEX, `10-year TAM` = Calculated_Historical_TAM_10_Year,
                                       `10-year gross revenue` = Calculated_Historical_Gross_Revenue_10_Year,
                                       `10-year SOM` = Calculated_Historical_SOM_10_Year,
                                       NPV = Calculated_Historical_NPV, IRR = Calculated_Historical_IRR) %>%
      datatable(rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatCurrency(c("Observed 12M all-product revenue", "CAPEX input", "Source TAM 2030", "Calculated TAM 2030", "CAPEX", "10-year TAM", "10-year gross revenue", "10-year SOM", "NPV"), "$", digits = 0) %>%
      formatPercentage("IRR", digits = 1)
  })
  
  output$quality_table <- renderDT({
    b <- model_bundle(); req(b); p <- b$all_onnet_profile
    quality <- tibble(
      Measure = c("Original workbook rows", "Original workbook fields", "Confirmed on-net unique GLMs",
                  "Eligible comparable GLMs (existing source filter)", "Excluded segment 6 / invalid / unknown",
                  "Duplicate on-net GLM rows ignored after first", "Valid-segment GLMs below $1,000 excluded",
                  "Valid-segment GLMs with missing revenue excluded", "Unknown/non-numeric build year",
                  "Unknown/nonpositive physical size", "Billed / normalized-base ratios above 100%",
                  "Unavailable billed / normalized-base ratio", "Billed counts above facility tenant estimates",
                  "GLMs without recorded quote detail", "Browser-model training rows"),
      Count = c(b$raw_rows, ncol(b$raw_source), b$onnet_rows, nrow(b$historical_profile), b$excluded_segment_rows,
                b$duplicate_rows, sum(p$DC_Segments %in% valid_segments & is.finite(p$Total_MRR_12M) & p$Total_MRR_12M < minimum_revenue),
                sum(p$DC_Segments %in% valid_segments & !is.finite(p$Total_MRR_12M)),
                sum(!is.finite(p$Year_Build)), sum(!is.finite(p$SQ_foot) | p$SQ_foot <= 0), sum(p$Normalized_Base_Conflict),
                sum(!is.finite(p$Billed_Percent_Normalized)), sum(p$Count_Conflict),
                sum(lengths(p$Quote_Tokens) == 0), b$eligible_rows))
    datatable(quality, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })
  output$matched_quality_table <- renderDT({
    r <- scoring_result(); req(r); p <- r$comparable_facilities
    quality <- tibble(Measure = c("Matched GLMs", "Unavailable billed percentage", "Billed percentages above 100% (not capped)",
                                  "Unknown/nonpositive size", "Billed counts above facility tenant estimates", "Missing quote detail"),
                      Count = c(nrow(p), sum(!is.finite(p$Billed_Percent_Normalized)), sum(p$Normalized_Base_Conflict),
                                sum(!is.finite(p$SQ_foot) | p$SQ_foot <= 0), sum(p$Count_Conflict), sum(lengths(p$Quote_Tokens) == 0)))
    datatable(quality, rownames = FALSE, options = list(dom = "t", ordering = FALSE))
  })
}


shinyApp(ui, server)
