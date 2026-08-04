## ----Setup, echo = FALSE, eval = FALSE--------------------------------
# # NOTE: Run Chunk 2 to install required packages
# 
# installed_packages <- installed.packages()[, "Package"]
# 
# required_packages_cran <-
#   c("cobalt", # Standardized Differences
#     "dplyr", # Wrangling Data
#     "kableExtra", # Printing Tables
#     "lmtest", # Robust CIs - Only used for illustration
#     "pak", # Managing Packages
#     "sandwich", # Robust SEs - Only used for illustration
#     "table1") # Tabulations
# 
# required_packages_github_repos <-
#   c("covariateadjustment/examplercts",
#     "jbetz-jhu/drwls",
#     "nt-williams/simul",
#     "nt-williams/adjrct")
# 
# required_packages_github <-
#   gsub(
#     pattern = "^[A-Za-z0-9\\_\\.\\-]*/",
#     replacement = "",
#     x = c(required_packages_github_repos)
#   )
# 
# 
# missing_packages <-
#   setdiff(
#     x = c(required_packages_cran, required_packages_github),
#     y = installed_packages
#   )
# 
# if(length(missing_packages) > 0){
#   stop(
#     "Required packages not installed: ",
#     paste(missing_packages, collapse = ","), ". ",
#     "See `cran_packages` and `github_packages` in the configuration file."
#   )
# }
# 
# 
# knitr::opts_chunk$set(
#   collapse = TRUE,
#   echo = TRUE,
#   message = FALSE,
#   error = TRUE,
#   purl = FALSE,
#   results = "markup",
#   fig.path = "figures/",
#   fig.width = 8,
#   fig.height = 8,
#   fig.align = "center",
#   fig.asp = 0.618,
#   out.height = "80%",
#   dev = "png"
# )
# 
# options(
#   width = 80,
#   knitr.kable.NA = ""
# )


## ----Install-Packages, eval = FALSE, echo = FALSE---------------------
# # Only need to run if packages haven't been previously installled
# if(length(missing_packages) > 0){
#   install_from_cran <-
#     setdiff(
#       x = required_packages_cran,
#       y = installed_packages
#     )
# 
#   if(length(install_from_cran) > 0){
#     pak::pkg_install(pkg = install_from_cran)
#   }
# 
#   install_from_github <-
#     setdiff(
#       x = required_packages_github,
#       y = installed_packages
#     )
# 
#   install_gh_i <-
#     match(x = install_from_github, table = required_packages_github)
# 
#   install_from_github <-
#     required_packages_github_repos[install_gh_i]
# 
#   if(length(install_from_github) > 0){
#     pak::pkg_install(pkg = install_from_cran)
#   }
# }


## ----Install-packages, eval = FALSE-----------------------------------
# install.packages(
#   pkgs = c("dplyr", "drord", "cobalt", "kableExtra", "lmtest", "pak",
#            "sandwich", "speff2trial", "table1")
# )
# 
# # Installing packages from GitHub
# pak::pak(
#   pkg =
#     c("covariateadjustment/examplercts",
#       "jbetz-jhu/drwls",
#       "nt-williams/simul",
#       "nt-williams/adjrct")
# )


## ----Load-examplercts-package, message = FALSE------------------------
# Continuous, Binary Analyses
library(dplyr) # Wrangling data
library(drwls) # g-Computation & DR-WLS
library(cobalt) # Standardized Differences
library(examplercts) # Example Datasets
library(kableExtra) # Printing Tables in HTML/TeX
library(lmtest) # Computing CIs with Robust SEs
library(sandwich) # Computing Robust SEs for GLMs
library(table1) # Tabulations

# Ordinal, Time-to-Event Analyses
library(adjrct) # Time-to-Event: Survival & RMST
library(drord) # Adjusted Ordinal Analyses
library(speff2trial) # Adjusted Marginal Hazard Ratio
library(survival) # Logrank tests; Cox PH Model


## ----licorice-gargle-dictionary---------------------------------------
data.frame(
  column = names(licorice_gargle),
  name =
    sapply(X = licorice_gargle, function(x) attr(x = x, which = "label")),
  row.names = NULL
) %>%
  kableExtra::kable(
    x = .,
    caption = "Columns of the `licorice_gargle` dataset."
  )


## ----licorice-gargle-baseline-covariates------------------------------
table1::table1(
  x = ~ age_bl + gender + asa_physical_status_bl + bmi_bl + mallampati_bl |
    arm,
  data = licorice_gargle
)


## ----licorice-gargle-covariate-balance, results = "markup"------------
cobalt::bal.tab(
  x = 
    # Only tabulate baseline variables
    licorice_gargle %>% 
    dplyr::select(
      dplyr::all_of(
        x = c("age_bl", "gender", "asa_physical_status_bl", "bmi_bl",
              "mallampati_bl")
      )
    ),
  treat = licorice_gargle$arm,
  # Compute standardized differences for both binary and continuous variables
  binary = "std",
  continuous = "std",
  s.d.denom = "pooled"
)


## ----licorice-gargle-outcomes-----------------------------------------
table1::table1(
  x = ~ pacu_30_min_throat_pain + pacu_30_min_cough +
    pacu_90_min_throat_pain + pacu_90_min_cough +
    postop_4h_throat_pain + postop_4h_cough +
    postop_1d_am_throat_pain + postop_1d_am_cough |
    arm,
  data = licorice_gargle
)


## ----licorice-gargle-30m-unadjusted-t-test, results = "markup"--------
licorice_30_min_pain_t_test <-
  t.test(
    pacu_30_min_throat_pain ~ arm,
    data = licorice_gargle
  )

licorice_30_min_pain_t_test

# Note: t-test is presenting [Control] (Reference) - [Treatment]
# This is simple to correct to give [Treatment] - [Control]
diff(licorice_30_min_pain_t_test$estimate)
-rev(licorice_30_min_pain_t_test$conf.int)

# Unadjusted estimate Standard Error
licorice_30_min_pain_t_test$stderr


## ----licorice-gargle-30m-ancova, results = "markup"-------------------
licorice_30_min_pain_glm_adjusted <-
  glm(
    pacu_30_min_throat_pain ~ arm + 
      age_bl + gender + asa_physical_status_bl + bmi_bl + mallampati_bl,
    data = licorice_gargle,
    family = gaussian # Default for GLM
  )

# Model-Based Inference: Assumes correctly-specified model
summary(licorice_30_min_pain_glm_adjusted)
confint(licorice_30_min_pain_glm_adjusted)["arm1. 0.5g Licorice",]

licorice_30_min_pain_ancova_robust_vcov <-
  sandwich::vcovHC(licorice_30_min_pain_glm_adjusted, type = "HC0")

# Tests based on Robust SEs:
lmtest::coeftest(
  x = licorice_30_min_pain_glm_adjusted,
  vcov. = licorice_30_min_pain_ancova_robust_vcov
)

# CIs based on Robust SEs
licorice_30_min_pain_ancova_robust_ci <-
  lmtest::coefci(
    x = licorice_30_min_pain_glm_adjusted,
    vcov. = licorice_30_min_pain_ancova_robust_vcov
  )
licorice_30_min_pain_ancova_robust_ci


## ----licorice-gargle-30m-g-computation-manual, results = "markup"-----
# Step 2: Use working model to predict outcomes under each treatment
pred_30_min_pain_licorice <-
  predict(
    object = licorice_30_min_pain_glm_adjusted,
    newdata =
      # Change all treatment assignment to Licorice Gargle arm
      within(
        data = licorice_gargle,
        expr = {arm = "1. 0.5g Licorice"}
      ),
    type = "response"
  )

pred_30_min_pain_sucrose <-
  predict(
    object = licorice_30_min_pain_glm_adjusted,
    newdata =
      # Change all treatment assignment to Sucrose Gargle arm
      within(
        data = licorice_gargle,
        expr = {arm = "0. 5g Sugar"}
      ),
    type = "response"
  )

# Step 3: Average predictions
mean_pred_30_min_pain_licorice <- mean(pred_30_min_pain_licorice)
mean_pred_30_min_pain_sucrose <- mean(pred_30_min_pain_sucrose)

# Step 4: Contrast Predictions
difference_licorice_sucrose <- 
  mean_pred_30_min_pain_licorice - mean_pred_30_min_pain_sucrose
difference_licorice_sucrose

ratio_licorice_sucrose <- 
  mean_pred_30_min_pain_licorice/mean_pred_30_min_pain_sucrose
ratio_licorice_sucrose


# ANCOVA coincides with G-Computation
difference_licorice_sucrose
coef(licorice_30_min_pain_glm_adjusted)["arm1. 0.5g Licorice"]


## ----licorice-gargle-30m-g-computation-drwls, results = "markup"------
licorice_30_min_pain_g_computation <-
  drwls::standardization(
    outcome_formula = 
      pacu_30_min_throat_pain ~ tx +
      age_bl + gender + asa_physical_status_bl + bmi_bl + mallampati_bl,
    outcome_family = gaussian,
    treatment_column = "tx",
    estimand = "difference", # Compute Difference in Means
    data = licorice_gargle,
    se_method = "asymptotic"
  )

licorice_30_min_pain_g_computation


## ----licorice-gargle-30m-precision------------------------------------
# Unadjusted SE
licorice_30_min_pain_t_test$stderr
# ANOVA: Robust SE
ate_i <- 
  which(colnames(licorice_30_min_pain_ancova_robust_vcov) == "arm1. 0.5g Licorice")
sqrt(licorice_30_min_pain_ancova_robust_vcov[ate_i, ate_i])
# G-Computation: Robust SE
licorice_30_min_pain_g_computation$result$se[1]

# Improvement in Efficiency
1 - (licorice_30_min_pain_g_computation$result$se[1]/licorice_30_min_pain_t_test$stderr)^2


## ----licorice-gargle-30m-g-computation-drwls-df-tsiatis, results = "markup"----
licorice_30_min_pain_g_computation_tsiatis <-
  drwls::standardization(
    outcome_formula = 
      pacu_30_min_throat_pain ~ tx +
      age_bl + gender + asa_physical_status_bl + bmi_bl + mallampati_bl,
    outcome_family = gaussian,
    treatment_column = "tx",
    estimand = "difference", # Compute Difference in Means
    data = licorice_gargle,
    se_method = "asymptotic",
    variance_adjustment = variance_adjustment_tsiatis
  )

licorice_30_min_pain_g_computation


## ----licorice-gargle-30m-drwls, results = "markup"--------------------
licorice_30_min_pain_drwls <-
  drwls::drwls(
    outcome_formula = 
      pacu_30_min_throat_pain ~ tx +
      age_bl + gender + asa_physical_status_bl + bmi_bl + mallampati_bl,
    outcome_family = gaussian,
    treatment_column = "tx",
    estimand = "difference", # Compute Difference in Means
    missing_formula = ~ tx + age_bl + bmi_bl,
    missing_family = binomial,
    data = licorice_gargle,
    se_method = "asymptotic"
  )

licorice_30_min_pain_drwls


## ----licorice-gargle-outcomes-binary----------------------------------
table1::table1(
  x = ~ 
    pacu_30_min_any_throat_pain + 
    pacu_90_min_any_throat_pain +
    postop_4h_any_throat_pain + 
    postop_1d_am_any_throat_pain |
    arm,
  data = licorice_gargle
)


## ----licorice-gargle-30m-any-logistic, results = "markup"-------------
licorice_30_min_any_pain_glm_adjusted <-
  glm(
    pacu_30_min_any_throat_pain ~ arm + 
      age_bl + gender + asa_physical_status_bl + bmi_bl + mallampati_bl,
    data = licorice_gargle,
    family = binomial
  )

# Summarize logistic regression coefficients
summary(licorice_30_min_any_pain_glm_adjusted)

# Obtain conditional odds ratios
exp(coef(licorice_30_min_any_pain_glm_adjusted))


## ----licorice-gargle-30m-drwls-risk-difference, results = "markup"----
licorice_30_min_any_pain_drwls_rd <-
  drwls::drwls(
    outcome_formula = 
      pacu_30_min_any_throat_pain ~ tx +
      age_bl + gender + asa_physical_status_bl + bmi_bl + mallampati_bl,
    outcome_family = binomial,
    treatment_column = "tx",
    estimand = "difference", # Compute Risk Difference
    missing_formula = ~ tx + age_bl + bmi_bl,
    missing_family = binomial,
    data = licorice_gargle,
    se_method = "asymptotic"
  )

licorice_30_min_any_pain_drwls_rd


## ----licorice-gargle-30m-drwls-odds-ratio, results = "markup"---------
licorice_30_min_any_pain_drwls_or <-
  drwls::drwls(
    outcome_formula = 
      pacu_30_min_any_throat_pain ~ tx +
      age_bl + gender + asa_physical_status_bl + bmi_bl + mallampati_bl,
    outcome_family = binomial,
    treatment_column = "tx",
    estimand = "oddsratio", # Compute Odds Ratio
    missing_formula = ~ tx + age_bl + bmi_bl,
    missing_family = binomial,
    data = licorice_gargle,
    se_method = "bootstrap"
  )

licorice_30_min_any_pain_drwls_or


## ----strep-tb-dictionary----------------------------------------------
data.frame(
  column = names(strep_tb),
  name =
    sapply(X = strep_tb, function(x) attr(x = x, which = "label")),
  row.names = NULL
) %>%
  kableExtra::kable(
    x = .,
    caption = "Columns of the `strep_tb` dataset."
  )


## ----strep-tb-baseline-covariates-------------------------------------
table1::table1(
  x = ~ gender + condition_bl + sed_rate_esr_bl + cxr_lung_cavitation_bl |
    arm,
  data = strep_tb
)


## ----strep-tb-outcomes------------------------------------------------
table1::table1(
  x = 
    ~ strep_resistance_f_6m +
    radiologic_outcome_f_6m +
    radiologic_improvement_f_6m | arm,
  data = strep_tb
)


## ----strep-tb-pool, results = "markup", results = "markup"------------
strep_tb_pooled <-
  strep_tb %>% 
  dplyr::mutate(
    radiologic_outcome_pool_6m =
      dplyr::case_when(
        radiologic_outcome_6m %in% 1:2 ~ radiologic_outcome_6m,
        radiologic_outcome_6m %in% 3:4 ~ 3,
        radiologic_outcome_6m %in% 5:6 ~ radiologic_outcome_6m - 1
      ),
    condition_bl_pool =
      dplyr::case_when(
        condition_bl %in% c("1. Good", "2. Fair") ~ "0. Good/Fair",
        condition_bl %in% c("3. Poor") ~ "1. Poor",
      ) %>% 
      factor(),
    sed_rate_esr_lte_50_bl_imp_0 =
      dplyr::case_when(
        sed_rate_esr_bl %in% c(NA, "2. 11-20", "3. 21-50") ~ "0. <= 50",
        sed_rate_esr_bl %in% c("4. 51+") ~ "1. > 50"
      ) %>% 
      factor(),
    sed_rate_esr_lte_50_bl_imp_1 =
      dplyr::case_when(
        sed_rate_esr_bl %in% c("2. 11-20", "3. 21-50") ~ "0. <= 50",
        sed_rate_esr_bl %in% c(NA, "4. 51+") ~ "1. > 50"
      ) %>%
      factor()
  )

# Check Pooled Variable
strep_tb_pooled %>% 
  dplyr::count(radiologic_outcome_6m, radiologic_outcome_pool_6m)

# Check Pooled + Imputed Covariate
strep_tb_pooled %>% 
  dplyr::count(
    sed_rate_esr_bl, sed_rate_esr_lte_50_bl_imp_0, sed_rate_esr_lte_50_bl_imp_1
  )


## ----strep-tb-covariate-balance, results = "markup"-------------------
cobalt::bal.tab(
  x = 
    # Only tabulate baseline variables
    strep_tb_pooled %>% 
    dplyr::select(
      dplyr::all_of(
        x = c("gender", "condition_bl_pool", 
              "sed_rate_esr_lte_50_bl_imp_0", "sed_rate_esr_lte_50_bl_imp_1")
      )
    ),
  treat = strep_tb_pooled$arm,
  # Compute standardized differences for both binary and continuous variables
  binary = "std",
  continuous = "std",
  s.d.denom = "pooled"
)


## ----summary.drord----------------------------------------------------
summary.drord <-
  function(object, ...){
    results_list <- list()
    for(i in c("mann_whitney", "log_odds", "weighted_mean")){
      if(i == "weighted_mean") {
        object[["weighted_mean"]]$est <- object[["weighted_mean"]]$est$est
      }
      if(i == "mann_whitney") {
        object[["mann_whitney"]]$ci <-
          lapply(
            X = object[["mann_whitney"]]$ci,
            FUN = function(x) if(!is.null(x)) matrix(x, ncol = 2)
          )
      }
      if(i %in% names(object)) results_list[[i]] <- c(results_list, object[[i]])
    }
    ci_wald <- "wald" %in% object$ci
    ci_bca <- "bca" %in% object$ci
    alpha <- object$alpha
    
    for(i in 1:length(results_list)){
      null_i <-
        switch(
          EXPR = names(results_list)[i],
          "mann_whitney" = c(0.5),
          "log_odds" = c(NA, NA, 0),
          "weighted_mean" = c(NA, NA, 0)
        )
      
      estimand_i <-
        switch(
          EXPR = names(results_list)[i],
          "mann_whitney" = "Mann-Whitney",
          "log_odds" = c("Log Odds: A = 1", "Log Odds: A = 1", "Log Odds Ratio: 1/0"),
          "weighted_mean" = c("Mean: A = 1", "Mean: A = 0", "Difference: 1-0")
        )
      
      results_list[[i]] <-
        with(
          data = results_list[[i]],
          expr =
            data.frame(
              estimand = estimand_i,
              estimate = est,
              null_value = null_i,
              se = 
                if(ci_wald){(ci$wald[, 2] - est)/qnorm(p = 1 - alpha/2)} else {NA},
              wald_lcl = if(ci_wald){ci$wald[, 1]} else {NA},
              wald_ucl = if(ci_wald){ci$wald[, 2]} else {NA},
              bca_lcl = if(ci_bca){ci$bca[, 1]} else {NA},
              bca_ucl = if(ci_bca){ci$bca[, 2]} else {NA}
            )
        )
    }
    
    results_table <- do.call(what = rbind, args = results_list)
    rownames(results_table) <- NULL
    results_table$wald_z <- 
      with(
        data = results_table,
        expr = (estimate - null_value)/se
      )
    
    results_table$`p-value` <-
      with(
        data = results_table,
        expr = 2*pnorm(q = -abs(wald_z))
      )
    
    if(!ci_wald){
      results_table[, 
                    c("se", "wald_lcl", "wald_ucl", "wald_z", "p-value")] <- NULL
    }
    
    if(!ci_bca){
      results_table[, c("bca_lcl", "bca_ucl")] <- NULL
    }
    
    return(
      results_table
    )
  }


## ----strep-tb-Ordinal-Unadjusted, results = "markup"------------------
strep_rad_6m_unadjusted <-
  with(
    data = strep_tb_pooled,
    expr = {
      drord::drord(
        out = as.numeric(radiologic_outcome_pool_6m),
        treat = tx,
        covar = data.frame(gender, condition_bl, sed_rate_esr_bl,
                           cxr_lung_cavitation_bl),
        out_form = "1", # Unadjusted - Intercept Only
        treat_form = "1", # Unadjusted - Intercept Only
      )
    }
  )

strep_rad_6m_unadjusted_table <-
  summary.drord(strep_rad_6m_unadjusted)

kableExtra::kbl(
  x = strep_rad_6m_unadjusted_table,
  digits = c(rep(x = 2, 7), 4),
  caption = "Unadjusted analyses of the `strep_tb` dataset."
)


## ----strep-tb-Ordinal-Adjusted, results = "markup"--------------------
strep_rad_6m_adjusted <-
  with(
    data = strep_tb_pooled,
    expr = {
      drord::drord(
        out = as.numeric(radiologic_outcome_pool_6m),
        treat = tx,
        covar = 
          data.frame(gender, condition_bl_pool, 
                     sed_rate_esr_lte_50_bl_imp_1, cxr_lung_cavitation_bl),
        out_form = "gender + condition_bl_pool + sed_rate_esr_lte_50_bl_imp_1",
        treat_form = "gender + condition_bl_pool + sed_rate_esr_lte_50_bl_imp_1"
      )
    }
  )

strep_rad_6m_adjusted_table <-
  summary.drord(strep_rad_6m_adjusted)

kableExtra::kbl(
  x = strep_rad_6m_adjusted_table,
  digits = c(rep(x = 2, 7), 4),
  caption = "Adjusted analyses of the `strep_tb` dataset."
)


## ----strep-tb-ordinal-gain, results = "markup"------------------------
est_labels <- c("Mann-Whitney", "Log Odds Ratio: 1/0", "Difference: 1-0")
est_rows <- which(strep_rad_6m_adjusted_table$estimand %in% est_labels)

strep_tb_gain <- 
  setNames(
    object = 1 - (strep_rad_6m_adjusted_table$se[est_rows]/
                    strep_rad_6m_unadjusted_table$se[est_rows])^2,
    nm = est_labels
  )

strep_tb_gain


## ----colon-cancer-dictionary------------------------------------------
data.frame(
  column = names(colon_cancer_active),
  name =
    sapply(X = colon_cancer_active, function(x) attr(x = x, which = "label")),
  row.names = NULL
) %>%
  kableExtra::kable(
    x = .,
    caption = "Columns of the `colon_cancer_active` dataset."
  )


## ----colon-cancer-baseline-covariates---------------------------------
table1::table1(
  x = ~ age_bl + sex + obstruction_bl + perforation_bl + organ_adherence_bl +
    positive_nodes_bl + differentiation_bl + local_spread_bl + 
    time_surgery_registration_bl |
    arm,
  data = colon_cancer_active
)


## ----colon-cancer-pool------------------------------------------------
colon_cancer_active_pool <-
  colon_cancer_active %>% 
  dplyr::mutate(
    local_spread_bl_pool =
      dplyr::case_when(
        local_spread_bl %in% c("1. Submucosa", "2. Muscle") ~ 
          "0. Submucosa or Muscle",
        local_spread_bl %in% c("3. Serosa", "4. Contiguous structures") ~ 
          "1. Serosa or Contiguous Structures",
      ) %>% 
      factor
  )

colon_cancer_active_pool %>% 
  count(local_spread_bl, local_spread_bl_pool)


## ----colon-cancer-covariate-balance, results = "markup"---------------
cobalt::bal.tab(
  x = 
    # Only tabulate baseline variables
    colon_cancer_active_pool %>% 
    dplyr::select(
      dplyr::all_of(
        x = c("age_bl", "sex", "obstruction_bl", "perforation_bl",
              "organ_adherence_bl", "positive_nodes_bl", "differentiation_bl",
              "local_spread_bl_pool", "time_surgery_registration_bl")
      )
    ),
  treat = colon_cancer_active_pool$arm,
  # Compute standardized differences for both binary and continuous variables
  binary = "std",
  continuous = "std",
  s.d.denom = "pooled"
)


## ----colon-cancer-logrank, results = "markup"-------------------------
survival::survdiff(
  formula = 
    survival::Surv(time = time_to_death, event = event_death) ~ tx,
  data = colon_cancer_active_pool
)


## ----colon-cancer-unadjusted-cox, results = "markup"------------------
colon_cancer_cox_unadjusted <-
  survival::coxph(
    formula = survival::Surv(time = time_to_death, event = event_death) ~ tx,
    data = colon_cancer_active_pool,
    robust = TRUE # Use Sandwich Standard Errors
  ) 

summary(colon_cancer_cox_unadjusted)


## ----colon-cancer-unadjusted-cox-ph-test, results = "markup"----------
survival::cox.zph(colon_cancer_cox_unadjusted)


## ----colon-cancer-adjusted-cox, results = "markup"--------------------
colon_cancer_cox_adjusted <-
  survival::coxph(
    formula = 
      survival::Surv(time = time_to_death, event = event_death) ~ tx +
      age_bl + sex + obstruction_bl + organ_adherence_bl +
      positive_nodes_bl + differentiation_bl + local_spread_bl_pool +
      time_surgery_registration_bl,
    data = colon_cancer_active_pool,
    robust = TRUE # Use Sandwich Standard Errors
  ) 

summary(colon_cancer_cox_adjusted)


## ----colon-cancer-adjusted-cox-ph-test, results = "markup"------------
survival::cox.zph(colon_cancer_cox_adjusted)


## ----colon-cancer-speffsurv, results = "markup"-----------------------
colon_cancer_speffsurv <-
  speff2trial::speffSurv(
    formula = 
      survival::Surv(time = time_to_death, event = event_death) ~
      age_bl + sex + obstruction_bl + organ_adherence_bl +
      positive_nodes_bl + differentiation_bl + local_spread_bl_pool +
      time_surgery_registration_bl,
    data = colon_cancer_active_pool,
    trt.id = "tx",
    fixed = TRUE,
    
  )

summary(colon_cancer_speffsurv)


## ----colon-cancer-speffsurv-gain, results = "markup"------------------
colon_cancer_speffsurv_gain <-
  with(
    data = colon_cancer_speffsurv,
    expr = as.numeric(1 - (varbeta["Speff"]/varbeta["Prop Haz"]))
  )

colon_cancer_speffsurv_gain


## ----colon-cancer-metadata-unadjusted, results = "markup"-------------
colon_cancer_meta_unadj <-
  adjrct::survrct(
    outcome.formula = 
      survival::Surv(time = time_to_death, event = event_death) ~ tx,
    trt.formula = tx ~ 1,
    data = colon_cancer_active_pool,
    coarsen = 30 # Time scale: Months vs. Days
  )

# Restricted Mean Survival Time
colon_cancer_rmst_unadj <-
  adjrct::rmst(
    metadata = colon_cancer_meta_unadj,
    # Survival at 1, 3, and 5-years post-randomization
    horizon = round(c(1, 3, 5)*365.25/30)
  )

colon_cancer_rmst_unadj

# Survival Probability
colon_cancer_sp_unadj <-
  adjrct::survprob(
    metadata = colon_cancer_meta_unadj,
    # Survival at 1, 3, and 5-years post-randomization
    horizon = round(c(1, 3, 5)*365.25/30)
  )

colon_cancer_sp_unadj


## ----summary.rmst-summary.survprob------------------------------------
summary.rmst <-
  summary.survprob <-
  function(object, ...){
    if(!inherits(x = object, what = c("rmst", "survprob"))){
      stop("`object` must inherit class \"rmst\" or \"survprob\"")
    }
    estimand <-
      switch(
        EXPR = class(object),
        "rmst" = "RMST:",
        "survprob" = "Survival:"
      )
    time_horizon <- object$horizon
    df_list <- list()
    for(i in 1:length(time_horizon)){
      df_list[[i]] <-
        with(
          data = object$estimates[[i]],
          data.frame(
            Estimand = paste(estimand, c("Arm 1", "Arm 0", "Difference")),
            Estimate = c(arm1, arm0, theta),
            Horizon = time_horizon[i],
            SE = c(arm1.std.error, arm0.std.error, std.error),
            LCL = c(arm1.conf.low, arm0.conf.low, theta.conf.low),
            UCL = c(arm1.conf.high, arm0.conf.high, theta.conf.high)
          )
        )
    }
    return(do.call(what = rbind, args = df_list))
  }


## ----colon-cancer-metadata-adjusted-----------------------------------
colon_cancer_meta_adj <-
  adjrct::survrct(
    outcome.formula = 
      survival::Surv(time = time_to_death, event = event_death) ~
      age_bl + sex + obstruction_bl + organ_adherence_bl +
      positive_nodes_bl + differentiation_bl + local_spread_bl +
      time_surgery_registration_bl,
    trt.formula = tx ~ 
      age_bl + sex + obstruction_bl + organ_adherence_bl +
      positive_nodes_bl + differentiation_bl + local_spread_bl +
      time_surgery_registration_bl,
    data = colon_cancer_active_pool,
    coarsen = 30
  )


## ----colon-cancer-rmst-sp-adjusted, results = "markup"----------------
colon_cancer_rmst_adj <-
  adjrct::rmst(
    metadata = colon_cancer_meta_adj,
    horizon = round(c(1, 3, 5)*365.25/30)
  )

summary(colon_cancer_rmst_adj)

colon_cancer_sp_adj <-
  adjrct::survprob(
    metadata = colon_cancer_meta_adj,
    horizon = round(c(1, 3, 5)*365.25/30)
  )

summary(colon_cancer_sp_adj)


## ----colon-cancer-adjrct-gain, results = "markup"---------------------
colon_cancer_rmst_gain <-
  1 - (colon_cancer_rmst_adj$estimates[[3]]$std.error/
         colon_cancer_rmst_unadj$estimates[[3]]$std.error)^2

colon_cancer_rmst_gain

colon_cancer_sp_gain <-
  1 - (colon_cancer_sp_adj$estimates[[3]]$std.error/
         colon_cancer_sp_unadj$estimates[[3]]$std.error)^2

colon_cancer_sp_gain

