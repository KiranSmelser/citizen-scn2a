# R/odds-ratio/functions/modeling_functions.R
# Functions to fit logistic regression models and extract key statistics

library(pROC)

# Fit a logistic regression model and return the model, its summary, and AUC
run_logistic_model <- function(formula, data) {
  model <- glm(formula, family = binomial(link = "logit"), data = data)
  summary_model <- summary(model)
  roc_obj <- roc(model$model[[1]], fitted(model))
  auc_val <- as.numeric(auc(roc_obj))
  list(model = model, summary = summary_model, auc = auc_val)
}

# Compile results over targets and predictors
compile_model_results <- function(data, predictors, targets, analysis_type, results_folder) {
  results_list <- list()
  for (target in targets) {
    for (pred in predictors) {
      formula <- as.formula(paste0("`", target, "` ~ ", pred))
      res <- run_logistic_model(formula, data)
      
      if (pred %in% c("Onset_group", "unique_types")) {
        # For categorical predictors, assume baseline is the first level
        lvls <- levels(data[[pred]])
        if(length(lvls) > 1) {
          level_vec <- c("(Intercept)", lvls[-1])
        } else {
          level_vec <- c("(Intercept)")
        }
        for (lvl in level_vec) {
          predictor_label <- if (lvl == "(Intercept)") "(Intercept)" else paste0(pred, lvl)
          beta_hat <- res$model$coefficients[predictor_label]
          se_beta_hat <- res$summary$coef[predictor_label, "Std. Error"]
          zstar <- qnorm(0.975)
          lb <- beta_hat - zstar * se_beta_hat
          ub <- beta_hat + zstar * se_beta_hat
          p_val <- res$summary$coef[predictor_label, "Pr(>|z|)"]
          
          result_entry <- data.frame(
            target      = target,
            predictor   = paste(pred, lvl, sep = "_"),
            coefficient = beta_hat,
            ci_lower    = exp(lb),
            ci_upper    = exp(ub),
            auc         = res$auc,
            p_value     = p_val,
            stringsAsFactors = FALSE
          )
          results_list[[length(results_list) + 1]] <- result_entry
          
          # Save individual model summary
          summary_file <- file.path(results_folder, 
                                    paste0("model_summary_", analysis_type, "_", target, "_", 
                                           pred, "_", gsub("[^A-Za-z0-9]", "", lvl), ".txt"))
          capture.output(res$summary, file = summary_file)
        }
      } else {
        # For continuous predictor use second coefficient
        beta_hat <- res$model$coefficients[2]
        se_beta_hat <- res$summary$coef[2, "Std. Error"]
        zstar <- qnorm(0.975)
        lb <- beta_hat - zstar * se_beta_hat
        ub <- beta_hat + zstar * se_beta_hat
        p_val <- res$summary$coef[2, "Pr(>|z|)"]
        
        result_entry <- data.frame(
          target      = target,
          predictor   = pred,
          coefficient = beta_hat,
          ci_lower    = exp(lb),
          ci_upper    = exp(ub),
          auc         = res$auc,
          p_value     = p_val,
          stringsAsFactors = FALSE
        )
        results_list[[length(results_list) + 1]] <- result_entry
        
        # Save individual model summary
        summary_file <- file.path(results_folder, 
                                  paste0("model_summary_", analysis_type, "_", target, "_", pred, ".txt"))
        capture.output(res$summary, file = summary_file)
      }
    }
  }
  do.call(rbind, results_list)
}