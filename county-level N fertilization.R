df <- read.csv("E:\\B_2024\\a_2024fall\\a_Research\\county-level N fertilization\\N rate VS NH3.csv")

library(dplyr)
overall_corr <- cor(df$NH3, df$N_rate, use = "complete.obs")
cat("Overall correlation coefficient:", overall_corr, "\n")
cor_test <- cor.test(df$NH3, df$N_rate, use = "complete.obs")

# 3. Calculate the correlation coefficient for each State
state_corr_test <- df %>%
  group_by(state) %>%
  summarise(
    correlation = tryCatch(
      cor(NH3, N_rate, use = "complete.obs"), 
      error = function(e) NA # If error occurs, return NA
    ),
    p_value = tryCatch(
      cor.test(NH3, N_rate)$p.value, 
      error = function(e) NA # If error occurs, return NA
    )
  )

# Print the correlation coefficients and p-values for each state
print(state_corr_test, n=38)


# regression
df$NH3_standardized <- scale(df$NH3)
df$N_rate_standardized <- scale(df$N_rate)
model <- lm(NH3_standardized ~ N_rate_standardized, data = df)



# Function to normalize data (Min-Max Scaling)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Normalize NH3 and N_rate
df$NH3_normalized <- normalize(df$NH3)
df$N_rate_normalized <- normalize(df$N_rate)
model <- lm(df$NH3_normalized ~ df$N_rate_normalized, data = df)
summary(model)


state_models <- df %>%
  group_by(state) %>%  # Group the data by 'State'
  do({
    tryCatch({
      # Perform regression for each group
      model <- lm(NH3_normalized ~ N_rate_normalized, data = .)
      
      # Return coefficients and p-values
      coef_summary <- summary(model)$coefficients
      data.frame(
        State = unique(.$state),
        Coefficient = coef_summary[2, 1],  # Coefficient for N_rate_normalized
        p_value = coef_summary[2, 4]       # p-value for N_rate_normalized
      )
    }, error = function(e) {
      # In case of an error, return NA for the coefficients and p-values
      data.frame(State = unique(.$state), Coefficient = NA, p_value = NA)
    })
  })

# View the results
print(state_models, n=38)

write.csv(state_models, "regression_results_by_state.csv", row.names = FALSE)
