# Install packages if not installed, then load packages
packages <- c('tidyverse','ggplot2','zoo','dplyr','smooth','timetk','forecast','tidyquant','sweep','Metrics')
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))


# Read in data
complete_data = read.csv("data/model_data/ModelDataComplete.csv")
complete_data = select(complete_data, -c("flag","states_covered"))
df <- complete_data

int_model_df <- df %>%
  mutate(date = zoo::as.yearmon(date)) %>%
  group_by(Region,date) %>%
  summarize(mean_interchange = mean(mean_interchange))

for(lag_val in 12:23) {
  lag_col_name <- paste("lag_", lag_val,sep="")
  int_model_df[[lag_col_name]] <- sapply(1:nrow(int_model_df), function(x) int_model_df$mean_interchange[x-lag_val])
}
 
linear_model <- lm(,data=df)

summary(linear_model)