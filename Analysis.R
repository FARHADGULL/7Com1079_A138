# ----------------------------------------------------
# 1. Install and load required packages
# ----------------------------------------------------
required_packages <- c("ggplot2", "dplyr", "readr", "lubridate")

for(pkg in required_packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ----------------------------------------------------
# 2. Load the dataset
# ----------------------------------------------------
df <- read_csv(file.choose())  # Select Bitcoin CZ.csv

# Ensure correct column types
if(!inherits(df$Date, "Date")){
  df$Date <- as.Date(df$Date)
}

if(!is.numeric(df$Close)){
  df$Close <- as.numeric(df$Close)
}

# Quick check
print(head(df))
print(summary(df$Close))
print(names(df))

# ----------------------------------------------------
# 3. SPLIT DATA INTO TWO LARGE PERIODS
# ----------------------------------------------------
median_date <- median(df$Date, na.rm = TRUE)

df <- df %>%
  mutate(period = ifelse(Date <= median_date, "Period 1", "Period 2"))

# Extract period vectors (Includes First & second half of the entire Data set)
period1 <- df %>% filter(period == "Period 1") %>% pull(Close)
period2 <- df %>% filter(period == "Period 2") %>% pull(Close)

# ----------------------------------------------------
# 4. Descriptive statistics
# ----------------------------------------------------
desc <- data.frame(
  period = c("Period 1", "Period 2"),
  mean = c(mean(period1, na.rm = TRUE), mean(period2, na.rm = TRUE)),
  sd = c(sd(period1, na.rm = TRUE), sd(period2, na.rm = TRUE)),
  n = c(length(period1), length(period2))
)

print(desc)

# ----------------------------------------------------
# 5. T-test
# ----------------------------------------------------
t_res <- t.test(period1, period2, alternative = "two.sided", var.equal = FALSE)
print(t_res)

# ----------------------------------------------------
# 6. PLOTS
# ----------------------------------------------------

# Full time-series plot
p1 <- ggplot(df, aes(x = Date, y = Close, color = period)) +
  geom_line() +
  labs(title = "Bitcoin Closing Price Over Time",
       x = "Date",
       y = "Closing Price") +
  geom_vline(xintercept = median_date, linetype = "dashed") +
  theme_minimal()
p1  # Display in RStudio

ggsave("close_timeseries_full.png", p1, width = 10, height = 5)

# Boxplot
p2 <- ggplot(df, aes(x = period, y = Close, fill = period)) +
  geom_boxplot() +
  labs(title = "Close Price by Period", x = "", y = "Close") +
  theme_minimal()
p2  # Display in RStudio

ggsave("close_boxplot_full.png", p2, width = 6, height = 4)

# Histogram
p3 <- ggplot(df, aes(x = Close, fill = period)) +
  geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
  labs(title = "Histogram of Close Prices", x = "Close", y = "Count") +
  theme_minimal()
p3  # Display in RStudio

ggsave("close_histogram_full.png", p3, width = 6, height = 4)

