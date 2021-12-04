# Load the Necessary Libraries
library(tidyverse)
library(here)
library(lubridate)
library(scales)
library(ggcorrplot)
library(relaimpo)

# Load the dataset
charted_spotify_data <- read_csv(here("data", "charted_spotify_data.csv"))

# Create a Dataset of Distinct Songs
distinct_ids <- charted_spotify_data %>%
  distinct(spotify_id, .keep_all = TRUE)


# Grouping the dataset by month and identifying averages in different variables
monthly_averages <- charted_spotify_data %>% 
  mutate(date = mdy(date)) %>%
  mutate(month_yr = ym(format_ISO8601(date, precision = "ym"))) %>%
  group_by(month_yr) %>%
  summarize(mean_popularity = mean(popularity), mean_danceability = mean(danceability),
            mean_energy = mean(energy),
            mean_loudness = mean(loudness),
            mean_acousticness = mean(acousticness),
            mean_instrumentalness = mean(instrumentalness),
            mean_liveness = mean(liveness),
            mean_valence = mean(valence),
            mean_tempo = mean(tempo),
            mean_duration = mean(duration_ms))

# Pulls the audio features that are relevant to analysis
# gather() converts the table from wide to long format, making it easier to
# facet the different audio features
acoustic_trends <- monthly_averages %>%
  dplyr::select(-mean_popularity) %>% 
  gather(audio_feature, measurement, mean_danceability:mean_duration,
         factor_key = TRUE)

# Creating a list to change facet labels to user-friendly labels
audio_features <- list(
  "mean_danceability" = "Danceability",
  "mean_energy" = "Energy",
  "mean_loudness" = "Loudness",
  "mean_acousticness" = "Acousticness",
  "mean_instrumentalness" = "Instrumentalness",
  "mean_liveness" = "Liveness",
  "mean_valence" = "Valence",
  "mean_tempo" = "Tempo",
  "mean_duration" = "Duration"
)
audio_labeller <- function(variable, value) {
  return(audio_features[value])
}

# Creating a plot to measure trends across different acoustic features
acoustic_chart <- ggplot(acoustic_trends) +
  geom_line(aes(month_yr, measurement, group = 1)) +
  labs(title = "Average Monthly Trends For Hot 100 Songs Across Different Features") +
  facet_wrap(~audio_feature, nrow = 3, ncol = 3, scales = "free_y",
             labeller = audio_labeller) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = function(x) seq.Date(as.Date("1960/1/1"),
                                                  as.Date("2020/1/1"),
                                                  "10 years"),
               date_minor_breaks = "1 year") +
  xlab("") + ylab("Spotify Score") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(here("output", "Hot_100_Monthly_Trends.png"),
       acoustic_chart, width=16, height=8.5)

# Weights Hot 100 Songs based on Rank. Higher ranked songs are
# assigned a higher weight
monthly_averages_weighted <- charted_spotify_data %>% 
  mutate(date = mdy(date)) %>% 
  mutate(month_yr = ym(format_ISO8601(date, precision = "ym"))) %>%
  mutate(danceability = danceability/sqrt(rank),
         energy = energy/sqrt(rank),
         loudness = loudness/sqrt(rank),
         acousticness = acousticness/sqrt(rank),
         instrumentalness = instrumentalness/sqrt(rank),
         liveness = liveness/sqrt(rank),
         valence = valence/sqrt(rank),
         tempo = tempo/sqrt(rank),
         duration_ms = duration_ms/sqrt(rank)) %>% 
  group_by(month_yr) %>% 
  summarize(mean_popularity = mean(popularity),
            mean_danceability = mean(danceability),
            mean_energy = mean(energy),
            mean_loudness = mean(loudness),
            mean_acousticness = mean(acousticness),
            mean_instrumentalness = mean(instrumentalness),
            mean_liveness = mean(liveness),
            mean_valence = mean(valence),
            mean_tempo = mean(tempo),
            mean_duration = mean(duration_ms))

# Pulls the Relevant Audio Features
acoustic_trends_weighted <- monthly_averages_weighted %>%
  dplyr::select(-mean_popularity) %>%
  gather(audio_feature, measurement, mean_danceability:mean_duration,
         factor_key = TRUE)

# Creates a Chart Showing Weighted Values
acoustic_chart_weighted <- ggplot(acoustic_trends_weighted) +
  geom_line(aes(month_yr, measurement, group = 1)) +
  labs(title = "Average Monthly Trends For Hot 100 Songs Across Different Features (Weighted)") +
  facet_wrap(~audio_feature, nrow = 3, ncol = 3, scales = "free_y",
             labeller = audio_labeller) +
  scale_x_date(labels = date_format("%Y"),
               date_breaks = function(x) seq.Date(as.Date("1960/1/1"),
                                                  as.Date("2020/1/1"),
                                                  "10 years"),
               date_minor_breaks = "1 year") +
  xlab("") + ylab("Spotify Score") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(here("output", "Hot_100_Monthly_Trends_Weighted.png"),
       acoustic_chart_weighted, width=16, height=8.5)

# Calculating Correlation Coefficients for Each Numerical Variable, then putting
# them in a matrix
corr <- distinct_ids[, c(2, 10, 12:13, 15, 17:22)] %>%
  cor(use="complete.obs") %>%
  round(1)
pvalue_matrix <- distinct_ids[, c(2, 10, 12:13, 15, 17:22)] %>% 
  cor_pmat() %>%
  round(2)
corr_matrix <- ggcorrplot(corr, hc.order = TRUE, type = "lower",
                          p.mat = pvalue_matrix)
ggsave(here("output", "Correlation_Matrix_All_Distinct_Songs.png"),
       corr_matrix, width = 16, height = 8.5)

# Calculating Correlations for the Averages
corr_trends <- monthly_averages %>%
  dplyr::select(-month_yr) %>%
  cor(use="complete.obs") %>%
  round(1)
pvalue_matrix_trends <- monthly_averages %>%
  dplyr::select(-month_yr) %>%
  cor_pmat() %>%
  round(2)
corr_matrix_trends <- ggcorrplot(corr_trends, hc.order = TRUE, type = "lower",
                                 p.mat = pvalue_matrix_trends)
ggsave(here("output", "Correlation_Matrix_Monthly_Averages.png"),
       corr_matrix_trends, width = 16, height = 8.5)

# Calculating Correlations for Weighted Data
corr_trends_weighted <- monthly_averages_weighted %>%
  dplyr::select(-month_yr) %>%
  cor(use="complete.obs") %>%
  round(1)
pvalue_matrix_trends_weighted <- monthly_averages_weighted %>%
  dplyr::select(-month_yr) %>%
  cor_pmat() %>%
  round(2)
corr_matrix_trends_weighted <- ggcorrplot(corr_trends_weighted, hc.order = TRUE,
                                          type = "lower",
                                          p.mat = pvalue_matrix_trends_weighted)
ggsave(here("output", "Correlation_Matrix_Monthly_Averages_Rank_Weighted.png"),
       corr_matrix_trends_weighted, width = 16, height = 8.5)

# Multiple Linear Regression Model
fit <- lm(popularity ~ danceability + energy + loudness + acousticness + instrumentalness + liveness + valence + tempo + duration_ms, data=distinct_ids)
bootresults <- boot.relimp(fit)
ci <- booteval.relimp(bootresults, norank = T)
png(here("output", "Relative_Importance_Coefficients.png"))
plot(ci)
dev.off()