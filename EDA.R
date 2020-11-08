library(patchwork)

# Price Variation within Country

# Density - Free Scales
p1 <- data_monthly %>%
  ggplot(aes(x = price_mean, fill = Country)) +
  geom_density() +
  facet_wrap(~ Country, scales = "free") +
  theme_minimal() +
  xlab("Price") +
  ylab("Density") +
  ggtitle("Price Density Distribution") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Density - Fixed Scales
p2 <- data_monthly %>%
  ggplot(aes(x = price_mean, fill = Country)) +
  geom_density() +
  facet_wrap(~ Country, scales = "fixed") +
  theme_minimal() +
  xlab("Price") +
  ylab("Density") +
  ggtitle("Price Density Distribution with same scales") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Box plot
p3 <- data_monthly %>%
  ggplot(aes(x = Country, y = price_mean, fill = Country)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Price") +
  ylab("Density") +
  ggtitle("Box Plot") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Price Variation within Country and Product
p4 <- data_monthly %>% 
  ggplot(aes(x = product, y = price_mean)) +
  geom_point() +
  facet_wrap(~ Country, scales = "free") +
  xlab("Products") +
  ylab("Price") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Price - Product Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

p1 + p2 + p3 + p4
