# Import Dataset
cfb23 <- read.csv("~/Desktop/archive-4/cfb23.csv", header=FALSE)
  View(cfb23)

# Import necessary libraries
library(ggplot2)
library(dplyr)

# Clean data
cfb23$`V20` <- as.numeric(cfb23$`V20`)
cfb23$`V7` <- as.numeric(cfb23$`V7`)
  
# Clean Data 
summary(cfb23)
cfb23_clean <- cfb23 %>%
  filter(!is.na(`V20`) & !is.na(`V7`))

# Data Visualization 1
ggplot(cfb23_clean, aes(x = `V20`, y = `V7`)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  labs(
    title = "3rd Down Rank vs. Off Rank",
    x = "3rd Down Rank",
    y = "Off Rank"
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +  
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.text.y = element_text(angle = 0, vjust = 0.5),
    panel.grid.major = element_line(color = "gray80", size = 0.5),
    panel.grid.minor = element_blank()
  )

# Clean Data
cfb23_clean <- cfb23 %>%
  mutate(
    `V11` = as.numeric(`V11`),
    `V22` = as.numeric(`V22`)
  ) %>%
  filter(!is.na(`V11`) & !is.na(`V22`))

# Data Visualization 2
ggplot(cfb23_clean, aes(x = `V11`, y = `V22`)) +
  geom_point(aes(color = `V11`, size = 4), alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  scale_color_gradient(low = "blue", high = "orange") +
  labs(
    title = "Comparison of Defensive Rank vs 3rd Down Defensive Rank (2023)",
    x = "Defensive Rank",
    y = "3rd Down Defensive Rank",
    color = "Def Rank"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "top",
    panel.grid.major = element_line(color = "gray80", size = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# Clean Data
cfb23_summary <- cfb23_clean %>%
  group_by(V3) %>%
  summarise(
    mean_def_rank = mean(V11, na.rm = TRUE),
    mean_3rd_down_def_rank = mean(V22, na.rm = TRUE)
  )

# Data Visualization 3
cfb23_filtered <- cfb23_clean %>%
  filter(V11 >= 1 & V11 <= 10) 
ggplot(cfb23_filtered, aes(x = reorder(V3, V11), y = V23)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Opponent 3rd Down Conversion for top 10 defences",
    x = "Team",
    y = "Opponent 3rd Down Conversion"
  ) +
  coord_flip() +  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10)
  )
