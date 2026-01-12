#Pearson Correlation Coefficient

#1.Sleep duration vs Quality of Sleep
p_cor1 <- ggplot(df, aes(x = sleep_duration, y = quality_of_sleep)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Correlation: Duration vs Quality")

ggsave("images/02_cor_duration.png", plot = p_cor1)

# รันสถิติและเก็บผล
cor_res1 <- cor.test(df$sleep_duration, df$quality_of_sleep)
print(cor_res1)

# 2.Physical Activity vs Quality
p_cor2 <- ggplot(df, aes(x = physical_activity_level, y = quality_of_sleep)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5, size = 1, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Correlation: Physical Activity vs Sleep Quality",
       x = "Physical Activity Level",
       y = "Quality of Sleep (Score)") +
  theme_minimal()

ggsave("images/cor_02_activity.png", plot = p_cor2, width = 7, height = 5)
cor_res2 <- cor.test(df$physical_activity_level, df$quality_of_sleep)
print(cor_res2)

# 3.Stress Level vs Quality
p_cor3 <- ggplot(df, aes(x = stress_level, y = quality_of_sleep)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5, size = 1, color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Correlation: Stress Level vs Sleep Quality",
       x = "Stress Level",
       y = "Quality of Sleep (Score)") +
  theme_minimal()

ggsave("images/cor_03_stress.png", plot = p_cor3, width = 7, height = 5)
cor_res3 <- cor.test(df$stress_level, df$quality_of_sleep)
print(cor_res3)

# 4.Daily Steps vs Quality
p_cor4 <- ggplot(df, aes(x = daily_steps, y = quality_of_sleep)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5, size = 1, color = "orange") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Correlation: Daily Steps vs Sleep Quality",
       x = "Daily Steps",
       y = "Quality of Sleep (Score)") +
  theme_minimal()

ggsave("images/cor_04_steps.png", plot = p_cor4, width = 7, height = 5)
cor_res4 <- cor.test(df$daily_steps, df$quality_of_sleep)

# บันทึกผลสถิติลงไฟล์ text ใช้แปะใน GitHub README
sink("output/correlation_summary.txt")
print(cor_res1); print(cor_res2); print(cor_res3); print(cor_res4)
sink()