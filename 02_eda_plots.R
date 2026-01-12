#Q1: Sleep health Indicator by BMI Category Plot
bmi_summary <- df %>%
  group_by(bmi_category) %>%
  summarise(
    sleep_duration = mean(sleep_duration, na.rm = TRUE),
    quality_of_sleep = mean(quality_of_sleep, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(sleep_duration, quality_of_sleep), names_to = "Variable", values_to = "Score")

#สร้างกราฟ
p1 <- ggplot(bmi_summary, aes(x = bmi_category, y = Score, fill = Variable)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.55) +
  scale_fill_manual(values = c("#00B7C7", "#F0444C")) +
  labs(title = "Sleep Health Indicators by BMI Category") +
  theme_minimal()

ggsave("images/01_bmi_analysis.png", plot = p1)

#Q2Average Sleep Duration and Quality by Gender
# 1. เตรียมข้อมูล: หาค่าเฉลี่ยแยกตามเพศ
gender_summary <- df %>%
  group_by(gender) %>%
  summarise(
    mean_sleep_duration = mean(sleep_duration, na.rm = TRUE),
    mean_sleep_quality  = mean(quality_of_sleep, na.rm = TRUE),
    .groups = "drop"
  )

# 2. แปลงเป็น Long Format เพื่อทำ Grouped Bar Chart
gender_long <- gender_summary %>%
  pivot_longer(
    cols = c(mean_sleep_duration, mean_sleep_quality),
    names_to = "variable",
    values_to = "mean_score"
  ) %>%
  mutate(
    variable = factor(variable, 
                      levels = c("mean_sleep_duration", "mean_sleep_quality"),
                      labels = c("Sleep Duration", "Sleep Quality")),
    gender = factor(gender, levels = c("Female", "Male"))
  )

# 3. สร้างกราฟ Gender
p_gender <- ggplot(gender_long, aes(x = gender, y = mean_score, fill = variable)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.5) +
  geom_text(aes(label = round(mean_score, 2)),
            position = position_dodge(width = 0.6),
            vjust = -0.5, size = 3.8) +
  scale_fill_manual(values = c("#00B7C7", "#F0444C"), name = "") +
  labs(
    title = "Average Sleep Duration and Quality by Gender",
    x = "Gender",
    y = "Mean Score / Hours"
  ) +
  ylim(0, 9) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title     = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x   = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y   = element_text(size = 13, margin = margin(r = 10)),
    legend.position = "top"
  )

ggsave("images/02_gender_analysis.png", plot = p_gender, width = 8, height = 6)

#Q3 Quality of Sleep by Occupation 
# 1. เตรียมข้อมูล: หาค่าเฉลี่ยคุณภาพการนอนแยกตามอาชีพ
occ_summary <- df %>%
  group_by(occupation) %>%
  summarise(
    mean_quality = mean(quality_of_sleep, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # จัดเรียงจากมากไปน้อย
  arrange(desc(mean_quality)) 

# 2. สร้างกราฟ Occupation
p_occ <- ggplot(occ_summary, aes(x = reorder(occupation, -mean_quality), y = mean_quality)) +
  geom_col(fill = "#00B7C7", width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", mean_quality)),
            vjust = -0.5, size = 3.8) +
  labs(
    title = "Quality of Sleep by Occupation",
    x = "Occupation",
    y = "Quality Sleep Score"
  ) +
  ylim(0, 10) + 
  theme_minimal(base_family = "sans") +
  theme(
    plot.title     = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title.x   = element_text(size = 14, margin = margin(t = 10)),
    axis.title.y   = element_text(size = 14, margin = margin(r = 10)),
    axis.text.x    = element_text(size = 11, angle = 30, hjust = 1)
  )

ggsave("images/03_occupation_analysis.png", plot = p_occ, width = 10, height = 6)
