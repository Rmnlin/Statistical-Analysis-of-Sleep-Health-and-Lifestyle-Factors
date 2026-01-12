# T-test: Gender
t_test_gender <- t.test(quality_of_sleep ~ gender, data = df)
print(t_test_gender)

# ANOVA: BMI
# 2.1 สรุปข้อมูลเบื้องต้น (Descriptive Statistics)
bmi_stats <- df %>%
  group_by(bmi_category) %>%
  summarise(
    count = n(),
    mean_quality = mean(quality_of_sleep, na.rm = TRUE),
    sd_quality = sd(quality_of_sleep, na.rm = TRUE),
    .groups = "drop"
  )

# 2.2 สร้างแผนภาพกล่อง (Boxplot) 
png("images/stats_01_bmi_boxplot.png", width = 800, height = 600)
boxplot(quality_of_sleep ~ bmi_category, data = df,
        main = "Sleep Quality by BMI Category",
        xlab = "BMI Category", 
        ylab = "Quality of Sleep",
        frame = FALSE,
        col = c("#00AFBB", "#E7B800", "#53836A", "#E7B800"))
dev.off()

anova_bmi <- aov(quality_of_sleep ~ bmi_category, data = df)
summary(anova_bmi)
TukeyHSD(anova_bmi)

# ANOVA: Occupation
# 3.1 สรุปข้อมูลเบื้องต้นตามอาชีพ
occ_stats <- df %>%
  group_by(occupation) %>%
  summarise(
    count = n(),
    mean_quality = mean(quality_of_sleep, na.rm = TRUE),
    sd_quality = sd(quality_of_sleep, na.rm = TRUE),
    .groups = "drop"
  )

#ANOVA: Occupation
anova_occ <- aov(quality_of_sleep ~ occupation, data = df)
summary(anova_occ)
TukeyHSD(anova_occ)

# บันทึกผลลัพธ์ลงไฟล์ Text เพื่อให้คนอื่นอ่านได้ง่ายใน GitHub
sink("output_stats_summary.txt")
print("--- T-Test Gender ---")
print(t_test_gender)
print("--- ANOVA BMI ---")
summary(anova_bmi)
print("--- ANOVA Occupation ---")
summary(anova_occ)
sink()