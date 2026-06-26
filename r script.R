# ==========================================================
# CALORIES BURNED DATASET
# ==========================================================

# ==========================================================
# 1. IMPORT DATA
# ==========================================================

calories_burned_data <- read.csv("calories_burned_data.csv")

head(calories_burned_data)
tail(calories_burned_data)
str(calories_burned_data)
summary(calories_burned_data)

# ==========================================================
# 2. CREATE CATEGORICAL VARIABLES
# ==========================================================

calories_burned_data$Calorie_Level <- ifelse(
  calories_burned_data$Calories.Burned >= 300,
  "High",
  "Low"
)

calories_burned_data$Age_Group <- cut(
  calories_burned_data$Age,
  breaks = c(0, 30, 40, 50, 100),
  labels = c("Below30", "31-40", "41-50", "Above50"),
  include.lowest = TRUE
)

calories_burned_data$BMI_Category <- cut(
  calories_burned_data$BMI,
  breaks = c(0, 18.5, 25, 30, 100),
  labels = c("Underweight", "Normal", "Overweight", "Obese"),
  include.lowest = TRUE
)

calories_burned_data$HR_Group <- cut(
  calories_burned_data$Average.Heart.Rate,
  breaks = c(0, 100, 120, 140, 200),
  labels = c("Low", "Moderate", "High", "Very High"),
  include.lowest = TRUE
)

# ==========================================================
# 3. NORMALITY TESTS
# ==========================================================

shapiro.test(calories_burned_data$Calories.Burned)
shapiro.test(calories_burned_data$Age)
shapiro.test(calories_burned_data$BMI)
shapiro.test(calories_burned_data$Running.Time.min.)
shapiro.test(calories_burned_data$Distance.km.)
shapiro.test(calories_burned_data$Average.Heart.Rate)

# ==========================================================
# 4. CORRELATION
# ==========================================================

cor_matrix <- cor(
  calories_burned_data[, c(
    "Age","Height.cm.","Weight.kg.","BMI",
    "Running.Time.min.","Running.Speed.km.h.",
    "Distance.km.","Average.Heart.Rate","Calories.Burned"
  )]
)

cor_matrix

cor.test(calories_burned_data$Age, calories_burned_data$Calories.Burned)
cor.test(calories_burned_data$BMI, calories_burned_data$Calories.Burned)
cor.test(calories_burned_data$Running.Time.min., calories_burned_data$Calories.Burned)
cor.test(calories_burned_data$Running.Speed.km.h., calories_burned_data$Calories.Burned)
cor.test(calories_burned_data$Distance.km., calories_burned_data$Calories.Burned)
cor.test(calories_burned_data$Average.Heart.Rate, calories_burned_data$Calories.Burned)

# ==========================================================
# 5. T-TESTS
# ==========================================================

t.test(Calories.Burned ~ Gender, data = calories_burned_data)
t.test(Running.Time.min. ~ Gender, data = calories_burned_data)
t.test(Distance.km. ~ Gender, data = calories_burned_data)
t.test(Average.Heart.Rate ~ Gender, data = calories_burned_data)

# ==========================================================
# 6. MANN-WHITNEY U TESTS
# ==========================================================

wilcox.test(Calories.Burned ~ Gender, data = calories_burned_data)
wilcox.test(Running.Time.min. ~ Gender, data = calories_burned_data)
wilcox.test(Distance.km. ~ Gender, data = calories_burned_data)
wilcox.test(Average.Heart.Rate ~ Gender, data = calories_burned_data)

# ==========================================================
# 7. CHI-SQUARE TESTS
# ==========================================================

table_gender_calorie <- table(calories_burned_data$Gender, calories_burned_data$Calorie_Level)
chisq.test(table_gender_calorie)

table_gender_age <- table(calories_burned_data$Gender, calories_burned_data$Age_Group)
chisq.test(table_gender_age)

# ==========================================================
# 8. ANOVA
# ==========================================================

anova_age <- aov(Calories.Burned ~ Age_Group, data = calories_burned_data)
summary(anova_age)

anova_bmi <- aov(Calories.Burned ~ BMI_Category, data = calories_burned_data)
summary(anova_bmi)

anova_hr <- aov(Calories.Burned ~ HR_Group, data = calories_burned_data)
summary(anova_hr)

TukeyHSD(anova_age)
TukeyHSD(anova_bmi)
TukeyHSD(anova_hr)

# ==========================================================
# 9. REGRESSIONS
# ==========================================================

lm(Calories.Burned ~ Age, data = calories_burned_data)
lm(Calories.Burned ~ BMI, data = calories_burned_data)
lm(Calories.Burned ~ Running.Time.min., data = calories_burned_data)
lm(Calories.Burned ~ Running.Speed.km.h., data = calories_burned_data)
lm(Calories.Burned ~ Distance.km., data = calories_burned_data)
lm(Calories.Burned ~ Average.Heart.Rate, data = calories_burned_data)
lm(Calories.Burned ~ Weight.kg., data = calories_burned_data)

multiple_model <- lm(
  Calories.Burned ~ Age + Weight.kg. + BMI +
    Running.Time.min. + Running.Speed.km.h. +
    Distance.km. + Average.Heart.Rate,
  data = calories_burned_data
)

summary(multiple_model)

# ==========================================================
# 10. GRAPHICAL ANALYSIS (10 FIGURES)
# ==========================================================

# GRAPH 1: HISTOGRAM
hist(calories_burned_data$Calories.Burned,
     main = "Distribution of Calories Burned",
     xlab = "Calories Burned",
     col = "blue")

# GRAPH 2: BOXPLOT (GENDER)
boxplot(Calories.Burned ~ Gender,
        data = calories_burned_data,
        col = c("blue","pink"),
        main = "Calories Burned by Gender")

# GRAPH 3: CORRELATION HEATMAP
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper")

# GRAPH 4: RUNNING TIME VS CALORIES
plot(calories_burned_data$Running.Time.min.,
     calories_burned_data$Calories.Burned,
     pch = 19, col = "blue")
abline(lm(Calories.Burned ~ Running.Time.min., data = calories_burned_data),
       col = "red", lwd = 2)

# GRAPH 5: DISTANCE VS CALORIES
plot(calories_burned_data$Distance.km.,
     calories_burned_data$Calories.Burned,
     pch = 19, col = "darkgreen")
abline(lm(Calories.Burned ~ Distance.km., data = calories_burned_data),
       col = "red", lwd = 2)

# GRAPH 6: AGE GROUP BOXPLOT
boxplot(Calories.Burned ~ Age_Group,
        data = calories_burned_data,
        col = "lightgreen",
        main = "Calories Burned by Age Group")

# GRAPH 7–10: REGRESSION DIAGNOSTICS
par(mfrow = c(2,2))
plot(multiple_model)

# ==========================================================
# END OF COMPLETE ANALYSIS
# ==========================================================