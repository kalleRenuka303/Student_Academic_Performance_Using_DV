# Student Performance Analysis using R

# Check the current working directory
getwd()

# List files and folders in the current directory
list.files()

# Check whether the data folder exists
dir.exists("data")
# Load dataset from the project's data folder
data <- read.csv(
  "data/student-mat.csv",
  sep = ";",
  header = TRUE
)

# Verify that the data loaded correctly
print(dim(data))
head(data)

# Show dataset dimensions
print(dim(data))

# Show first six records
print(head(data))

# Show all column names
print(colnames(data))

# Open complete dataset in a table
View(data)



# ==========================================
# Step 2: Analyze student final grades
# ==========================================

# Display basic statistics for final grades
summary(data$G3)

# Create a histogram of final grades
png("plots/01_final_grade_distribution.png",
    width = 800, height = 600)

hist(data$G3,
     main = "Distribution of Student Final Grades",
     xlab = "Final Grade (G3)",
     ylab = "Number of Students",
     col = "skyblue",
     border = "white",
     breaks = 10)

dev.off()

# Display the average final grade
mean(data$G3)

# Display the highest final grade
max(data$G3)

# Display the lowest final grade
min(data$G3)


# ==========================================
# STEP 2: UNDERSTAND THE DATA
# ==========================================

# Display first 6 student records
head(data)

# Check number of rows and columns
dim(data)

# Display column names
colnames(data)

# Check the structure and data types
str(data)

# Get summary statistics for all columns
summary(data)

# ==========================================
# HISTOGRAM
# Final Grade Distribution
# ==========================================

# Create the plots folder if it does not exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Save the histogram as a PNG image
png(
  "plots/01_final_grade_distribution.png",
  width = 800,
  height = 600
)

# Draw the histogram using final grades (G3)
hist(
  data$G3,
  main = "Distribution of Student Final Grades",
  xlab = "Final Grade (G3)",
  ylab = "Number of Students",
  col = "skyblue",
  border = "white",
  breaks = 10
)

# Finish saving the image
dev.off()

# Display summary statistics for final grades
summary(data$G3)

# Calculate average final grade
mean(data$G3)

# Check whether histogram image was created
file.exists("plots/01_final_grade_distribution.png")

# Open the histogram image
if (file.exists("plots/01_final_grade_distribution.png")) {
  browseURL(normalizePath("plots/01_final_grade_distribution.png"))
}

# ==========================================
# STEP 4: BAR CHART
# Study Time vs Average Final Grade
# ==========================================

# Create plots folder if it does not exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Calculate average final grade for each study-time group
avg_grade <- aggregate(
  G3 ~ studytime,
  data = data,
  FUN = mean
)

# Print average grades in the console
print(avg_grade)

# Save the bar chart as an image
png(
  "plots/02_studytime_vs_performance.png",
  width = 800,
  height = 600
)

# Draw the bar chart
barplot(
  avg_grade$G3,
  names.arg = avg_grade$studytime,
  main = "Study Time vs Average Final Grade",
  xlab = "Study Time Group",
  ylab = "Average Final Grade (G3)",
  col = "lightgreen",
  border = "white",
  ylim = c(0, 20)
)

# Add the average grade value above each bar
text(
  x = seq_along(avg_grade$G3),
  y = avg_grade$G3,
  labels = round(avg_grade$G3, 1),
  pos = 3
)

# Finish saving the image
dev.off()

# Open the saved picture automatically
browseURL(
  normalizePath("plots/02_studytime_vs_performance.png")
)


# ==========================================
# SCATTER PLOT
# Student Absences vs Final Grade
# ==========================================

# Create plots folder if it does not exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Save the scatter plot as a PNG image
png(
  "plots/03_absences_vs_final_grade.png",
  width = 800,
  height = 600
)

# Plot absences against final grades
plot(
  data$absences,
  data$G3,
  main = "Student Absences vs Final Grade",
  xlab = "Number of Absences",
  ylab = "Final Grade (G3)",
  pch = 19,
  col = "steelblue"
)

# Add a red trend line
abline(
  lm(G3 ~ absences, data = data),
  col = "red",
  lwd = 2
)

# Finish saving the image
dev.off()

# Calculate correlation between absences and final grades
correlation <- cor(data$absences, data$G3)

# Display correlation in the console
print(correlation)

# Open the saved graph picture
browseURL(
  normalizePath("plots/03_absences_vs_final_grade.png")
)

# ==========================================
# HEAT MAP
# Study Time vs Final Grade
# ==========================================

# Create plots folder if it does not exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Divide final grades into four ranges
grade_group <- cut(
  data$G3,
  breaks = c(-1, 5, 10, 15, 20),
  labels = c("0-5", "6-10", "11-15", "16-20")
)

# Count students in each study-time and grade group
heatmap_data <- table(
  Study_Time = data$studytime,
  Final_Grade = grade_group
)

# Display the student counts
print(heatmap_data)

# Save heat map as a PNG image
png(
  "plots/04_studytime_grade_heatmap.png",
  width = 900,
  height = 650
)

# Convert table into a matrix
heatmap_matrix <- as.matrix(heatmap_data)

# Draw the heat map
heatmap(
  heatmap_matrix,
  Rowv = NA,
  Colv = NA,
  scale = "none",
  col = colorRampPalette(
    c("white", "orange", "red")
  )(100),
  margins = c(8, 8),
  main = "Study Time vs Final Grade Heat Map",
  xlab = "Final Grade Range",
  ylab = "Study Time Group"
)

# Finish saving the image
dev.off()

# Open the saved heat map picture
browseURL(
  normalizePath("plots/04_studytime_grade_heatmap.png")
)




# ==========================================
# STEP 9: PIE CHART
# Students Distribution by Gender
# ==========================================

# Create plots folder if it does not exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Count male and female students
gender_count <- table(data$sex)

# Calculate gender percentages
gender_percent <- round(
  100 * gender_count / sum(gender_count),
  1
)

# Create labels with counts and percentages
gender_labels <- paste0(
  names(gender_count),
  "\nCount: ", as.vector(gender_count),
  "\n", gender_percent, "%"
)

# Save the pie chart as a PNG image
png(
  "plots/06_students_by_gender_piechart.png",
  width = 800,
  height = 600
)

# Draw the pie chart
pie(
  gender_count,
  labels = gender_labels,
  col = c("lightblue", "lightpink"),
  main = "Student Distribution by Gender",
  clockwise = TRUE,
  init.angle = 90
)

# Finish saving the image
dev.off()

# Display counts and percentages in the console
print(gender_count)
print(gender_percent)

# Open the saved picture
browseURL(
  normalizePath("plots/06_students_by_gender_piechart.png")
)
