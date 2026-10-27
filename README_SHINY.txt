STUDENT ACADEMIC PERFORMANCE ANALYTICS — R SHINY WEBSITE

This is the R Shiny website version of the uploaded Data Visualization project.
It uses the project's data/student-mat.csv and preserves main.R and the supplied plots.

RUN IN VS CODE OR RSTUDIO
1. Extract the ZIP file.
2. Open the student-performance-analytics folder in VS Code.
3. Open an R console in that folder, or set the working directory to this folder.
4. Install packages once:
   install.packages(c("shiny", "ggplot2", "DT"), repos="https://cloud.r-project.org")
5. Start the website:
   shiny::runApp()

IMPORTANT
- app.R and the data folder must stay together.
- The dataset is read from data/student-mat.csv with sep=";".
- If R starts in the parent folder, run setwd("FULL/PATH/TO/student-performance-analytics") first.
- The dashboard includes grade distribution, grade bands, study-time charts, attendance scatter plot,
  G1/G2/G3 progression, gender/school summaries, filters and searchable/exportable records.
