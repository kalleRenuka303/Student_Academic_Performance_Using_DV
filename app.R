# Student Academic Performance Analytics — R Shiny website
# Run from this folder with: shiny::runApp()

library(shiny)
library(ggplot2)
library(DT)

# The project dataset is semicolon-separated.
data_file <- file.path("data", "student-mat.csv")
if (!file.exists(data_file)) {
  stop("Cannot find data/student-mat.csv. Keep the data folder beside app.R.")
}
students <- read.csv(data_file, sep = ";", stringsAsFactors = FALSE)
for (v in c("G1", "G2", "G3", "studytime", "absences", "age")) {
  students[[v]] <- as.numeric(as.character(students[[v]]))
}
students$StudyGroup <- factor(
  students$studytime,
  levels = c(1, 2, 3, 4),
  labels = c("< 2 hours", "2–5 hours", "5–10 hours", "> 10 hours")
)
students$GradeBand <- cut(
  students$G3, breaks = c(-1, 9, 13, 16, 20),
  labels = c("Needs support", "Developing", "Good", "Excellent")
)

ui <- fluidPage(
  tags$head(
    tags$title("EduMetrics | Student Academic Performance Analytics"),
    tags$style(HTML("\
      body { background:#f4f6fb; color:#202641; font-family:'Segoe UI',Arial,sans-serif; }\
      .hero { background:linear-gradient(120deg,#202852,#6259d9); color:white; padding:26px 30px; border-radius:16px; margin:16px 0 20px; }\
      .hero h1 { font-weight:800; margin-top:0; } .hero p { margin-bottom:4px; opacity:.9; }\
      .well { background:white; border:1px solid #e5e9f3; border-radius:13px; box-shadow:0 3px 12px rgba(25,35,70,.04); }\
      .stat { background:white; border:1px solid #e5e9f3; border-radius:13px; padding:16px; min-height:102px; margin-bottom:15px; }\
      .stat-label { color:#717991; font-size:12px; text-transform:uppercase; letter-spacing:.06em; }\
      .stat-value { color:#302e72; font-size:27px; font-weight:800; margin-top:8px; }\
      h3 { color:#302e72; font-weight:700; } .note { color:#717991; font-size:12px; }\
      .btn-primary { background:#6259d9; border-color:#6259d9; }\
      .nav-tabs>li>a { font-weight:600; }\
    "))
  ),
  div(class="hero",
      h1("EduMetrics"),
      p("Student Academic Performance Analytics"),
      p("Data Visualization with R • Explore grades, study time and attendance")),
  sidebarLayout(
    sidebarPanel(
      h3("Explore the data"),
      sliderInput("grade", "Final grade range (G3)", min=0, max=20, value=c(0,20), step=1),
      selectInput("study", "Study-time group", choices=c("All groups", levels(students$StudyGroup))),
      selectInput("school", "School", choices=c("All schools", sort(unique(students$school)))),
      sliderInput("absence", "Absences", min=0, max=max(students$absences, na.rm=TRUE),
                  value=c(0,max(students$absences, na.rm=TRUE)), step=1),
      actionButton("reset", "Reset filters", class="btn-primary"),
      hr(),
      p(class="note", "Dataset: Student Performance Mathematics data supplied with this project."),
      p(class="note", "Grades are on a 0–20 scale. Patterns show association, not causation.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", br(),
          fluidRow(
            column(3, div(class="stat", div(class="stat-label","Students in view"), div(class="stat-value", textOutput("n_students", inline=TRUE)))),
            column(3, div(class="stat", div(class="stat-label","Average final grade"), div(class="stat-value", textOutput("avg_grade", inline=TRUE)))),
            column(3, div(class="stat", div(class="stat-label","Highest grade"), div(class="stat-value", textOutput("max_grade", inline=TRUE)))),
            column(3, div(class="stat", div(class="stat-label","Average absences"), div(class="stat-value", textOutput("avg_abs", inline=TRUE))))
          ),
          fluidRow(column(6, wellPanel(plotOutput("grade_hist", height=300))),
                   column(6, wellPanel(plotOutput("grade_bands", height=300)))),
          wellPanel(h3("Quick insight"), textOutput("insight"))
        ),
        tabPanel("Study time", br(),
          fluidRow(column(6, wellPanel(plotOutput("study_avg", height=340))),
                   column(6, wellPanel(plotOutput("study_box", height=340)))),
          wellPanel(plotOutput("progression", height=320))
        ),
        tabPanel("Attendance", br(),
          wellPanel(plotOutput("absence_plot", height=390)),
          wellPanel(h3("Absence and grade correlation"), textOutput("correlation_text"))
        ),
        tabPanel("Gender & school", br(),
          fluidRow(column(6, wellPanel(plotOutput("gender_plot", height=330))),
                   column(6, wellPanel(plotOutput("school_plot", height=330))))
        ),
        tabPanel("Student records", br(),
          p("Search, sort and export the filtered student records."),
          downloadButton("download_csv", "Download filtered CSV", class="btn-primary"), br(), br(),
          DTOutput("student_table")
        ),
        tabPanel("About", br(), wellPanel(
          h3("Project objective"),
          p("This project uses R visualizations to explore student grades and their relationship with study time and absences."),
          h3("Visualizations included"),
          tags$ul(tags$li("Histogram of final grade distribution"),
                  tags$li("Bar chart and box plot for study-time groups"),
                  tags$li("Scatter plot of absences versus final grade with trend line"),
                  tags$li("Progression of G1, G2 and G3 average grades"),
                  tags$li("Student distribution by gender and school")),
          h3("Interpretation"),
          p("The dataset describes the students included in the supplied file. Relationships shown in charts are descriptive and do not prove cause and effect."),
          p(class="note", "Built with R, Shiny, ggplot2 and DT.")
        ))
      )
    )
  ),
  tags$footer(div(style="text-align:center;color:#858ba0;padding:16px;",
                  "Student Academic Performance Analytics • R Shiny Dashboard"))
)

server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSliderInput(session,"grade",value=c(0,20))
    updateSelectInput(session,"study",selected="All groups")
    updateSelectInput(session,"school",selected="All schools")
    updateSliderInput(session,"absence",value=c(0,max(students$absences,na.rm=TRUE)))
  })

  filtered <- reactive({
    d <- students[students$G3 >= input$grade[1] & students$G3 <= input$grade[2] &
                    students$absences >= input$absence[1] & students$absences <= input$absence[2], , drop=FALSE]
    if (input$study != "All groups") d <- d[d$StudyGroup == input$study, , drop=FALSE]
    if (input$school != "All schools") d <- d[d$school == input$school, , drop=FALSE]
    d
  })
  output$n_students <- renderText(nrow(filtered()))
  output$avg_grade <- renderText(if (!nrow(filtered())) "—" else sprintf("%.2f",mean(filtered()$G3,na.rm=TRUE)))
  output$max_grade <- renderText(if (!nrow(filtered())) "—" else max(filtered()$G3,na.rm=TRUE))
  output$avg_abs <- renderText(if (!nrow(filtered())) "—" else sprintf("%.1f",mean(filtered()$absences,na.rm=TRUE)))

  output$grade_hist <- renderPlot({
    d <- filtered(); validate(need(nrow(d)>0,"No records match these filters."))
    ggplot(d,aes(x=G3))+geom_histogram(binwidth=1,boundary=0,fill="#7168e8",color="white")+
      scale_x_continuous(breaks=seq(0,20,2),limits=c(0,20))+labs(title="Final grade distribution",x="Final grade (G3)",y="Students")+theme_minimal(base_size=12)
  })
  output$grade_bands <- renderPlot({
    d <- filtered(); validate(need(nrow(d)>0,"No records match these filters."))
    ggplot(d,aes(x=GradeBand,fill=GradeBand))+geom_bar(show.legend=FALSE)+
      scale_fill_manual(values=c("#e88a8a","#f0c36d","#69b8b0","#7168e8"))+
      labs(title="Students by grade band",x=NULL,y="Students")+theme_minimal(base_size=11)+
      theme(axis.text.x=element_text(angle=15,hjust=1))
  })
  output$insight <- renderText({
    d <- filtered(); if(!nrow(d)) return("No records match the current filters. Widen the selected ranges.")
    paste0("For ",nrow(d)," selected students, the average final grade is ",round(mean(d$G3,na.rm=TRUE),2),"/20. ",
           "The full dataset average is ",round(mean(students$G3,na.rm=TRUE),2),"/20.")
  })
  output$study_avg <- renderPlot({
    d <- filtered(); validate(need(nrow(d)>0,"No records match these filters."))
    s <- aggregate(G3~StudyGroup,d,FUN=mean, na.action=na.pass); s <- s[!is.na(s$StudyGroup),]
    ggplot(s,aes(x=StudyGroup,y=G3))+geom_col(fill="#58b9a9",width=.7)+geom_text(aes(label=round(G3,1)),vjust=-.5)+
      coord_cartesian(ylim=c(0,20))+labs(title="Average final grade by study time",x="Weekly study time",y="Average G3")+
      theme_minimal(base_size=11)+theme(axis.text.x=element_text(angle=15,hjust=1))
  })
  output$study_box <- renderPlot({
    d <- filtered(); validate(need(nrow(d)>0,"No records match these filters."))
    ggplot(d,aes(x=StudyGroup,y=G3,fill=StudyGroup))+geom_boxplot(show.legend=FALSE,alpha=.85)+
      labs(title="Grade spread by study time",x="Weekly study time",y="Final grade (G3)")+theme_minimal(base_size=11)+
      theme(axis.text.x=element_text(angle=15,hjust=1))
  })
  output$progression <- renderPlot({
    d <- filtered(); validate(need(nrow(d)>0,"No records match these filters."))
    vals <- data.frame(Period=factor(c("G1","G2","G3"),levels=c("G1","G2","G3")),Average=c(mean(d$G1,na.rm=TRUE),mean(d$G2,na.rm=TRUE),mean(d$G3,na.rm=TRUE)))
    ggplot(vals,aes(Period,Average,group=1))+geom_line(color="#7168e8",linewidth=1.2)+geom_point(color="#7168e8",size=4)+
      geom_text(aes(label=round(Average,1)),vjust=-1)+scale_y_continuous(limits=c(0,20))+labs(title="Average grade progression",x="Assessment period",y="Average grade")+theme_minimal(base_size=12)
  })
  output$absence_plot <- renderPlot({
    d <- filtered(); validate(need(nrow(d)>1,"Select at least two records to view this plot."))
    p <- ggplot(d,aes(x=absences,y=G3))+geom_jitter(width=.2,height=.15,alpha=.55,color="#5d63c9")+
      labs(title="Absences versus final grade",x="Number of absences",y="Final grade (G3)")+theme_minimal(base_size=12)
    if(length(unique(d$absences))>1) p <- p+geom_smooth(method="lm",se=TRUE,color="#e27d62")
    p
  })
  output$correlation_text <- renderText({
    d<-filtered(); if(nrow(d)<2 || length(unique(d$absences))<2 || length(unique(d$G3))<2) return("Not enough variation in the selected records to calculate correlation.")
    paste0("Pearson correlation: ",round(cor(d$absences,d$G3,use="complete.obs"),3),". Correlation is an association, not proof of causation.")
  })
  output$gender_plot <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, "No records match these filters."))
    gender_counts <- as.data.frame(table(d$sex), stringsAsFactors = FALSE)
    names(gender_counts) <- c("Gender", "Students")
    gender_counts$Percent <- 100 * gender_counts$Students / sum(gender_counts$Students)
    gender_counts$Label <- paste0(
      gender_counts$Gender, "\n", gender_counts$Students,
      " students\n", sprintf("%.1f%%", gender_counts$Percent)
    )
    ggplot(gender_counts, aes(x = "", y = Students, fill = Gender)) +
      geom_col(width = 1, color = "white", linewidth = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = Label), position = position_stack(vjust = 0.5),
                color = "white", fontface = "bold", size = 4) +
      scale_fill_manual(values = c("F" = "#7168e8", "M" = "#58b9a9")) +
      labs(title = "Student distribution by gender", fill = "Gender") +
      theme_void(base_size = 12) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            legend.position = "bottom")
  })
  output$school_plot <- renderPlot({
    d <- filtered()
    validate(need(nrow(d) > 0, "No records match these filters."))
    d$GradeRange <- cut(
      d$G3,
      breaks = c(-1, 5, 10, 15, 20),
      labels = c("0–5", "6–10", "11–15", "16–20"),
      include.lowest = TRUE
    )
    heat <- as.data.frame(table(d$StudyGroup, d$GradeRange), stringsAsFactors = FALSE)
    names(heat) <- c("StudyGroup", "GradeRange", "Students")
    ggplot(heat, aes(x = GradeRange, y = StudyGroup, fill = Students)) +
      geom_tile(color = "white", linewidth = 1) +
      geom_text(aes(label = Students), color = "#252742", size = 4) +
      scale_fill_gradient(low = "#fff7ec", high = "#f03b20") +
      labs(title = "Study time vs final grade heat map",
           x = "Final grade range (G3)", y = "Study-time group", fill = "Students") +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            panel.grid = element_blank())
  })
  output$student_table <- renderDT({
    d<-filtered(); keep<-intersect(c("school","sex","age","studytime","absences","G1","G2","G3","GradeBand"),names(d))
    datatable(d[,keep,drop=FALSE],filter="top",rownames=FALSE,options=list(pageLength=10,scrollX=TRUE,dom="Bfrtip",buttons=c("copy","csv","excel")),extensions="Buttons")
  },server=FALSE)
  output$download_csv <- downloadHandler(
    filename=function() paste0("student-performance-filtered-",Sys.Date(),".csv"),
    content=function(file) write.csv(filtered(),file,row.names=FALSE)
  )
}
shinyApp(ui=ui,server=server)
