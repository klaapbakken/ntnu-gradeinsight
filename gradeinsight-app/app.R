library(tidyverse)
library(rvest)
library(magrittr)
library(httr)
library(glue)
library(shiny)
library(shinythemes)
library(cowplot)

html_to_df <- function(filename){
    results_html <- filename %>% 
        read_html() %>% 
        html_table()
    
    grades_raw <- results_html[[2]]$Resultat
    courses_raw <- results_html[[2]]$Emne
    credits_raw <- results_html[[2]]$stp.
    period_raw <- results_html[[2]]$Semester
    
    indices <- grades_raw %>%
        map_lgl(~str_detect(.x, "[ABCDEF]$")) %>%
        which()
    
    grades <- indices %>%
        map_chr(~grades_raw %>% extract2(.x)) %>%
        map_chr(~str_sub(.x, start=-1)) %>% 
        as_factor()
    
    numeric_grades <- grades %>%
        recode(A = 5, B = 4, C = 3, D = 2, E = 1, F = 0) %>%
        as.numeric()
    
    courses <- indices %>%
        map_chr(~courses_raw %>% extract2(.x)) %>%
        map_chr(~str_extract(.x, "[ÆØÅA-Z\\-]{2,5}\\d{3,6}"))
    
    course_names <- indices %>% 
        map_chr(~courses_raw %>% extract2(.x)) %>%
        map_chr(~str_extract(.x, "[^\t]+$"))
    
    credits <- indices %>% 
        map_chr(~credits_raw %>% extract2(.x)) %>% 
        map_chr(~str_extract(.x, "[^Studiepoeng]+$")) %>% 
        str_replace("[,]", ".") %>% 
        as.numeric()
    
    periods <- indices %>% 
        map_chr(~period_raw %>% extract2(.x)) %>% 
        map_chr(~str_extract(.x, "20[0-9][0-9] ...[T]?")) %>%
        as_factor()
    
    period_codes <-  periods %>%
        map_chr(~str_replace(.x, "HØST", "H")) %>%
        map_chr(~str_replace(.x, "VÅR", "V")) %>% 
        map(~str_split(.x, " "))
    
    grades_df <- tibble(
        course_name = course_names,
        course_code = courses,
        numeric_grade = numeric_grades,
        grade = grades %>% fct_reorder(numeric_grades),
        period = periods,
        credits = credits,
        period_list = period_codes
    ) %>% 
        drop_na() %>%
        mutate(
            url = glue("https://grades.no/course/{course_code}/grades") %>% 
                tolower() %>% 
                as.character()
        ) %>%
        mutate(
            year = map(
                period_list,
                ~.x %>%
                    extract2(1) %>%
                    extract(1)),
            season = map(
                period_list,
                ~.x %>%
                    extract2(1) %>%
                    extract(2))
        ) %>% 
        select(-period_list) %>% 
        unnest() %>%
        mutate(period_code = paste0(season, year))
    
    return(grades_df)
}
query_grade_statistic <- function(url, period_code){
    request <- httr::GET(url)
    
    success <- request %>% httr::http_status() %>%
        extract2("message") %>%
        str_detect("(200)")
    
    if (!success){
        return(NA)
    }
    
    grade_stats <- request %>%
        content() %>% 
        map_dfr(as_tibble) %>% 
        filter(semester_code == period_code)
    
    grade_stats <- grade_stats %>%
        mutate(
            passed = a + b + c + d + e,
            failed = f,
            total = passed + failed,
            percentage_failed = failed/total * 100,
            url = url
        )
    incProgress(amount = 1)
    return(grade_stats)
    
}
quantile_range <- function(v, x, q){
    l <- v %>% is_less_than(x) %>% mean()
    u <- v %>% is_weakly_less_than(x) %>% mean()
    return(list(l = l, u = u) %>% extract2(q))
}
extend_grades_df <- function(df){
    grade_stats <- map2(
        df$url,
        df$period_code,
        query_grade_statistic
    ) %>%
        map_dfr(as_tibble)
    
    extended_df <- df %>%
        left_join(grade_stats, by="url") %>%
        select(-c(value, period, period_code)) %>% 
        mutate(semester_code = semester_code %>% as_factor())
    
    grade_percentile <- function(a, b, c, d, e, f, numeric_grade){
        uq <- partial(quantile_range, q="u")
        c(a, b, c, d, e, f) %>% 
            map2(c(5, 4, 3, 2, 1, 0), ~ rep(.y, .x)) %>%
            unlist() %>%
            as.vector() %>% 
            uq(numeric_grade)
    }
    
    l_grade_percentile <- function(a, b, c, d, e, f, numeric_grade){
        lq <- partial(quantile_range, q="l")
        c(a, b, c, d, e, f) %>% 
            map2(c(5, 4, 3, 2, 1, 0), ~ rep(.y, .x)) %>%
            unlist() %>%
            as.vector() %>% 
            lq(numeric_grade)
    }
    
    extended_df <- extended_df %>% drop_na() %>%
        mutate(
            percentile = extended_df %>%
                drop_na() %>% 
                select(a:f, numeric_grade) %>% 
                pmap_dbl(
                    grade_percentile
                ),
            l_percentile = extended_df %>%
                drop_na() %>% 
                select(a:f, numeric_grade) %>% 
                pmap_dbl(
                    l_grade_percentile
                )
            
        )
}


ui <- fluidPage(theme = shinytheme("cerulean"),
    
    titlePanel("NTNU GradeInsight"),
    
    navlistPanel(
        tabPanel("Submit grades",
                 textOutput("submit_description", h5),
                 fileInput("grades_file", label = h3("HTML input")),
                 dataTableOutput("grades_plot")
                 ),
        tabPanel("Course statistics",
                 textOutput("course_statistics_description", h5),
                 actionButton("query", "Collect grade distributions"),
                 dataTableOutput("extended_grades_plot")
                 ),
        tabPanel("Performance summary",
                 downloadButton("csv", "Download CSV"),
                 downloadButton("pdf", "Download PDF"),
                 textOutput("text_summary", h4),
                 plotOutput("grade_distribution"),
                 plotOutput("percentile"),
                 plotOutput("semester_performance")
                 )
    ),
    hr(h6("Created by Øyvind Klåpbakken using R Shiny. Uploaded files are only
       stored temporarily and only for the purpose of providing the user with grade statistics.
          The source code is available at",
          a("https://github.com/klaapbakken/ntnu-gradeinsight"))
    )
)

server <- function(input, output) {
    colors <- c(
        A = "#536D3D",
        B = "#9AB73C",
        C = "#E5D017",
        D = "#E59E15",
        E = "#DB4801",
        F = "#FF0000"
    )
    empty_tibble <- tibble()
    empty_plot <- plot_grid
    grades_df <- reactiveVal(value = NULL)
    extended_grades_df <- reactiveVal(value = NULL)
    plots <- reactiveValues()
    
    
    csv_to_file <- function(file){
        if (is.null(extended_grades_df())){
            empty_tibble %>% write_csv(file)
        }
        else{
        extended_grades_df() %>% write_csv(file)
        }
    }
    pdf_to_file <- function(file){
        if (length(plots) > 0){
        plot <- plot_grid(plots$dist_plot,
                          plots$perc_plot,
                          plots$semester_performance,
                          nrow=3, 
                          rel_heights = c(1,2,1)) 
        cowplot::ggsave(file, plot = plot, height=12, width=12/sqrt(2))
        }
        else{
            cowplot::ggsave(file, plot=empty_plot)
        }
    }
    
    output$grades_plot <- renderDataTable({
        validate(need(grades_df(), label = "Grades"))
        grades_df() %>% 
            select(course_name, course_code, period, grade, numeric_grade, credits)
    })
    
    output$extended_grades_plot <- renderDataTable({
        validate(need(extended_grades_df(), label = "Grade distributions"))
        extended_grades_df() %>% 
            select(course_name, course_code, semester_code, grade, numeric_grade, credits,
                   percentile, average_grade, percentage_failed)
    })
    
    output$grade_distribution <- renderPlot({
        validate(need(grades_df(), label = "Grades", message=FALSE))
        plots$dist_plot <- grades_df() %>% 
            mutate(grade = fct_reorder(grade, numeric_grade)) %>% 
            ggplot() +
            geom_bar(aes(x=grade, fill=grade)) +
            theme_classic() +
            scale_fill_manual(values = colors)
        plots$dist_plot
    })
    
    output$percentile <- renderPlot({
        validate(need(extended_grades_df(), label = "Grade statistics", message=FALSE))
        plots$perc_plot <- extended_grades_df() %>% 
            mutate(course_name = course_name %>%
                       as_factor %>%
                       fct_reorder(percentile)
                   ) %>% 
            ggplot(aes(y = percentile, x = course_name, fill=grade)) +
            geom_col() +
            geom_hline(aes(yintercept = mean(percentile)), color="black") + 
            coord_flip() +
            theme_classic() +
            scale_fill_manual(values = colors)
        plots$perc_plot
    })
    
    output$semester_performance <- renderPlot({
        validate(need(extended_grades_df(), label = "Grade statistics", message=FALSE))
        plots$semester_performance <- grades_df() %>% 
            ggplot(aes(x=period, fill=grade)) +
            geom_bar() + 
            coord_flip() +
            theme_classic() +
            scale_fill_manual(values = colors)
        plots$semester_performance
    })
    
    output$text_summary <- renderText({
        validate(need(extended_grades_df(), label = "Grade statistics", message=FALSE))
        perc <- extended_grades_df() %>%
            pull(percentile) %>% 
            mean() %>% 
            round(3) %>% 
            multiply_by(100)
        l_perc <- extended_grades_df() %>%
            pull(l_percentile) %>% 
            mean() %>% 
            round(3) %>% 
            multiply_by(100)
        av_grade <- grades_df() %>%
            pull(numeric_grade) %>% 
            mean() %>% 
            round(2)
        lb_grade <- av_grade %>%
            round() %>%
            as.character() %>%
            recode("5" = "A", "4" = "B", "3" = "C", "2" = "D", "1" = "E", "0" = "F")
        glue("Your average numeric grade is {av_grade} and is bounded below by the grade {lb_grade}. 
             On average, you performed as good or better than {perc}% of your class. You performed
             better than {l_perc}% of your class.")
    })
    
    output$submit_description <- renderText({
        "On this tab you can submit your grades by uploading the HTML - file from the \"Results\" section
        on StudWeb"
    })
    
    output$course_statistics_description <- renderText({
        "Pressing the button automatically collects course statistics for the courses you've submitted.
        Please allow up to 30 seconds for the query to complete."
    })
    
    output$csv <- downloadHandler(
        "performance_table.csv",
        csv_to_file,
        "text/csv"
        )
    output$pdf <- downloadHandler(
        "performance_visuals.pdf",
        pdf_to_file
    )
    
    observeEvent(input$grades_file, {
        grades_df(html_to_df(input$grades_file$datapath))
    })
    
    observeEvent(input$query, {
        validate(need(grades_df(), label = "Grades", message=FALSE))
        withProgress({
            extended_grades_df(extend_grades_df(grades_df()))
        },
        message = "Querying grade statistics",
        max=nrow(grades_df()),
        value=0
        )
    })
}

shinyApp(ui = ui, server = server)
