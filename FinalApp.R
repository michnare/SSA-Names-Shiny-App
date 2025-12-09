# ---------------- Packages ----------------
library(shiny)
library(readr)
library(plotly)
library(tidyverse)
library(scales)
library(bslib)
library(viridis)

# ---------------- Data Setup ----------------
rds_file <- "babynames_small.rds"

if (!file.exists(rds_file)) stop("babynames_small.rds not found!")

# Load and filter data to reduce memory usage
babynames <- readRDS(rds_file) %>%
  filter(year >= 1880) %>%  # keep only relevant years
  mutate(name_lower = tolower(name))

# ---------------- UI ----------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = "sans-serif",    # Ubuntu-friendly font
    heading_font = "sans-serif",
    primary = "#0072B2",
    secondary = "#D55E00",
    success = "#009E73",
    info = "#56B4E9",
    warning = "#F0E442",
    danger = "#CC79A7"
  ),
  
  tags$head(tags$style(HTML("
      .instruction-box {padding:15px; border-radius:8px; background:#f8f8f8; border:1px solid #cccccc; margin-bottom:15px;}
      .card {padding:15px; border-radius:10px; background:white; box-shadow:0 3px 8px rgba(0,0,0,0.05); margin-bottom:15px;}
      .navbar-brand {font-weight:600;}
      .page-header {padding:25px; background:linear-gradient(135deg, #e0e0e0, #d0d0d0); border-radius:10px; margin-bottom:20px;}
  "))),
  
  div(class="page-header",
      h2("United States Baby Name History Explorer 🔍", style="margin:0; font-weight:700;"),
      p("Explore historical baby name trends, cultural influences, and statistical changes.", style="margin-top:10px;")
  ),
  
  navbarPage(
    id = "main_nav",
    title = "Name Explorer",
    
    # ---------------- Tab: Trends Over Time ----------------
    tabPanel(
      "Trends Over Time 📈",
      sidebarLayout(
        sidebarPanel(
          div(class="card",
              textInput("trend_names", "Enter names (comma-separated):", "Olivia, Liam"),
              uiOutput("trend_gender_ui"),
              actionButton("trend_go", "Update Plot 📊", class="btn btn-primary w-100")
          )
        ),
        mainPanel(
          tags$button(class="btn btn-outline-primary mb-2", "Instructions ℹ️", `data-bs-toggle`="collapse", `data-bs-target`="#inst_trends"),
          div(id="inst_trends", class="collapse instruction-box", strong("Instructions ℹ️:"), p("Enter names and choose a gender for each. Click 'Update Plot 📊' to view popularity trends over time.")),
          div(class="card", plotlyOutput("trend_plot", height="500px")),
          textOutput("trend_note")
        )
      )
    ),
    
    # ---------------- Tab: Compare by Year ----------------
    tabPanel(
      "Compare by Year 🔁",
      sidebarLayout(
        sidebarPanel(
          div(class="card",
              numericInput("compare_year", "Select year (1880–2024):", 2024, min=1880, max=2024),
              textInput("compare_names", "Names to compare:", "Olivia, Liam"),
              uiOutput("compare_gender_ui"),
              actionButton("compare_go", "Show Comparison 🔎", class="btn btn-success w-100")
          )
        ),
        mainPanel(
          tags$button(class="btn btn-outline-success mb-2", "Instructions ℹ️", `data-bs-toggle`="collapse", `data-bs-target`="#inst_compare"),
          div(id="inst_compare", class="collapse instruction-box", strong("Instructions ℹ️:"), p("Select a year and names to compare their popularity during that year.")),
          div(class="card", plotlyOutput("compare_plot", height = "500px")),
          textOutput("compare_note")
        )
      )
    ),
    
    # ---------------- Tab: Top Names ----------------
    tabPanel(
      "Top Names ⭐",
      sidebarLayout(
        sidebarPanel(
          div(class="card",
              numericInput("top_year", "Select year (1880–2024):", 2024, min=1880, max=2024)
          )
        ),
        mainPanel(
          tags$button(class="btn btn-outline-warning mb-2", "Instructions ℹ️", `data-bs-toggle`="collapse", `data-bs-target`="#inst_top"),
          div(id="inst_top", class="collapse instruction-box", strong("Instructions ℹ️:"), p("Choose a year to display the top 10 male and female names.")),
          h4("Top 10 Names by Gender"),
          div(class = "text-danger", textOutput("top_names_error")),
          div(class="card", tableOutput("top_names_table"))
        )
      )
    ),
    
    # ---------------- Tab: Name Event Analysis ----------------
    tabPanel(
      "Name Event Analysis 📅",
      sidebarLayout(
        sidebarPanel(
          div(class="card",
              textInput("event_name", "Enter a name:", "Olivia"),
              selectInput("event_gender", "Gender:", choices=c("M","F","All"), selected="All"),
              numericInput("event_year", "Event year:", 2000, min=1880, max=2024),
              actionButton("event_go", "Run Analysis ▶️", class="btn btn-info w-100"),
              hr(),
              tags$button(
                class = "btn btn-outline-info mb-2",
                "About This Test ℹ️",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = "#slope_test_info"
              ),
              div(
                id = "slope_test_info",
                class = "collapse instruction-box",
                strong("About the Slope-Based Trend Test ℹ️:"),
                p("This test uses segmented regression on ten years before and after a given year to evaluate how the popularity of a name changes before and after a specific event year."),
                tags$ul(
                  tags$li(strong("Immediate Level Change (β₂): "), "Was there an instant jump or drop the year after the event?"),
                  tags$li(strong("Slope Change (β₃): "), "Did the long-term rate of growth increase or decrease after the event?")
                ),
                p("These effects are independent. A name may spike immediately but then decline (e.g., ", em("Elsa"), ")."),
                p("Note that for names with accelerating/decelerating trends, the test can give “significant” results even if the event didn’t actually alter the trajectory. Referring to the graph can help determine if this is the case.")
              ),
              hr(),
              h4("Slope-Based Trend Test"),
              htmlOutput("event_slope")
          )
        ),
        
        mainPanel(
          tags$button(
            class = "btn btn-outline-info mb-2",
            "Instructions ℹ️",
            `data-bs-toggle` = "collapse",
            `data-bs-target` = "#inst_event"
          ),
          div(
            id = "inst_event",
            class = "collapse instruction-box",
            strong("Instructions ℹ️:"),
            p("Enter a name, pick a gender, and choose an event year.")
          ),
          h4("Event-Based Popularity Plot"),
          div(class="card", plotlyOutput("event_plot", height = "500px"))
        )
      )
    ),
    
    # ---------------- Tab: Name Highlights ----------------
    tabPanel(
      "Name Highlights ✨",
      sidebarLayout(
        sidebarPanel(
          div(class="card",
              selectInput("highlight_name", "Choose a highlighted name:",
                          choices = c("Adele", "Ariel", "Annabeth", "Barack", "Elsa",
                                      "Hermione", "Khaleesi", "Luke", "Moana", "Rey", "Ursula"),
                          selected="Adele"),
              p("These names were chosen due to interesting cultural or historical events that influenced popularity."),
              hr(),
              tags$button(
                class = "btn btn-outline-info mb-2",
                "About This Test ℹ️",
                `data-bs-toggle` = "collapse",
                `data-bs-target` = "#slope_test_info"
              ),
              div(
                id = "slope_test_info",
                class = "collapse instruction-box",
                strong("About the Slope-Based Trend Test ℹ️:"),
                p("This test uses segmented regression on ten years before and after a given year to evaluate how the popularity of a name changes before and after a specific event year."),
                tags$ul(
                  tags$li(strong("Immediate Level Change (β₂): "), "Was there an instant jump or drop the year after the event?"),
                  tags$li(strong("Slope Change (β₃): "), "Did the long-term rate of growth increase or decrease after the event?")
                ),
                p("These effects are independent. A name may spike immediately but then decline (e.g., ", em("Elsa"), ")."),
                p("Note that for names with accelerating/decelerating trends, the test can give “significant” results even if the event didn’t actually alter the trajectory. Referring to the graph can help determine if this is the case.")
              ),
              h4("Slope-Based Trend Test"),
              uiOutput("slope_result")
          )
        ),
        mainPanel(
          tags$button(class="btn btn-outline-dark mb-2", "Instructions ℹ️", `data-bs-toggle`="collapse", `data-bs-target`="#inst_highlight"),
          div(id="inst_highlight", class="collapse instruction-box", strong("Instructions ℹ️:"), p("View trend lines and event-year annotations for culturally-relevant names.")),
          h4("Event Description"),
          uiOutput("highlight_description"),
          div(class="card", plotlyOutput("highlight_plot", height = "500px"))
        )
      )
    ),
    
    # ---------------- Tab: Dataset Info ----------------
    tabPanel(
      "Dataset Info 🗂️",
      fluidRow(
        column(
          width = 10,
          offset = 1,
          div(
            class = "card",
            h3("About the Dataset"),
            p("This app uses U.S. baby name data obtained from the Social Security Administration (SSA). 
          The dataset contains national counts of baby names by year, starting from 1880. 
          It includes counts of male and female names from Social Security card applications, which are submitted shortly after a child's birth."),
            p(HTML(
              'You can access the dataset here: 
           <a href="https://catalog.data.gov/dataset/baby-names-from-social-security-card-applications-national-data" target="_blank">
             Baby Names from Social Security Card Applications — Data.gov
           </a>.'
            ))
          )
        )
      )
    )
    
  )
)

# ---------------- Server ----------------
server <- function(input, output, session) {
  
  # ---------------- Segmented Regression Helper ----------------
  segmented_test <- function(df, event_year, window = 10) {
    # Limit data to a window around the event year
    df <- df %>% 
      filter(year >= event_year - window,
             year <= event_year + window) %>%
      arrange(year)
    
    # Need enough points to run regression
    if (nrow(df) < 10) return(NULL)
    
    # Build segmented regression variables
    df <- df %>% mutate(
      time = year,
      after = ifelse(year > event_year, 1, 0),
      time_after = ifelse(year > event_year, year - event_year, 0)
    )
    
    model <- lm(percent ~ time + after + time_after, data = df)
    sm <- summary(model)
    coef <- sm$coefficients
    
    list(
      beta_level = coef["after", "Estimate"],
      p_level = coef["after", "Pr(>|t|)"],
      beta_slope = coef["time_after", "Estimate"],
      p_slope = coef["time_after", "Pr(>|t|)"]
    )
  }
  
  
  
  # ---------------- Gender UI ----------------
  create_gender_ui <- function(names_input, prefix) {
    names_selected <- str_split(names_input, ",\\s*")[[1]]
    tagList(lapply(seq_along(names_selected), function(i) {
      selectInput(paste0(prefix, "_gender_", i),
                  paste("Select gender for", names_selected[i]),
                  choices = c("M","F","All"), selected="All")
    }))
  }
  
  output$trend_gender_ui <- renderUI({
    req(input$trend_names)
    create_gender_ui(input$trend_names, "trend")
  })
  
  output$compare_gender_ui <- renderUI({
    req(input$compare_names)
    create_gender_ui(input$compare_names, "compare")
  })
  
  shared <- reactiveValues(names=c("olivia","liam"),
                           genders=c("All","All"),
                           compare_year=2024)
  
  get_gender_selection <- function(names_input, prefix) {
    names_selected <- str_split(names_input, ",\\s*")[[1]]
    sapply(seq_along(names_selected),
           function(i) input[[paste0(prefix, "_gender_", i)]])
  }
  
  observeEvent(input$trend_go, {
    names_selected <- str_split(input$trend_names, ",\\s*")[[1]] %>% tolower()
    genders_selected <- get_gender_selection(input$trend_names, "trend")
    shared$names <- names_selected
    shared$genders <- genders_selected
    
    updateTextInput(session, "compare_names",
                    value=paste(names_selected, collapse=", "))
    
    lapply(seq_along(names_selected), function(i) {
      updateSelectInput(session,
                        paste0("compare_gender_", i),
                        selected=genders_selected[i])
    })
  })
  
  observeEvent(input$compare_go, {
    names_selected <- str_split(input$compare_names, ",\\s*")[[1]] %>% tolower()
    genders_selected <- get_gender_selection(input$compare_names, "compare")
    shared$names <- names_selected
    shared$genders <- genders_selected
    if (!is.null(input$compare_year))
      shared$compare_year <- input$compare_year
  })
  
  # ---------------- Trend Plot ----------------
  trend_data <- reactive({
    req(shared$names, shared$genders)
    
    bind_rows(lapply(seq_along(shared$names), function(i) {
      df <- babynames %>% filter(name_lower == shared$names[i])
      if (shared$genders[i] == "All") {
        df <- df %>%
          group_by(year) %>%
          summarize(percent = sum(percent, na.rm = TRUE)) %>%
          mutate(name = shared$names[i], gender = "All")
      } else {
        df <- df %>% filter(gender == shared$genders[i])
      }
      df
    }))
  })
  
  output$trend_plot <- renderPlotly({
    df <- trend_data()
    if (nrow(df)==0)
      return(plotly_empty() %>% layout(title="The inputted name(s) do not appear in the SSA dataset."))
    
    df <- df %>% mutate(name_gender=paste0(name, " (", gender, ")"))
    
    p <- ggplot(df, aes(year, percent, color=name_gender)) +
      geom_line(size=1.2) +
      scale_color_viridis(discrete=TRUE) +
      labs(title="Name Popularity Over Time", x="Year", y="Percent of Total Babies") +
      scale_y_continuous(labels=scales::label_number(scale=1, suffix="%")) +
      theme_minimal()
    
    ggplotly(p, dynamicTicks=FALSE)
  })
  
  output$trend_note <- renderText({
    paste0("📌 Showing data for: ",
           paste(shared$names, shared$genders, sep=" (", collapse="), "), ")")
  })
  
  # ---------------- Compare by Year ----------------
  compare_data <- reactive({
    req(shared$names, shared$genders, shared$compare_year)
    
    bind_rows(lapply(seq_along(shared$names), function(i) {
      df <- babynames %>%
        filter(name_lower == shared$names[i], year == shared$compare_year)
      
      if (shared$genders[i] == "All") {
        df <- df %>%
          group_by(year) %>%
          summarize(percent = sum(percent, na.rm = TRUE)) %>%
          mutate(name = shared$names[i], gender="All")
      } else {
        df <- df %>% filter(gender == shared$genders[i])
      }
      df
    })) %>%
      mutate(name_gender = paste0(name," (",gender,")"))
  })
  
  output$compare_plot <- renderPlotly({
    df <- compare_data()
    if (nrow(df)==0)
      return(plotly_empty() %>% layout(title="The inputted name(s) do not appear in the SSA dataset."))
    
    p <- ggplot(df, aes(x=name_gender, y=percent, fill=name_gender)) +
      geom_col() +
      scale_fill_viridis(discrete=TRUE) +
      labs(title="Popularity Comparison", x="Name", y="Percent") +
      scale_y_continuous(labels=scales::label_number(scale=1, suffix="%")) +
      theme_minimal()
    
    ggplotly(p, dynamicTicks=FALSE)
  })
  
  output$compare_note <- renderText({
    paste0("📌 Showing data for: ",
           paste(shared$names, shared$genders, sep=" (", collapse="), "),
           ") | Year: ", shared$compare_year)
  })
  
  # ---------------- Top Names ----------------
  output$top_names_table <- renderTable({
    req(input$top_year)
    
    top_male <- babynames %>%
      filter(gender=="M", year==input$top_year) %>%
      arrange(desc(percent)) %>% slice_head(n=10)
    
    top_female <- babynames %>%
      filter(gender=="F", year==input$top_year) %>%
      arrange(desc(percent)) %>% slice_head(n=10)
    
    tibble(
      rank=1:10,
      `Female Name`=top_female$name,
      `Percent of Females`=scales::percent(top_female$percent/100, accuracy=0.01),
      `Male Name`=top_male$name,
      `Percent of Males`=scales::percent(top_male$percent/100, accuracy=0.01)
    )
  })
  
  output$top_names_error <- renderText({
    if (is.null(input$top_year) ||
        input$top_year < 1880 ||
        input$top_year > 2024)
      "Error: Please enter a valid year between 1880 and 2024."
    else ""
  })
  
  # ---------------- Event Analysis ----------------
  event_data <- eventReactive(input$event_go, {
    req(input$event_name, input$event_year)
    
    df <- babynames %>%
      filter(name_lower == tolower(input$event_name))
    
    if (input$event_gender != "All")
      df <- df %>% filter(gender == input$event_gender)
    
    df %>% arrange(year)
  })
  
  output$event_plot <- renderPlotly({
    df <- event_data()
    if (nrow(df) == 0)
      return(plotly_empty() %>% layout(title="The inputted name(s) do not appear in the SSA dataset."))
    
    df <- df %>%
      group_by(year) %>%
      summarize(percent = sum(percent, na.rm = TRUE))
    
    p <- ggplot(df, aes(year, percent)) +
      geom_line(color="#5D3A9B", size=1) +
      geom_vline(xintercept=input$event_year, linetype="dotted", color="#0072B2") +
      labs(title=paste("Popularity of", input$event_name),
           x="Year", y="Percent of Total Babies") +
      scale_y_continuous(labels=scales::label_number(scale=1, suffix="%")) +
      theme_minimal()
    
    ggplotly(p, dynamicTicks=FALSE)
  })
  
  output$event_slope <- renderUI({
    df <- event_data() %>%
      group_by(year) %>%
      summarize(percent = sum(percent, na.rm=TRUE)) %>%
      arrange(year)
    
    res <- segmented_test(df, input$event_year)
    if (is.null(res))
      return(HTML("<b>Not enough data for segmented regression.</b>"))
    
    interp_level <- if (res$p_level < 0.05) {
      if (res$beta_level > 0)
        "Significant upward jump immediately after the event."
      else
        "Significant drop immediately after the event."
    } else {
      "No significant immediate jump after the event."
    }
    
    interp_slope <- if (res$p_slope < 0.05) {
      if (res$beta_slope > 0)
        "Significant sustained increase in the rate of growth after the event."
      else
        "Significant sustained decrease in the rate of growth after the event."
    } else {
      "No significant sustained change in the rate of growth after the event."
    }
    
    HTML(paste0(
      "<b>Segmented Regression Result:</b><br>",
      "Immediate level change (β₂): ", round(res$beta_level, 6),
      " (p=", signif(res$p_level, 4), ")<br>",
      "Slope change (β₃): ", round(res$beta_slope, 6),
      " (p=", signif(res$p_slope, 4), ")<br><br>",
      "<b>Interpretation:</b><br>",
      interp_level, "<br>",
      interp_slope
    ))
  })
  
  # ---------------- Highlight Tab ----------------
  highlight_info <- list(
    "Elsa"="Release of Disney's *Frozen*.",
    "Khaleesi"="Popularity of Daenerys 'Khaleesi' from *Game of Thrones*.",
    "Ariel"="Release of Disney's *The Little Mermaid*.",
    "Barack"="Obama announces his run for president.",
    "Hermione"="*Harry Potter* increases in popularity.",
    "Luke"="First *Star Wars* installment.",
    "Adele"="Popularity of the same-named singer.",
    "Moana"="Release of Disney's *Moana*",
    "Rey"="Release of *Star Wars: The Force Awakens*",
    "Annabeth"="Publication of *The Lightning Thief*",
    "Ursula" ="Release of Disney's *The Little Mermaid*."
  )
  
  event_years <- list(
    "Elsa"=2013,"Khaleesi"=2011,"Ariel"=1989,"Barack"=2007,
    "Hermione"=2001,"Luke"=1977,"Adele"=2008,"Moana"=2016,
    "Rey"=2015,"Annabeth"=2005,"Ursula"=1989
  )
  
  output$highlight_description <- renderUI({
    desc_html <- gsub("\\*(.*?)\\*", "<i>\\1</i>",
                      highlight_info[[input$highlight_name]])
    
    HTML(paste0(
      '<div style="background-color:#E0E0E0;padding:12px;',
      'border-radius:8px;border:1px solid #5D3A9B;font-weight:bold;">',
      desc_html,'</div>'
    ))
  })
  
  output$highlight_plot <- renderPlotly({
    df <- babynames %>%
      filter(name_lower == tolower(input$highlight_name)) %>%
      group_by(year) %>%
      summarize(percent = sum(percent, na.rm=TRUE))
    
    if (nrow(df)==0)
      return(plotly_empty() %>% layout(title="The inputted name(s) do not appear in the SSA dataset."))
    
    e_year <- event_years[[input$highlight_name]]
    
    p <- ggplot(df, aes(year, percent)) +
      geom_line(color="#5D3A9B", size=1) +
      geom_vline(xintercept=e_year, linetype="dotted", color="#0072B2") +
      annotate("text", x=e_year-17, y=max(df$percent, na.rm=TRUE),
               label=paste("Event Year:", e_year), hjust=0) +
      labs(title=paste("Popularity of", input$highlight_name),
           x="Year", y="Percent of Total Babies") +
      scale_y_continuous(labels=scales::label_number(scale=1, suffix="%")) +
      theme_minimal()
    
    ggplotly(p, dynamicTicks=FALSE)
  })
  
  output$slope_result <- renderUI({
    df <- babynames %>%
      filter(name_lower == tolower(input$highlight_name)) %>%
      group_by(year) %>%
      summarize(percent=sum(percent, na.rm=TRUE)) %>%
      arrange(year)
    
    e_year <- event_years[[input$highlight_name]]
    
    res <- segmented_test(df, e_year)
    if (is.null(res))
      return(HTML("Not enough data for segmented regression."))
    
    interp_level <- if (res$p_level < 0.05) {
      if (res$beta_level > 0)
        "Significant upward jump immediately after the event."
      else
        "Significant decline immediately after the event."
    } else {
      "No significant immediate change after the event."
    }
    
    interp_slope <- if (res$p_slope < 0.05) {
      if (res$beta_slope > 0)
        "Significant increase in growth rate after the event."
      else
        "Significant decrease in growth rate after the event."
    } else {
      "No significant change in the growth rate after the event."
    }
    
    HTML(paste0(
      "<b>Segmented Regression:</b><br>",
      "Immediate level change (β₂): ", round(res$beta_level, 6),
      " (p=", signif(res$p_level, 4), ")<br>",
      "Slope change (β₃): ", round(res$beta_slope, 6),
      " (p=", signif(res$p_slope, 4), ")<br><br>",
      "<b>Interpretation:</b><br>",
      interp_level, "<br>",
      interp_slope
    ))
  })
}

# ---------------- Run App ----------------
shinyApp(ui, server)

