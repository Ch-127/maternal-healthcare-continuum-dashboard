# ================================================================
# West Hararghe Maternal Health Observatory
# ================================================================

required_packages <- c("shiny", "haven", "dplyr", "ggplot2", "DT")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Install missing packages first: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(shiny)
library(haven)
library(dplyr)
library(ggplot2)
library(DT)

# ------------------------- CONFIGURATION --------------------------

DATA_FILE <- "S1-Dataset.sav"

ANC_FOUR_VISITS_CONFIRMED <- TRUE
SBA_SKILLED_AND_FACILITY_CONFIRMED <- TRUE
PNC_WITHIN_SIX_WEEKS_CONFIRMED <- TRUE

ANC_VAR <- "Antenatalcarevisit"
SBA_VAR <- "deliverywithskilledbirthattendant"
PNC_VAR <- "postnatalcare"

# ---------------------- HELPER FUNCTIONS -------------------------

normalise_binary <- function(x) {
  z <- trimws(tolower(as.character(x)))
  yes_values <- c("yes", "y", "1", "true", "completed", "complete")
  no_values  <- c("no", "n", "0", "false", "not completed", "incomplete")
  out <- rep(NA_integer_, length(z))
  out[z %in% yes_values] <- 1L
  out[z %in% no_values] <- 0L
  out
}

safe_percent <- function(x) {
  valid <- !is.na(x)
  if (!any(valid)) return(NA_real_)
  100 * mean(x[valid] == 1)
}

safe_n <- function(x) sum(!is.na(x))

fmt_pct <- function(x, digits = 1) {
  if (is.na(x)) return("Not available")
  paste0(formatC(x, format = "f", digits = digits), "%")
}

make_kpi <- function(label, value, note = NULL, accent = "blue") {
  div(
    class = paste("kpi-card", paste0("accent-", accent)),
    div(class = "kpi-label", label),
    div(class = "kpi-value", value),
    if (!is.null(note)) div(class = "kpi-note", note)
  )
}

# -------------------------- LOAD DATA ----------------------------

if (!file.exists(DATA_FILE)) {
  stop(paste0("Cannot find '", DATA_FILE, "'."), call. = FALSE)
}

raw_data <- haven::read_sav(DATA_FILE)

maternal_data <- raw_data %>%
  mutate(across(where(haven::is.labelled), haven::as_factor))

maternal_data <- maternal_data %>%
  mutate(
    ANC_completed = normalise_binary(.data[[ANC_VAR]]),
    SBA_component = normalise_binary(.data[[SBA_VAR]]),
    PNC_component = normalise_binary(.data[[PNC_VAR]])
  ) %>%
  mutate(
    SBA_completed = SBA_component,
    PNC_completed = PNC_component,
    CoC_completion = case_when(
      ANC_completed == 1L & SBA_completed == 1L & PNC_completed == 1L ~ 1L,
      (ANC_completed == 0L | SBA_completed == 0L | PNC_completed == 0L) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# ----------------------------- UI --------------------------------

theme_css <- "
:root { --navy:#10243A; --blue:#246B8F; --teal:#168C83; --ink:#203247;
        --muted:#6B7B8C; --line:#E4EAF0; --canvas:#F4F7FA; }
body { background:var(--canvas); color:var(--ink); font-family:Inter,Arial,sans-serif; }
.navbar { background:var(--navy)!important; border:0; border-radius:0; }
.navbar .navbar-brand, .navbar .navbar-nav>li>a { color:white!important; }
.navbar .navbar-nav>.active>a { background:#1D3A55!important; }
.container-fluid { max-width:1500px; }
.page-heading { padding:20px 0 12px; }
.page-heading h2 { color:var(--navy); font-weight:750; margin:0 0 5px; }
.page-heading p { color:var(--muted); margin:0; }
.kpi-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(190px,1fr));
            gap:14px; margin:16px 0 20px; }
.kpi-card { background:white; border:1px solid var(--line); border-radius:13px;
            padding:18px; box-shadow:0 3px 12px rgba(16,36,58,.045);
            border-top:4px solid var(--blue); min-height:112px; }
.accent-teal { border-top-color:var(--teal); }
.accent-gold { border-top-color:#C18B2C; }
.accent-red { border-top-color:#B95B5B; }
.kpi-label { color:var(--muted); font-size:12px; font-weight:700;
             text-transform:uppercase; letter-spacing:.06em; }
.kpi-value { color:var(--navy); font-size:27px; font-weight:800; margin-top:7px; }
.kpi-note { color:var(--muted); font-size:12px; margin-top:3px; }
.panel-card { background:white; border:1px solid var(--line); border-radius:13px;
              padding:18px; margin:12px 0; box-shadow:0 3px 12px rgba(16,36,58,.035); }
.panel-card h4 { color:var(--navy); font-weight:750; margin-top:0; }
.small-muted { color:var(--muted); font-size:12px; }
"

ui <- navbarPage(
  title = "West Hararghe | Maternal Health Observatory",
  id = "main_nav",
  header = tags$head(
    tags$style(HTML(theme_css)),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")
  ),
  
  tabPanel(
    "Overview",
    fluidPage(
      div(class = "page-heading",
          h2("Maternal healthcare continuum"),
          p("Community-based evidence from West Hararghe Zone, Ethiopia")),
      uiOutput("overview_kpis"),
      fluidRow(
        column(7, div(class = "panel-card",
                      h4("Maternal healthcare service indicators"),
                      plotOutput("component_plot", height = 320))),
        column(5, div(class = "panel-card",
                      h4("Study outcome definition"),
                      p("Completion requires all three study-defined components: ANC, SBA, and PNC.")))
      )
    )
  ),
  
  tabPanel(
    "Continuum patterns",
    fluidPage(
      div(class = "page-heading",
          h2("Continuum completion and service patterns"),
          p("Examine the three components and their joint completion.")),
      fluidRow(
        column(7, div(class = "panel-card",
                      h4("Observed component combinations"),
                      plotOutput("combination_plot", height = 340))),
        column(5, div(class = "panel-card",
                      h4("Completion summary"),
                      tableOutput("continuum_table")))
      )
    )
  ),
  
  tabPanel(
    "Access & autonomy",
    fluidPage(
      div(class = "page-heading",
          h2("Healthcare access and women's autonomy"),
          p("Descriptive distributions of reported barriers and enabling factors.")),
      fluidRow(
        column(6, div(class = "panel-card",
                      h4("Transportation"),
                      plotOutput("transport_plot", height = 280))),
        column(6, div(class = "panel-card",
                      h4("Travel time"),
                      plotOutput("time_plot", height = 280))),
        column(6, div(class = "panel-card",
                      h4("Money"),
                      plotOutput("money_plot", height = 280))),
        column(6, div(class = "panel-card",
                      h4("Ability to go alone"),
                      plotOutput("goalone_plot", height = 280)))
      )
    )
  ),
  
  tabPanel(
    "Satisfaction",
    fluidPage(
      div(class = "page-heading",
          h2("Maternal healthcare experience"),
          p("Reported satisfaction with time, service delivery and distance.")),
      fluidRow(
        column(4, div(class = "panel-card",
                      h4("Time spent"),
                      plotOutput("sat_time_plot", height = 300))),
        column(4, div(class = "panel-card",
                      h4("Service delivery"),
                      plotOutput("sat_service_plot", height = 300))),
        column(4, div(class = "panel-card",
                      h4("Distance"),
                      plotOutput("sat_distance_plot", height = 300)))
      )
    )
  ),
  
  tabPanel(
    "Data quality",
    fluidPage(
      div(class = "page-heading",
          h2("Data quality and variable audit"),
          p("Missingness, distinct values and verification checks.")),
      div(class = "panel-card",
          h4("Variable-level audit"),
          DTOutput("quality_table"))
    )
  )
)

# --------------------------- SERVER ------------------------------

server <- function(input, output, session) {
  
  filtered_data <- reactive({ maternal_data })
  
  output$overview_kpis <- renderUI({
    d <- filtered_data()
    anc <- safe_percent(d$ANC_completed)
    sba <- safe_percent(d$SBA_completed)
    pnc <- safe_percent(d$PNC_completed)
    coc <- safe_percent(d$CoC_completion)
    
    div(
      class = "kpi-grid",
      make_kpi("Participants", format(nrow(d), big.mark = ","), "Records in loaded dataset"),
      make_kpi("ANC ≥4 visits", fmt_pct(anc), paste0("Valid n = ", safe_n(d$ANC_completed)), "teal"),
      make_kpi("Skilled birth", fmt_pct(sba), "Skilled professional + facility", "gold"),
      make_kpi("PNC check-up", fmt_pct(pnc), "At least one check-up", "red"),
      make_kpi("Complete continuum", fmt_pct(coc), "All three components", "teal")
    )
  })
  
  output$component_plot <- renderPlot({
    d <- filtered_data()
    plot_df <- data.frame(
      Component = c("ANC ≥4 visits", "SBA at facility", "PNC check-up"),
      Percent = c(safe_percent(d$ANC_completed), safe_percent(d$SBA_completed), safe_percent(d$PNC_completed))
    )
    plot_df <- plot_df[!is.na(plot_df$Percent), , drop = FALSE]
    validate(need(nrow(plot_df) > 0, "No verified component estimates are available."))
    
    ggplot(plot_df, aes(x = reorder(Component, Percent), y = Percent)) +
      geom_col(fill = "#168C83", width = .62) +
      geom_text(aes(label = paste0(round(Percent, 1), "%")), hjust = -0.15, size = 4) +
      coord_flip() +
      scale_y_continuous(limits = c(0, min(115, max(100, max(plot_df$Percent) + 12)))) +
      labs(x = NULL, y = "Percentage") +
      theme_minimal(base_size = 12)
  })
  
  output$combination_plot <- renderPlot({
    d <- filtered_data() %>%
      filter(!is.na(ANC_completed), !is.na(SBA_completed), !is.na(PNC_completed)) %>%
      mutate(Pattern = paste0("ANC ", ANC_completed, " / SBA ", SBA_completed, " / PNC ", PNC_completed)) %>%
      count(Pattern, name = "Women")
    
    validate(need(nrow(d) > 0, "No complete component records are available."))
    
    ggplot(d, aes(x = reorder(Pattern, Women), y = Women)) +
      geom_col(fill = "#246B8F") +
      coord_flip() +
      labs(x = "Service pattern", y = "Number of women") +
      theme_minimal(base_size = 12)
  })
  
  output$continuum_table <- renderTable({
    d <- filtered_data()
    completed <- sum(d$CoC_completion == 1, na.rm = TRUE)
    not_completed <- sum(d$CoC_completion == 0, na.rm = TRUE)
    
    data.frame(
      Indicator = c("Completed continuum", "Did not complete"),
      Number = c(as.numeric(completed), as.numeric(not_completed))
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")
  
  make_distribution_plot <- function(variable) {
    d <- maternal_data
    x <- as.character(d[[variable]])
    x[is.na(x) | trimws(x) == ""] <- "Missing"
    
    dd <- as.data.frame(table(x), stringsAsFactors = FALSE)
    colnames(dd) <- c("Category", "Count")
    
    ggplot(dd, aes(x = reorder(Category, Count), y = Count)) +
      geom_col(fill = "#246B8F", width = .65) +
      coord_flip() +
      labs(x = NULL, y = "Number of participants") +
      theme_minimal(base_size = 11)
  }
  
  output$transport_plot <- renderPlot(make_distribution_plot("transportation"))
  output$time_plot <- renderPlot(make_distribution_plot("time"))
  output$money_plot <- renderPlot(make_distribution_plot("money"))
  output$goalone_plot <- renderPlot(make_distribution_plot("Goalone"))
  
  output$sat_time_plot <- renderPlot(make_distribution_plot("Levelofsatisfactionwithtimespenttogettheservice"))
  output$sat_service_plot <- renderPlot(make_distribution_plot("Levelofsatisfactionwithservicedelivery"))
  output$sat_distance_plot <- renderPlot(make_distribution_plot("Levelofsatisfactionwiththedistanceoftheservice"))
  
  output$quality_table <- renderDT({
    vars <- names(maternal_data)
    audit <- data.frame(
      Variable = vars,
      Class = vapply(maternal_data, function(x) paste(class(x), collapse = "/"), character(1)),
      Missing_n = vapply(maternal_data, function(x) sum(is.na(x)), integer(1)),
      Missing_percent = round(vapply(maternal_data, function(x) mean(is.na(x)) * 100, numeric(1)), 1),
      Distinct_nonmissing = vapply(maternal_data, function(x) length(unique(x[!is.na(x)])), integer(1))
    )
    datatable(audit, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
}

# Run the App
shinyApp(ui, server)