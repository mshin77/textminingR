suppressPackageStartupMessages({
    library(quanteda)
    library(shiny)
})

ui <- shinyUI(fluidPage(

    tags$head(tags$style(HTML(".nav.nav-pills.nav-stacked > .active > a, 
    .nav.nav-pills.nav-stacked > .active > a:hover {background-color: #0c1f4a;}"))),
    includeCSS("css/styles.css"),
    titlePanel("Text Mining"),
    navbarPage(
        "",
        tabPanel("textminingR",
                 navlistPanel(widths = c(3,9),
                              tabPanel("About", includeMarkdown("markdown/about.md"))
                 )
        ),
        tabPanel("Upload",
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,
                         fileInput("file", "Choose a .csv or .xlsx file",
                                   multiple = TRUE,
                                   accept = c(".csv", ".xls", ".xlsx")
                         )
                     ),

                     mainPanel(
                         width = 9,
                         DT::dataTableOutput("data_table")
                     )
                 )
        ),
        tabPanel("Preprocess",
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,
                         # Step 1
                         conditionalPanel(
                             condition="input.conditioned == 1",
                             checkboxGroupInput("show_vars", "Select one or multiple columns."),
                             actionButton("apply", "Apply", icon = icon("table")),
                             downloadButton("download_table", "Download")
                         ),
                         # Step 2
                         conditionalPanel(
                             condition="input.conditioned == 2",
                             helpText(strong('Segment a corpus into tokens (words).')),
                             actionButton("preprocess", "Apply", icon = icon("table"))
                         ),
                         # Step 3
                         conditionalPanel(
                             condition="input.conditioned == 3",
                             helpText(strong('Apply researcher-developed dictionaries.')),
                             actionButton("dictionary", "Apply", icon = icon("table"))
                         ),
                         # Step 4
                         conditionalPanel(
                             condition="input.conditioned == 4",
                             helpText(strong('Use the default stopword list from', a("quanteda", href = "https://quanteda.io"), '.')),
                             actionButton("stopword", "Apply", icon = icon("table"))
                         ),
                         # Step 5
                         conditionalPanel(
                             condition="input.conditioned == 5",
                             helpText(strong("Construct a document-feature matrix.")),
                             actionButton("dfm_btn", "DFM", icon = icon("file-alt"))
                         ),
                         # Step 6
                         conditionalPanel(
                             condition="input.conditioned == 6",
                             selectizeInput("remove.var", "Remove common words.", choices = NULL, options = list(maxItems = 20)),
                             actionButton("remove", "Remove", icon = icon("minus-circle"))
                         )
                     ),

                     mainPanel(
                         width = 9,
                         tabsetPanel(
                             id = "conditioned",
                             tabPanel("1. Unite texts", value = 1,
                                      DT::dataTableOutput("step1_table")),
                             tabPanel("2. Preprocess", value = 2,
                                      shiny::verbatimTextOutput("step2_print_preprocess")),
                             tabPanel("3. Dictionary", value = 3,
                                      shiny::verbatimTextOutput("step2_print_dictionary")),
                             tabPanel("4. Stopword", value = 4,
                                      shiny::verbatimTextOutput("step2_print_stopword")),
                             tabPanel("5. Document-feature matrix", value = 5,
                                      plotly::plotlyOutput("step3_plot"),
                                      br(),
                                      DT::dataTableOutput("step3_table")),
                             tabPanel("6. Remove common words", value = 6,
                                      plotly::plotlyOutput("step4_plot"),
                                      br(),
                                      DT::dataTableOutput("step4_table"))
                         )
                     )
                 )

        ),
        tabPanel("Structural topic model",
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,
                         # Step 1
                         conditionalPanel(
                             condition="input.conditioned2 == 1",
                             helpText(strong("Please don't close your screen while running the model.")),
                             sliderInput("K_range_1", "Range of topic numbers", value = c(5, 30), min = 0, max = 100),
                             selectizeInput("categorical_var", "Select categorical covariate(s).", choices = NULL, multiple = TRUE),
                             selectizeInput("continuous_var", "Select continuous covariate(s).", choices = NULL, multiple = TRUE),
                             actionButton("search", "Search K", icon = icon("search"))
                         ),
                         # Step 2
                         conditionalPanel(
                             condition="input.conditioned2 == 2",
                             helpText(strong("Display highest word probabilities for each topic.")),
                             uiOutput('K_number_uiOutput'),
                             selectizeInput("categorical_var_2", "Select categorical covariate(s).", choices = NULL, multiple = TRUE),
                             selectizeInput("continuous_var_2", "Select continuous covariate(s).", choices = NULL, multiple = TRUE),
                             tags$hr(),
                             sliderInput("top_term_number_1", "Display the highest per-topic-per-word probabilities.", value = 5, min = 0, max = 10),
                             actionButton("run", "Display", icon = icon("play")),
                             tags$hr(),
                             textInput("label_topics", "Label topics. Use a comma for the next topic.", value = "", placeholder = "Type labels here")
                         ),
                         # Step 3
                         conditionalPanel(
                             condition="input.conditioned2 == 3",
                             helpText(strong("Display the highest per-document-per-topic probabilities.")),
                             uiOutput("topic_number_uiOutput"),
                             sliderInput("top_term_number_2", "Choose the top term numbers to display", value = 5, min = 0, max = 10),
                             actionButton("display", "Display", icon = icon("file-alt"))
                         ),
                         # Step 4
                         conditionalPanel(
                             condition="input.conditioned2 == 4",
                             helpText(strong("Explore example documents for each topic.")),
                             uiOutput("quote_topic_number_uiOutput"),
                             selectizeInput("topic_texts", "Example quotes to display", choices = NULL, multiple = TRUE),
                             # uiOutput("quote_texts_uiOutput"),
                             actionButton("quote", "Quote", icon = icon("file-alt"))
                         ),
                         # Step 5
                         conditionalPanel(
                             condition="input.conditioned2 == 5",
                             helpText(strong("Estimate relationships between document-level covariates/topics and metadata."),
                                      tags$hr(),
                                      strong("Choose covariates in the word-topic combinations tab")),
                             actionButton("effect", "Estimate", icon = icon("table")),
                             downloadButton("effect_download_table", "Download")
                         ), 
                         # Step 6
                         conditionalPanel(
                             condition="input.conditioned2 == 6",
                             helpText(strong("Plot topic prevalence effects by categorical covariates.")),
                             selectizeInput("effect_by_cat_btn", "Select a categorical covariate.", choices = NULL, multiple = TRUE),
                             actionButton("display_cat", "Display", icon = icon("file-alt"))
                         ),
                         # Step 7
                         conditionalPanel(
                             condition="input.conditioned2 == 7",
                             helpText(strong("Plot topic prevalence effects by continuous covariates.")),
                             selectizeInput("effect_by_con_btn", "Select a continuous covariate.", choices = NULL, multiple = TRUE),
                             actionButton("display_con", "Display", icon = icon("file-alt"))
                         )
                     ),

                     mainPanel(
                         width = 9,
                         tabsetPanel(
                             id = "conditioned2",
                             tabPanel("1. Search K ", value = 1,
                                      shinycssloaders::withSpinner(
                                          plotOutput("search_K_plot"))),
                             tabPanel("2. Word-topic", value = 2,
                                      shinycssloaders::withSpinner(
                                          plotOutput("topic_term_plot"))),
                             tabPanel("3. Document-topic", value = 3,
                                          plotly::plotlyOutput("topic_by_prevalence_plot2"),
                                      br(),
                                          DT::dataTableOutput("topic_by_prevalence_table")),
                             tabPanel("4. Quotes", value = 4,
                                      DT::dataTableOutput("quote_table")),
                             tabPanel("5. Effects", value = 5,
                                          DT::dataTableOutput("effect_table")),
                             tabPanel("6. Categorical variable", value = 6,
                                      plotOutput("by_cat_plot"),
                                      br(),
                                      DT::dataTableOutput("by_cat_table")),
                             tabPanel("7. Continuous variable", value = 7,
                                      plotOutput("by_con_plot"),
                                      br(),
                                      DT::dataTableOutput("by_con_table"))
                         )

                     )
                 )
        ),
        tabPanel("Network analysis",
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,
                         # Step 1
                         conditionalPanel(
                             condition="input.conditioned3 == 10",
                             helpText(strong("Create dendrogram.")),
                             actionButton("plot_dendrogram", "Plot", icon = icon("search"))
                         ),
                         # Step 2
                         conditionalPanel(
                             condition="input.conditioned3 == 11",
                             helpText(strong("Visualize word network.")),
                             sliderInput("co_occurence_number", "Minimum co-occurence numbers", value = 50, min = 0, max = 500),
                             sliderInput("correlation_value", "Minimum correlation betweewn pairwise words", value = 0.2, min = 0, max = 1, step = 0.1),
                             # selectizeInput("color", "Select or type keywords to color.", choices = NULL, options = list(maxItems = 20)),
                             actionButton("plot_word_network", "Plot", icon = icon("search"))
                         ),
                         # Step 3
                         conditionalPanel(
                             condition="input.conditioned3 == 12",
                             helpText(strong("Display selected terms that have changed in frequency over time.")),
                             selectizeInput("continuous_var_3", "Select a time-related variable.", choices = NULL, multiple = TRUE),
                             selectizeInput("type_terms", "Select or type terms.", choices = NULL, options = list(maxItems = 20, multiple = TRUE)),
                             actionButton("plot_term", "Plot", icon = icon("play"))
                         )
                     ),

                     mainPanel(
                         width = 9,
                         tabsetPanel(
                             id = "conditioned3",
                             tabPanel("1. Hierarchical clustering", value = 10, plotly::plotlyOutput("dendro_plot")),
                             tabPanel("2. Text network", value = 11, 
                                      shinycssloaders::withSpinner(
                                          plotOutput("word_network_plot"))),
                             tabPanel("3. Term frequency over time", value = 12,
                                      plotOutput("line_year_term_plot"))
                         )
                     )
                 )
        )
    )
))
 