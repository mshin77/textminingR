suppressPackageStartupMessages({
    library(DT)
    library(quanteda)
    library(plotly)
    library(shiny)
})

ui <- shinyUI(fluidPage(

    includeCSS("css/styles.css"),
    titlePanel("Text Mining"),
    navbarPage(
        "Let's start",
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
                             checkboxGroupInput("show_vars", "Select one or multiple columns for text data pre-processing."),
                             actionButton("apply", "Apply", icon = icon("table")),
                             downloadButton("download_table", "Download")
                         ),
                         # Step 2
                         conditionalPanel(
                             condition="input.conditioned == 2",
                             helpText(strong('Segment text data in a corpus into tokens (words).')),
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
                             helpText(strong('Use the default stopword list from quanteda.')),
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
                             selectizeInput("remove.var", "Select or type common words to remove.", choices = NULL, options = list(maxItems = 20)),
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
                             condition="input.conditioned2 == 4",
                             helpText(strong("The search can take 15 min. or more. Please don't close your screen.")),
                             sliderInput("K_range_1", "Range of K (number of topics)", value = c(5, 30), min = 0, max = 100),
                             selectizeInput("categorical_var", "Categorical moderator variable.", choices = NULL, multiple = TRUE),
                             selectInput("continuous_var", "Continuous moderator variable.", choices = NULL),
                             actionButton("search", "Search K", icon = icon("search"))
                         ),
                         # Step 2
                         conditionalPanel(
                             condition="input.conditioned2 == 5",
                             helpText(strong("Place the selected number of K.
                                             Display highest word probabilities for each topic.")),
                             uiOutput('K_number_uiOutput'),
                             selectizeInput("categorical_var_2", "Categorical moderator variable.", choices = NULL, multiple = TRUE),
                             selectInput("continuous_var_2", "Continuous moderator variable.", choices = NULL),
                             tags$hr(),
                             sliderInput("top_term_number_1", "Choose the top term numbers to display", value = 5, min = 0, max = 10),
                             actionButton("run", "Display", icon = icon("play")),
                             textInput("label_topics", "Label topics.", value = "", placeholder = 'Type text here')
                         ),
                         # Step 3
                         conditionalPanel(
                             condition="input.conditioned2 == 6",
                             helpText(strong("Display highest per-document-per topic for each topic.")),
                             uiOutput("topic_number_uiOutput"),
                             sliderInput("top_term_number_2", "Choose the top term numbers to display", value = 5, min = 0, max = 10),
                             actionButton("display", "Display", icon = icon("file-alt"))
                         ),
                         # Step 4
                         conditionalPanel(
                             condition="input.conditioned2 == 7",
                             helpText(strong("Explore example documents for each topic.")),
                             uiOutput("quote_topic_number_uiOutput"),
                             selectInput("topic_texts", "Example quotes to display", choices = NULL),
                             # uiOutput("quote_texts_uiOutput"),
                             actionButton("quote", "Quote", icon = icon("file-alt"))
                         ),
                         # Step 5
                         conditionalPanel(
                             condition="input.conditioned2 == 8",
                             helpText(strong("Plot topic prevalence effects by a categorical variable.")),
                             selectizeInput("effect_by_cat_btn", "Categorical moderator variable.", choices = NULL, multiple = TRUE),
                             actionButton("display_cat", "Display", icon = icon("file-alt"))
                         ),
                         # Step 6
                         conditionalPanel(
                             condition="input.conditioned2 == 9",
                             helpText(strong("Plot topic prevalence effects by a continuous variable.")),
                             selectInput("effect_by_con_btn", "Continuous moderator variable.", choices = NULL),
                             actionButton("display_con", "Display", icon = icon("file-alt"))
                         )
                     ),

                     mainPanel(
                         width = 9,
                         tabsetPanel(
                             id = "conditioned2",
                             tabPanel("1. Search K ", value = 4,
                                      shinycssloaders::withSpinner(
                                          plotOutput("search_K_plot"))),
                             tabPanel("2. Word-topic combinations", value = 5,
                                      shinycssloaders::withSpinner(
                                          plotOutput("topic_term_plot"))),
                             tabPanel("3. Document-topic combinations", value = 6,
                                      shinycssloaders::withSpinner(
                                          plotly::plotlyOutput("topic_by_prevalence_plot2")),
                                      br(),
                                      shinycssloaders::withSpinner(
                                          DT::dataTableOutput("topic_by_prevalence_table"))),
                             tabPanel("4. Quotes", value = 7,
                                      DT::dataTableOutput("quote_table")),
                             tabPanel("5. Plot by a categorical variable", value = 8,
                                      plotOutput("by_cat_plot"),
                                      br(),
                                      DT::dataTableOutput("by_cat_table")),
                             tabPanel("6. Plot by a continuous variable", value = 9,
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
                             selectInput("continuous_var_3", "Select a continous variable.", choices = NULL),
                             selectizeInput("type_terms", "Select or type terms.", choices = NULL, options = list(maxItems = 20)),
                             actionButton("plot_term", "Plot", icon = icon("play"))
                         )
                     ),

                     mainPanel(
                         width = 9,
                         tabsetPanel(
                             id = "conditioned3",
                             tabPanel("1. Hierarchical clustering", value = 10, plotly::plotlyOutput("dendro_plot")),
                             tabPanel("2. Word networkanalysis", value = 11, plotOutput("word_network_plot")),
                             tabPanel("3. Term frequency over time", value = 12, plotOutput("line_year_term_plot"),
                                      plotOutput("volcan_year_term_plot"))
                         )
                     )
                 )
        ),
        tabPanel("About",
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,
                         h4("Run on a web brower."),
                         p("Text mining web app is available on any devices. You can upload any text data (.csv file) to this app."),
                         br()
                     ),
                     mainPanel(
                         width = 9,
                         h3("Text mining web app"),
                         p("This app is developed in R using Shiny. This app is developed and maintained by",
                           a("Mikyung Shin", href = "https://www.researchgate.net/profile/Mikyung-Shin-3"), "."),
                         p("The website for the textminingR package is",
                           a("https://mshin77.github.io/textminingR/", href = "https://mshin77.github.io/textminingR/"), "."),
                         p("Browse source code at",
                           a("https://github.com/mshin77/textminingR/", href = "https://github.com/mshin77/textminingR/"), "."),
                         p("Comments, suggestions, and questions:",
                           a("mikyung.shin@wtamu.edu", href = "mailto:mikyung.shin@wtamu.edu"), "."),
                         br(),
                         h3("Reference R packages"),
                         p("install.packages", a("quanteda", href = "https://quanteda.io")),
                         p("install.packages", a("stm", href = "https://doi.org/10.18637/jss.v091.i02")),
                         p("install.packages", a("tidytext", href = "http://dx.doi.org/10.21105/joss.00037"))
                     )
                 )
        )
    )
))
