suppressPackageStartupMessages({
    library(dplyr)
    library(DT)
    library(ggdendro)
    library(ggraph)
    library(ggplot2)
    library(igraph)
    library(plotly)
    library(quanteda)
    library(quanteda.textstats)
    library(readxl)
    library(shiny)
    library(stats)
    library(stm)
    library(stminsights)
    library(textmineR)
    library(tidyr)
    library(tidytext)
    library(tools)
    library(utils)
    library(vroom)
})

server <- shinyServer(function(input, output, session) {

    mydata <- reactive({
        req(input$file)
        filename <- input$file$datapath
        tryCatch({
            if (grepl("*[.]xlsx$|[.]xls$|[.]xlsm$", filename)) {
                data <- as.data.frame(read_excel(filename))
            } else if(grepl("*[.]csv$|*", filename)) {
                data <- read.csv(filename)
            }
        })
    })

  output$data_table <- DT::renderDataTable(mydata(), rownames = FALSE)

  # "Preprocess" page
  # Step 1: Unite texts
  # Display checkbox
  colnames <- reactive(names(mydata()))

  observe({
    updateCheckboxGroupInput(session, "show_vars",
                             choices = colnames(), selected = "")
  })

  listed_vars <- eventReactive(input$show_vars, {
    print(input$show_vars)
  })

  # Select one or multiple columns for text data pre-processing
  united_tbl <- eventReactive(input$apply, {

    united_texts_tbl <- mydata() %>%
      select(listed_vars()) %>%
      tidyr::unite(col = united_texts,
                   sep = " ",
                   remove = FALSE)
    docvar_tbl <- mydata()
    united_texts_tbl %>% dplyr::left_join(docvar_tbl)

  })

  # Print data table
  output$step1_table <- DT::renderDataTable(united_tbl(),
                                            rownames = FALSE)
  # Download data table as a csv file
  output$download_table <- downloadHandler(
    filename = function() {
      paste(input$file, ".csv", sep = "")
    },
    content = function(file) {
      utils::write.csv(united_tbl(), file, row.names = FALSE)
    }
  )

  # Step 2
  # Preprocess data
  processed_tokens <- eventReactive(
    eventExpr = input$preprocess, {
      united_tbl() %>% preprocess_texts()
    })

  output$step2_print_preprocess <- renderPrint({
    processed_tokens() %>% glimpse()
  })

  # Apply researcher-developed dictionaries
  tokens_dict <- eventReactive(input$dictionary, {

      tokens_dict_int <- processed_tokens() %>% quanteda::tokens_lookup(
          dictionary = dictionary(dictionary_list_1),
          valuetype = "glob",
          verbose = TRUE,
          exclusive = FALSE,
          capkeys = FALSE)

      tokens_dict_int %>% quanteda::tokens_lookup(
          dictionary = dictionary(dictionary_list_2),
          valuetype = "glob",
          verbose = TRUE,
          exclusive = FALSE,
          capkeys = FALSE)
  })

  output$step2_print_dictionary <- renderPrint({
      tokens_dict() %>% glimpse()
  })

  # Remove researcher-developed stop words.
  tokens_dict_no_stop <- eventReactive(input$stopword, {
      tokens_dict() %>% tokens_remove(stopwords_list)
  })

  output$step2_print_stopword <- renderPrint({
      tokens_dict_no_stop() %>% glimpse()
  })

  # Step 3
  # Construct a document-feature matrix
  dfm_init <- eventReactive(
    eventExpr = input$dfm_btn, {
      dfm_init <- processed_tokens() %>% quanteda::dfm()
    })

  output$step3_plot <- plotly::renderPlotly({
    dfm_init() %>% plot_word_frequency(n = 20)
  })

  output$step3_table <- DT::renderDataTable({
    quanteda.textstats::textstat_frequency(dfm_init())
  })


  # Step 4
  # Display the most frequent words (top 20)
  top_frequent_word  <- reactive({
    tstat_freq <- quanteda.textstats::textstat_frequency(dfm_init())
    tstat_freq_n_20 <- utils::head(tstat_freq, 20)
    top_frequent_word <- tstat_freq_n_20$feature
  })

  observe({
    updateSelectizeInput(session, "remove.var",
                         choices = top_frequent_word(),
                         options = list(create = TRUE),
                         server = TRUE)
  })

  # Remove common words across documents
  dfm_outcome <- reactive({
    print(input$remove)

    rm <- isolate(input$remove.var)

    if(!is.null(rm)){
      removed_processed_tokens <- quanteda::tokens_remove(processed_tokens(), rm)
      removed_processed_tokens %>% process_dictionary() %>% quanteda::dfm()
    }else{
      dfm_init()
    }
  })

  observeEvent(input$remove,{
    if(!is.null(input$remove.var)){
      output$step4_plot <- plotly::renderPlotly({
        dfm_outcome() %>% plot_word_frequency(n = 20)
      })
    }
  })

  observeEvent(input$remove,{
    if(!is.null(input$remove.var)){
      output$step4_table <- DT::renderDataTable({
        quanteda.textstats::textstat_frequency(dfm_outcome())
      })
    }
  })

  # "Structural topic model" page
  # 1. Search K
  colnames_cat <- reactive({
    categorical <- mydata() %>% select(which(sapply(.,is.character)))
    names(categorical)
  })

  observe({
    updateSelectizeInput(session, "categorical_var",
                         choices = colnames_cat(),
                         selected = "")
  })

  colnames_con <- reactive({
    continuous <- mydata() %>% select(which(sapply(.,is.numeric)))
    names(continuous)
  })

  observe({
    updateSelectInput(session, "continuous_var",
                      choices = colnames_con(),
                      selected = "")
  })

  print_K_range_1 <- eventReactive(eventExpr = input$K_range_1, {
    print(input$K_range_1[1]:input$K_range_1[2])
  })

  observeEvent(eventExpr = input$categorical_var, {
    print(input$categorical_var)
  })

  observeEvent(eventExpr = input$continuous_var, {
    print(input$continuous_var)
  })

  out <- reactive({
    quanteda::convert(dfm_outcome(), to = "stm")
  })

  K_search <- eventReactive(
    eventExpr = input$search, {
      utils::memory.limit(24000)
      set.seed(01234)

      stm::searchK(
        data = out()$meta,
        documents = out()$documents,
        vocab = out()$vocab,
        K = print_K_range_1(),
        prevalence = ~eval(parse(text = input$categorical_var)) +
          stm::s(eval(parse(text = input$continuous_var))),
        verbose = TRUE)
    })

  output$search_K_plot <- renderPlot({
    plot(K_search())
  })


  # 2. Step 2: Run a model and display highest word probabilities for each labeled topic
  output$K_number_uiOutput <- renderUI({
    sliderInput("K_number", "Choose K (number of topics)",
                value = 15, min = 0, max = 50)
  })

  print_K_number <- eventReactive(eventExpr = input$K_number, {
    print(input$K_number)
  })

  colnames_cat_2 <- reactive({

    categorical_2 <- mydata() %>% select(which(sapply(.,is.character)))

    names(categorical_2)
  })

  observe({
    updateSelectizeInput(session, "categorical_var_2",
                         choices = colnames_cat_2(),
                         server = TRUE)
  })

  colnames_con_2 <- reactive({
    continuous_2 <- mydata() %>% select(which(sapply(.,is.numeric)))
    names(continuous_2)
  })

  observe({
    updateSelectInput(session, "continuous_var_2",
                      choices = colnames_con_2(),
                      selected = "")
  })

  observeEvent(eventExpr = input$categorical_var_2, {
    print(input$categorical_var_2)
  })

  observeEvent(eventExpr = input$continuous_var_2, {
    print(input$continuous_var_2)
  })

  effect_stm_K_number <- reactiveVal()
  stm_K_number <- reactiveVal()

  observeEvent(input$run,{

    utils::memory.limit(24000)
    set.seed(01234)
    stm_K_number =   stm::stm(data = out()$meta,
                              documents = out()$documents,
                              vocab = out()$vocab,
                              K = print_K_number(),
                              prevalence = ~eval(parse(text = input$categorical_var_2)) +
                                stm::s(eval(parse(text = input$continuous_var_2))),
                              max.em.its = 75,
                              init.type = 'Spectral',
                              verbose = FALSE)

    stm_K_number(stm_K_number)

    ff = paste0('1:',input$K_number,' ~ ', input$categorical_var_2,
                ' +', ' stm::s(', input$continuous_var_2, ')')

    stmm =stm::estimateEffect(formula =eval(parse(text = ff)),
                              stmobj = stm_K_number(),
                              metadata = out()$meta,
                              uncertainty = "None")

    effect_stm_K_number(stmm)
  })


  # Tidy the word-topic combinations
  beta_td <- reactive({
    tidytext::tidy(stm_K_number(), document_names = rownames(dfm_outcome()))
  })

  observe({
    print(input$label_topics)
  })

  # Display highest word probabilities for each topic
  output$topic_term_plot <- renderPlot({
    print(input$top_term_number_1)
    req(!is.na(stm_K_number()))

    if(input$label_topics == ''){
      tn = NULL
    }else{
      tn = strsplit(input$label_topics, split = ',')[[1]]
    }

    beta_td() %>% plot_topic_term(topic_names = tn,
                                  top_n = input$top_term_number_1)
  })


  # Step 3: Display highest per-document-per topic for each topic
  output$topic_number_uiOutput <- renderUI({
    req(print_K_number())
    sliderInput("topic_number", "Choose the topic numbers to display",
                value = print_K_number(), min = 0, step = 1, max = print_K_number())
  })


  top_terms_selected <- reactive({
    beta_td() %>% examine_top_terms(top_n = input$top_term_number_2)
  })

  # Display per-document-per topic probabilities by topics
  gamma_terms <- reactive({
    print('calc gamma3')

    gamma_td = tidytext::tidy(stm_K_number(), matrix = "gamma",
                             document_names = rownames(dfm_outcome()))

    dd = gamma_td %>% examine_gamma_terms(top_terms = top_terms_selected())
    dd
  })

  # Visualize per-document-per topic probabilities ----
  output$topic_by_prevalence_plot2 <- NULL

  observeEvent(input$display,{
    gt =  gamma_terms()
    # save(gt, file = 'gt.Rdata')

    if(input$label_topics == ''){
      tn = NULL
    }else{
      tn = strsplit(input$label_topics, split = ',')[[1]]
    }
    # browser()
    output$topic_by_prevalence_plot2 <- plotly::renderPlotly({
      gamma_terms() %>%
        plot_topic_probability(topic_names = tn,
                               top_n = input$topic_number)
    })

  })

  # Display topics ordered by prevalence
  observeEvent(input$display,{

    if(input$label_topics == ''){
      tn = NULL
    }else{
      tn = strsplit(input$label_topics, split = ',')[[1]]
    }

    output$topic_by_prevalence_table <- DT::renderDataTable({
      gamma_terms() %>%
        plot_topic_probability_table(topic_names = tn,
                                     top_n = input$topic_number) %>%
        DT::datatable(rownames = FALSE)
    })
  })


  # Step 4: Explore example documents for each topic (display quotes)
  print_K_number_from_1 <- eventReactive(eventExpr = input$K_number, {
    print(1:input$K_number)
  })

  output$quote_topic_number_uiOutput <- renderUI({
    req(print_K_number_from_1())
    selectInput("topic_number_quote", "Topic number",
                choices = print_K_number_from_1(),
                selected = "")
  })

  observeEvent(eventExpr =input$topic_number_quote, {
    print(input$topic_number_quote)
  })

  observe({
    updateSelectInput(session, "topic_texts",
                      choices = colnames_cat_2(),
                      selected = "")
  })

  observeEvent(eventExpr = input$topic_texts, {
    print(input$topic_texts)
  })

  thoughts <- eventReactive(
    eventExpr = input$quote, {

      pn <- input$topic_number_quote %>% as.numeric()
      tn <- input$topic_texts

      stm::findThoughts(stm_K_number(), texts = out()$meta[[tn]],
                        n = 3, topics = pn)$docs[[1]]
    })

  output$quote_table <- DT::renderDataTable({
    thoughts() %>% tibble()
  })


  # Step 5: Plot topic prevalence effects by a categorical variable
  observe({
    updateSelectizeInput(session, "effect_by_cat_btn",
                         choices = colnames_cat_2(),
                         server = TRUE)
  })

  observeEvent(eventExpr = input$effect_by_cat_btn, {
    print(input$effect_by_cat_btn)
  })

  effects_categorical_var <- reactive({

    stminsights::get_effects(estimates = effect_stm_K_number(),
                             variable = input$effect_by_cat_btn,
                             type = 'pointestimate')
  })

  observeEvent(input$display_cat,{
    output$by_cat_plot <- renderPlot({
      effects_categorical_var() %>%
        ggplot(aes(x = value, y = proportion)) +
        facet_wrap(~ topic, ncol = 5) +
        geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 0.5, color = "#337ab7") +
        geom_point(color = "#337ab7", size = 1.5) +
        scale_y_continuous() +
        theme_bw()
    })
  })

  observeEvent(input$display_cat,{
    output$by_cat_table <- DT::renderDataTable({
      effects_categorical_var() %>%
        DT::datatable(rownames = FALSE)
    })
  })

  # Step 6: Plot topic prevalence effects by a continuous variable
  observe({
    updateSelectInput(session, "effect_by_con_btn",
                      choices = colnames_con_2(),
                      selected = "")
  })

  observeEvent(input$effect_by_con_btn, {
    print(input$effect_by_con_btn)
  })

  effects_continuous_var <- reactive({

    stminsights::get_effects(estimates = effect_stm_K_number(),
                             variable = input$effect_by_con_btn,
                             type = 'continuous')
  })

  observeEvent(input$display_con, {
    output$by_con_plot <- renderPlot({

      effects_continuous_var() %>%
        ggplot(aes(x = value, y = proportion)) +
        facet_wrap(~ topic, ncol = 5) +
        geom_smooth(color = "#337ab7", size = 0.7) +
        geom_ribbon(aes(ymin = lower, ymax = upper),
                    alpha = 0.2,
                    size = 1,
                    fill = "#337ab7") +
        scale_y_continuous() +
        theme_bw()
    })
  })


  observeEvent(input$display_con, {
    output$by_con_table <- DT::renderDataTable({
      effects_continuous_var() %>%
        DT::datatable(rownames = FALSE)
    })
  })

  # "Network analysis" page
  # 1. Hierarchical clustering
  hclust <- reactive({
    stm_dist <- textmineR::CalcHellingerDist(stm_K_number()$theta, by_rows = FALSE)
    stats::hclust(stats::as.dist(stm_dist), "ward.D")
  })

  # create dendrogram
  observeEvent(input$plot_dendrogram, {
    output$dendro_plot <- plotly::renderPlotly({
      ggdendro::ggdendrogram(hclust(),
                             rotate = FALSE, size = 2)+
        labs(title = NULL, x = 'Distance', y = 'Height')+
        theme_bw()
    })
  })


  # 2. Visualize word network
  observeEvent(input$plot_word_network,{
    output$word_network_plot <- renderPlot({
      set.seed(1234)

      # Convert quanteda objects (document-feature matrices) into tidy format
      dfm_td <-tidytext::tidy(dfm_outcome())

      co_occur_n <- as.numeric(input$co_occurence_number)
      print(co_occur_n)
      corr_n <- as.numeric(input$correlation_value)
      print(corr_n)
      term_cor <- dfm_td %>% tibble::as_tibble() %>%
        group_by(term) %>%
        filter(n() >= co_occur_n) %>%
        widyr::pairwise_cor(term, document, sort = TRUE)

      term_cor %>%
        filter(correlation > corr_n) %>%
        ggraph(layout = "fr")+
        geom_edge_link(aes(edge_alpha = correlation,
                           edge_width = correlation), edge_colour = "#337ab7")+
        geom_node_point(size = 5, color = "white")+
        ggraph::geom_node_text(aes(label = name),
                               repel = FALSE,
                               check_overlap = FALSE)+theme_void()
    })
  })


  # Display selected terms that have changed in frequency over time

  observe({
    updateSelectInput(session, "continuous_var_3",
                      choices = colnames_con_2(),
                      selected = "")
  })

  top_frequent_over_time  <- reactive({
    tstat_freq <- quanteda.textstats::textstat_frequency(dfm_outcome())
    tstat_freq_n_20 <- head(tstat_freq, 20)
    tstat_freq_n_20$feature
  })

  observe({
    updateSelectizeInput(session, "type_terms",
                         choices = top_frequent_over_time(),
                         options = list(create = TRUE),
                         server = TRUE)
  })

  observeEvent(input$type_terms, {
    print(input$type_terms)
  })

  observeEvent(input$continuous_var_3, {
    print(input$continuous_var_3)
  })

  observeEvent(input$plot_term,{

    vm <- isolate(input$type_terms)

    if(!is.null(vm)){
      dfm_outcome_obj <- dfm_outcome()
      dfm_td <-tidytext::tidy(dfm_outcome())
      gamma_td <- tidytext::tidy(stm_K_number(), matrix = "gamma",
                                 document_names = rownames(dfm_outcome()))
      dfm_outcome_obj@docvars$document <- dfm_outcome_obj@docvars$docname_

      # Bind data by column
      dfm_gamma_td <- gamma_td %>%
        left_join(dfm_outcome_obj@docvars, by = c("document" = "document")) %>%
        left_join(dfm_td, by = c("document" = "document"))

      year_term_counts <- dfm_gamma_td %>% tibble::as_tibble() %>%
        group_by(eval(parse(text = input$continuous_var_3))) %>%
        mutate(con_3_total = sum(count), percent = count/con_3_total) %>%
        ungroup()

      output$line_year_term_plot <- renderPlot({
        year_term_counts %>%
          filter(term %in% vm) %>%
          ggplot(aes(eval(parse(text = input$continuous_var_3)), count/con_3_total))+
          geom_point(color = "#337ab7")+
          geom_smooth(color = "#337ab7")+
          theme_bw()+
          facet_wrap(~ term, scales = "free_y")+
          scale_y_continuous(labels = scales::percent_format())+
          labs(x = "", y = "")
      })


      # Test the significance of word frequency over time
      model_gls <- year_term_counts %>%
        mutate(word = term) %>%
        filter(word %in% vm) %>%
        group_by(word) %>%
        do(tidy(glm(cbind(count, con_3_total - count) ~ eval(parse(text = input$continuous_var_3)), .,
                    family = "binomial"))) %>%
        ungroup() %>%
        filter(term == "eval(parse(text = input$continuous_var_3))") %>%
        arrange(desc(abs(estimate)))

      # Display a volcano plot
      output$volcan_year_term_plot <- renderPlot({
        model_gls %>%
          mutate(adjusted.p.value = p.adjust(p.value)) %>%
          ggplot(aes(estimate, adjusted.p.value)) +
          geom_point(color = "#337ab7") +
          scale_y_log10() +
          geom_text(aes(label = word), vjust = 1, hjust = 0.2,
                    check_overlap = TRUE) +
          xlab("Estimated change") +
          ylab("Adjusted p-value") +
          theme_bw()
      })
    }
  })
  session$onSessionEnded(stopApp)

})
