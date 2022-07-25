# Preprocess Text Data ----

#' @title Preprocess text data
#'
#' @name preprocess_texts
#'
#' @description
#' Preprocess text data by conducting the following functions:
#' construct a corpus; segment texts in a corpus into tokens; preprocess tokens;
#' convert the features of tokens to lowercase;
#' remove stopwords; specify the minimum length in characters for tokens (at least 2).
#'
#' @param data A data frame that contains text as data.
#' @param text_field A name of column that contains text data in a data frame.
#' @param ... Further arguments passed to \code{corpus}.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' processed_tokens <- SpecialEduTech %>% preprocess_texts(text_field = "abstract")
#'
#' }
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang := enquos

preprocess_texts <-
    function(data, text_field = "united_texts", ...) {

        # Construct a corpus
        corp <- quanteda::corpus(data, text_field = text_field, ...)

        # Segment texts in a corpus into tokens (words or sentences) by word boundaries
        toks <- quanteda::tokens(corp)

        # Preprocess tokens
        toks_clean <- quanteda::tokens(
            toks,
            what = "word",
            remove_punct = TRUE,
            remove_symbols = TRUE,
            remove_numbers = TRUE,
            remove_url = TRUE,
            remove_separators = TRUE,
            split_hyphens = TRUE,
            split_tags = TRUE,
            # remove_hyphens = TRUE,
            include_docvars = TRUE,
            padding = FALSE,
            verbose = TRUE)

        # Convert the features of tokens to lowercase.
        toks_lower <- quanteda::tokens_tolower(toks_clean,
                                               keep_acronyms = FALSE)

        # Remove English stopwords.
        toks_lower_no_stop <- toks_lower %>%
            quanteda::tokens_remove(stopwords("en"),
                          valuetype = "glob",
                          window = 0,
                          verbose = TRUE,
                          padding = TRUE)

        # Specify the minimum length in characters for tokens (at least 2).
        toks_lower_no_stop_adj <- toks_lower_no_stop %>%
            quanteda::tokens_select(min_nchar=2L,
                          verbose = TRUE)

        return(toks_lower_no_stop_adj)
    }


#' @title Plot word frequency results.
#'
#' @name plot_word_frequency
#'
#' @description
#' Plot the frequently observed top n terms.
#'
#' @param data A document-feature matrix (dfm) object through the quanteda package.
#' @param n The number of top n features (terms or words).
#' @param ... Further arguments passed to \code{quanteda.textstats::textstat_frequency}.
#'
#' @examples
#' \dontrun{
#' dfm <- processed_tokens %>% construct_dfm()
#'
#' dfm %>% plot_word_frequency(n = 20)
#' }
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rlang := enquos
#' @importFrom ggplot2 ggplot geom_point coord_flip labs theme_bw
#'
plot_word_frequency <-
    function(data, n = 20, ...) {
        word_frequency_plot <- data %>%
            quanteda.textstats::textstat_frequency(n = n, ...) %>%
            ggplot(aes(x = stats::reorder(feature, frequency), y = frequency)) +
            geom_point(colour = "#5f7994", size = 1) +
            coord_flip() +
            labs(x = NULL, y = "Word frequency") +
            theme_bw()
        return(word_frequency_plot)
    }


#' @title Extract frequently observed words
#'
#' @name extract_frequent_word
#'
#' @description
#' Extract frequently observed top n features (terms or words).
#'
#' @param data A document-feature matrix (dfm) object through the quanteda package.
#' @param n A number of top n features (terms or words) frequently observed.
#' @param ... Further arguments passed to \code{quanteda.textstats::textstat_frequency}.
#'
#' @examples
#' \dontrun{
#' dfm <- processed_tokens %>% construct_dfm()
#'
#' dfm %>% extract_frequent_word()
#' }
#'
#' @export
#'
#' @importFrom rlang := enquos
#'
extract_frequent_word <-
    function(data, n = 20, ...) {

        tstat_freq <- quanteda.textstats::textstat_frequency(data, ...)
        tstat_freq_n_20 <- utils::head(tstat_freq, n = n)
        top_frequency_word <- tstat_freq_n_20$feature

        return(extract_frequent_word)
    }


# Display text mining results from the structural topic model ----

#' @title Visualize a plot for highest word probabilities within each topic
#'
#' @name plot_topic_term
#'
#' @description
#' Visualize a plot for highest word probabilities within each topic.
#'
#' @param beta_td A tidy data frame that includes term (word)-topic probabilities
#'                (probabilities of each word per each topic).
#' @param top_n A number of top n terms frequently observed in each document.
#' @param topic_names (Labeled) topic names
#' @param ... Further arguments passed to \code{group_by}.
#'
#' @examples
#' \dontrun{
#' library(stm)
#' library(tidytext)
#'
#' # Apply structural topic model
#' # Convert quanteda objects to the stm package format
#' memory.limit(24000)
#' out <- convert(dfm, to = "stm")
#'
#' K_search <- searchK(out$documents,
#'                     out$vocab, K = c(5:10),
#'                     data = out$meta,
#'                     prevalence = ~ reference_type + s(year),
#'                     verbose = TRUE)
#'
#' # Run model with 15 topics
#' stm_15 <- stm(out$documents,
#'               out$vocab,
#'               data = out$meta,
#'               prevalence =  ~ reference_type + s(year),
#'               max.em.its = 75,
#'               init.type = 'Spectral',
#'               K = 15,
#'               verbose = FALSE)
#'
#' # Tidy the word-topic combinations
#' beta_td <- tidy(stm_15, document_names = rownames(dfm))
#'
#' beta_td %>% plot_topic_term(top_n = 10)
#' }
#'
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang := enquos
#' @importFrom tidytext scale_x_reordered reorder_within
#'
plot_topic_term <-
    function(beta_td, top_n,topic_names = NULL, ...) {

        topic_term_plot <- beta_td %>%
            group_by(topic, ...) %>%
            top_n(top_n, beta) %>%
            ungroup() %>%
            mutate(
                ord = factor(topic, levels = c(min(topic): max(topic))),
                tt = as.numeric(topic),
                topic = paste("Topic", topic),
                term = reorder_within(term, beta, topic)) %>%
            arrange(ord)
        levelt = paste("Topic", topic_term_plot$ord) %>% unique()
        topic_term_plot$topic = factor(topic_term_plot$topic,
                                       levels = levelt)
        if(!is.null(topic_names)){
            topic_term_plot$topic = topic_names[topic_term_plot$tt]
            topic_term_plot <- topic_term_plot %>%
                mutate(topic = as.character(topic)) %>%
                mutate(topic = ifelse(!is.na(topic), topic, paste('Topic',tt)))
            topic_term_plot$topic =
                factor(topic_term_plot$topic, levels = topic_term_plot$topic %>% unique())
        }
        topic_term_plot$tt = NULL
        topic_term_plot <- topic_term_plot %>%
            ggplot(aes(term, beta, fill = topic)) +
            geom_col(show.legend = FALSE, alpha = 0.8) +
            facet_wrap(~ topic, scales = "free", ncol = 3) +
            scale_x_reordered() +
            scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 3)) +
            coord_flip() +
            xlab("") +
            ylab("Word probability") +
            theme_minimal(base_size = 14) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "#3B3B3B", size = 0.3),
                axis.ticks = element_line(color = "#3B3B3B", size = 0.3),
                strip.text.x = element_text(size = 14, color = "#3B3B3B"), 
                axis.text.x = element_text(size = 14, color = "#3B3B3B"),
                axis.text.y = element_text(size = 14, color = "#3B3B3B"),
                axis.title = element_text(size = 14, color = "#3B3B3B"),
                axis.title.x = element_text(margin = margin(t = 7)),
                axis.title.y = element_text(margin = margin(r = 7)))
        
        return(topic_term_plot)
    }


#' @title Examine highest word probabilities for each topic
#'
#' @name examine_top_terms
#'
#' @description
#' Examine highest document-topic probabilities.
#'
#' @param beta_td A tidy data frame that includes term-topic probabilities
#'                (probabilities of each word per each topic).
#' @param top_n A number of top n terms with highest term-topic probabilities in each document.
#' @param ... Further arguments passed to \code{group_by}.
#'
#' @examples
#' \dontrun{
#' library(tidytext)
#'
#' # Tidy the word-topic combinations
#' beta_td <- tidy(stm_15, document_names = rownames(dfm))
#' beta_td %>% examine_top_terms(top_n = 5)
#' }
#'
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang := enquos
#' @importFrom tidyr unnest
#'
examine_top_terms <-
    function(beta_td, top_n, ...) {
        top_terms <- beta_td %>%
            arrange(beta) %>%
            group_by(topic, ...) %>%
            top_n(top_n, beta) %>%
            arrange(beta) %>%
            select(topic, term) %>%
            summarise(terms = list(term)) %>%
            mutate(terms = purrr::map(terms, paste, collapse = ", ")) %>%
            unnest(cols = c(terms))
        return(top_terms)
    }


#' @title Examine highest document-topic probabilities
#'
#' @name examine_gamma_terms
#
#' @description
#' Examine highest document-topic probabilities.
#'
#' @param gamma_td A tidy data frame that includes document-topic probabilities.
#'                 (probabilities of each topic per each document).
#' @param top_terms A tidy data frame that contains top terms with highest
#'                  document-topic probabilities within each topic.
#' @param ... Further arguments passed to \code{group_by}.
#'
#' @examples
#' \dontrun{
#' library(tidytext)
#'
#' # Tidy the document-topic combinations
#' gamma_td <- tidy(stm_15, matrix = "gamma",
#'                 document_names = rownames(dfm))
#' gamma_td %>% examine_gamma_terms(top_terms = top_terms_selected)
#' }
#'
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang := enquos
#'
examine_gamma_terms <-
    function(gamma_td, top_terms, ...) {
        gamma_terms <- gamma_td %>%
            group_by(topic, ...) %>%
            summarise(gamma = mean(gamma)) %>%
            left_join(top_terms, by = "topic")
        return(gamma_terms)

    }


#' @title Visualize a plot for document-topic probabilities
#'
#' @name plot_topic_probability
#'
#' @description
#' Visualize a plot for document-topic probabilities.
#'
#' @param gamma_terms A tidy data frame that includes document-topic probabilities.
#'                 (probabilities of each topic per each document).
#' @param top_n A number of top n terms with highest document-topic probabilities.
#' @param topic_names Topic names
#'
#' @examples
#' \dontrun{
#' library(tidytext)
#'
#' # Tidy the document-topic combinations
#' top_terms_selected <- beta_td %>% examine_top_terms(top_n = 5)
#' gamma_td <- tidy(stm_15, matrix = "gamma",
#'                 document_names = rownames(dfm))
#' gamma_terms <- gamma_td %>% examine_gamma_terms(top_terms = top_terms_selected)
#' gamma_terms %>% plot_topic_probability(top_n = 15)
#' }
#'
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#'
plot_topic_probability <-
    function(gamma_terms, top_n, topic_names = NULL) {
        topic_by_prevalence_plot <- gamma_terms %>%
            top_n(top_n, gamma) %>%
            mutate(tt = as.numeric(topic)) %>%
            mutate(ord = topic) %>%
            mutate(topic = paste('Topic',topic)) %>%  arrange(ord)
        levelt = paste("Topic", topic_by_prevalence_plot$ord) %>% unique()
        topic_by_prevalence_plot$topic = factor(topic_by_prevalence_plot$topic,
                                                levels = levelt)
        if(!is.null(topic_names)){
            reft  = 1:length(topic_by_prevalence_plot$tt)
            topic_by_prevalence_plot$topic =
                topic_names[reft]
            topic_by_prevalence_plot <- topic_by_prevalence_plot %>%
                mutate(topic = as.character(topic)) %>%
                mutate(topic = ifelse(!is.na(topic), topic, paste('Topic',tt)))
            topic_by_prevalence_plot$topic =
                factor(topic_by_prevalence_plot$topic, levels = topic_by_prevalence_plot$topic)
        }
        topic_by_prevalence_plot <- topic_by_prevalence_plot %>%
            ggplot(aes(topic, gamma, label = terms, fill = topic)) +
            geom_col(show.legend = FALSE, alpha = 0.8) +
            coord_flip() +
            scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 2)) +
            xlab("") +
            ylab("Topic proportion") +
            theme_minimal(base_size = 11) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "#3B3B3B", size = 0.3),
                axis.ticks = element_line(color = "#3B3B3B", size = 0.3),
                strip.text.x = element_text(size = 11, color = "#3B3B3B"), 
                axis.text.x = element_text(size = 11, color = "#3B3B3B"),
                axis.text.y = element_text(size = 11, color = "#3B3B3B"),
                axis.title = element_text(size = 11, color = "#3B3B3B"),
                axis.title.x = element_text(margin = margin(t = 9)),
                axis.title.y = element_text(margin = margin(r = 9)))
        
        return(topic_by_prevalence_plot)
    }


#' @title Visualize a table for document-topic probabilities
#'
#' @name plot_topic_probability_table
#'
#' @description
#' Visualize a table for document-topic probabilities.
#'
#' @param gamma_terms A tidy data frame that includes document-topic probabilities.
#'                 (probabilities of each topic per each document).
#' @param top_n A number of top n terms with highest document-topic probabilities.
#' @param topic_names Topic names
#'
#' @examples
#' \dontrun{
#' library(tidytext)
#'
#' # Tidy the document-topic combinations
#' gamma_td <- tidy(stm_15, matrix = "gamma",
#'                 document_names = rownames(dfm))
#' gamma_terms <- gamma_td %>% examine_gamma_terms(top_terms = top_terms_selected)
#' gamma_terms %>% plot_topic_probability_table(top_n = 15)
#' }
#'
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#'
plot_topic_probability_table <-
    function(gamma_terms, top_n, topic_names = NULL) {

        topic_by_prevalence_table <- gamma_terms %>%
            top_n(top_n, gamma) %>%
            mutate(tt = as.numeric(topic)) %>%
            mutate(ord = topic) %>%
            mutate(topic = paste('Topic',topic)) %>%  arrange(ord)
        levelt = paste("Topic", topic_by_prevalence_table$ord) %>% unique()
        topic_by_prevalence_table$topic = factor(topic_by_prevalence_table$topic,
                                                 levels = levelt)
        if(!is.null(topic_names)){
            reft  = 1:length(topic_by_prevalence_table$tt)
            topic_by_prevalence_table$topic =
                topic_names[reft]
            topic_by_prevalence_table <- topic_by_prevalence_table %>%
                mutate(topic = as.character(topic)) %>%
                mutate(topic = ifelse(!is.na(topic), topic, paste('Topic',tt)))
            topic_by_prevalence_table$topic =
                factor(topic_by_prevalence_table$topic)
        }
        topic_by_prevalence_table <- topic_by_prevalence_table %>%
            select(topic, gamma, terms)
        return(topic_by_prevalence_table)
    }










