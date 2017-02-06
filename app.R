#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#-------------------------------------------------------
# things we want globally

packages <- c("plyr", "dplyr", "purrr", "ggplot2", "tidytext", "wordcloud", "viridis", "DT")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(plyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(viridis)
library(DT)

#-------------------------------------------------------
# read in data


scopus_data <-  readr::read_csv("scopus_all_archaeology.csv")
year_range <- range(scopus_data$Year) 

#-------------------------------------------------------


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Explore the titles of archaeology articles from Scopus"),
   withTags({
     div(class = "paragraph", checked=NA,
         p(paste0("The data are ", prettyNum(nrow(scopus_data), 
                                             big.mark = ","),
                  " articles obtained from Scopus by searching for 'archaeology' in the Social Science subject area on 1 Feb 2017. All code and data are available at ")),
         a(href = "https://github.com/benmarwick/scopusarchaeology", 
           "https://github.com/benmarwick/scopusarchaeology"))}),
   
  
      
      fluidRow(
        # slider to adjust the time period that we query
        column(3, sliderInput("year_slider", 
                    label = h3("Adjust the year range"), 
                    min = year_range[1], 
                    max = year_range[2], 
                    value = c(1990, year_range[2]),
                    sep = "")),
        # text box to enter the words to investigate
        column(3, textInput(inputId = "the_term",
                  label = h3("Start typing a term to explore"), 
                  value = " 3D ")),
        column(3, helpText("Note: The search term will partially matched with words",
                 "in the titles. For example, the term 'GIS' will be matched with ",
                 "agistment. If you want to match on whole words only, you need",
                 "to put a space at the start and end of your search term,",
                 "like this: ' GIS '. Quotation marks are not necessary ",
                 "and makes no difference if you use upper or lower case.",
                 " It may take a few seconds for your search term to be ",
                 "processed, please by patient. Contact benmarwick@gmail.com",
                 " with any questions, etc.")),
        verbatimTextOutput("the_term")
       
      
      ),
      
      mainPanel(
        h3("Number of articles per year containing the search term"),
        plotOutput('my_plot'),
        h3("Citations per year for articles containing the search term"),
        plotOutput('my_plot_citations'),
        h3("Word cloud of highly cited articles (titles only) containing the search term"),
        plotOutput('my_plot_wordcloud'),
        #plotOutput('my_plot_pop_words'),
        #plotOutput('my_plot_authors'),
        h3("Table of articles containing the search term in the year range"),
        p("The search box searches all fields"),
        DT::dataTableOutput('scopus_all_archaeology_year_range_term'),
        h3("Table of all articles in the year range"),
        p("The search box searches all fields"),
        DT::dataTableOutput('scopus_all_archaeology_year_range_table')
      )
      
    
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  

  
  #-------------------------------------------------------
  # work with some inputs from the UI
  
  # filter by year range, get the year values from the slider
  scopus_all_archaeology_year_range <- 
   reactive({
   scopus_data[scopus_data$Year %in% c(round(input$year_slider[1],0):round(input$year_slider[2],0)), ]
   })
  
  # show table in the UI
  output$scopus_all_archaeology_year_range_table <- renderDataTable({
    scopus_all_archaeology_year_range()
  })
  
  # make year breaks
  lab_brk <- reactive({ round(sort(unique(scopus_all_archaeology_year_range()$Year, 0))) })
  
  # so we can send the value back to the UI
  output$lab_brk <-   reactive({
    lab_brk()
  })
  
  
  # filter by term, get the term from the text box in the UI
  scopus_all_archaeology_year_range_term <- 
    reactive({
      # get the data that has been filtered by year
     # the_term_lower <- tolower(input$the_term)
      scopus_all_archaeology_year_range <- scopus_all_archaeology_year_range()
      # filter by term
      scopus_all_archaeology_year_range[grepl(tolower(input$the_term), 
                                              tolower(scopus_all_archaeology_year_range$Title)),
                                        ]
    })
  
  # show table in the UI
  output$scopus_all_archaeology_year_range_term <- renderDataTable({
    scopus_all_archaeology_year_range_term()
  })
  

  #-------------------------------------------------------
  # make some tables globally available for the plots
  
  # count the number of articles for each year
  my_data <- reactive({ 
    scopus_all_archaeology_year_range() %>% 
    group_by(Year) %>% 
    dplyr::summarise(articles_per_year = n()) 
  })
  
  # count the number of articles for each year for only rows that contain the term
  my_data_term <- reactive({ 
    scopus_all_archaeology_year_range_term() %>% 
    group_by(Year) %>% 
    dplyr::summarise(articles_per_year = n()) 
  })
  
  my_data_citation <- reactive({ 
    scopus_all_archaeology_year_range() %>% 
    filter(`Cited by` >= 1) %>% 
    mutate(group = "all")
  })
  
  # count the number of articles for each year for only rows that contain the term
  my_data_citation_term <- reactive({ 
    scopus_all_archaeology_year_range_term() %>% 
    filter(`Cited by` >= 1) %>% 
    mutate(group = "term")
  })
  
  # combine both
  cites_per_year_all <- reactive({
    rbind(my_data_citation(), 
          my_data_citation_term())
  })
  
  # get the top cited articles containing the term
  my_data_citation_term_top_cited <- reactive({ 
    scopus_all_archaeology_year_range_term() %>% 
      filter(`Cited by` >= 5) 
  })
  
  #-------------------------------------------------------
  # make some plots
  
  
  # plot article count in year range

  
  output$my_plot <- 
    renderPlot({
   
  
   
  # plot the number of articles per year with the search term
  # over the total number of articles per year

      
      my_data <- my_data()
      my_data_term <- my_data_term()
 
  my_plot <-    
    ggplot() +
    geom_col(data = my_data,
             aes(Year,
                 articles_per_year),
             fill = "grey80") +
    geom_col(data = my_data_term,
             aes(Year,
                 articles_per_year),
             fill = "black") +
    # add the number of articles as text at the top of the bars
    geom_text(data = my_data,
              aes(Year,
                  articles_per_year - (articles_per_year * 0.2),
                  label = articles_per_year),
              size = 3) +
    geom_text(data = my_data_term,
              aes(Year,
                  articles_per_year - (articles_per_year * 0.2),
                  label = articles_per_year),
              size = 4,
              colour = "white") +
    theme_bw() +
    scale_x_continuous(labels = lab_brk(), 
                       breaks = lab_brk() ) +
    scale_y_log10() +
    theme(axis.text.x = element_text( angle = 90,
                                      vjust = 0.5,
                                      hjust = 0)) +
    # make a title for the plot
    ggtitle(paste0('All SOCI articles on Scopus with "',
                   input$the_term,
                   '" in the title during ',
                   input$year_slider[1], 
                   '-',
                   input$year_slider[2],
                   ' (n = ', prettyNum(sum(my_data_term$articles_per_year), 
                                       big.mark = ","), 
                   " out of ",
                   prettyNum(sum(my_data$articles_per_year), 
                             big.mark = ","),
                   " articles in total)")) 
    
    print(my_plot)
    })
  
# citation rates for all articles vs articles with the search term 
  
output$my_plot_citations <- 
      
      renderPlot({
        my_data <- my_data()
        my_data_term <- my_data_term()
        cites_per_year_all <- cites_per_year_all()
        
        # count the number of articles for each year
      
        
        cites_per_year_all$group <- as.character(cites_per_year_all$group)
        
        # plot the number of articles per year with the search term
        # over the total number of articles per year
        
        my_plot <- 
          ggplot(cites_per_year_all, 
                 aes(as.character(Year),
                     `Cited by`,
                     colour = group
                     )) +
          geom_boxplot() +
          theme_bw() +
          scale_colour_manual(values = c("grey80",
                                         "black")) +
          scale_fill_manual(values = c(alpha("grey80", 0.000001),
                                         "black")) +
          scale_x_discrete(labels = lab_brk(), 
                             breaks = lab_brk() ) +
          scale_y_log10() +
          theme(axis.text.x = element_text( angle = 90,
                                            vjust = 0.5,
                                            hjust = 0)) +
          # suppress the legend
          guides(colour = "none") +
          # make a title for the plot
          # make a title for the plot
          ggtitle(paste0('All SOCI articles on Scopus (with at least one citation) containing "',
                         input$the_term,
                         '" in the title \nduring ',
                         input$year_slider[1], 
                         '-',
                         input$year_slider[2],
                         ' (n = ', prettyNum(sum(my_data_term$articles_per_year), 
                                             big.mark = ","), 
                         " out of ",
                         prettyNum(sum(my_data$articles_per_year), 
                                   big.mark = ","),
                         " articles in total)")) 
         
        
        
        print(my_plot)
      })
    
# word cloud of frequent words in titles


output$my_plot_wordcloud <- 
  
  renderPlot({

    

    my_data_citation_term_top_cited <- my_data_citation_term_top_cited()
    
    my_data_term_top_cites <- 
      my_data_citation_term_top_cited %>% 
      filter(`Cited by` >= 10) 

    my_data_term_top_cites %>% 
  unnest_tokens(word, Title)  %>%
  anti_join(bind_rows(stop_words, 
                      data_frame(word = c(
                        "elsevier", 
                        "2006",
                        "2008",
                        "2009",
                        "2010",
                        "2011",
                        "2012",
                        "sas",
                        "va",
                        "als",
                        "tls"),
                        lexicon = "0")))  %>%
      dplyr::count(word, sort = TRUE) %>% 
  with(wordcloud( paste0(word, " (", n, ")"),
                  n, 
                  #max.words = 20,
                  min.freq = 2,
                  random.order = FALSE,
                  random.color = FALSE,
                  rot.per= 0, 
                  scale = c(8, 0.3),
                  colors = plasma(10)))
  })

# what are the most popular words each decade

output$my_plot_pop_words <- 
  # This isn't very interesting, it needs to show the words that are most different from the other time periods. How to do that?
  
  renderPlot({
    
    scopus_all_archaeology_year_range <- scopus_all_archaeology_year_range()
    
    # top words per n years
    n <- 5
    scopus_all_archaeology_top_words_per_decade <- 
      scopus_all_archaeology_year_range %>% 
      dplyr::mutate(decade = round_any(Year, n, floor)) %>% 
      dplyr::select(Title, Year, decade) %>% 
      unnest_tokens(word, Title) %>% 
      dplyr::anti_join(bind_rows(stop_words, 
                          data_frame(word = c(
                            "elsevier", 
                            "archaeology",
                            "archaeological",
                            "archeology",
                            "century",
                            "de",
                            "la",
                            "en",
                            "el",
                            "del",
                            "der",
                            "und",
                            "late",
                            "analysis",
                            "age",
                            "study",
                            "site",
                            "history",
                            "ancient",
                            "2006",
                            "2008",
                            "2009",
                            "2010",
                            "2011",
                            "2012",
                            "sas",
                            "va",
                            "als",
                            "tls"),
                            lexicon = "0"))) %>% 
      dplyr::group_by(decade) %>% 
      dplyr::count(word, sort = TRUE) %>% 
      dplyr::arrange(desc(n)) %>% 
      dplyr::slice(1:10) %>% 
      dplyr::mutate(ranks = rank(n, ties.method = "random")) %>% 
      dplyr::mutate(rev_rank = 11 - ranks) %>% 
      dplyr::mutate(word = gsub("\\d", "", word)) %>% 
      dplyr::mutate(sizes = as.numeric(scale(ranks, 0, 5) ))
    
    my_plot <- 
    ggplot(scopus_all_archaeology_top_words_per_decade,
           aes(decade, 
               rev_rank,
               label = word
           )) +
      geom_text(size = scopus_all_archaeology_top_words_per_decade$sizes + 2,
                position = position_jitter(width = 0.1, 
                                           height = 0.1)) +
      theme_bw() +
      scale_y_reverse()
    print(my_plot)
    
  })


# numbers of authors per paper

output$my_plot_authors <- 

  renderPlot({
    
    scopus_all_archaeology_year_range <- scopus_all_archaeology_year_range()
    
    names(scopus_all_archaeology_year_range) <- make.names(names(scopus_all_archaeology_year_range))
    authors <- gsub("[A-z].,| [A-Z]\\.", "", scopus_all_archaeology_year_range$X.U.FEFF.Authors)
    authors_per_paper <- map_int(authors, ~length(unlist(strsplit(.x, " "))))
    authors_per_paper_df <- data_frame(authors_per_paper = authors_per_paper)
    
    my_plot <- 
    ggplot(authors_per_paper_df, 
           aes(authors_per_paper)) +
      geom_histogram() +
      geom_vline(xintercept = 3,
                 colour = "red",
                 size = 2)  +
      xlim(0,30) +
      theme_bw() 
    # +
    #   ggtitle(paste0('Number of authors per paper for  "',
    #                  input$the_term,
    #                  '" in TITLE-ABS-KEY \nduring ',
    #                  input$year_slider[1], 
    #                  '-',
    #                  input$year_slider[2],
    #                  ' (n = ', prettyNum(sum(my_data_term$articles_per_year), 
    #                                      big.mark = ","), 
    #                  " out of ",
    #                  prettyNum(sum(my_data$articles_per_year), 
    #                            big.mark = ","),
    #                  " articles in total)")) 
    # 
        
    print(my_plot)
    
  })

    






  
   
}

# Run the application 
shinyApp(ui = ui, server = server)
# runApp( display.mode = "showcase") # with code visible

