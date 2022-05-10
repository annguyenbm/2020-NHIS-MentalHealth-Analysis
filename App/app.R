# Visualizing and Testing: 2020 NHIS; DATA 613 Final Project
# Ben Stano, Binh Minh An Nguyen, and Andrew Erickson
# Presentation Date: 5/2/22

# Packages used in app: 
library(shiny)
library(bslib)
library(tidyverse)
library(ggmosaic)
library(DT)
library(broom)
library(forcats)
library(gridExtra)
library(tree)
library(reshape2)
library(randomForest)

options(width = 90)

# Load in rds files from the data folder: ################
adult <- read_rds("../data/adult_main.rds")

adult_meta <- read_rds("../data/adult_meta.rds")

tab7_dictionary <- read_rds("../data/tab7_dictionary.rds")


######### Tab 2 Specific Business Logic: ####################################

# Vector of Months for Tab 2 sidebar: 
month <- c("January", "February", "March", "April", "May", "June", "July", 
           "August", "September", "October", "November", "December")

# Relevel month factors so they are shown in order in graph:  
adult <- adult %>% 
  mutate(interview_month = fct_relevel(interview_month, month))

# Add region vector for Tab 2 sidebar: 
region <- c("South", "West", "Northeast", "Midwest")

# Adjust metadata columns for tab2 by adding interview_month and region: 
tab2_metadata <- bind_cols(adult_meta, adult) %>%
  select(ends_with("_meta"), interview_month, region) %>%
  rename(real_month = interview_month, real_region = region)

# Remove "meta" from meta df for tab 2 so that var input works for both plots:
tab2_metadata <- rename_with(tab2_metadata, ~ str_replace_all(., "_meta", ""))

# Create functions for tab 2 to make main and meta reactives simpler: 
# Main filtering:
filter_rows <- function(month, location) {
  adult %>%
    filter(interview_month %in% month,
           region %in% location)
}

# Meta filtering: 
filter_metadata <- function(month, location) {
  tab2_metadata %>%
    filter(real_month %in% month,
           real_region %in% location)
}

########### Tab 4 and 5 Business Logic ########################################

# Transform `adult` dataset
adult_tab4 <- adult %>% 
  mutate(anxiety_freq = ifelse(anxiety_freq == "Severe", 1, 0),
         depression_freq = ifelse(depression_freq == "Severe", 1, 0))

# Transform `adult` for tab 5
adult_tab5 <- adult %>% 
  mutate(anxiety_freq = ifelse(anxiety_freq == "Severe", 1, 0),
         depression_freq = ifelse(depression_freq == "Severe", 1, 0))

adult_predictor <- adult %>%
  select(c(-anxiety_freq, -depression_freq))

############ UI code below: ##########################################

ui <- navbarPage("2020 NHIS Analysis",
                  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
                 
########## Tab 1: Intro UI Code: ####################################
                 
                 tabPanel("Introduction",
                          tags$h1("2020 National Health Interview Survey: Data Analysis and Testing"),
                          tags$h3("Using select NHIS data to build a basic picture of severe anxiety and depression among survey subjects"),
                          tags$hr(),
                          tags$p("Since 1957, the Centers for Disease Control and Prevention's National Center for Health Statistics has conducted the comprehensive National Health Interview Survey (NHIS), interviewing individuals and households to help understand health and habits in the United States. Over more than six decades, its publicly available survey results have also allowed researchers and health care institutions to do the same. The data that users will visualize, test, and manipulate in this app come from the 2020 NHIS, the results for which were published in fall 2021. This app uses select health and demographic attributes of all 31568 US adults who participated in the 2020 NHIS.", style = "font-size:16px;"),
                          tags$a(href = "https://www.cdc.gov/nchs/nhis/2020nhis.htm", "The full survey can be accessed here.", style = "font-size:16px;"),
                          tags$br(),
                          tags$br(),
                          tags$p("The 2020 NHIS features more than 600 variables, including both collected demographics and responses to questions asked during an in-person or phone interview. With efficiency, specificity, and usability in mind, this app features just 45. During variable selection, a specific emphasis was placed on several themes, the research justification for which is explained in greater detail in the app vignette. These themes include: demographics of adult subjects; general health habits; health insurance, health care access, and health care affordability; physical exercise; and food security. And, of course, anxiety, depression, and mental health, the slate of questions for which", tags$a(href = "https://www.youtube.com/watch?v=GvxOdRlhoXY", "was expanded in the 2019 survey."), style = "font-size:16px;"),
                          tags$br(), 
                          tags$p("This app features a data set that has been both filtered from the original NHIS data set down to the columns of interest and recoded in many cases to make number-coded factor variables more interpretable and easier to understand to the user. Additionally, variables have been reordered for more appropriate theme grouping. More complete descriptions of original variable names, factor levels, and transformations from the original data set appear in the app data dictionary tab", tags$a(href = "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/NHIS/2020/adult-codebook.pdf", "and more information about each original variable can be found in the NHIS 2020 adult codebook."), style = "font-size:16px;"),
                          tags$hr(),
                          tags$h3("A Word of Caution"),
                          tags$p("This app, which allows users to visualize the distributions of 2020 NHIS responses, test for dependence between response levels, build logistic regression and classification tree models, and build random forest models, is in no way intended to serve as a diagnosis of severe anxiety or depression. It is merely a guide to help identify characteristics that tend to be associated with anxiety and depression so that health care professionals may remain vigilant, as well as to help interested and statistics-educated individuals better understand a portion of the 2020 NHIS. Model-building should come with a healthy dose of skepticism. Small models tend to be biased, and the NHIS is imperfect. LGBTQ individuals, for example, are underrepresented in this survey. White subjects, on the other hand, are overrepresented. And the survey only collects two factor levels--male and female--for sex. Incomplete or biased subsets or variables should be taken into account in any analysis or insight stemming from this app. Finally, the emergence of the COVID-19 pandemic presented challenges for data collection in 2020 that might have impacted results.", tags$a(href = "https://www.cdc.gov/nchs/nhis/2020nhisdata.htm", "Learn about a few of those difficulties here."),
                                 style = "font-size:16px")
                 ),
                 
################### Tab 2: EDA UI Code: #######################################
                 
                 tabPanel("EDA",
                          tags$h2("Visualizing Distribution of 2020 NHIS Variables"),
                          sidebarLayout(
                            sidebarPanel(
                              varSelectInput("tab2_var", "Select a variable",
                                             selected = "anxiety_freq",
                                             data = adult),
                              checkboxInput("tab2_metadata", "Show metadata?", 
                                            value = FALSE),
                              tags$hr(),
                              checkboxGroupInput("tab2_month", 
                                                 "Select interview months",
                                                 choices = month,
                                                 selected = month),
                              checkboxGroupInput("tab2_region", 
                                                 "Select interview regions",
                                                 choices = region,
                                                 selected = region),
                              tags$hr(),
                              checkboxInput("tab2_flip", 
                                            "Flip Coordinates on Factors?", 
                                            value = FALSE)
                            ),
                            mainPanel(
                              plotOutput("tab2_plot"),
                              tags$br(),
                              tags$hr(),
                              plotOutput("tab2_metaplot")
                            )
                          )
                 ),
                 
########## Tab 3: Chi square UI Code: ######################################
                 
                 tabPanel("Chi Square",
                      tags$h2("Chi Squared, ANOVA, and Correlation Analysis"),
                          sidebarLayout(
                            sidebarPanel(
                              varSelectInput("tab3_var1", 
                                             "Select the first variable",
                                             selected = "anxiety_freq",
                                             data = adult),
                              varSelectInput("tab3_var2", 
                                             "Select the second variable",
                                             selected = "income_group",
                                             data = adult),
                              tags$hr(),
                              checkboxGroupInput("tab3_month", 
                                                 "Select interview months",
                                                 choices = month,
                                                 selected = month),
                              checkboxGroupInput("tab3_region", 
                                                 "Select interview regions",
                                                 choices = region,
                                                 selected = region)
                            ),
                            mainPanel(
                              tags$hr("Graphical Output"),
                              plotOutput("tab3_plot"),
                              tags$hr("Statistical Output"),
                              verbatimTextOutput("tab3_print")
                            )
                          )),
                 
                 
  ########### Tab 4: Logistic Regression UI Code: ############################
                 
                 tabPanel("Logistic Regression",
                          tags$h2("Logistic Regression: Severe Depression and Anxiety Modeling with 2020 NHIS Variables"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("tab4_varY", 
                                           "Select an Outcome Variable",
                                           choices = names(adult_tab4 %>% select(anxiety_freq, depression_freq))),
                              selectInput("tab4_varX", 
                                          "Select Predictor Variable(s)",
                                          multiple = TRUE,
                                          choices = names(adult_predictor)),
                            sliderInput("tab4_split", 
                                        "Select proportion for Train dataset", 
                                        value = 0.7, 
                                        min = 0.0, 
                                        max = 1.0, 
                                        step = 0.1),
                              textOutput("tab4_ntrain"),
                              textOutput("tab4_ntest"),
                              tags$br(),
                              checkboxInput("tab4_pred", 
                                            "Check prediction outcome", 
                                            value = FALSE)
                            ),
                            mainPanel(
                              verbatimTextOutput("glmout_tab4"),
                              tags$br(),
                              tags$hr(),
                              plotOutput("matrix_tab4"),
                              tags$hr(),
                              tableOutput("rate_tab4")
                            )
                          )
                 ),
                 
######## Tab 5: Classification Trees UI Code: #################################
                 
                 tabPanel("Classification Trees",
                          tags$h2("Classification Trees: Severe Depression and Anxiety Modeling with 2020 NHIS Variables"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("tab5_varY", 
                                           "Select an Outcome Variable",
                                           choices = names(adult_tab5 %>% select(anxiety_freq, depression_freq))),
                              selectInput("tab5_varX", 
                                          "Select Predictor Variable(s)",
                                          multiple = TRUE,
                                          choices = names(adult_predictor)),
                              sliderInput("tab5_split", 
                                        "Select proportion for Train dataset", 
                                        value = 0.7, 
                                        min = 0.0, 
                                        max = 1.0, 
                                        step = 0.1),
                              sliderInput("tab5_mindev", "Select mindev", 
                                          value = 0.05, min = 0.005, 
                                          max = 0.1, step = 0.005)
                              
                            ),
                            mainPanel(
                              plotOutput("tab5_plot"),
                              tags$br(),
                              tags$hr(),
                              tableOutput("tab5_matrix"),
                              tags$hr(),
                              tableOutput("tab5_rate")
                              
                            )
                          )),
                 
                 
########## Tab 6: Variable Importance UI Code: ################################
                 
                 tabPanel("Random Forest",
                          tags$h2("Random Forest: Severe Depression and Anxiety Modeling with 2020 NHIS Variables"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("tab6_varY", 
                                           "Select an Outcome Variable",
                                           choices = names(adult_tab5 %>% select(anxiety_freq, depression_freq))),
                              selectInput("tab6_varX", 
                                          "Select Up to 10 Predictor Variable(s)",
                                          multiple = TRUE,
                                          choices = names(adult_predictor)),
                              sliderInput("tab6_split", 
                                          "Select proportion for Train dataset", 
                                          value = 0.7, 
                                          min = 0.0, 
                                          max = 1.0, 
                                          step = 0.1),
                              sliderInput("tab6_tree", "Select number of trees", 
                                          value = 200, min = 50, 
                                          max = 300, step = 1),
                              sliderInput("tab6_nvar", "Select predictors per tree",
                                          value = 1, min = 1,
                                          max = 10, step = 1)
                              
                            ),
                            mainPanel(
                              plotOutput("tab6_plot"),
                              plotOutput("tab6_vi_plot"),
                              tags$hr(),
                              tableOutput("tab6_matrix"),
                              tableOutput("tab6_rate")
                            )
                          )),
                 
########## Tab 7: Data Dictionary UI Code: #################
                 
                 tabPanel("Data Dictionary",
                          tags$h2("NHIS Application Data Dictionary"),
                          tags$h4("Learn more about select 2020 NHIS variables used in this app and transformations made from the raw survey data"),
                          checkboxInput("tab7_factoronly", 
                                        "Show Factor Variables Only?"),
                          DT::dataTableOutput("tab7_table"))
                 
)

####### SERVER CODE GOES HERE#################################################

server <- function(input, output, session) {
  
#### Tab 1: Intro Server Code: ############################################
  
  # None; please see UI code for this tab. This tab relies entirely on text and html tags. 
  
  
####### Tab 2: EDA Server Code: ###########################################
  
  # Use function from business logic and sidebar inputs to filter main data:
  filtered_df <- reactive({
    filter_rows(input$tab2_month, input$tab2_region)
  })
  
  output$tab2_plot <- renderPlot({
    # Two checks to ensure data isn't filtered down to 0 rows: 
    validate(
      need(input$tab2_month,
           "Please select at least 1 month!"), 
      need(input$tab2_region,
           "Please select at least 1 region!")
    )
    
    # Build the skeleton of the main plot: 
    main_plot <- ggplot(data = filtered_df(),
                        aes(!!input$tab2_var))
    
    # Histogram if numeric: 
    if (is.numeric(filtered_df()[[input$tab2_var]])) {
      main_plot <- main_plot +
        geom_histogram(bins = sqrt(length(filtered_df()[[input$tab2_var]])))
      # Bar plot if factor (true of most variables), and coord_flip if selected
    } else {
      if (input$tab2_flip) {
        main_plot <- main_plot +
          geom_bar() +
          coord_flip()
      } else {
        main_plot <- main_plot +
          geom_bar()
      }
    }
    # Display the plot, adding dynamic title, subtitle based on var & row count:
    main_plot + 
      ggtitle(paste(
        "Distribution of", input$tab2_var, "in 2020 Adult NHIS Results"),
        subtitle = str_glue("N = {nrow(filtered_df())}")) +
      theme(plot.title = element_text(size = 20, face = "bold"),
            plot.subtitle = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14)) + 
      geom_text(aes(label = ..count..), stat = "count", vjust = -0.3)
  })
  
  # Beneath main plot will be meta plot; data also filtered by month & region:
  filtered_meta_df <- reactive({
    filter_metadata(input$tab2_month, input$tab2_region)
  })
  
  output$tab2_metaplot <- renderPlot ({
    # Since these will all be bar plots, make one complete spine for meta plot:
    ggplot(data = filtered_meta_df(),
           aes(!!input$tab2_var)) + 
      geom_bar() +
      ggtitle(paste(
        "Completeness of Answers for", input$tab2_var, 
        "in 2020 Adult NHIS Results"),
        subtitle = str_glue("N = {nrow(filtered_df())}")) +
      theme(plot.title = element_text(size = 20, face = "bold"),
            plot.subtitle = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14)) +
      geom_text(aes(label = ..count..), stat = "count", vjust = -0.3) ->
      meta_plot
    
    # Only display the plot if tab2_metadata is selected: 
    if(req(input$tab2_metadata)) {
      meta_plot
    } else {
      
    }
  })
  
###### Tab 3: Chi square Server Code: ######################################
  
  ## Filter DF for Months Tab 3
  filtered_df3 <- reactive({
    filter_rows(input$tab3_month, input$tab3_region)
  })
  
  # Determine Comparison Type
  tab3_type <- reactive({
    var1 <- filtered_df3()[[input$tab3_var1]]
    var2 <- filtered_df3()[[input$tab3_var2]]
    # Type 1 = Quantitative Var1 and Quantitative Var2
    if (is.numeric(var1) && is.numeric(var2)) {
      type <- 1
      # Type 2 = Categorical Var1 and Quantitative Var2
    } else if (!is.numeric(var1) && is.numeric(var2)) {
      type <- 2
      # Type 3 = Quantitative Var1 and Categorical Var2
    } else if (is.numeric(var1) && !is.numeric(var2)) {
      type <- 3
      # Type 4 = Categorical Var1 and Categorical Var2
    } else if (!is.numeric(var1) && !is.numeric(var2)) {
      type <- 4
    }
    return(type)
  })
  
  output$tab3_plot <- renderPlot({
    type <- tab3_type()
    
    p <- ggplot(data = filtered_df3())
    
    ## Scatterplot - (Quantitative and Quantitative)
    if (type == 1) {
      p <- p + geom_point(mapping = aes(x = !!input$tab3_var1,
                                        y = !!input$tab3_var2)) +
        ggtitle(paste("Comparison between", input$tab3_var1, "and", input$tab3_var2))
      ## Boxplots - (Quantitative and Categorical)
    } else if (type == 2) {
      p <- p + geom_boxplot(mapping = aes(x = !!input$tab3_var1,
                                          y = !!input$tab3_var2,
                                          fill = !!input$tab3_var1)) +
        ggtitle(paste("Comparison between", input$tab3_var1, "and", input$tab3_var2)) +
        theme(axis.text.x = element_text(angle = 90))
    } else if (type == 3) {
      p <- p + geom_boxplot(mapping = aes(x = !!input$tab3_var2,
                                          y = !!input$tab3_var1,
                                          fill = !!input$tab3_var2)) +
        ggtitle(paste("Comparison between", input$tab3_var1, "and", input$tab3_var2)) +
        theme(axis.text.x = element_text(angle = 90))
      
      ## Mosaic Plot - (Categorical and Categorical)
    } else if (type == 4) {
      p <- p + geom_mosaic(mapping = aes(x = product(!!input$tab3_var2, !!input$tab3_var1),
                                         fill = !!input$tab3_var2,
                                         offset = 0.02)) +
      ggtitle(
        paste("Comparison between", input$tab3_var1, "and", input$tab3_var2)) +
        labs(y = "") +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(angle = 90))
    }
    p
  })
  
  output$tab3_print <- renderPrint({
    type <- tab3_type()
    data <- filtered_df3()
    ## Correlation - (Quantitative and Quantitative)
    if (type == 1) {
      t <- cor(select(data, !!input$tab3_var1, !!input$tab3_var2), 
               use = "complete.obs")
      ## ANOVA - (Quantitative and Categorical)
    } else if (type == 2) {
      t <- summary(aov(data[[input$tab3_var2]] ~ data[[input$tab3_var1]]))
    } else if (type == 3) {
      t <- summary(aov(data[[input$tab3_var1]] ~ data[[input$tab3_var2]]))
      ## Chi Square - (Categorical and Categorical)
    } else if (type == 4) {
      t <- chisq.test(data[[input$tab3_var1]], data[[input$tab3_var2]],
                      correct = FALSE)
    }
    t
  })
  
####### Tab 4: Logistic Regression Server Code: ########################
  
  InputModel_tab4 <- reactive({
    if (is.null(input$tab4_varX)) {
      
    } else {
      data <- adult_tab4
    }
  })
  
  splitSlider <- reactive({input$tab4_split})
  
  # Split data into train and test set 
  set.seed(1)
  tr_row <- reactive({
    sample(1:nrow(InputModel_tab4()),
           splitSlider() * nrow(InputModel_tab4()))
  })
  
  train_dt <- reactive({
    train <- InputModel_tab4()
    train[tr_row(), ]
  })
  
  test_dt <- reactive({
    test <- InputModel_tab4()
    test[-tr_row(), ]
  })
  
  # Count the number of rows in the train and test sets
  output$tab4_ntrain <- renderText({
    validate(
      need(!is.null(InputModel_tab4()),
           "Train set: Please select at least 1 variable!")
    )
    paste("Train set:", nrow(train_dt()), "observations")
  })
  
  output$tab4_ntest <- renderText({
    validate(
      need(!is.null(InputModel_tab4()),
           "Test set: Please select at least 1 variable!")
    )
    paste("Test set:", nrow(test_dt()), "observations")
  })
  
  # Create Logistic Regression model
  f_tab4 <- reactive({
    as.formula(
      paste(input$tab4_varY, "~", str_c(input$tab4_varX, collapse = "+")))
  })
  
  glm_model_tab4 <- reactive({
    glm(f_tab4(),
        data = train_dt(),
        family = binomial(link = "logit"),
        na.action = na.exclude)
  })
  
  output$glmout_tab4 <- renderPrint({
    validate(
      need(input$tab4_varX, "Please select at least 1 predictor variable!")
    )
    R_sq_tab4 <- round(
      abs(glm_model_tab4()$deviance - glm_model_tab4()$null.deviance) * 100/glm_model_tab4()$null.deviance, 
      digits = 2)
    print(paste("dev.Rsq = ", R_sq_tab4, "%"))
    cat("\n")
    tidy(glm_model_tab4()) %>% 
      as.tibble() %>% 
      mutate(
        odds = round(exp(estimate), 2),
        probs = round(odds * 100/(1 + odds), 2),
      ) %>% 
      select(term, estimate, odds, probs, everything())
  })
  
  # Whole dataset validation
  probs_tab4 <- reactive({
    predict(glm_model_tab4(),
            InputModel_tab4(),
            type = "response")
  })
  
  pred_tab4 <- reactive({
    ifelse(probs_tab4() > 0.5, 1, 0)
  })
  
  cm_tab4 <- reactive({
    cm <- table(pred_tab4(),
                InputModel_tab4()[[input$tab4_varY]])
    colnames(cm) <- c("No", "Yes")
    rownames(cm) <- c("No", "Yes")
    cm %>% 
      reshape2::melt() %>% 
      rename("Prediction" = Var1, "Actual" = Var2, "Observations" = value) %>% 
      mutate_if(is.character, as.factor)
  })
  
  # Tab 4 - Confusion Matrix Heatmap
  output$matrix_tab4 <- renderPlot({
    if (input$tab4_pred) {
      validate(
        need(input$tab4_varX,
             "Please select at least 1 variable!")
      )
      cm_tab4() %>% 
        ggplot(aes(x = Actual, y = Prediction, fill = Observations)) +
        geom_tile(show.legend = NULL) +
        geom_text(aes(label = Observations)) + 
        scale_fill_gradient(low = "white", high = "#18d0f5") +
        labs(title = "Confusion Matrix",
             subtitle = "Default Threshold: 50%") +
        theme_bw() +
        theme(plot.title = element_text(face = "bold", size = 18))
    } else {
      
    }
  })
  
  # Tab 4 - Model Statistic Rates
  output$rate_tab4 <- renderTable({
    if (input$tab4_pred) {
      validate(
        need(input$tab4_varX,
             "Please select at least 1 variable!")
      )
      TruP <- cm_tab4()$Observations[4]
      TruN <- cm_tab4()$Observations[1]
      FalN <- cm_tab4()$Observations[3]
      FalP <- cm_tab4()$Observations[2]
      
      TotP <- TruP + FalN
      TotN <- TruN + FalP
      Tot <- TotP + TotN
      
      Accuracy <- (TruP + TruN)/Tot
      Error <- (FalP + FalN)/Tot
      Sensitivity <- TruP/TotP
      Specificity <- TruN/TotN
      FalPos <- 1 - Specificity
      
      rates <- tibble(Accuracy = Accuracy, 
                      Error = Error, 
                      Sensitivity = Sensitivity, 
                      Specificity = Specificity, 
                      FalPos = FalPos)

      round(rates, digits = 3)
    } else {
      print("Please select the prediction checkbox first!")
    }
  })
  
  
### Tab 5: Classification Trees Server Code: #############################
  InputDataset_tab5 <- reactive({adult_tab5})
  
  InputTree_tab5 <- reactive({
    if (is.null(input$tab5_varX)) {
      
    } else {
      data <- adult_tab5
    }
  })
  
  splitSlider_tab5 <- reactive({input$tab5_split})
  
  # Split data into train and test set 
  set.seed(1)
  tr_row_tab5 <- reactive({
    sample(1:nrow(InputTree_tab5()),
           splitSlider_tab5()*nrow(InputTree_tab5()))
  })
  
  train_dt_tab5 <- reactive({
    train_tab5 <- InputTree_tab5()
    train_tab5[tr_row_tab5(), ]
  })
  
  test_dt_tab5 <- reactive({
    test_tab5 <- InputTree_tab5()
    test_tab5[-tr_row_tab5(), ]
  })
  
  f_tab5 <- reactive({
    as.formula(paste(input$tab5_varY, "~", str_c(input$tab5_varX, collapse = "+")))})
  
  classi_tree_tab5 <- reactive({
    tree(f_tab5(),
         data = train_dt_tab5(),
         mindev = input$tab5_mindev,
         na.action = na.exclude)
  })
  
  output$tab5_plot <- renderPlot({
    validate(
      need(input$tab5_varX, "Please select at least 1 predictor variable!")
    )
    classitree <- plot(classi_tree_tab5())
    classitree
    text(classi_tree_tab5(), pretty = 0)
  })
  
  # Tab 5 - Tree Validating using test set
  pred_tab5 <- reactive({round(predict(classi_tree_tab5(), test_dt_tab5()),
                               digits = 0)})
  test_result_tab5 <- reactive({
    test_dt_tab5()[[input$tab5_varY]] > 0.5})
  
  cm_tab5 <- reactive({
    cm <- table(pred_tab5(), test_result_tab5())
    cm %>% 
      melt() %>% 
      rename("Prediction" = Var1, "Actual" = Var2,
             "Observations" = value) %>% 
      mutate_if(is.logical, as.factor)
  })
  
  # Tab 5 - Confusion Matrix Table
  output$tab5_matrix <- renderTable({
    validate(
      need(input$tab5_varX,
           "Please select at least 1 variable!")
    )
    cm_tab5()})
  
  # Tab 5 - Statistic indexes for Classification Tree
  output$tab5_rate <- renderTable({
      validate(
        need(input$tab5_varX,
             "Please select at least 1 variable!")
      )
      TruP_tab5 <- cm_tab5()$Observations[4]
      TruN_tab5 <- cm_tab5()$Observations[1]
      FalN_tab5 <- cm_tab5()$Observations[3]
      FalP_tab5 <- cm_tab5()$Observations[2]
      
      TotP_tab5 <- TruP_tab5 + FalN_tab5
      TotN_tab5 <- TruN_tab5 + FalP_tab5
      Tot_tab5 <- TotP_tab5 + TotN_tab5
      
      Accuracy_tab5 <- (TruP_tab5 + TruN_tab5)/Tot_tab5
      Error_tab5 <- (FalP_tab5 + FalN_tab5)/Tot_tab5
      Sensitivity_tab5 <- TruP_tab5/TotP_tab5
      Specificity_tab5 <- TruN_tab5/TotN_tab5
      FalPos_tab5 <- 1 - Specificity_tab5
      
      rates_tab5 <- tibble(Accuracy = Accuracy_tab5, 
                           Error = Error_tab5, 
                           Sensitivity = Sensitivity_tab5, 
                           Specificity = Specificity_tab5, 
                           FalPos = FalPos_tab5)

      rates_tab5
      
  })
  
  
#####Tab 6: Variable Importance Server Code: ###########################
  
  InputTree_tab6 <- reactive({
    if (is.null(input$tab6_varX)) {
      
    } else {
      adult %>%
        select(c(input$tab6_varY, input$tab6_varX)) %>%
        na.omit() -> data
      data
    }
  })
  
  splitSlider_tab6 <- reactive({input$tab6_split})
  
  # Split data into train and test set 
  set.seed(1)
  tr_row_tab6 <- reactive({
    sample(1:nrow(InputTree_tab6()),
           splitSlider_tab6()*nrow(InputTree_tab6()))
  })
  
  train_dt_tab6 <- reactive({
    train_tab6 <- InputTree_tab6()
    train_tab6[tr_row_tab6(), ]
  })
  
  test_dt_tab6 <- reactive({
    test_tab6 <- InputTree_tab6()
    test_tab6[-tr_row_tab6(), ]
  })
  
  f_tab6 <- reactive({
    as.formula(paste(input$tab6_varY, "~", str_c(input$tab6_varX, collapse = "+")))})
  
  mtry_tab6 <- reactive({
    if (length(input$tab6_varX) < input$tab6_tree) {
      mtry <- length(input$tab6_varX)
    } else {
      mtry <- input$tab6_tree
    }
    mtry
  })
  rf_tab6 <- reactive({
    randomForest(f_tab6(),
         data = train_dt_tab6(),
         ntree = input$tab6_tree,
         mtry = mtry_tab6(),
         importance = TRUE)
  })
  
  # Tab6 Classification accuracy plot
  output$tab6_plot <- renderPlot({
    validate(
      need(input$tab6_varX, "Please select at least 1 predictor variable!")
    )
    validate(
      if (length(input$tab6_varX) > 10) {
        "Please select 10 predictors or less"
      }
    )
    rf <- plot(rf_tab6(), main = "Random Forest Classification Rate")
    rf
  })
  
  # Tab6 Variable Importance Plot
  output$tab6_vi_plot <- renderPlot({
    validate(
      if (length(input$tab6_varX) < 2) {
        "Please select at least 2 predictor variables!"
      }
    )
    validate(
      if (length(input$tab6_varX) > 10) {
        "Please select 10 predictors or less"
      }
    )
    vi <- varImpPlot(rf_tab6(), main = "Variable Importance Plot")
    vi
  })
  
  # Tab 6 - Tree Validating using test set
  pred_tab6 <- reactive({predict(rf_tab6(), test_dt_tab6())})
  
  cm_tab6 <- reactive({
    preds <- pred_tab6()
    actual <- test_dt_tab6()[[input$tab6_varY]]
    
    preds <- ifelse(preds == "Severe", 1, 0)
    actual <- ifelse(actual == "Severe", 1, 0)
    cm <- table(preds, actual)
    cm %>% 
      melt() %>% 
      rename("Prediction" = 1, 
             "Actual" = 2,
             "Observations" = 3) %>% 
      mutate_if(is.logical, as.factor) ->
    cm
    cm
  })
  
  # Tab 6 - Confusion Matrix Table
  output$tab6_matrix <- renderTable({
    validate(
      need(input$tab6_varX,
           "Please select at least 1 variable!")
    )
    validate(
      if (length(input$tab6_varX) > 10) {
        "Please select 10 predictors or less"
      }
    )
    cm_tab6()})
  
  # Tab 6 - Statistic indexes for Random Forest
  output$tab6_rate <- renderTable({
    validate(
      need(input$tab6_varX,
           "Please select at least 1 variable!")
    )
    validate(
      if (length(input$tab6_varX) > 10) {
        "Please select 10 predictors or less"
      }
    )
    TruP_tab6 <- cm_tab6()$Observations[4]
    TruN_tab6 <- cm_tab6()$Observations[1]
    FalN_tab6 <- cm_tab6()$Observations[3]
    FalP_tab6 <- cm_tab6()$Observations[2]
    
    TotP_tab6 <- TruP_tab6 + FalN_tab6
    TotN_tab6 <- TruN_tab6 + FalP_tab6
    Tot_tab6 <- TotP_tab6 + TotN_tab6
    
    Accuracy_tab6 <- (TruP_tab6 + TruN_tab6)/Tot_tab6
    Error_tab6 <- (FalP_tab6 + FalN_tab6)/Tot_tab6
    Sensitivity_tab6 <- TruP_tab6/TotP_tab6
    Specificity_tab6 <- TruN_tab6/TotN_tab6
    FalPos_tab6 <- 1 - Specificity_tab6
    
    rates_tab6 <- tibble(Accuracy = Accuracy_tab6, 
                         Error = Error_tab6, 
                         Sensitivity = Sensitivity_tab6, 
                         Specificity = Specificity_tab6, 
                         FalPos = FalPos_tab6)
    
    rates_tab6
    
  })
  
########## Tab 7: Data Dictionary Server Code ##########################
  
  output$tab7_table <- DT::renderDataTable({
    # Call the above reactive expression. Create a df object exclusive to tab 7. Create an if statement for when the input tab7_factoronly is selected. If it is, then the tab7_df object is filtered by variable name to remove the variables that are not factor variables in the adult data set. If it's not selected, nothing happens. Then display the data frame with 20 rows per page:
    
    # Save df exclusive to tab 7: 
    tab7_df <- tab7_dictionary
    
    # Update the df if tab7_factoronly is selected: 
    
    if (input$tab7_factoronly) {
      tab7_df <- tab7_dictionary %>% 
        filter(!app_variable_name %in% c("age", "house_people_number",
                                         "family_income", "poverty_rate",
                                         "sleep_hours", "days_drink_alcohol",
                                         "physical_activity_moderate_freq", 
                                         "strength_act_freq_w", 
                                         "vig_act_freq_w"))
    }
    
    # Show the data dictionary data frame, with 20 rows per page: 
    
    DT::datatable(tab7_df, options = list(pageLength = 20))
  })
  
#### END OF SERVER SECTION ############################################## 
}

shinyApp(ui, server)