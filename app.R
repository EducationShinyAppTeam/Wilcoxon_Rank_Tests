# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(truncnorm)
library(dplyr)
library(readr)
library(DT)

# Load additional dependencies and setup functions
# source("global.R")

# Load Data ----

ARM_data <- data.frame (Student = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                        Grip_Strength = c(58.0, 52.5, 46.0, 57.5, 52.0, 45.5, 
                                          65.5, 71.0, 57.0, 54.0, 48.0, 58.5, 
                                          35.5, 44.0, 53.0))



Rural <- c(3,2,1,1,2,1,3,2,2,2,2,5,1,4,1,1,1,1,6,2,2,2,1,1)
Urban <- c(1,0,1,1,0,0,1,1,1,8,1,1,1,0,1,1,2) 
Siblings_data <- data.frame( 
  Area = c(rep("Rural", times=length(Rural)), 
           rep("Urban", times=length(Urban))),
  NumOfSbls = c(Rural, Urban)
)

Diff <- Rural - Urban
Diff <- data.frame("Rural - Urban" = c(Diff))

## One sample population plot
AirbnbData <- read_csv("Airbnb.csv") %>%
  dplyr::filter(room_type == "Private room")

PricePlot <- ggplot(
  data = AirbnbData,
  mapping = aes(x = price)
) +
  geom_histogram(
    mapping = aes(y = ..density..),
    fill = "skyblue",
    col = "black"
  ) +
  labs(
    title = "Population Histogram",
    x = "Prices",
    y = "Density"
  ) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 18),
    text = element_text(size = 18),
    axis.title = element_text(size = 16)
  )

## Two sample population plot 

AirbnbData2 <- read_csv("Airbnb.csv") %>%
  dplyr::filter(neighbourhood == "Near North Side" | neighbourhood == "West Town")

AirbnbDataNorth <- read_csv("Airbnb.csv") %>%
  dplyr::filter(neighbourhood == "Near North Side")

AirbnbDataWest <- read_csv("Airbnb.csv") %>%
  dplyr::filter(neighbourhood == "West Town")


PopularPlot1 <- ggplot(
) +
  # geom_boxplot(
  #   mapping = aes(x = availability_365, y = neighbourhood),
  #   fill = "skyblue",
  #   col = "black"
  # ) +
  geom_histogram(
    data = AirbnbDataNorth,
    mapping = aes(x = availability_365),
    fill = "skyblue",
    col = "black"
  ) +
  # geom_histogram(
  #   data = AirbnbDataWest,
  #   mapping = aes(x = availability_365, fill = "West Town"),
  #   # fill = "purple",
  #   col = "black"
  # ) +
  labs(
    title = "Population Histograms",
    # x = "Number of Days available after March 1st, 2022",
    y = "Count"
  ) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 18),
    text = element_text(size = 18),
    legend.position = "bottom",
    axis.title.x = element_blank()
    # axis.title = element_text(size = 16),
    # axis.ticks.y = element_blank()
  ) 
  # scale_fill_manual(
  #   name = "Neighborhood",
  #   labels = c(
  #     "Near North Side" = "Near North Side",
  #     "West Town" = "West Town"
  #   ),
  #   values = c(
  #     "Near North Side" = "skyblue",
  #     "West Town" = "purple"
  #   )
  # )

PopularPlot2 <- ggplot(
) +
  # geom_histogram(
  #   data = AirbnbDataNorth,
  #   mapping = aes(x = availability_365, fill = "Near North Side"),
  #   # fill = "skyblue",
  #   col = "black"
  # ) +
  geom_histogram(
    data = AirbnbDataWest,
    mapping = aes(x = availability_365, fill = "West Town"),
    # fill = "purple",
    col = "black"
  ) +
  labs(
    # title = "Population Histogram",
    x = "Number of Days available after March 1st, 2022",
    y = "Count"
  ) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 18),
    text = element_text(size = 18),
    legend.position = "bottom"
    # axis.title = element_text(size = 16),
    # axis.ticks.y = element_blank()
  ) +
  scale_fill_manual(
    name = "Neighborhood",
    labels = c(
      "Near North Side" = "Near North Side",
      "West Town" = "West Town"
    ),
    values = c(
      "Near North Side" = "skyblue",
      "West Town" = "purple"
    )
  )


# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "purple",
    ### Create the app header ----
    dashboardHeader(
      title = "Wilcoxon Rank Tests",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Wilcoxon_Rank_Tests")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Example", tabName = "example", icon = icon("wpexplorer")),
        menuItem("Wilcoxon Signed Rank Test", tabName = "explore1", icon = icon("wpexplorer")),
        menuItem("Wilcoxon Rank Sum Test", tabName = "explore2", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Wilcoxon Rank Tests"), # This should be the full name.
          p("This app allows students to explore the concept of two non-parametric 
            tests called Wilcoxon Signed Rank Test for one sample data or two sample
            paired data and Wilcoxon Rank Sum Test for two sample unpaired data."),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li("Review any prerequiste ideas by clicking the Go button below 
                    or using the Prerequistes link on the left."),
            tags$li("View the example page to view examples of each test and 
                    associated confidence intervals."),
            tags$li("Explore the Wilcoxon Signed Rank Test and its associated 
                    confidence interval for one-sample data."),
            tags$li("Explore the Wilcoxon Rank Sum Test and its associated 
                    confidence interval for two-sample data.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Wanyi Su.",
            br(),
            "I would like to extend a special thanks to Dennis K. Pearl and 
            Neil J. Hatfield.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 07/03/2022 by WS.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          box(
            title = strong("Non-parametric Tests"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            "The basic idea is to draw inferences about properties of a distribution 
            without assuming a specific parametric form (for example, by using 
            ranks to draw inferences about percentiles such as the median)."
          ),
          # h2(strong("Non-parametric Tests")),
          # tags$ul(
          #   tags$li("The basic idea is to use data to infer an unknown quantity 
          #           while making as few assumptions as possible."),
          #   tags$li("It drops the distribution assumptions."),
          #   tags$li("It aims to estimate the population median."),
          #   tags$li("It is useful in small sample sizes.")
          # ),
          box(
            title = strong("Wilcoxon Signed Rank Test"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "This test can be used to test the location of a single population 
            median, m (including the special case of whether the difference 
            between paired samples has a median of zero). The test works by 
            ranking all of the observations from smallest to largest and then 
            summing the ranks of those above the hypothesized median. The null 
            distribution of the statistic is the same for independent observations 
            arising from any continuous symmetric distribution. The test is often 
            viewed as an alternative to the Student's one sample t-test that 
            examines a null hypothesis about the mean assuming independent normally 
            distributed data. The assumptions for the Wilcoxon signed rank test 
            are then stronger than the sign test (which doesn't require symmetry) 
            but weaker than the t-test." 
          ),
          box(
            title = strong("Wilcoxon Rank Sum Test"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "This test can be used to test the difference, m1 - m2, between the 
            population median of two independent samples (including the special 
            case of whether the difference is zero). The test works by ranking 
            all of the observations pooled from both samples combined and then 
            summing the ranks of those that came from one of the two samples. 
            The test is often viewed as an alternative to the Student's two sample 
            t-test that examines a null hypothesis about the difference in population 
            means assuming independent normally distributed data."
          )
        ),
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
        #### Set up an Example Page ----
        tabItem(
          tabName = "example",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "One Sample Example",
              br(),
              h2("Grip Strength data"),
              tags$ul(tags$li("This is based on Wilcoxon Signed Rank Test."),
                      tags$li("Adjust the sliders to change the confidence level."),
                      tags$li("Select the alternative hypothesis.")),
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    h3("Sample Info"),
                    p("This data contains 15 observations of the posttest grip
                      strengths in the right arms of male freshmen in a study
                      of health dynamics. We shall test the null hypothesis"),
                    div("\\(H_0\\!:m = 50\\)"),
                    p("against an appropriate hypothesis that you select."),
                    #p("\\(H_0\\!:m = 50\\)", class = "largerFont"),
                    br(),
                    sliderInput(
                      inputId = "level",
                      label = "Confidence Level",
                      min = 85,
                      max = 99,
                      value = 95,
                      post = "%"
                    ),
                    br(),
                    selectInput(
                      inputId = "AltHypo",
                      label = "Alternative Hypothesis",
                      choices = c("Two Sided" = "two.sided", 
                                  "Less than" = "less",
                                  "Greater than" = "greater"),
                      selected = "two.sided"
                    )
                  )
                ),
                column(
                  width = 8,
                  plotOutput(outputId = "boxplot1", height = "300px"),
                  br(),
                  plotOutput(outputId = "dotplot1", height = "250px")
                )
              ),
                # h3("Sample  Boxplot"),
                # column(
                #   width = 8,
                #   plotOutput(outputId = "boxplot1", height = "300px")),
                # br(),
                # br(),
                # br(),
                # br(),
                # h3("Sample  Dotplot"),
                # column(
                #   width = 8,
                #   plotOutput(outputId = "dotplot1", height = "200px")
                # ),
                  checkboxInput(
                    inputId = "CIcheckbox",
                    label = "Results table",
                    value = FALSE,
                    width = "100%"
                  ),
              DT::DTOutput(outputId = "CItable")
              # tableOutput(outputId = "CItable")
            ),
            tabPanel(
              "Two Sample Example",
              br(),
              h2("Siblings data"),
              tags$ul(tags$li("This is based on Wilcoxon Rank Sum Test."),
                      tags$li("Adjust the sliders to change the confidence level."),
                      tags$li("Select the alternative hypothesis.")),
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    h3("Sample Info"),
                    p("This unpaired data contains 41 observations of the number 
                      of siblings in urban and rural areas, 24 for rural and 17 
                      for urban. We shall test the null hypothesis"),
                    div("\\(H_0\\!:m_u = m_r\\)"),
                    p("against an appropriate hypothesis that you select."),
                    br(),
                    sliderInput(
                      inputId = "level2",
                      label = "Confidence Level",
                      min = 85,
                      max = 99,
                      value = 95,
                      post = "%"
                    ),
                    selectInput(
                      inputId = "AltHypo2",
                      label = "Alternative Hypothesis",
                      choices = c("Two Sided" = "two.sided", 
                                  "Less than" = "less",
                                  "Greater than" = "greater"),
                      selected = "two.sided"
                    )
                  )
                ),
                column(
                  width = 8,
                  plotOutput(outputId = "boxplot2", height = "300px"),
                  br(),
                  plotOutput(outputId = "dotplot2", height = "250px")
                )
              ),
              plotOutput(outputId = "mixplot", height = "300px"),
              checkboxInput(
                inputId = "CIcheckbox2",
                label = "Results table",
                value = FALSE,
                width = "100%"
              ),
              DT::DTOutput(outputId = "CItable2")
              # tableOutput(outputId = "CItable2")
            )
          )
        ),
        #### Set up an Explore1 Page ----
        tabItem(
          tabName = "explore1",
          withMathJax(),
          h2("Wilcoxon Signed Rank Test"),
          box(
            title = "Context",
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",
            p("A researcher plans to take a random sample of size n Airbnbs to
              do a test about their prices. The researcher
              makes a confidence interval for the prices and compares it 
              to the median of $56 for the population
              of airbnb prices. These data are about Airbnb prices in Chicago.")
          ),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Hypothesis"),
                p("\\(H_0\\!:m = 56\\)", class = "largerFont"),
                sliderInput(
                  inputId = "level3",
                  label = "Confidence level",
                  min = 60,
                  max = 99,
                  value = 95,
                  post = "%"
                ),
                sliderInput(
                  inputId = "nsamp",
                  label = "Sample size (n > 2)",
                  min = 2,
                  max = 30,
                  value = 10,
                  step = 1
                ),
                p("Click the following button to create 50 samples of your
                  selected sample size to see their confidence intervals and
                  boxplots."),
                bsButton(
                  inputId = "new",
                  label = "Generate 25 Samples",
                  icon = icon("retweet"),
                  size = "large"
                )
              )
            ),
            column(
              width = 8,
              plotOutput(outputId = "popMedian", height = "400px"),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('popMean').setAttribute('aria-labelledby',
                'mathPopPara')
                })"
              )),
              p(
                id = "mathPopPara",
                "The population chart shows the distribution of all airbnb prices,
                N = 1,562 rooms. The population median is the line in the middle 
                of the box, \\(m = 89\\)."
              )
            )
          ),
          uiOutput("sampleMessage"),
          fluidRow(
            column(
              width = 6,
              plotOutput("sampMedian"),
              tags$script(HTML(
                "$(document).ready(function() {
              document.getElementById('sampMean').setAttribute('aria-label',
              `This is a bar chart for a single sample.`)
              })"
              )),
              uiOutput("sampleColors")
            ),
            column(
              width = 6,
              plotOutput(
                outputId = "CIplot",
                click = "plot_click"
              ),
              tags$script(HTML(
                "$(document).ready(function() {
              document.getElementById('CIplot').setAttribute('aria-label',
              `This is the confidence interval plot.`)
              })"
              )),
              textOutput("CoverageRate")
            )
          )
        ),
        #### Set up an Explore2 Page ----
        tabItem(
          tabName = "explore2",
          withMathJax(),
          h2("Wilcoxon Rank Sum Test"),
          box(
            title = "Context",
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",
            p("A researcher plans to take a random sample of size n Airbnbs 
              to do a test about their popularity, that is, the number of days 
              available in the rest of 2022 after March 1st. The researcher
              makes a confidence interval for difference between the available 
              days for Airbnbs in two areas: near north side and in west town. 
              These data are about Airbnbs in Chicago, which is the same as in 
              previous page.")
          ),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                h3("Hypothesis"),
                p("\\(H_0\\!:m_{WT} = m_{NNS}\\)", class = "largerFont"),
                sliderInput(
                  inputId = "level4",
                  label = "Confidence level",
                  min = 60,
                  max = 99,
                  value = 95,
                  post = "%"
                ),
                sliderInput(
                  inputId = "nsamp2",
                  label = "Sample size (n > 30)",
                  min = 2,
                  max = 30,
                  value = 10,
                  step = 1
                ),
                p("Click the following button to create 25 samples of your
                  selected sample size to see their confidence intervals and
                  boxplots."),
                bsButton(
                  inputId = "new2",
                  label = "Generate 25 Samples",
                  icon = icon("retweet"),
                  size = "large"
                )
              )
            ),
            column(
              width = 8,
              plotOutput(outputId = "popMedian2", height = "200px"),
              plotOutput(outputId = "popMedian3", height = "200px"),
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementById('popMean').setAttribute('aria-labelledby',
                'mathPopPara')
                })"
              )),
              p(
                id = "mathPopPara",
                "The population chart shows the distribution of the number of days
                available in the rest of 2022 after March 1st around two places. 
                There are 669 airbnbs in West Town (WT) and 783 airbnbs in the 
                Near North Side (NNS). The population medians are 167 for WT and
                245 for NNS."
              )
            )
          ),
          uiOutput("sampleMessage2"),
          fluidRow(
            column(
              width = 6,
              plotOutput("sampMedian2"),
              tags$script(HTML(
                "$(document).ready(function() {
              document.getElementById('sampMean').setAttribute('aria-label',
              `This is a bar chart for a single sample.`)
              })"
              )),
              uiOutput("sampleColors2")
            ),
            column(
              width = 6,
              plotOutput(
                outputId = "CIplot2",
                click = "plot_click2"
              ),
              tags$script(HTML(
                "$(document).ready(function() {
              document.getElementById('CIplot').setAttribute('aria-label',
              `This is the confidence interval plot.`)
              })"
              )),
              textOutput("CoverageRate2")
            )
          )
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61.1). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2022). boastUtils: BOAST utlities.
            (v 0.1.12.3). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Change, W., and Borges Ribeiro, B. (2021). shinydashboard: Create
            dashboards with 'shiny'. (v 0.7.2) [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny:
            Web application framework for R. (v 1.7.1). [R package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "James J. Higgins. An introduction to modern nonparametric 
            statistics. Pacific Grove, 2004."
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2022). shinyWidgets: Custom
            inputs widgets for shiny. (v 0.7.0). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Robert V. Hogg, Elliot A. Tanis, and Dale L. Zimmerman. Probability
            and statistical inference. Pearson Education, 2015."
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Springer-Verlag:New York. (v 3.3.6) [R package]. Available from
            https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K. (2021). dplyr: A 
            Grammar of Data Manipulation. R package version 1.0.6. Available from
            https://CRAN.R-project.org/package=dplyr"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  # This is for Example page
  ## Boxplot one-sample----
  observeEvent(
    eventExpr = c(input$level, input$AltHypo),
    handlerExpr = {
      output$boxplot1 <- renderPlot(
        expr = {
          result <- wilcox.test(ARM_data$Grip_Strength, 
                               mu = 50, 
                               conf.level = input$level/100,
                               conf.int = T, 
                               correct = F, 
                               exact = T,
                               alternative = input$AltHypo)
          ggplot(ARM_data, aes(y = Grip_Strength)) + 
            geom_boxplot() +
            coord_flip() +
            labs(
              title = "Boxplot of Grip Strength",
              x = NULL,
              y = "Grip Strength (pounds)"
            ) +
            theme_bw() +
            theme(
              plot.caption = element_text(size = 18),
              text = element_text(size = 18),
              axis.title = element_text(size = 16),
              legend.position = "bottom",
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()
            ) +
            geom_rect(fill="purple",alpha=0.03,
                      aes(xmin=-0.375,
                          xmax=0.375,
                          ymin=ifelse(result$conf.int[1] < 0,
                                      0,
                                      result$conf.int[1]),
                          ymax=result$conf.int[2]))
        }
      )
    }
  )
  
  ## Dotplot one-sample ----
  observeEvent(
    eventExpr = c(input$level, input$AltHypo),
    handlerExpr = {
      output$dotplot1 <- renderPlot(
        expr = {
          result <- wilcox.test(ARM_data$Grip_Strength, 
                                mu = 0, 
                                conf.level = input$level/100,
                                conf.int = T, 
                                correct = F, 
                                exact = T,
                                alternative = input$AltHypo)
          ggplot(ARM_data, aes(x = Grip_Strength)) + 
            theme_bw() +
            labs(
              title = "Dotplot of Grip Strength",
              x = "Grip Strength (pounds)",
              y = NULL
            ) +
            geom_vline(
              mapping = aes(xintercept = round(result$conf.int[1],2),
                            colour = "CI"),
              size = 1.25,
              alpha = 1
            ) +
            geom_vline(
              mapping = aes(xintercept = round(result$conf.int[2],2),
                            color = "CI"),
              size = 1.25,
              alpha = 1
            ) +
            
            scale_color_manual(
              name = NULL,
              labels = c(
                "CI" = "Confidence\n Interval"
              ),
              values = c(
                "CI" = "purple"
              )) +
            theme(
              plot.caption = element_text(size = 18),
              text = element_text(size = 18),
              axis.title = element_text(size = 16),
              legend.position = "bottom",
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()
            ) +
            geom_dotplot(binwidth = 1,
                         stackratio = 1.5,
                         right = FALSE)
        }
      )
    }
  )
      
  
  ## CI table one-sample ----
  
  # output$CItable <- renderTable({
  #   result <- wilcox.test(ARM_data$Grip_Strength, 
  #                         mu = 50, 
  #                         conf.level = input$level/100,
  #                         conf.int = T, 
  #                         correct = F, 
  #                         exact = T,
  #                         alternative = input$AltHypo)
  #   if (input$CIcheckbox) {
  #     ctable <- matrix(c(round(result$conf.int[1],2), round(result$conf.int[2],2),
  #                        round(result$p.value,2)), 
  #                      nrow = 1)
  #     colnames(ctable) <- c("Lower bound", "Upper bound",  "p-value")
  #     ctable
  #   }
  # })
  
  observeEvent(
    eventExpr = c(input$CIcheckbox, input$AltHypo),
    handlerExpr = {
      if (input$CIcheckbox) {
        result <- wilcox.test(
          ARM_data$Grip_Strength, 
          mu = 50, 
          conf.level = input$level/100,
          conf.int = T, 
          correct = F, 
          exact = T,
          alternative = input$AltHypo
          )
        
        ctable <- data.frame(
          lower = round(result$conf.int[1],2),
          upper = round(result$conf.int[2],2),
          p.value = round(result$p.value,2)
        )
        # print(ctable)
        names(ctable) <- c("Lower Bound", "Upper Bound", "p-value")
        
        if (input$AltHypo == "less") {
          print('less')
          resultTable <- ctable[, -1]
        } else if (input$AltHypo == "greater") {
          print('greater')
          resultTable <- ctable[, -2]
        } else {
          resultTable <- ctable
        }
        
        # print("final call")
        # print(resultTable)
        
        # output$CItable <- renderTable({resultTable})
        output$CItable <- DT::renderDT(
          expr = resultTable,
          caption = "Confidence Interval and p-value", # Add a caption to your table
          style = "bootstrap4", # You must use this style
          rownames = TRUE,
          options = list( # You must use these options
            responsive = TRUE, # allows the data table to be mobile friendly
            scrollX = TRUE, # allows the user to scroll through a wide table
            columnDefs = list(  # These will set alignment of data values
              # Notice the use of ncol on your data frame; leave the 1 as is.
              list(className = 'dt-center', targets = 1:ncol(resultTable))
            )
          )
        )
      } else {
        output$CItable <- NULL
      }
    }
  )
  
  ## Boxplot two-sample ----
  
  observeEvent(
    eventExpr = c(input$level2, input$AltHypo2),
    handlerExpr = {
      output$boxplot2 <- renderPlot(
        expr = {
          result2 <- wilcox.test(Rural, 
                                mu = 2, 
                                conf.level = input$level2/100,
                                conf.int = T, 
                                correct = T, 
                                exact = F,
                                alternative = input$AltHypo2)
          result3 <- wilcox.test(Urban, 
                                mu = 1, 
                                conf.level = input$level2/100,
                                conf.int = T, 
                                correct = T, 
                                exact = F,
                                alternative = input$AltHypo2)
          ggplot(Siblings_data, aes(x = Area, y = NumOfSbls)) + 
            geom_boxplot() +
            coord_flip() +
            theme_bw() +
            labs(
              title = "Boxplot of Number of Siblings by Area",
              y = "Number of Siblings"
            ) +
            theme(
              plot.caption = element_text(size = 18),
              text = element_text(size = 18),
              axis.title = element_text(size = 16),
              legend.position = "bottom",
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()
            ) +
            geom_rect(fill="red",alpha=0.01,
                      aes(xmin=0.8,
                          xmax=1.2,
                          ymin=ifelse(result2$conf.int[1] < 0,
                                      0,
                                      result2$conf.int[1]),
                          ymax=result2$conf.int[2])) +
            geom_rect(fill="blue",alpha=0.01,
                      aes(xmin=1.8,
                          xmax=2.2,
                          ymin=ifelse(result3$conf.int[1] < 0,
                                      0,
                                      result3$conf.int[1]),
                          ymax=result3$conf.int[2]))
        }
      )
    }
  )
  
  ## Dotplot two sample ----
  
  observeEvent(
    eventExpr = c(input$level2, input$AltHypo2),
    handlerExpr = {
      output$dotplot2 <- renderPlot(
        expr = {
          result2 <- wilcox.test(Rural, 
                                 mu = 2, 
                                 conf.level = input$level2/100,
                                 conf.int = T, 
                                 correct = T, 
                                 exact = F,
                                 alternative = input$AltHypo2)
          result3 <- wilcox.test(Urban, 
                                 mu = 1, 
                                 conf.level = input$level2/100,
                                 conf.int = T, 
                                 correct = T, 
                                 exact = F,
                                 alternative = input$AltHypo2)
          ggplot(Siblings_data, 
                 aes(x = NumOfSbls, fill = Area, color = Area)) + 
            theme_bw() +
            theme(
              plot.caption = element_text(size = 18),
              text = element_text(size = 18),
              axis.title = element_text(size = 16),
              legend.position = "bottom",
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()
            ) +
            labs(
              title = "Dotplot of Number of Siblings by Area",
              x = "Number of Siblings",
              y = NULL
            ) +
            scale_fill_manual(values = c("red", "blue")) +
            geom_vline(
              mapping = aes(xintercept = round(result2$conf.int[1],2),
                            colour = "CI"),
              size = 1.25,
              alpha = 1
            ) +
            geom_vline(
              mapping = aes(xintercept = round(result2$conf.int[2],2),
                            color = "CI"),
              size = 1.25,
              alpha = 0.55
            ) +
            geom_vline(
              mapping = aes(xintercept = round(result3$conf.int[1],2),
                            colour = "CI2"),
              size = 1.25,
              alpha = 1
            ) +
            geom_vline(
              mapping = aes(xintercept = round(result3$conf.int[2],2),
                            color = "CI2"),
              size = 1.25,
              alpha = 0.55
            ) +
            scale_color_manual(
              name = NULL,
              labels = c(
                "CI" = "CI for\nRural",
                "CI2" = "CI for\nUrban"
              ),
              values = c(
                "CI" = "red",
                "CI2" = "blue"
              )) +
            geom_dotplot(binwidth = 0.15,
                         right = FALSE,
                         alpha = 0.55)
        }
      )
    }
  )
  
  ## Mixplot two sample ----
  
  observeEvent(
    eventExpr = c(input$level2, input$AltHypo2),
    handlerExpr = {
      output$mixplot <- renderPlot(
        expr = {
          result4 <- wilcox.test(NumOfSbls ~ Area,
                                data = Siblings_data,
                                paired = FALSE,
                                conf.level = input$level2/100,
                                conf.int = T, 
                                correct = T, 
                                exact = F,
                                alternative = input$AltHypo2)
          ggplot(Diff, aes(x = Rural - Urban)) + 
            geom_boxplot() +
            theme_bw() +
            theme(
              plot.caption = element_text(size = 18),
              text = element_text(size = 18),
              axis.title = element_text(size = 16),
              legend.position = "bottom",
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()
            ) +
            labs(
              title = "Boxplot of the difference of number of siblings between Areas",
              x = "Rural - Urban",
              y = NULL
            ) +
            geom_rect(fill="purple",alpha=0.03,
                      aes(xmin=result4$conf.int[1],
                          xmax=result4$conf.int[2],
                          ymin=-0.375,
                          ymax=0.375)) +
            geom_dotplot(binwidth = 0.2,
                         stackratio = 1,
                         right = FALSE,
                         stackdir = "center")
        }
      )
    }
  )
  
  ## CI table two-sample ----
  
  # output$CItable2 <- renderTable({
  #   # result2 <- wilcox.test(Rural, 
  #   #                        mu = 2, 
  #   #                        conf.level = input$level2/100,
  #   #                        conf.int = T, 
  #   #                        correct = F, 
  #   #                        exact = T,
  #   #                        alternative = input$AltHypo2)
  #   # result3 <- wilcox.test(Urban, 
  #   #                        mu = 1, 
  #   #                        conf.level = input$level2/100,
  #   #                        conf.int = T, 
  #   #                        correct = F, 
  #   #                        exact = T,
  #   #                        alternative = input$AltHypo2)
  #   result4 <- wilcox.test(
  #     formula = NumOfSbls ~ Area,
  #     data = Siblings_data,
  #     paired = FALSE,
  #     conf.level = input$level2/100,
  #     conf.int = T, 
  #     correct = F, 
  #     exact = T,
  #     alternative = input$AltHypo2
  #   )
  #   
  #   ctable <- matrix(
  #     data = c(round(result4$conf.int[1],2), round(result4$conf.int[2],2),
  #              round(result4$p.value,2)),
  #     nrow = 1)
  #   colnames(ctable) <- c("Lower bound", "Upper bound", "p-value")
  #   print(ctable)
  #   
  #   if (input$AltHypo2 == "less") {
  #     print('less')
  #     resultTable <- ctable[, -1]
  #   } else if (input$AltHypo2 == "greater") {
  #     print('greater')
  #     resultTable <- ctable[, -2]
  #   } else {
  #     resultTable <- ctable
  #   }
  #   
  #   # if (input$CIcheckbox2) {
  #   #   ctable <- matrix(c("Rural", "Urban","Diff",round(result2$conf.int[1],2), 
  #   #                      round(result3$conf.int[1],2), round(result4$conf.int[1],2),
  #   #                      round(result2$conf.int[2],2), round(result3$conf.int[2],2),
  #   #                      round(result4$conf.int[2],2), round(result2$p.value,2),
  #   #                      round(result3$p.value,2), round(result4$p.value,2)), 
  #   #                    nrow = 3)
  #   #   colnames(ctable) <- c("Name", "Lower bound", "Upper bound",  "p-value")
  #   # 
  #   # }
  #   print("final call")
  #   print(resultTable)
  #   if (input$CIcheckbox2) {
  #     resultTable
  #   } else {
  #     NULL
  #   }
  # })
  
  observeEvent(
    eventExpr = c(input$CIcheckbox2, input$AltHypo2),
    handlerExpr = {
      if (input$CIcheckbox2) {
        result4 <- wilcox.test(
          formula = NumOfSbls ~ Area,
          data = Siblings_data,
          paired = FALSE,
          conf.level = input$level2/100,
          conf.int = T, 
          correct = F, 
          exact = T,
          alternative = input$AltHypo2
        )
        
        ctable <- data.frame(
          lower = round(result4$conf.int[1],2),
          upper = round(result4$conf.int[2],2),
          p.value = round(result4$p.value,2)
        )
        # print(ctable)
        names(ctable) <- c("Lower Bound", "Upper Bound", "p-value")
        
        if (input$AltHypo2 == "less") {
          print('less')
          resultTable <- ctable[, -1]
        } else if (input$AltHypo2 == "greater") {
          print('greater')
          resultTable <- ctable[, -2]
        } else {
          resultTable <- ctable
        }
        
        # print("final call")
        # print(resultTable)
        
        # output$CItable2 <- renderTable({resultTable})
        output$CItable2 <- DT::renderDT(
          expr = resultTable,
          caption = "Confidence Interval and p-value", # Add a caption to your table
          style = "bootstrap4", # You must use this style
          rownames = TRUE,
          options = list( # You must use these options
            responsive = TRUE, # allows the data table to be mobile friendly
            scrollX = TRUE, # allows the user to scroll through a wide table
            columnDefs = list(  # These will set alignment of data values
              # Notice the use of ncol on your data frame; leave the 1 as is.
              list(className = 'dt-center', targets = 1:ncol(resultTable))
            )
          )
        )
      } else {
        output$CItable2 <- NULL
      }
    }
  )
  
  # sample message
  observeEvent(input$new, {
    output$sampleMessage <- renderUI({
      "Click on the confidence intervals (on the right) to view the boxplot of
      the sample on the left."
    })
    
    output$sampleColors <- renderUI({
      "The shaded region on the boxplot represent the confidence interval 
      corresponding to the plot on the right. It will change colors (red or blue) 
      depending on whether the confidence interval constructed from the sample 
      contains the population median."
    })
  })
  
  # Signed Rank part ----
  ## population mean plot with true mean
  output$popMedian <- renderPlot({
    PricePlot 
  })
  
  
  ### Generate 50 new samples ----
  Samples <- eventReactive(input$new, {
    temp <- data.frame() 
    for (i in 1:25) {
      temp <- rbind(
        temp,
        cbind(index = i, dplyr::slice_sample(AirbnbData, n = input$nsamp))
      )
    }
    temp
  }
  )
    
  
  ### Calculate the interval ----
  
  Intervals <- reactive({
    Samples() %>%
      dplyr::select(index, price) %>%
      group_by(index) %>%
      summarise(
        SampleMedian = suppressWarnings(wilcox.test(price,conf.level = input$level3/100,conf.int = T)$estimate),
        lowerbound = suppressWarnings(wilcox.test(price,conf.level = input$level3/100,conf.int = T)$conf.int[1]),
        upperbound = suppressWarnings(wilcox.test(price,conf.level = input$level3/100,conf.int = T)$conf.int[2])
      ) %>%
      mutate(cover = (lowerbound < 56) & (56 < upperbound))
  }
  )
  
  ### Default as all the samples are selected ----
  ### TODO: What is this code's purpose?
  selected_sample <- 25
  selectedSample <- reactive({
    if (!is.null(input$plot_click)) {
      selected_sample <<- round(input$plot_click$y)
      if (selected_sample < 1) {
        selected_sample <<- 1
      }
      if (selected_sample > 25) {
        selected_sample <<- 25
      }
    }
    selected_sample
  })
  
  # OneSample <- reactive({
  #   Samples() %>%
  #     filter(index == selectedSample())
  # })
  
  OneSampleColor <- reactive({
    colors <- c("TRUE" = "skyblue1", "FALSE" = "lightcoral")
    covers <- (Intervals() %>% filter(index == selectedSample()))$cover
    colors[as.character(covers)]
  })
  
  ### Store xAPI interacted statement ----
  observeEvent(input$plot_click, {
    
    stmt <- boastUtils::generateStatement(
      session,
      verb = "interacted",
      object = "CIplot",
      description = "90% Confidence Intervals for the Median",
      interactionType = "numeric",
      response = jsonlite::toJSON(input$plot_click)
    )
    
    boastUtils::storeStatement(session, stmt)
  })
  
  ### Text messages ----
  output$CoverageRate <- renderText({
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input sample size"
      )
    )
    
    paste0(
      sum(Intervals()$cover),
      " of these ",
      nrow(Intervals()),
      " intervals cover the parameter value. The coverage rate is ",
      round(100 * sum(Intervals()$cover) / nrow(Intervals()), 2),
      "%."
    )
  })
  
  ### CIplot ----
  observeEvent(
    eventExpr = selectedSample(),
    handlerExpr = {
      output$CIplot <- renderPlot(
        expr = {
    validate(
      need(is.numeric(input$nsamp),
           message = "Please input sample size"
      )
    )
    
    validate(
      need(input$nsamp >= 2,
           message = "Please input sample size larger than 2"
      )
    )
    
    ggplot(data = Intervals()) +
      geom_pointrange(
        mapping = aes(
          x = index,
          ymin = lowerbound,
          ymax = upperbound,
          y = SampleMedian,
          colour = cover,
          alpha = index == selectedSample(),
          size = index == selectedSample()
        )
      ) +
      geom_hline(
        mapping = aes(yintercept = 56, color = "zpop"),
        size = 1.25,
        alpha = 1
      ) +
      coord_flip() +
      scale_size_manual(
        values = c("TRUE" = 1.5, "FALSE" = .7),
        guide = FALSE
      ) +
      scale_color_manual(
        name = NULL,
        labels = c(
          "TRUE" = "Captures",
          "FALSE" = "Fails",
          "zpop" = "Population Median"
        ),
        values = c(
          "TRUE" = "dodgerblue3",
          "FALSE" = "red",
          "zpop" = boastPalette[3]
        )
      ) +
      scale_alpha_manual(
        values = c("TRUE" = 1, "FALSE" = .5),
        guide = FALSE
      ) +
      labs(
        title = paste0(input$level3, "% Confidence Intervals for the Median"),
        x = NULL,
        y = "Prices"
      ) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
      )
  })})
  
  ###Sample boxplot ----
  SelectedIndex <- reactiveVal(1)
  
  observeEvent(
    eventExpr = selectedSample(),
    handlerExpr = {
      output$sampMedian <- renderPlot(
        expr = {
          validate(
            need(is.numeric(input$nsamp),
                 message = "Please input sample size"
            ),
            need(input$nsamp >= 2,
                 message = "Please input sample size larger than 2"
            )
          )
            ggplot(NULL) +
            geom_boxplot(Samples() %>%
                           dplyr::filter(index == isolate(selectedSample())),
              mapping = aes(y = price),
              bins = 15,
              col = "black"
            ) +
            geom_rect(fill = OneSampleColor(),
                      alpha = 0.5,
                      data = Intervals() %>%
                        dplyr::filter(index == isolate(selectedSample())),
                      mapping = aes(xmin=-0.375,
                                    xmax=0.375,
                                    ymin=lowerbound,
                                    ymax=upperbound)
            ) +
            geom_hline(
              mapping = aes(yintercept = suppressWarnings(wilcox.test((Samples() %>%
                                                                         dplyr::filter(
                                                                           index == isolate(selectedSample())))$price,
                                                                      conf.level = 0.95,
                                                                      conf.int = T)$estimate), 
                            color = "Est"),
              size = 1
            ) +
            geom_hline(
              mapping = aes(yintercept = 56, color = "pop"),
              size = 1
            ) +
            coord_flip() +
            labs(
              title = "Boxplot of Selected Sample",
              y = "Prices"
            ) +
            theme_bw() +
            theme(
              plot.caption = element_text(size = 18),
              text = element_text(size = 18),
              axis.title = element_text(size = 16),
              legend.position = "bottom",
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()
            ) +
            scale_color_manual(
              name = NULL,
              labels = c(
                "Est" = "Point Estimate",
                "pop" = "Population median"
              ),
              values = c(
                "Est" = "red",
                "pop" = boastPalette[3]
              )
            )
        }
      )
    }
  )
  
  
  # Rank Sum part ----
  
  # sample message
  observeEvent(input$new2, {
    output$sampleMessage2 <- renderUI({
      "Click on the confidence intervals (on the right) to view the boxplot of
      the sample on the left."
    })
    
    output$sampleColors2 <- renderUI({
      "The shaded region on the boxplot represent the confidence interval 
      corresponding to the plot on the right. It will change colors (red or blue) 
      depending on whether the confidence interval constructed from the sample 
      contains the population median."
    })
  })
  
  ## population mean plot with true mean
  output$popMedian2 <- renderPlot({
    PopularPlot1 
  })
  
  output$popMedian3 <- renderPlot({
    PopularPlot2 
  })
  
  #### Generate 25 new samples ----
  west <- AirbnbData2 %>%
    select(neighbourhood, availability_365) %>%
    filter(neighbourhood == "West Town")

  north <- AirbnbData2 %>%
    select(neighbourhood, availability_365) %>%
    filter(neighbourhood == "Near North Side")

  Samples_west <- eventReactive(input$new2, {
    temp <- data.frame()
    for (i in 1:25) {
      temp <- rbind(
        temp,
        cbind(index = i, dplyr::slice_sample(west, n = input$nsamp2))
      )
    }
    temp
  }
  )

  Samples_north <- eventReactive(input$new2, {
    temp <- data.frame()
    for (i in 1:25) {
      temp <- rbind(
        temp,
        cbind(index = i, dplyr::slice_sample(north, n = input$nsamp2))
      )
    }
    temp
  }
  )

  Samples2 <- eventReactive(input$new2, {
    rbind(Samples_north(), Samples_west())
  }
  )
  
  North <- eventReactive(input$new2, {
    Samples_north() %>%
    rename(availability_north = availability_365,
           north = neighbourhood,
           i = index)
  }
  )
  
  West <- eventReactive(input$new2, {
    Samples_west() %>%
    rename(availability_west = availability_365,
           west = neighbourhood,
           j = index)
  }
  )
  
  Samples_diff <- eventReactive(input$new2, {
    cbind(North(), West()) %>%
      dplyr::mutate(diff = availability_north - availability_west)
  }
  )


  #### Calculate the interval ----

  Intervals2 <- reactive({
    Samples2() %>%
      #dplyr::select(index, availability_365) %>%
      group_by(index) %>%
      summarise(
        SampleMedian = suppressWarnings(wilcox.test(availability_365 ~ neighbourhood,
                                                    conf.level = input$level4/100,
                                                    conf.int = T)$estimate),
        lowerbound = suppressWarnings(wilcox.test(availability_365 ~ neighbourhood,
                                                  conf.level = input$level4/100,
                                                  conf.int = T)$conf.int[1]),
        upperbound = suppressWarnings(wilcox.test(availability_365 ~ neighbourhood,
                                                  conf.level = input$level4/100,
                                                  conf.int = T)$conf.int[2])
      ) %>%
      mutate(cover = (lowerbound < 0) & (0 < upperbound))
  }
  )

  ### Default as all the samples are selected ----
  ### TODO: What is this code's purpose?
  selected_sample2 <- 25
  selectedSample2 <- reactive({
    if (!is.null(input$plot_click2)) {
      selected_sample2 <<- round(input$plot_click2$y)
      if (selected_sample2 < 1) {
        selected_sample2 <<- 1
      }
      if (selected_sample2 > 25) {
        selected_sample2 <<- 25
      }
    }
    selected_sample2
  })

  # # OneSample <- reactive({
  # #   Samples() %>%
  # #     filter(index == selectedSample())
  # # })
  #
  OneSampleColor2 <- reactive({
    colors <- c("TRUE" = "skyblue1", "FALSE" = "lightcoral")
    covers <- (Intervals2() %>% filter(index == selectedSample2()))$cover
    colors[as.character(covers)]
  })

  ### Store xAPI interacted statement ----
  observeEvent(input$plot_click2, {

    stmt2 <- boastUtils::generateStatement(
      session,
      verb = "interacted",
      object = "CIplot2",
      description = "90% Confidence Intervals for the Median",
      interactionType = "numeric",
      response = jsonlite::toJSON(input$plot_click2)
    )

    boastUtils::storeStatement(session, stmt2)
  })

  ### Text messages ----
  output$CoverageRate2 <- renderText({
    validate(
      need(is.numeric(input$nsamp2),
           message = "Please input sample size"
      )
    )

    paste0(
      sum(Intervals2()$cover),
      " of these ",
      nrow(Intervals2()),
      " intervals cover the parameter value. The coverage rate is ",
      round(100 * sum(Intervals2()$cover) / nrow(Intervals2()), 2),
      "%."
    )
  })

  ### CIplot ----
  observeEvent(
    eventExpr = selectedSample2(),
    handlerExpr = {
      output$CIplot2 <- renderPlot(
        expr = {
          validate(
            need(is.numeric(input$nsamp2),
                 message = "Please input sample size"
            )
          )

          validate(
            need(input$nsamp2 >= 2,
                 message = "Please input sample size larger than 2"
            )
          )

          ggplot(data = Intervals2()) +
            geom_pointrange(
              mapping = aes(
                x = index,
                ymin = lowerbound,
                ymax = upperbound,
                y = SampleMedian,
                colour = cover,
                alpha = index == selectedSample2(),
                size = index == selectedSample2()
              )
            ) +
            geom_hline(
              mapping = aes(yintercept = 0, color = "zpop"),
              size = 1.25,
              alpha = 1
            ) +
            coord_flip() +
            scale_size_manual(
              values = c("TRUE" = 1.5, "FALSE" = .7),
              guide = FALSE
            ) +
            scale_color_manual(
              name = NULL,
              labels = c(
                "TRUE" = "Captures",
                "FALSE" = "Fails",
                "zpop" = "Population Median"
              ),
              values = c(
                "TRUE" = "dodgerblue3",
                "FALSE" = "red",
                "zpop" = boastPalette[3]
              )
            ) +
            scale_alpha_manual(
              values = c("TRUE" = 1, "FALSE" = .5),
              guide = FALSE
            ) +
            labs(
              title = paste0(input$level4, "% Confidence Intervals for the Median"),
              x = NULL,
              y = "# days available"
            ) +
            theme_bw() +
            theme(
              plot.caption = element_text(size = 18),
              text = element_text(size = 18),
              axis.title = element_text(size = 16),
              legend.position = "bottom",
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()
            )
        })})

  ###Sample boxplot ----
  SelectedIndex <- reactiveVal(1)

  observeEvent(
    eventExpr = selectedSample2(),
    handlerExpr = {
      output$sampMedian2 <- renderPlot(
        expr = {
          validate(
            need(is.numeric(input$nsamp2),
                 message = "Please input sample size"
            ),
            need(input$nsamp2 >= 2,
                 message = "Please input sample size larger than 2"
            )
          )
          ggplot(NULL) +
            geom_boxplot(Samples_diff() %>%
                           dplyr::filter(i == isolate(selectedSample2())),
                         mapping = aes(y = diff),
                         bins = 15,
                         col = "black"
            ) +
            geom_rect(fill = OneSampleColor2(),
                      alpha = 0.5,
                      data = Intervals2() %>%
                        dplyr::filter(index == isolate(selectedSample2())),
                      mapping = aes(xmin=-0.375,
                                    xmax=0.375,
                                    ymin=lowerbound,
                                    ymax=upperbound)
            ) +
            geom_hline(
              mapping = aes(yintercept = suppressWarnings(
                wilcox.test(((Samples2() %>%
                               dplyr::filter(
                                 index == isolate(selectedSample2())))$availability_365) ~ 
                              ((Samples2() %>%
                              dplyr::filter(
                                index == isolate(selectedSample2())))$neighbourhood),
                            conf.level = 0.95,
                            conf.int = T)$estimate),
                color = "Est"),
              size = 1
            ) +
            geom_hline(
              mapping = aes(yintercept = 0, color = "pop"),
              size = 1
            ) +
            coord_flip() +
            labs(
              title = "Boxplot of Selected Sample",
              y = "# days available"
            ) +
            theme_bw() +
            theme(
              plot.caption = element_text(size = 18),
              text = element_text(size = 18),
              axis.title = element_text(size = 16),
              legend.position = "bottom",
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank()
            ) +
            scale_color_manual(
              name = NULL,
              labels = c(
                "Est" = "Point Estimate",
                "pop" = "Population median"
              ),
              values = c(
                "Est" = "red",
                "pop" = boastPalette[3]
              )
            )
        }
      )
    }
  )


  ## Set Go Button ----
  observeEvent(
    eventExpr = input$go,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )
  
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App will help you learn Wilcoxon Rank Tests."
      )
    }
  )

}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
