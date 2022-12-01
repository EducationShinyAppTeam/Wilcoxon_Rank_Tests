# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(dplyr)
library(ggplot2)
# library(truncnorm)
#library(readr)
library(DT)

# Load additional dependencies and setup functions
# source("global.R")

# Load Data ----

armData <- data.frame (student = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                        gripStrength = c(58.0, 52.5, 46.0, 57.5, 52.0, 45.5,
                                          65.5, 71.0, 57.0, 54.0, 48.0, 58.5,
                                          35.5, 44.0, 53.0))



rural <- c(3,2,1,1,2,1,3,2,2,2,2,5,1,4,1,1,1,1,6,2,2,2,1,1)
urban <- c(1,0,1,1,0,0,1,1,1,8,1,1,1,0,1,1,2)
siblingsData <- data.frame(
  area = c(rep("rural", times=length(rural)),
           rep("urban", times=length(urban))),
  numOfSbls = c(rural, urban)
)

difference <- data.frame(
  difference = c(as.vector(outer(rural, urban, FUN = "-")))
)

## One sample population plot
airbnbData <- read.csv("Airbnb.csv") %>%
  dplyr::filter(room_type == "Private room")

pricePlot <- ggplot(
  data = airbnbData,
  mapping = aes(x = price)
) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    fill = "skyblue",
    col = "black",
    boundary = 0,
    bins = 30
  ) +
  labs(
    title = "Population Histogram",
    x = "Prices ($ per day)",
    y = "Density"
  ) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 18),
    text = element_text(size = 18),
    axis.title = element_text(size = 16)
  )

## Two sample population plot

airbnbData2 <- read.csv("Airbnb.csv") %>%
  dplyr::filter(neighbourhood == "Near North Side" | neighbourhood == "West Town")

airbnbDataNorth <- read.csv("Airbnb.csv") %>%
  dplyr::filter(neighbourhood == "Near North Side")

airbnbDataWest <- read.csv("Airbnb.csv") %>%
  dplyr::filter(neighbourhood == "West Town")


popularPlot1 <- ggplot(
) +
  # geom_boxplot(
  #   mapping = aes(x = availability_365, y = neighbourhood),
  #   fill = "skyblue",
  #   col = "black"
  # ) +
  geom_histogram(
    data = airbnbDataNorth,
    mapping = aes(x = availability_365),
    fill = "skyblue",
    col = "black",
    bins = 30
  ) +
  # geom_histogram(
  #   data = airbnbDataWest,
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

popularPlot2 <- ggplot(
) +
  # geom_histogram(
  #   data = airbnbDataNorth,
  #   mapping = aes(x = availability_365, fill = "Near North Side"),
  #   # fill = "skyblue",
  #   col = "black"
  # ) +
  geom_histogram(
    data = airbnbDataWest,
    mapping = aes(x = availability_365, 
                  fill = "West Town"),
    # fill = "purple",
    col = "black",
    bins = 30
  ) +
  labs(
    # title = "Population Histogram",
    x = "Number of days available after March 1st, 2022",
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

popularPlot3 <- ggplot(
) +
  geom_histogram(
    data = airbnbData2,
    mapping = aes(x = availability_365),
    fill = "skyblue",
    col = "black",
    boundary = 0,
    bins = 30
  ) +
  facet_wrap(~ neighbourhood,
             nrow = 2) +
  labs(
    title = "Population Histogram",
    x = "Number of Days available after March 1st, 2022",
    y = "Count"
  ) +
  theme_bw() +
  theme(
    plot.caption = element_text(size = 18),
    text = element_text(size = 18),
    legend.position = "bottom"
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
            div(class = "updated", "Last Update: 12/01/2022 by WS.")
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
            median, m (including the special case of whether the differenceerence 
            between paired samples has a median of zero). The test works by 
            ranking all of the observations from smallest to largest and then 
            summing the ranks of those above the hypothesized median. The null 
            distribution of the statistic is the same for independent observations 
            arising from any continuous symmetric distribution. The test is often 
            viewed as an alternative to the student's one sample t-test that 
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
            "This test can be used to test the differenceerence, m1 - m2, between the 
            population median of two independent samples (including the special 
            case of whether the differenceerence is zero). The test works by ranking 
            all of the observations pooled from both samples combined and then 
            summing the ranks of those that came from one of the two samples. 
            The test is often viewed as an alternative to the student's two sample 
            t-test that examines a null hypothesis about the differenceerence in population 
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
              h2("Grip Strength Data"),
              tags$ul(tags$li("This is based on Wilcoxon Signed Rank Test."),
                      tags$li("Adjust the sliders to change the confidence level."),
                      tags$li("Select the alternative hypothesis.")),
              br(),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    h3("Sample Info"),
                    p("This data contains 15 observations of the post-test grip
                      strengths in the right arms of male freshmen in a study
                      of health dynamics. We shall test the null hypothesis"),
                    div("\\(H_0\\!:m = 50\\)"),
                    p("against an appropriate hypothesis that you select."),
                    #p("\\(H_0\\!:m = 50\\)", class = "largerFont"),
                    br(),
                    sliderInput(
                      inputId = "level",
                      label = "Confidence Level",
                      min = 60,
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
                  plotOutput(outputId = "boxplot1", height = "150px"),
                  br(),
                  plotOutput(outputId = "dotplot1", height = "200px"),
                  checkboxInput(
                    inputId = "CIcheckbox",
                    label = "Results table",
                    value = FALSE,
                    width = "100%"
                  ),
                  DT::DTOutput(outputId = "CItable")
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
              #     checkboxInput(
              #       inputId = "CIcheckbox",
              #       label = "Results table",
              #       value = FALSE,
              #       width = "100%"
              #     ),
              # DT::DTOutput(outputId = "CItable")
              # tableOutput(outputId = "CItable")
            ),
            tabPanel(
              "Two Sample Example",
              br(),
              h2("Siblings Data"),
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
                      min = 60,
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
              plotOutput(outputId = "mixplot", height = "250px"),
              plotOutput(outputId = "mixplot2", height = "250px"),
              checkboxInput(
                inputId = "CIcheckbox2",
                label = "Results table",
                value = FALSE,
                width = "100%"
              ),
              fluidRow(
                column(
                  width = 9,
                  offset = 3,
                  DT::DTOutput(outputId = "CItable2", width = "50%")
                )
              )
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
            p("A researcher plans to take a random sample of size n rentals to
              do a test about their prices. The researcher
              makes a confidence interval for the prices and compares it 
              to the median of $56 for the population
              of Airbnb rental prices. These data are about Airbnb rental prices
              in Chicago.")
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
                "The population chart shows the distribution of all Airbnb rental
                prices,
                N = 1,562 rooms."
              )
            )
          ),
          uiOutput("sampleMessage"),
          fluidRow(
            column(
              width = 6,
              plotOutput("sampMedian", height = "200px"),
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
            p("A researcher plans to take a random sample of size n rentals from
              two areas, near north side and in west town, to do a test about 
              their popularity, that is, the number of days available in the rest 
              of 2022 after March 1st. The researcher makes a confidence interval 
              for differenceerence between the available days. These data are about 
              rentals in Chicago, which is the same as in previous page.")
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
              # plotOutput(outputId = "popMedian2", height = "200px"),
              # plotOutput(outputId = "popMedian3", height = "200px"),
              plotOutput(outputId = "popMedian4", height = "400px"),
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
                There are 669 rentals in West Town (WT) and 783 rentals in the 
                Near North Side (NNS). The population medians are 167 for WT and
                245 for NNS."
              )
            )
          ),
          uiOutput("sampleMessage2"),
          fluidRow(
            column(
              width = 6,
              plotOutput("sampMedian2", height = "200px"),
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
          # p(
          #   class = "hangingindent",
          #   "Robert V. Hogg, Elliot A. Tanis, and Dale L. Zimmerman. Probability
          #   and statistical inference. Pearson Education, 2015."
          # ),
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
          p(
            class = "hangingindent",
            "Yihui Xie, Joe Cheng and Xianying Tan (2022). DT: A Wrapper of the 
            JavaScript Library 'DataTables'. R package version 0.21.
            https://CRAN.R-project.org/package=DT"
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
          gripstrengthWilcoxon <- wilcox.test(
            armData$gripStrength, 
            mu = 50, 
            conf.level = input$level/100,
            conf.int = T, 
            correct = T, 
            exact = F,
            alternative = input$AltHypo)
          ggplot(armData, 
                 aes(y = gripStrength)) + 
            geom_boxplot(lwd = 1, 
                         fatten = 2) +
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
            geom_rect(fill="purple",
                      alpha=0.03,
                      aes(xmin=-0.2,
                          xmax=0.2,
                          ymin=ifelse(gripstrengthWilcoxon$conf.int[1] < 0,
                                      0,
                                      gripstrengthWilcoxon$conf.int[1]),
                          ymax=gripstrengthWilcoxon$conf.int[2]))
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
          gripstrengthWilcoxon <- wilcox.test(
            armData$gripStrength, 
            mu = 0, 
            conf.level = input$level/100,
            conf.int = T, 
            correct = T, 
            exact = F,
            alternative = input$AltHypo)
          ggplot(armData, 
                 aes(x = gripStrength)) + 
            theme_bw() +
            labs(
              title = "Dotplot of Grip Strength",
              x = "Grip Strength (pounds)",
              y = NULL
            ) +
            geom_vline(
              mapping = aes(xintercept = round(gripstrengthWilcoxon$conf.int[1],2),
                            colour = "CI"),
              linewidth = 1.25,
              alpha = 1
            ) +
            geom_vline(
              mapping = aes(
                xintercept = round(
                  gripstrengthWilcoxon$conf.int[2],2),
                color = "CI"),
              linewidth = 1.25,
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
            geom_dotplot(binwidth = 0.8,
                         stackratio = 1.5,
                         right = FALSE)
        }
      )
    }
  )
  
  
  ## CI table one-sample ----
  
  # output$CItable <- renderTable({
  #   gripstrengthWilcoxon <- wilcox.test(armData$gripStrength, 
  #                         mu = 50, 
  #                         conf.level = input$level/100,
  #                         conf.int = T, 
  #                         correct = F, 
  #                         exact = T,
  #                         alternative = input$AltHypo)
  #   if (input$CIcheckbox) {
  #     ctable <- matrix(c(round(gripstrengthWilcoxon$conf.int[1],2), round(gripstrengthWilcoxon$conf.int[2],2),
  #                        round(gripstrengthWilcoxon$p.value,2)), 
  #                      nrow = 1)
  #     colnames(ctable) <- c("Lower bound", "Upper bound",  "p-value")
  #     ctable
  #   }
  # })
  
  observeEvent(
    eventExpr = c(input$CIcheckbox, input$AltHypo, input$level),
    handlerExpr = {
      if (input$CIcheckbox) {
        gripstrengthWilcoxon <- wilcox.test(
          armData$gripStrength, 
          mu = 50, 
          conf.level = input$level/100,
          conf.int = T, 
          correct = T, 
          exact = F,
          alternative = input$AltHypo
        )
        
        ctable <- data.frame(
          lower = round(gripstrengthWilcoxon$conf.int[1],2),
          upper = round(gripstrengthWilcoxon$conf.int[2],2),
          p.value = round(gripstrengthWilcoxon$p.value,2)
        )
        # print(ctable)
        names(ctable) <- c(paste0(input$level,"% Lower Bound"),
                           paste0(input$level,"% Upper Bound"), 
                           "p-value")
        
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
          options = list(
            responsive = TRUE,
            scrollX = TRUE,
            ordering = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            pageLength = 1,
            searching = FALSE,
            info = FALSE,
            columnDefs = list(
              list(visible = FALSE, 
                   targets = 0),
              list(className = 'dt-center', 
                   targets = 1:ncol(resultTable))
            )
          )
        )
      } else {
        output$CItable <- NULL
      }
    }
  )

  # Wanyi look here ----
  ## Two-sample Example ----
  observeEvent(
    eventExpr = c(input$level2, input$AltHypo2),
    handlerExpr = {
      ### Get Values from Wilcoxon ----
      ruralWilcoxon <- suppressWarnings(
        wilcox.test(
          x = rural, 
          mu = 2, 
          conf.level = input$level2/100,
          conf.int = T, 
          correct = T, 
          exact = F,
          alternative = input$AltHypo2
        )
      )
      urbanWilcoxon <- suppressWarnings(
        wilcox.test(
          x = urban, 
          mu = 1, 
          conf.level = input$level2/100,
          conf.int = T, 
          correct = T, 
          exact = F,
          alternative = input$AltHypo2
        )
      )
      
      ### Two sample box plot ----
      output$boxplot2 <- renderPlot(
        expr = {
          ggplot(
            data = siblingsData, 
            mapping = aes(
              y = area, 
              x = numOfSbls)) + 
            geom_boxplot(lwd = 1, 
                         fatten = 2) +
            theme_bw() +
            labs(
              title = "Boxplot of Number of Siblings by area",
              x = "Number of Siblings",
              y = NULL
            ) +
            theme(
              plot.caption = element_text(size = 18),
              text = element_text(size = 18),
              axis.title = element_text(size = 16),
              legend.position = "bottom",
              axis.text.y = element_text(angle = 90, hjust = 0.5)
            ) +
            geom_rect(
              fill = "red",
              alpha = 0.01,
              mapping = aes(
                ymin = 0.8,
                ymax = 1.2,
                xmin = ifelse(
                  test = ruralWilcoxon$conf.int[1] < 0,
                  yes = 0,
                  no = ruralWilcoxon$conf.int[1]),
                xmax = ruralWilcoxon$conf.int[2])
            ) +
            geom_rect(
              fill = "blue",
              alpha = 0.01,
              mapping = aes(
                ymin = 1.8,
                ymax = 2.2,
                xmin = ifelse(
                  test = urbanWilcoxon$conf.int[1] < 0,
                  yes = 0,
                  no = urbanWilcoxon$conf.int[1]),
                xmax = urbanWilcoxon$conf.int[2])
            )
        },
        alt = "Here should be the boxplot of number of siblings by areas"
      )
      
      ### Two sample dot plot ----
      output$dotplot2 <- renderPlot(
        expr = {
          ggplot(
            data = siblingsData,
            mapping = aes(
              x = numOfSbls, 
              fill = area, 
              color = area)
          ) + 
            theme_bw() +
            theme(
              plot.caption = element_text(size = 18),
              text = element_text(size = 18),
              axis.title = element_text(size = 16),
              legend.position = "bottom",
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              plot.margin = margin(t = 0, r = 5, b = 0, l = 20, unit = "pt")
            ) +
            labs(
              title = "Dotplot of Number of Siblings by area",
              x = "Number of Siblings",
              y = NULL
            ) +
            scale_fill_manual(values = c("red", "blue")) +
          geom_dotplot(
            binwidth = 0.15,
            method = "histodot",
            right = FALSE,
            binpositions = "all",
            alpha = 0.55,
            stackgroups = FALSE,
            position = position_dodge(width = 0.27)
          )
        },
        alt = "Here should be the dotplot of number of siblings by areas"
      )
      
      ### Get Two Sample Wilcoxon Results ----
      siblingsWilcoxon <- wilcox.test(
        formula = numOfSbls ~ area,
        data = siblingsData,
        paired = FALSE,
        conf.level = input$level2/100,
        conf.int = T, 
        correct = T, 
        exact = F,
        alternative = input$AltHypo2
      )
      
      ### Form Combined Plot 1 ----
      output$mixplot <- renderPlot(
        expr = {
          ggplot(
            data = difference, 
            mapping = aes(
              x = difference)) + 
            geom_boxplot(lwd = 1, 
                         fatten = 2, 
                         width = 200) +
            scale_x_continuous(
              breaks = seq.int(from = -8, 
                               to = 8, 
                               by = 2)
            ) +
            labs(
              title = paste("Plots of the differences of number of siblings",
                            "between areas for all (rural, urban) pairs"),
              x = NULL,
              y = NULL
            ) +
            geom_rect(
              fill = "purple",
              alpha = 0.007,
              mapping = aes(
                xmin = siblingsWilcoxon$conf.int[1],
                xmax = siblingsWilcoxon$conf.int[2],
                ymin = -80,
                ymax = 80
              )
            ) +
            theme_bw() +
            theme(
              plot.caption = element_text(size = 18),
              text = element_text(size = 18),
              axis.title = element_text(size = 16),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              plot.margin = margin(t = 0, r = 5, b = 0, l = 50, unit = "pt")
            )
        },
        alt = "Here should be the boxplot of differences of number of siblings between 
        areas for all (rural, urban) pairs"
      )
      
      ### Form Combined Plot 2 ----
      output$mixplot2 <- renderPlot(
        expr = {
          ggplot(
            data = difference, 
            mapping = aes(
              x = difference)) + 
            geom_histogram(
              fill = "skyblue",
              col = "black",
              boundary = 0,
              binwidth = 1
            ) +
            theme_bw() +
            scale_x_continuous(
              breaks = seq.int(
                from = -8, 
                to = 8, 
                by = 2)
            ) +
            theme(
              plot.caption = element_text(size = 18),
              text = element_text(size = 18),
              axis.title = element_text(size = 16),
            ) +
            labs(
              x = "rural - urban"
            ) 
        },
        alt = "Here should be the histogram of differences of number of siblings between 
        areas for all (rural, urban) pairs"
      )
    }
  )
  
  ## CI table two-sample ----
  
  # output$CItable2 <- renderTable({
  #   # ruralWilcoxon <- wilcox.test(rural, 
  #   #                        mu = 2, 
  #   #                        conf.level = input$level2/100,
  #   #                        conf.int = T, 
  #   #                        correct = F, 
  #   #                        exact = T,
  #   #                        alternative = input$AltHypo2)
  #   # urbanWilcoxon <- wilcox.test(urban, 
  #   #                        mu = 1, 
  #   #                        conf.level = input$level2/100,
  #   #                        conf.int = T, 
  #   #                        correct = F, 
  #   #                        exact = T,
  #   #                        alternative = input$AltHypo2)
  #   siblingsWilcoxon <- wilcox.test(
  #     formula = numOfSbls ~ area,
  #     data = siblingsData,
  #     paired = FALSE,
  #     conf.level = input$level2/100,
  #     conf.int = T, 
  #     correct = F, 
  #     exact = T,
  #     alternative = input$AltHypo2
  #   )
  #   
  #   ctable <- matrix(
  #     data = c(round(siblingsWilcoxon$conf.int[1],2), round(siblingsWilcoxon$conf.int[2],2),
  #              round(siblingsWilcoxon$p.value,2)),
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
  #   #   ctable <- matrix(c("rural", "urban","difference",round(ruralWilcoxon$conf.int[1],2), 
  #   #                      round(urbanWilcoxon$conf.int[1],2), round(siblingsWilcoxon$conf.int[1],2),
  #   #                      round(ruralWilcoxon$conf.int[2],2), round(urbanWilcoxon$conf.int[2],2),
  #   #                      round(siblingsWilcoxon$conf.int[2],2), round(ruralWilcoxon$p.value,2),
  #   #                      round(urbanWilcoxon$p.value,2), round(siblingsWilcoxon$p.value,2)), 
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
    eventExpr = c(input$CIcheckbox2, input$AltHypo2, input$level2),
    handlerExpr = {
      if (input$CIcheckbox2) {
        siblingsWilcoxon <- wilcox.test(
          formula = numOfSbls ~ area,
          data = siblingsData,
          paired = FALSE,
          conf.level = input$level2/100,
          conf.int = T, 
          correct = F, 
          exact = T,
          alternative = input$AltHypo2
        )
        
        ctable <- data.frame(
          lower = round(siblingsWilcoxon$conf.int[1],2),
          upper = round(siblingsWilcoxon$conf.int[2],2),
          p.value = if (input$AltHypo2 == "less") {
            as.numeric(substr(as.character(signif(siblingsWilcoxon$p.value,4)),1,4))
          }
          else {signif(siblingsWilcoxon$p.value,2)
          }
        )
        # print(ctable)
        names(ctable) <- c(paste0(input$level2,"% Lower Bound"),
                           paste0(input$level2,"% Upper Bound"), 
                           "p-value")
        
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
          options = list(
            responsive = TRUE,
            scrollX = TRUE,
            ordering = FALSE,
            paging = FALSE,
            lengthChange = FALSE,
            pageLength = 1,
            searching = FALSE,
            info = FALSE,
            columnDefs = list(
              list(visible = FALSE, targets = 0),
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
      "Click on one of the confidence intervals (on the right) to view the boxplot of
      the sample on the left."
    })
    
    output$sampleColors <- renderUI({
      "The shaded region on the boxplot represents the confidence interval 
      corresponding to the hilighted intervalon the right. It will change colors (red or blue) 
      depending on whether the confidence interval constructed from the sample 
      contains the population median."
    })
  })
  
  # Signed Rank part ----
  ## population mean plot with true mean
  output$popMedian <- renderPlot({
    pricePlot 
  })
  
  
  ### Generate 50 new samples ----
  Samples <- eventReactive(input$new, {
    temp <- data.frame() 
    for (i in 1:25) {
      temp <- rbind(
        temp,
        cbind(index = i, dplyr::slice_sample(airbnbData, n = input$nsamp))
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
              mapping = aes(
                yintercept = 56, 
                color = "zpop"),
              linewidth = 1.25,
              alpha = 1
            ) +
            coord_flip() +
            scale_size_manual(
              values = c("TRUE" = 1.5, 
                         "FALSE" = .7),
              guide = "none"
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
              values = c("TRUE" = 1, 
                         "FALSE" = .5),
              guide = "none"
            ) +
            labs(
              title = paste0(input$level3, "% Confidence Intervals for the Median"),
              x = NULL,
              y = "Prices ($ per day)"
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
                           dplyr::filter(
                             index == isolate(
                               selectedSample())),
                         mapping = aes(y = price),
                         # bins = 15,
                         col = "black",
                         lwd = 1, fatten = 2,
            ) +
            geom_rect(fill = OneSampleColor(),
                      alpha = 0.5,
                      data = Intervals() %>%
                        dplyr::filter(
                          index == isolate(
                            selectedSample())),
                      mapping = aes(
                        xmin=-0.2,
                        xmax=0.2,
                        ymin=lowerbound,
                        ymax=upperbound)
            ) +
            geom_hline(
              mapping = aes(
                yintercept = suppressWarnings(
                  wilcox.test((Samples() %>%
                                 dplyr::filter(
                                   index == isolate(selectedSample())))$price,
                              conf.level = 0.95,
                              conf.int = T)$estimate), 
                color = "Est"),
              linewidth = 1
            ) +
            geom_hline(
              mapping = aes(
                yintercept = 56, 
                color = "pop"),
              linewidth = 1
            ) +
            coord_flip() +
            labs(
              title = "Boxplot of Selected Sample",
              y = "Prices ($ per day)"
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
                "pop" = "Population Median"
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
      "Click on one of the confidence intervals (on the right) to view the boxplot of
      the sample on the left."
    })
    
    output$sampleColors2 <- renderUI({
      "The shaded region on the boxplot represents the confidence interval 
      corresponding to the hilighted intervals on the right. It will change colors (red or blue) 
      depending on whether the confidence interval constructed from the sample 
      contains the population median."
    })
  })
  
  ## population mean plot with true mean
  output$popMedian2 <- renderPlot({
    popularPlot1 
  })
  
  output$popMedian3 <- renderPlot({
    popularPlot2 
  })
  
  output$popMedian4 <- renderPlot({
    popularPlot3 
  })
  
  #### Generate 25 new samples ----
  west <- airbnbData2 %>%
    select(neighbourhood, availability_365) %>%
    filter(neighbourhood == "West Town")
  
  north <- airbnbData2 %>%
    select(neighbourhood, availability_365) %>%
    filter(neighbourhood == "Near North Side")
  
  samplesWest <- eventReactive(input$new2, {
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
  
  samplesNorth <- eventReactive(input$new2, {
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
    rbind(samplesNorth(), samplesWest())
  }
  )
  
  North <- eventReactive(input$new2, {
    samplesNorth() %>%
      rename(availability_north = availability_365,
             north = neighbourhood,
             i = index)
  }
  )
  
  West <- eventReactive(input$new2, {
    samplesWest() %>%
      rename(availability_west = availability_365,
             west = neighbourhood,
             j = index)
  }
  )
  
  sampleDifference <- eventReactive(input$new2, {
    cbind(North(), West()) %>%
      dplyr::mutate(difference = availability_north - availability_west)
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
              linewidth = 1.25,
              alpha = 1
            ) +
            coord_flip() +
            scale_size_manual(
              values = c("TRUE" = 1.5, "FALSE" = .7),
              guide = "none"
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
              guide = "none"
            ) +
            labs(
              title = paste0(input$level4, "% Confidence Intervals for the Median"),
              x = NULL,
              y = "# days available"
            ) +
            theme_bw() +
            ylim(-400,400) +
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
            geom_boxplot(sampleDifference() %>%
                           dplyr::filter(i == isolate(selectedSample2())),
                         mapping = aes(y = difference),
                         # bins = 15,
                         col = "black",
                         lwd = 1, fatten = 2
            ) +
            ylim(-400,400) +
            geom_rect(fill = OneSampleColor2(),
                      alpha = 0.5,
                      data = Intervals2() %>%
                        dplyr::filter(index == isolate(selectedSample2())),
                      mapping = aes(xmin=-0.2,
                                    xmax=0.2,
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
              linewidth = 1
            ) +
            geom_hline(
              mapping = aes(yintercept = 0, color = "pop"),
              linewidth = 1
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
