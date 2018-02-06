library(shiny)
library(glmnet)
library(shiny)
library(rpart)
library(rpart.plot)
library(readr)
library(class)
library(MLmetrics)

shinyUI(navbarPage("Business Analytics Project",
  
  # Application title
  tabPanel("Home",fluidPage(
      img(src = "columbia.png", height = 100, align = "left"),
      p(height = 100),
      h1("Business Analytics Project", align = 'center'),
      h3("Group 5", align = 'center')),
      p(height = 100),
      h4("Presentation"),
      p("This shiny app is based on a dataset comming from DoMinho University in Portugal. The final goal is to predict whether a given 
        client is likely to subscribe or not to the advertied product."),
      p("The marketing campaigns were based on phone calls. Often, more than one contact to the same client was required, 
      in order to access if the product would be (or not) subscribed."),
      p("The adversised product is a bank term deposit"),
      p("For each phone call, 16 attributes of the client and the final outcome of the call were recorded. We then want to use these
        attributes to create a model that will take these attributes as an input and return the probability that this client will subscribe to the bank term deposit."),
      p("This dataset is available here"),
      a("[Moro et al., 2011] S. Moro, R. Laureano and P. Cortez. Using Data Mining for Bank Direct Marketing: An Application of the CRISP-DM Methodology. 
  In P. Novais et al. (Eds.), Proceedings of the European Simulation and Modelling Conference - ESM'2011, pp. 117-121, Guimarães, Portugal, October, 2011. EUROSIS.", href = "https://archive.ics.uci.edu/ml/datasets/Bank+Marketing", align = "center")),

  tabPanel("Predictive Model",
sidebarLayout(
  sidebarPanel(
    #   Predicted    No    Yes
    #No           0       -100
    #Yes          0       1000  
    #
    selectInput("country", label = "Country in which the callcenter is located", choices = c("U.S.", "Mid-cost countries", "Low-cost countries")),
    numericInput("average_length", label = "Average length of the call in minutes", value = 7),
    numericInput("price", label = "Profit when selling this product", value = 30)
  ),
    
    mainPanel(
      h2("Cost Matrix"),
      p("Using the tree model, we get a profit of"),
      htmlOutput("profit_tree"),
      p("Using the logistic regression, we get a profit of"),
      htmlOutput("profit_lg"),
      p("Using k-Nearest Neighbors, we get a profit of"),
      htmlOutput("profit_knn"),
      h2('Summary'),
      h3("Decision Tree"),
      plotOutput("Tree"),
      dataTableOutput("TreeMatrix"),
      h3("Logistic Regression"),
      verbatimTextOutput("LG_summary"),
      dataTableOutput("LGMatrix"),
      h3("K-Nearest Neighbors"),
      dataTableOutput("KNNmatrix")
    )
  )),
  tabPanel("Prediction",
           sidebarPanel( 
            h3("Client characteristics"),
            h4("Personnal characteristics"),
            sliderInput("age","Age",value = 30,min = 18, max = 100),
            selectInput("job","Job",choices=list("admin.","blue-collar","unemployed","management","housemaid","entrepreneur","student","self-employed","retired","technician","services","unknown")),
            selectInput("marital","Martial Status", choices = list("married","divorced","single")),
            selectInput("education","Education Level", choices = list("secondary","primary","tertiary","unknown")),
            radioButtons("default","Has credit in default?", choices = list("yes","no")),
            sliderInput("balance","Average yearly balance", value= 1360, min = -8000, max = 100000),
            radioButtons("housing","has housing loan?", choices = list("yes","no")),
            radioButtons("loan","has personal loan?", choices = list("yes","no")),
            h4("Durant la campagne marketing en cours"),
            selectInput("contact","contact communication type", choices = list("telephone","cellular","unknown")),
            sliderInput("day","last contact day of the month", value = 15,min = 1, max = 31),
            selectInput("month","last contact month of year", choices = list("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")),
            sliderInput("duration","last contact duration",value = 260, min = 0, max = 5000),
            h4("Données sur la campagne marketing précédente"),
            sliderInput("campaign","number of contacts performed during this campaign and for this client",value = 3,min = 1, max = 70),
            sliderInput("pdays","number of days that passed by after the client was last contacted from a previous campaign", value = 40, min = -1, max = 900),
            sliderInput("previous","number of contacts performed before this campaign and for this client",value = 1, min = 0, max = 300),
            radioButtons("poutcome","outcome of the previous marketing campaign", choices = list("failure","success","other","unknown"))
           ),
          mainPanel(
            h3("Prediction"),
            p("Using the characteristics on the left, we use the predictive model to predict the likelihood that this client
              will subscribe to the advertised product"),
            tableOutput("PredictionArbre")
          ), rownames = TRUE, digits = 0, align="c")
))