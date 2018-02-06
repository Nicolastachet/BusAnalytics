df = read.table("bank-full2.csv",header=TRUE,sep=";")
df_light = read.table("bank.csv",header=TRUE,sep=";")

df$result<-as.factor(df$result)

set.seed(123)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  Arbre <- reactive({
    df_train = df
    tpsArbreDebut <<- proc.time()
    # arbre <- rpart(df_train$result~., data = df_train, parms = list(split = "information",prior = c(input$threshold,1-input$threshold)), control = rpart.control(minsplit = 2,maxdepth = 12,cp = 0))
    # arbreSimplifie <- prune(arbre,cp = arbre$cptable[which.min(arbre$cptable[,4]),1])
    tree_bank <- rpart(df_train$result ~ ., data = df_train, parms = list(split = "information",prior = c(0.117,0.883)), control = rpart.control(minsplit = 2,maxdepth = 13,cp = 0))
    tree_simple <- prune(tree_bank,cp = tree_bank$cptable[which.min(tree_bank$cptable[,4]),1])
    tpsArbreFin <<- proc.time()
    tree_simple
  })
  
  GetTpsArbre <- reactive({
    tpsArbreFin - tpsArbreDebut
  })
  
  LG <- reactive({
    df_train = df
    tpsLGDebut <<- proc.time()
    # lgmodel=glm(df_train$result~., data=df_train, family= binomial())
    df2 <- df
    df2$result <- ifelse(df2$result == "yes", 1, 0)
    x = model.matrix(result~., df2)[,-1]
    ModelLogReg = cv.glmnet(x, df2$result, family="binomial", alpha=0)
    tpsLGFin <<- proc.time()
    ModelLogReg
  })
  
  GetTpsLG <- reactive({
    tpsLGFin - tpsLGDebut
  })
  
  KNN <- reactive({
    bank_full2 = model.matrix(y~., data=df_light)
    bank_full2 <- bank_full2[,-1]
    bank_full_scaled = scale(x=bank_full2, center = TRUE, scale = TRUE)
    fit = knn.cv(bank_full_scaled, bank_full$y, k = 7, prob = TRUE)
    prob = ifelse(fit == "yes", attributes(fit)$prob, 1 - attributes(fit)$prob)
    prob
  })
  
  CM <- reactive({
    if (input$country == "U.S."){
      fp = -0.6
    }
    else if (input$country == "Mid-cost countries") {
     fp = -2.2/6
    }
    else
    {
      fp = -1/6
    }
    
    costs = c(c(0, 0), c(fp * input$average_length, input$price - fp * input$average_length))
    costs
  })
  
  
  Donnees <- reactive({
    client <- c(as.integer(as.character(input$age)),input$job,input$marital,input$education,input$default,
                as.integer(input$balance),input$housing,input$loan,input$contact,as.integer(input$day),input$month,
                as.integer(input$duration),as.integer(input$campaign),as.integer(input$pdays),as.integer(input$previous),input$poutcome)
    client <- as.data.frame(t(client))
    colnames(client) <- c("age","job","marital","education","default","balance","housing","loan","contact","day","month","duration","campaign","pdays","previous","poutcome")
    client$age <- as.integer(as.character(client$age))
    client$job <- factor(as.character(input$job), levels = c("admin.", "blue-collar", "entrepreneur", "housemaid", "management", "retired", "self-employed", "services", "student", "technician", "unemployed", "unknown"))
    client$marital <- factor(as.character(input$marital), levels = c("divorced", "married","single"))
    client$education <- factor(as.character(input$education), levels = c("primary", "unknown","secondary","tertiary"))
    client$default <- factor(as.character(input$default), levels = c("no", "yes"))
    client$balance <- as.integer(as.character(client$balance))
    client$housing <- factor(as.character(client$housing), levels = c("no", "yes"))
    client$loan <- factor(as.character(client$loan), levels = c("no", "yes"))
    client$contact <- factor(as.character(client$contact), levels = c("cellular", "unknown","telephone"))
    client$day <- as.integer(as.character(client$day))
    client$month <- factor(as.character(client$month), levels = c("apr", "jan","feb","mar","may","jun","jul","aug","sep","oct","nov","dec"))
    client$duration <- as.integer(as.character(client$duration))
    client$campaign <- as.integer(as.character(client$campaign))
    client$pdays <- as.integer(as.character(client$pdays))
    client$previous <- as.integer(as.character(client$previous))
    client$poutcome <- factor(as.character(client$poutcome), levels = c("failure","success","other","unknown"))
    client$result <- as.integer(1)
    is.integer(client$age)
    print(client)
    client
  })
  
  output$Tree <- renderPlot({
    arbre = Arbre()
    arbre <- prune(arbre, cp = 1e-02)
    rpart.plot(arbre,extra = 1, box.col=ifelse(arbre$frame$yval == 1, 'grey', rgb(0.725,0.027,0.176)))
  })
  
  output$LG_summary <- renderPrint({
    fit <- LG()
    coef(fit)
  })
  
  tree_confusion_matrix <- reactive({
    arbre = Arbre()
    df_test = df
    pred <- predict(arbre,df_test,type=c("vector"))
    tableau = table(truth = df_test$result, predict = pred)
    colnames(tableau) <- c("Predicted = No", "Predicted = Yes")
    rownames(tableau) <- c("Actual = No", "Actual = Yes")
    tableau
  })
  
  output$TreeMatrix <- renderDataTable({
    tableau <- tree_confusion_matrix() 
    tableau <- cbind(rownames(tableau), tableau)
    tableau
  })
  
  output$profit_tree <- renderText({
    tableau <- tree_confusion_matrix()
    costs <- CM()
    c = sum(tableau*costs)
    (paste("<center><h3><b>", "$", format(round(as.numeric(c), 0), nsmall=0, big.mark=","), "</center></h3></b>"))
  })
  
  lg_confusion_matrix <- reactive({
    lgmodel = LG()
    df_test <- df
    x = model.matrix(result~., df_test)[,-1]
    pred <- predict(lgmodel,x, type="response")
    # 0.07 is the best threshold
    pred[pred>0.07] <- 1
    pred[pred<0.07] <- 0

    tableau = table(truth = df_test$result, predict = pred)
    colnames(tableau) <- c("Predicted = No", "Predicted = Yes")
    rownames(tableau) <- c("Actual = No", "Actual = Yes")
    tableau
  })
  
  output$LGMatrix <- renderDataTable({
    tableau <- lg_confusion_matrix()
    tableau <- cbind(rownames(tableau), tableau)
    tableau
  })
  
  output$profit_lg <- renderText({
    tableau <- lg_confusion_matrix()
    costs <- CM()
    c = sum(tableau*costs)
    (paste("<center><h3><b>", "$", format(round(as.numeric(c), 0), nsmall=0, big.mark=","), "</center></h3></b>"))
  })
  
  KNN_confusion_matrix <- reactive({
    pred = KNN()
    cost = CM()
    # alpha = input$threshold
    profitPerThreshold = vector("numeric",100)
    alpha = seq(min(pred), max(pred), length.out = 100)
    for (s in 2:99){ 
      pred2 = ifelse(pred > alpha[s], 1, 0)
      tableau = table(truth = df_light$y, predict = pred2)
      c = sum(tableau*costs)
      profitPerThreshold[s] = c
    }
    alpha = alpha[which.max(profitPerThreshold[2:99])+1]
    pred2 = ifelse(pred > 0.07, 1, 0)
    tableau = table(truth = df_light$y, predict = pred2)
    colnames(tableau) <- c("Predicted = No", "Predicted = Yes")
    rownames(tableau) <- c("Actual = No", "Actual = Yes")
    tableau <- tableau * c(c(10,10), c(10,10))
    tableau
  })
  
  output$KNNmatrix <- renderDataTable({
    tableau <- KNN_confusion_matrix()
    tableau <- cbind(rownames(tableau), tableau)
  })
  
  output$profit_knn <- renderText({
    tableau <- KNN_confusion_matrix()
    costs <- CM()
    c = sum(tableau*costs)
    (paste("<center><h3><b>", "$", format(round(as.numeric(c), 0), nsmall=0, big.mark=","), "</center></h3></b>"))
  })
  
  #Prediction
  output$PredictionArbre <- renderTable({
    df_test <- df_light
    client = Donnees()
    new_client <- t(as.matrix(model.matrix(result~., data = client)[,-1]))
    print(new_client)

    arbre = Arbre()
    lgmodel = LG()
    predArbre <- predict(arbre,client)[2]
    predLG <- predict(lgmodel,newx=new_client, type="response")
    
    bank_full2 = model.matrix(y~., data=df_light)
    bank_full2 <- bank_full2[,-1]
    bank_full_scaled = scale(x=rbind(bank_full2, new_client), center = TRUE, scale = TRUE)
    predKNN = knn(bank_full_scaled[-length(bank_full_scaled[,1]),], bank_full_scaled[length(bank_full_scaled[,1]),] ,df_light$y,7, prob = TRUE)
    print(predKNN)
    probKNN = ifelse(predKNN == "yes", attributes(predKNN)$prob, 1 - attributes(predKNN)$prob)
    
    mat = matrix(c(predArbre > 0.5,predLG > 0.07, probKNN > 0.07),ncol = 1)
    rownames(mat) <- c("Decision Tree","Logistic Regression", "k-Nearest Neighbors")
    colnames(mat) <- c("Is the client likely to buy the advertised product?")
    mat
  }, rownames = TRUE, digits = 3, align="c")
  
})