
# Exploratory_Analysis.R
library(shiny)
library(tidyverse)
library(janitor)
library(skimr)
library(naniar)
library(gridExtra)
library(GGally)
library(tidymodels)
library(pROC)
library(yardstick)
library(countrycode)
library(zoo)
library(rpart)
library(caret)
library(rpart.plot)

# =======================loading data
base_url <- "https://raw.githubusercontent.com/Nemuiyume/Data-Analyzor/main/datasets/"

# raw data
box_office <- read_csv(paste0(base_url, "raw/Box_office_data_2000_2024.csv"))
gdp <- read_csv(paste0(base_url, "raw/GDP_2020_2024.csv"))
population <- read_csv(paste0(base_url, "raw/Population_2000_2024.csv"))

# processed data
panel <- read_csv(paste0(base_url, "processed/panel.csv"))
panel_sel <- read_csv(paste0(base_url, "processed/panel_selected.csv"))

data_list <- list(
  "Box Office" = box_office,
  "GDP" = gdp,
  "Population" = population,
  "Panel" = panel,
  "Panel Selected" = panel_sel
)

# =======================data tools
num_cols <- function(df) names(df)[vapply(df, is.numeric, TRUE)]
ensure_factor_hilo <- function(x) factor(as.character(x), levels = c("High","Low"))

make_task_df <- function(df, feats) {
  df %>%
    mutate(box_class = ensure_factor_hilo(box_class)) %>%
    select(all_of(c("box_class", feats))) %>%
    drop_na()
}

# =======================UI
ui <- fluidPage(
  titlePanel("Dashboard (Box Office × GDP × Population)"),
  tabsetPanel(
    # ---- Tab1 ----
    tabPanel("Tab1 Data Overview",
             selectInput("dataset_choice", "Select Dataset：",
                         choices = names(data_list), selected = "Box Office"),
             h4("Summary"), verbatimTextOutput("summary_out"),
             h4("Str"), verbatimTextOutput("str_out"),
             h4("Head (10)"), tableOutput("head_out")
    ),
    # ---- Tab2 ----
    tabPanel("Tab2 Visualisation",
             h5("Single Varible"),
             fluidRow(
               column(4, selectInput("uni_var","Variable：",choices=setdiff(names(panel), c("Country_Name.x","Country_Name.y","Country_Code","Year")))),
               column(4, radioButtons("uni_geom","Plot Type",c("Histogram"="hist","Density"="density","Box"="box"))),
               column(4, checkboxInput("uni_fill_class","Color by box_class",FALSE))
             ),
             plotOutput("uni_plot",height=300),
             hr(),
             h5("Double Variables"),
             fluidRow(
               column(4, selectInput("bi_x","X",choices=setdiff(names(panel), c("Country_Name.x","Country_Name.y","Country_Code","Year")))),
               column(4, selectInput("bi_y","Y",choices=setdiff(names(panel), c("Country_Name.x","Country_Name.y","Country_Code","Year")))),
               column(4, checkboxInput("uni_fill_class2","Color by box_class",FALSE))
             ),
             radioButtons("bi_geom","Plot Type",c("Scatter"="point","Smooth"="smooth","Box"="box2")),
             plotOutput("bi_plot",height=300),
             hr(),
             h5("ggpairs"),
             selectizeInput("pair_vars","Variables(2-6)：",choices=setdiff(names(panel), c("Country_Name.x","Country_Name.y","Country_Code","Year")),multiple=TRUE),
             actionButton("draw_pairs","Draw"),
             plotOutput("pairs_plot",height=500)
    ),
    # ---- Tab3 ----
    tabPanel("Tab3 Modelling",
             h4("Target Variable: box_per_capita"),
             selectizeInput("features","Features",
                            choices=setdiff(num_cols(panel),c("Country_Name.x","Country_Name.y","Country_Code","Year")),
                            selected=head(setdiff(num_cols(panel),c("Country_Name.x","Country_Name.y","Country_Code","Year")),5),
                            multiple=TRUE,
                            options = list(plugins = list("remove_button"))),
             sliderInput("train_prop","Train Scale",0.6,0.9,0.8,0.05),
             actionButton("fit_btn","Train"),
             h4("Confusion Matrix"),
             fluidRow(column(6,h5("Decision Tree")),
                      column(6,h5("Logistic Regression"))),
             fluidRow(column(6,verbatimTextOutput("cm_dt")),
                      column(6,verbatimTextOutput("cm_lr"))),
             h4("ROC Curves"),
             plotOutput("roc_plot",height=300)
    )
  )
)

# =======================Server
server <- function(input, output, session) {
  # ---- Tab1 ----
  current_data <- reactive({
    data_list[[input$dataset_choice]]
  })
  
  output$summary_out <- renderPrint({
    summary(current_data())
  })
  
  output$str_out <- renderPrint({
    str(current_data())
  })
  
  output$head_out <- renderTable({
    head(current_data(), 10)
  })
  
  # ---- Tab2 ----
  # Single plot
  output$uni_plot <- renderPlot({
    req(input$uni_var, input$uni_geom)
    v <- input$uni_var
    
    if (input$uni_geom=="hist" && is.numeric(panel[[v]])) {
      if (input$uni_fill_class && "box_class" %in% names(panel)) {
        ggplot(panel,aes(x=.data[[v]],fill=box_class)) +
          geom_histogram(alpha=0.6,bins=30,position="identity") +
          geom_density(aes(y=after_stat(count)),alpha=0.2) +
          theme_minimal()
      } else {
        ggplot(panel,aes(x=.data[[v]])) +
          geom_histogram(bins=30,fill="violet",alpha=0.7) +
          geom_density(aes(y=after_stat(count)),alpha=0.3) +
          theme_minimal()
      }
      
    } else if (input$uni_geom == "density" && is.numeric(panel[[v]])) {
      if (input$uni_fill_class && "box_class" %in% names(panel)) {
        ggplot(panel, aes(x=.data[[v]], color=box_class, fill=box_class)) +
          geom_density(alpha=0.3) + theme_minimal()
      } else {
        ggplot(panel, aes(x=.data[[v]])) +
          geom_density(fill="steelblue", alpha=0.4) + theme_minimal()
      }
      
    } else if (input$uni_geom=="box" && is.numeric(panel[[v]]) && "box_class" %in% names(panel)) {
      ggplot(panel,aes(x=box_class,y=.data[[v]],fill=box_class)) +
        geom_boxplot() + coord_flip() + theme_minimal()
      
    } else {
      validate(need(FALSE,"Variable does not match graph type"))
    }
  })
  
  # Bivariate plot
  output$bi_plot <- renderPlot({
    req(input$bi_x,input$bi_y,input$bi_geom)
    x <- input$bi_x; y <- input$bi_y
    
    if (input$bi_geom == "point") {
      if (input$uni_fill_class2) {
        ggplot(panel, aes(x = .data[[x]], y = .data[[y]], color = box_class)) +
          geom_point(alpha = 0.7) + theme_minimal()
      } else {
        ggplot(panel, aes(x = .data[[x]], y = .data[[y]])) +
          geom_point(alpha = 0.7) + theme_minimal()
      }
      
    } else if (input$bi_geom == "smooth") {
      if (input$uni_fill_class2) {
        ggplot(panel, aes(x = .data[[x]], y = .data[[y]], color = box_class)) +
          geom_point(alpha = 0.3) +
          geom_smooth(se = FALSE) + theme_minimal()
      } else {
        ggplot(panel, aes(x = .data[[x]], y = .data[[y]])) +
          geom_point(alpha = 0.3) + geom_smooth(se = FALSE) + theme_minimal()
      }
      
    } else if (input$bi_geom == "box2" && !is.numeric(panel[[x]]) && is.numeric(panel[[y]])) {
      if (input$uni_fill_class2) {
        ggplot(panel, aes(x = .data[[x]], y = .data[[y]], fill = box_class)) +
          geom_boxplot(outlier.alpha = 0.5) + coord_flip() + theme_minimal()
      } else {
        ggplot(panel, aes(x = .data[[x]], y = .data[[y]])) +
          geom_boxplot(outlier.alpha = 0.5) + coord_flip() + theme_minimal()
      }
    }
  })
  
  # Pairs plot
  pairs_dat <- eventReactive(input$draw_pairs,{
    req(input$pair_vars)
    panel %>% select(all_of(input$pair_vars)) %>% drop_na() %>% head(500)
  })
  output$pairs_plot <- renderPlot({
    req(pairs_dat()); pairs(pairs_dat())
  })
  
  # ---- Tab3 ----
  trained<-eventReactive(input$fit_btn,{
    df<-make_task_df(panel,input$features)
    sp<-initial_split(df,prop=input$train_prop,strata=box_class)
    train<-training(sp); test<-testing(sp)
    
    # Decision tree
    fit_dt<-rpart(box_class~.,data=train,method="class")
    pred_dt<-predict(fit_dt,test,type="class")
    prob_dt<-predict(fit_dt,test,type="prob")[,"High"]
    
    # Logistic regression
    fit_lr<-glm(box_class~.,data=train,family=binomial())
    prob_lr<-predict(fit_lr,test,type="response")
    pred_lr<-factor(ifelse(prob_lr<0.5,"High","Low"),levels=c("High","Low"))
    
    list(
      cm_dt=confusionMatrix(pred_dt,test$box_class,positive="High"),
      cm_lr=confusionMatrix(pred_lr,test$box_class,positive="High"),
      roc_dt=roc(test$box_class,prob_dt,levels=c("Low","High")),
      roc_lr=roc(test$box_class,prob_lr,levels=c("Low","High")),
      vi_dt=fit_dt$variable.importance,
      coefs=coef(fit_lr)[-1]
    )
  })
  
  output$cm_dt<-renderPrint({req(trained());trained()$cm_dt})
  output$cm_lr<-renderPrint({req(trained());trained()$cm_lr})
  
  output$roc_plot<-renderPlot({
    req(trained()); r<-trained()
    plot(r$roc_dt,col="blue"); plot(r$roc_lr,add=TRUE,col="red")
    legend("bottomright",c("DT","LR"),col=c("blue","red"),lty=1)
  })
  
}

shinyApp(ui,server)