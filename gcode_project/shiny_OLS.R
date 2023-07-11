

library(tidyverse)
library(tidyverse)
library(ggeffects)
library(ggplot2)
library(shiny)
library(DT)
#

n1 = 500; n2 = 500; intercept1 = 300; intercept2 = 300; beta1 = 20; beta2 = 20; sd1 = 10; sd2 = 10


#
f = function(n1 = 500, n2 = 500, 
             intercept1 = 300, intercept2 = 300,  beta1 = 20, beta2 = 20, sd1 = 10, sd2 = 10){
  
  #
  x1 = rnorm(n1)
  d1 = rbinom(n1, 1, plogis(x1))
  #
  
  #
  x2 = rnorm(n2)
  d2 = rbinom(n2, 1, plogis(x2))
  #
  
  #
  y1 = intercept1 + d1*beta1 + rnorm(n1, sd = sd1)
  y2 = intercept2 + d2*beta2 + rnorm(n2, sd = sd2)
  #
  
  #
  a = data.frame(id = 1:n1, x = x1, d = d1, period = 0, y = y1)
  b = data.frame(id = 1:n2, x = x2, d = d2, period = 1, y = y2)
  
  df = bind_rows(a,b)
  #
  return(df)
}
#

#
f_graph = function(df){
  
  #
  #pp_spread = df %>% 
  #  group_by(period, d) %>% 
  #  dplyr::summarise(y = mean(y)) %>% 
  #  spread(d, y) %>% 
  #  dplyr::mutate(diff = `1`-`0`)
  #
  
  t0 = tidy(t.test(y~d, data = df, subset = period == 0))
  t1 = tidy(t.test(y~d, data = df, subset = period == 1))
  
  t_test = bind_rows(t0, t1) %>% select(1:5) %>% mutate(periods = c(0,1))
  t_test = select(t_test, periods, everything())
  #
  
  #
  m0 = lm(y ~ period, df)
  m1 = lm(y ~ period + d, df)
  m2 = lm(y ~ period * d, df)
  #
  
  #
  ex = expand.grid(period = c(0,1), d = c(0,1))
  
  pred0 = predict(m0, ex, se.fit = F)
  pred1 = predict(m1, ex, se.fit = F)
  pred2 = predict(m2, ex, se.fit = F)
  
  pp = data.frame(ex, pred0, pred1, pred2)
  #
  
  #
  mydf_m1 <- ggpredict(m1, terms = c("period", 'd'))
  mydf_m2 <- ggpredict(m2, terms = c("period", 'd'))
  mydf1 = data.frame(mydf_m1)
  mydf2 = data.frame(mydf_m2)
  
  #
  mydf1$model = 'Additive'
  mydf2$model = 'Multiplicative'
  
  #
  bd = bind_rows(mydf1, mydf2)
  #
  
  #
  fig = ggplot(mydf2, aes(x, predicted, colour = group)) +
    geom_line() +
    # geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = .1) + 
    #facet_wrap(~model) + 
    scale_x_continuous(breaks = c(0, 0.5,1), labels = c(1950, 1975, 2020)) + 
    theme_minimal(base_size = 16)
  #
  
  return(list(list(m0, m1, m2), models_comparison = pp, diff_mult = t_test, bd = bd, fig = fig, tidym1 = tidy(m1), tidy = tidy(m2)))
}


#df = f(intercept1 = 100, intercept2 = 120, beta1 = 0, beta2 = 20)
#f_graph(df = df)$models_comparison
#f_graph(df = df)$diff_mult
#

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    

    # Sidebar panel for inputs ----
    sidebarPanel(
      
      helpText("Create demographic maps with 
               information from the 2010 US Census."),
 
      numericInput(inputId = "n1", label = 'n1', value = 500), 
      numericInput(inputId = "n2", label = 'n2', value = 500), 
      
      numericInput(inputId = "intercept1", label = 'inter1', value = 90), 
      numericInput(inputId = "intercept2", label = 'inter2', value = 120), 
      
      sliderInput(inputId = "beta1",
                  label = "Beta1",
                  min = 0,
                  max = 50,
                  value = 10),
      
      sliderInput(inputId = "beta2",
                  label = "Beta2",
                  min = 0,
                  max = 50,
                  value = 10),
      
      tableOutput("table"),
      
      textInput("text", h3("Text input"), 
                value = "Enter text...")
      
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "ggplot"),
      p("H0 : Y[d = 1] - Y[d = 0] = 0"),
      p("H1 : Y[d = 1] - Y[d = 0] ! 0"),
      # Output: Histogram ----
      tableOutput("box"),
      tableOutput("mult")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  #
  output$ggplot <- renderPlot({ 
    
    df = f(n1 = input$intercept1,
           n2 = input$intercept2,
           intercept1 = input$intercept1,
           intercept2 = input$intercept2, 
           beta1 = input$beta1, 
           beta2 = input$beta2)
    f_graph(df = df)$fig
    })
  
  #
  output$mult <- renderTable({ 
    df = f(n1 = input$intercept1,
           n2 = input$intercept2,
           intercept1 = input$intercept1,
           intercept2 = input$intercept2, 
           beta1 = input$beta1, 
           beta2 = input$beta2)
    f_graph(df = df)$tidy
  })
  output$box <- renderTable({
    df = f(n1 = input$intercept1,
      n2 = input$intercept2,
      intercept1 = input$intercept1,
      intercept2 = input$intercept2, 
      beta1 = input$beta1, 
      beta2 = input$beta2)
    f_graph(df = df)$diff_mult
  })
}

#
shinyApp(ui = ui, server = server)
#
