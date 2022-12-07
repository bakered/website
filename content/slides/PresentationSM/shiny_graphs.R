
library(ggplot2)
library(shiny)
library(tidyr)
library(dplyr)
library(gridExtra)
library(ggrepel)


rm(list=ls())

cbp1 = c("#999999", "#E69F00", "#56B4E9", "#009E73",
         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

a = 1
b = 1
sigma.c = sqrt(a^2 + b^2)
t= 1
adjustment = 1-pnorm(t, mean = 0, sd=sqrt((1+sigma.c^2))) #(1-p(i.j.signal.dist)(p))

(1 / (sigma.c * sqrt(1 + (1/sigma.c)^2))) * dnorm(-t / (sigma.c * sqrt(1 + (1/sigma.c)^2))) / adjustment

(1 / (sigma.c * sqrt(1 + (1/sigma.c)^2))) * dnorm(-t / (sigma.c * sqrt(1 + (1/sigma.c)^2))) / adjustment


### set up fluidpage
ui <- fluidPage(
  titlePanel("Model"),
  
  sidebarLayout(
    
    sidebarPanel( 
      checkboxGroupInput("plot.what", "Show Function:", choices = c("Prior Value" = "prior.value",
                                                                    "Signal j" = "signal.j",
                                                                    "Posterior" = "posterior")),
      checkboxGroupInput("options", "Options:", choices = c("PDF Graph" = "pdfgraph",
                                                            "Threshold" = "threshold",
                                                            "E Value Graph" = "EVgraph",
                                                            "Both Buyer and Seller" = "both",
                                                            "i is Buyer" = "buyer"
                                                            )),
      sliderInput("t", "Define Threshold:", min = -10, max = 10, value = 0, step = 0.01),
      numericInput("sigma.i", "Define sd of i's signal:",  value = 1, step = 0.01),
      numericInput("sigma.s", "Define sd of (dis)similarity:",  value = 1, step = 0.01),
      numericInput("sigma.j", "Define sd of j's signal:",  value = 1, step = 0.01),
      numericInput("min.x", "from-x-axis:",value = -10),
      numericInput("max.x", "to-x-axis:", value = 10),
      numericInput("sensitivity", "Define sensitivity:", value = 200, step = 10)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Distribution", plotOutput("pdfPlot")),
                  tabPanel("Best Response", plotOutput("BRPlot"))
      )
    )
  )
)


server <- function(input, output) {
  
  
  output$pdfPlot <- renderPlot({
    
    sigma.c = sqrt(input$sigma.s^2 + input$sigma.j^2)
    do.posterior = "posterior" %in% input$plot.what
    do.prior = "prior.value" %in% input$plot.what
    do.signal.j = "signal.j" %in% input$plot.what
    do.threshold = "threshold" %in% input$options
    do.buyer = "buyer" %in% input$options
    do.EVgraph = "EVgraph" %in% input$options
    do.pdfgraph = "pdfgraph" %in% input$options
    do.both = "both" %in% input$options
    signal.i = 0
    
    ## define 'error function'
    erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
    ## (see Abramowitz and Stegun 29.2.29)
    
    ## function f(x.) is pdf of posterior value of item.
    posterior.pdf = function(x., t., signal.i., sigma.i., sigma.c., scaler.=1){
      numerator = (0.5 - 0.5*erf((t.-x.) / (sigma.c.*sqrt(2)))) *
        ((1 / sqrt(2*pi*sigma.i.^2))* exp(-(x. - signal.i.)^2 / (2*sigma.i.^2))) 
      return(numerator/scaler.)
    }
    
    
    #min.val and max.val for which values of x to calculate
    min.val = qnorm(0.00001, mean = signal.i, sd=(sqrt(input$sigma.i^2 + sigma.c^2)))
    max.val = qnorm(0.99999, mean = signal.i, sd=(sqrt(input$sigma.i^2 + sigma.c^2)))
    distr.table = data.frame(x.d = seq(from = min.val, to = max.val, length.out = input$sensitivity))
    
    #reflect in y axis if buyer
    if(do.buyer){distr.table$x.d = signal.i - distr.table$x.d}
    #plot the prior distribution
    if(do.prior){
      distr.table$prior.value = sapply(distr.table$x.d, FUN = function(x){dnorm(x, mean = signal.i, sd=input$sigma.i)})
    }
    #plot the posterior distribution
    if(do.buyer){t = -input$t} else {t = input$t}
    if(do.posterior){
      if(do.threshold){
        distr.table$raw.pdf = sapply(distr.table$x.d, FUN = function(x.){posterior.pdf(x = x., t.= t, signal.i.=signal.i, sigma.i.=input$sigma.i, sigma.c.=sigma.c)})
        scaler = abs(distr.table$x.d[input$sensitivity] - distr.table$x.d[1]) * sum(distr.table$raw.pdf) / input$sensitivity
        # scaler = integrate(f = posterior.pdf,  
        #                    lower = input$min.x, #-Inf,
        #                    upper = input$max.x, #Inf,
        #                    t.=input$t, signal.i.=signal.i, sigma.i.=input$sigma.i, sigma.c.=sigma.c, scaler.=1)$value
        distr.table$posterior = distr.table$raw.pdf / scaler
      } else {
        distr.table$posterior = distr.table$prior.value
      }
      E.posterior = sum(distr.table$x.d * distr.table$posterior / sum(distr.table$posterior))
      print("Eposterior")
      print(E.posterior)
    }
    
    
    #signal j pdf
    if(do.signal.j){
      distr.table$signal.j = sapply(distr.table$x.d, FUN = function(x){dnorm(x, mean = signal.i, sd=sqrt(input$sigma.i^2 + sigma.c^2))})
      if(do.threshold){
        distr.table$signal.j[distr.table$x.d < t] = 0
        scaler.signal.j = abs(distr.table$x.d[input$sensitivity] - distr.table$x.d[1]) * sum(distr.table$signal.j) / input$sensitivity
        distr.table$signal.j = distr.table$signal.j / scaler.signal.j
      }
    }
    #reflect back for plotting
    if(do.buyer){
      distr.table$x.d = signal.i - distr.table$x.d 
      if(do.posterior){
        E.posterior = signal.i - E.posterior
      }
    }
    
    #data for ggplot
    col.select = which(colnames(distr.table) %in% input$plot.what)
    LD = distr.table %>% gather(key = distributions, value = density, col.select)
    
    if(do.pdfgraph){
      plot1 = ggplot(data=LD, aes(x=x.d, y= density)) + 
        geom_area(alpha=0.2, position = "identity", aes(fill=distributions)) +
        geom_line(aes(colour=distributions)) +
        lims(x=c(input$min.x, input$max.x))+
        scale_colour_manual(name = NULL,
                            breaks=c("posterior", "signal.j", "prior.value"),
                            values = c("posterior" = cbp1[2], 
                                       "signal.j" = cbp1[3], 
                                       "prior.value" = cbp1[4]),
                            labels = c("Posterior", "j Signal", "Prior Value")) +
        scale_fill_manual(name = NULL,
                          breaks=c("posterior", "signal.j", "prior.value"),
                          values = c("posterior" = cbp1[2], 
                                     "signal.j" = cbp1[3], 
                                     "prior.value" = cbp1[4]),
                          labels = c("Posterior", "j Signal", "Prior Value")) +
        theme_classic() + 
        labs(title=NULL, x="Value", y="Density") +
        theme(legend.justification=c(1,1), legend.position=c(1,1))
      if(do.posterior){plot1 = plot1 + geom_vline(aes(xintercept = E.posterior), colour = cbp1[2], show.legend = T, linetype = "dashed")} 
      if(do.threshold){plot1 = plot1 + geom_vline(aes(xintercept = input$t), show.legend = F)}
    }
    
    ### original version for i signal = 1
    #EValue = function(p, s, d) dnorm(p/sqrt(1+s^2))/((1-pnorm(p/sqrt(1+s^2)))*sqrt(1+s^2))
    ### new version
    EValue = function(p, s, d) {d^2 * dnorm((p)/sqrt(d^2+(s)^2)) / ((1-pnorm((p)/sqrt(d^2+(s)^2)))*sqrt(d^2+(s)^2))}
    ### different version
    #EValue2 = function(t, sigma.c, sigma.i){
    #  adjustment = 1-pnorm(t, mean = 0, sd=sqrt((1+sigma.c^2))) #(1-p(i.j.signal.dist)(p))
    #  (1 / (sigma.c * sqrt(1 + (1/sigma.c)^2))) * dnorm(-t / (sigma.c * sqrt(1 + (1/sigma.c)^2))) / adjustment
    #}
    
    print("Eval")
    print(EValue(input$t, sigma.c, input$sigma.i))
    
    if(do.EVgraph){
      plot2 = ggplot(data.frame(x = c(input$min.x, input$max.x)), aes(x))
      if(do.both){
        plot2 = plot2 + stat_function(fun = function(x) -EValue(-x, s=sigma.c, d = input$sigma.i), aes(colour="expected.val.buyer"), size =1.5)
        plot2 = plot2 + stat_function(fun = function(x) EValue(x, s=sigma.c, d = input$sigma.i), aes(colour="expected.val"), size =1.5)
      } else if(do.buyer){
        plot2 = plot2 + stat_function(fun = function(x) -EValue(-x, s=sigma.c, d = input$sigma.i), aes(colour="expected.val.buyer"), size =1.5)
        #plot2 = plot2 + stat_function(fun = function(x) -EValue2(-x, sigma.c, sigma.i = input$sigma.i), aes(colour="expected.val.buyer"), size =1.5)
        
      } else {
        plot2 = plot2 + stat_function(fun = function(x) EValue(x, s=sigma.c, d = input$sigma.i), aes(colour="expected.val"), size =1.5)
        #plot2 = plot2 + stat_function(fun = function(x) EValue2(x, sigma.c, sigma.i = input$sigma.i), aes(colour="expected.val"), size =1.5)
        
      }
      if(do.buyer){plot2y = 0; plot2ymin = -input$max.x/2} else {plot2y = input$max.x/2; plot2ymin = 0}
      if(do.both){plot2y = input$max.x/2; plot2ymin = -input$max.x/2}
      plot2 = plot2 +
        geom_abline(slope=1, linetype=2) +
        geom_abline(slope=1/2, linetype=2) +
        geom_hline(yintercept = 0, linetype=2) + 
        geom_vline(xintercept = 0, linetype=2) +
        lims(x = c(input$min.x, input$max.x),
             y = c(plot2ymin, plot2y)) +
        scale_colour_manual(name = NULL,
                            breaks=c("expected.val", "expected.val.buyer"),
                            values = c("expected.val" = cbp1[7], 
                                       "expected.val.buyer" = cbp1[8]),
                            labels = c("E Value Seller", "E Value Buyer")) +
        theme_classic() +
        labs(title=NULL, x="Threshold", y="Expected Value") +
        theme(legend.justification=c(1,1), legend.position=c(1,1))
      if(do.threshold){plot2 = plot2 + geom_vline(aes(xintercept = input$t), show.legend = F)}
    }
    
    
    if(exists("plot1") & !exists("plot2")){
      plot1
    } else if(!exists("plot1") & exists("plot2")) {
      plot2
    } else {
      grid.arrange(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "first"))
    }
  }, height = 500)
  output$BRPlot <- renderPlot({
    a=input$sigma.i  # sigma i
    b=input$sigma.j  # sigma j
    c=input$sigma.s  #sigma c
    
    F.func.i = function(x,y) {
      if(!is.na(((a^2) / sqrt(a^2+b^2+c^2)) * dnorm((x+y)/sqrt(a^2+b^2+c^2)) / (1 - pnorm((x+y)/sqrt(a^2+b^2+c^2))))){
        return(((a^2) / sqrt(a^2+b^2+c^2)) * dnorm((x+y)/sqrt(a^2+b^2+c^2)) / (1 - pnorm((x+y)/sqrt(a^2+b^2+c^2))))
      }
      } 
    
    F.func.j = function(x,y) {
      if(!is.na(((b^2) / sqrt(a^2+b^2+c^2)) * dnorm((x+y)/sqrt(a^2+b^2+c^2)) / (1 - pnorm((x+y)/sqrt(a^2+b^2+c^2))))){
        return(((b^2) / sqrt(a^2+b^2+c^2)) * dnorm((x+y)/sqrt(a^2+b^2+c^2)) / (1 - pnorm((x+y)/sqrt(a^2+b^2+c^2))))
      }
      
      } 
    
    plot.table = distr.table = data.frame(x.d = seq(from = 0, to = 5, length.out = input$sensitivity))
    
    # for every value of x find the y such that x = F_i(x,y) 
    plot.table$BR.i = sapply(plot.table$x.d, FUN = function(x){optimise(function(y) {abs(x - F.func.i(x,y))}, c(input$min.x, input$max.x))$minimum})
   
    plot.table$BR.i[plot.table$BR.i < input$max.x/input$sensitivity | plot.table$BR.i > (input$max.x - input$max.x/input$sensitivity)] = NA
   # plot.table$BR.i[min(which(plot.table$BR.i >= c(plot.table$BR.i[-1],0))):length(plot.table$BR.i)] = NA
    
    print(table(is.na(plot.table$BR.i)))
    #FUNtest = function(x){optimise(function(y) {abs(x - F.func.i(x,y))}, c(min.val, max.val))}
    #FUNtest(0)
    #abs(0 - F.func.i(0,0))
    
    # for every value of x find the y such that y = F_j(x,y)
    plot.table$BR.j = sapply(plot.table$x.d, FUN = function(x){optimise(function(y) {abs(y - F.func.j(x,y))}, c(input$min.x, input$max.x))$minimum})
    plot.table$BR.j[plot.table$BR.j < input$max.x/input$sensitivity | plot.table$BR.j > (input$max.x - input$max.x/input$sensitivity)] = NA
   # plot.table$BR.j[min(which(plot.table$BR.j >= c(plot.table$BR.j[-1],0))):length(plot.table$BR.j)] = NA
    
    index.x = which(abs(plot.table$BR.i - plot.table$BR.j) ==min(abs(plot.table$BR.i - plot.table$BR.j), na.rm = T))
    label.x = plot.table$x.d[index.x]
    label.y = plot.table$BR.j[index.x]
    
    LD = plot.table %>% gather(key = BR_func, value = y.d, BR.i, BR.j)
    
    
    ggplot(data=LD, aes(x=x.d, y= y.d)) +
      geom_line(aes(colour=BR_func), size=2) +
      lims(x=c(0, 5),
           y= c(0, 5)) +
      scale_colour_manual(name = NULL,
                          breaks=c("BR.i", "BR.j"),
                          values = c("BR.i" = cbp1[5], 
                                     "BR.j" = cbp1[6]),
                          labels = c("Best Response i", "Best Response j")) +
      theme_classic() + 
      labs(title=NULL, x="Gap i", y="Gap j") +
      theme(legend.justification=c(1,1), legend.position=c(1,1)) +
      annotate("text", x = label.x, y = 0, label = paste(round(label.x, 3)), colour =cbp1[5] ) +
      annotate("text", x = 0 , y = label.y, label = paste(round(label.y, 3)), colour = cbp1[6]) 
    
  } , height = 500, width = 500)
}
shinyApp(ui, server)
