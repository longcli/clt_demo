# central limit theorem demonstration
# courtesy of Dr. Juan Hu (DePaul University)
# March 2014


# distributions with examples
# normal
# exponential
# weibull
# lognormal
# uniform
# poisson
# binomial

library(car)



shinyServer(function(input, output) {

  output$ui_norm <- renderUI({

    fluidPage(
      
      fluidRow(
        column( 4, sliderInput("in_mu", "Mean:", min = -50, max = 50, value = 0) ),
        column( 4, sliderInput("in_stdev", "Std Dev:", min = 1, max = 20, value = 1) )
      ),  # fluidRow
      
      fluidRow(a("Wikipedia", href = "https://en.wikipedia.org/wiki/Normal_distribution", target="_blank")),
      fluidRow( plotOutput("distPlotNorm") )
      
    )  # end fluidPage
    
  })  # end ui_norm
  
  output$ui_exp <- renderUI({
    
    fluidPage(
      
      fluidRow(
        column( 4, sliderInput("in_lambda", "Lambda:", min = 0.1, max = 50, value = 1) )
      ),  # fluidRow
      
      fluidRow(a("Wikipedia", href = "https://en.wikipedia.org/wiki/Exponential_distribution", target="_blank")),
      fluidRow( plotOutput("distPlotExp") )
      
    )  # end fluidPage
    
  })  # end ui_exp
  
  output$ui_weib <- renderUI({
    
    fluidPage(
              
              fluidRow(
                # Shape parameter < 1 indicates that the failure rate decreases over time.
                # Shape parameter = 1 indicates that the failure rate is constant over time. Exp dist
                # Shape parameter > 1 indicates that the failure rate increases with time.
                column( 4, sliderInput("in_shape", "Shape:", min = 0.5, max = 5, value = 5) ),
                column( 4, sliderInput("in_scale", "Scale:", min = 1, max = 50, value = 1) )
              ),  # fluidRow
              
              fluidRow(a("Wikipedia", href = "https://en.wikipedia.org/wiki/Weibull_distribution", target="_blank")),
              fluidRow( plotOutput("distPlotWeib") )
              
    )  # end fluidPage
    
  })  # end ui_weib
  
  
  output$ui_unif <- renderUI({
    
    fluidPage(
      
      fluidRow(
        column( 4, sliderInput("in_min", "Minimum:", min = 0, max = 1, value = 0) ),
        column( 4, sliderInput("in_max", "Maximum:", min = 1, max = 20, value = 1) )
      ),  # fluidRow
      
      fluidRow(a("Wikipedia", href = "https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)", target="_blank")),
      fluidRow( plotOutput("distPlotUnif") )
      
    )  # end fluidPage
    
  })  # end ui_unif
  
  
  ## end ui section ###########################################################
  
  output$distPlotNorm <- renderPlot({
    
    # NORMAL ##################################################################
    
    # Central limit theorem for a normally distributed population
    # n is number of r.v. in one set of sample
    # m is how many sets of such samples
    # mu is the mean of the distribution
    # stdev is the standard deviation
    # red line is the normal curve for reference
    
    n_ssize = input$in_ssize
    m_numsamples = input$in_numsamples
    mu = input$in_mu 
    stdev = input$in_stdev
    xlim_choice = input$in_histxlim
    
    CLT_norm_fn = function(m, n, mu, sd = stdev){
      bar_y = rep(0, m)
      for (i in 1:m){
        bar_y[i] = mean(rnorm(n, mean = mu, sd = stdev))
      }

      par(mfrow=c(1,3))
      
      # plot of population -----
      plot(density(rnorm(1000000, mean = mu, sd = stdev)),
           main =  'Normal Population Distribution')
      
      # determine xlim for sample means plot -----
      if (xlim_choice == 1){
        # compresses the sample dist plot x-axis to highlight dist of sample means
        minx = mean(bar_y) - 4*sd(bar_y)
        maxx = mean(bar_y) + 4*sd(bar_y)
      } else if(xlim_choice == 2) {
        # expands the sample dist plot x-axis to show pop dist for comparison
        minx = mu - 4*stdev
        maxx = mu + 4*stdev
      }
      
      # create data for plot of sample means -----
      x = seq(minx, maxx, length.out = 4000)
      y = dnorm(x, mean(bar_y), sd(bar_y))
      # y = dnorm(x, mean = mu, sd = stdev/sqrt(n_ssize))
      
      # histogram of sample means -----
      hist(bar_y, freq = FALSE, 
           xlim = c(minx, maxx),
           xlab = 'Sample Means',
           main = 'Distribution of Sample Means')
      
      # adds density plot of estimate norm dist for sample -----
      lines(x, y, 6, 14, col = "red", type = "l")
      
      # QQ normality plot with conf bands 
      qqPlot(bar_y) 
      
    }
    
    CLT_norm_fn(m_numsamples, n_ssize, mu, sd)
    
  })  # end normal
  
  
  output$distPlotExp <- renderPlot({
    
    # EXPONENTIAL ###########################################################
    
    # Central limit theorem for an expontial population
    # n is number of r.v. in one set of sample
    # m is how many sets of such samples
    # theta is the parameter for exponential distribution
    # red line is the normal curve for reference
    
    n_ssize = input$in_ssize
    m_numsamples = input$in_numsamples
    lambda = input$in_lambda 
    theta = 1/lambda  # added for clarity in notation
    xlim_choice = input$in_histxlim
      
    CLT_exp_fn = function(m, n, theta){
      bar_y = rep(0, m)
      for (i in 1:m){
        bar_y[i] = mean(rexp(n, rate = 1/theta))
      }

      par(mfrow=c(1,3))
        
      # plot of population -----
      plot(density(rexp(1000000, rate = lambda)), 
           xlim = c(0, qexp(p = 0.999, rate = lambda)),
           main =  'Exponential Population Distribution')
      
      # determine xlim for sample means plot -----
      if (xlim_choice == 1){
        # compresses the sample dist plot x-axis to highlight dist of sample means
        minx = mean(bar_y) - 4*sd(bar_y)
        maxx = mean(bar_y) + 4*sd(bar_y)
      } else if(xlim_choice == 2) {
        # expands the sample dist plot x-axis to show pop dist for comparison
        minx = 0
        maxx = qexp(p = 0.999, rate = lambda)
      }

      # create data for plot of sample means -----
      x = seq(minx, maxx, length.out = 4000)
      y = dnorm(x, mean(bar_y), sd(bar_y))
      # y = dnorm(x, theta, theta/sqrt(n_ssize))

      # histogram of sample means -----
      hist(bar_y, freq = FALSE, 
           xlim = c(minx, maxx),
           xlab = 'Sample Means',
           main = 'Distribution of Sample Means')
      
      # adds density plot of estimate norm dist for sample -----
      lines(x, y, 6, 14, col = "red", type = "l")
      
      # QQ normality plot with conf bands -----
      qqPlot(bar_y) 
       
    }
      
    CLT_exp_fn(m_numsamples, n_ssize, theta)
      
  }) # exponential
  
  
  # the histogram of sample means has a normal distribution overlaid
  # the overlaid normal distribution has the mean and stderr of the sample means
  
  
  output$distPlotWeib <- renderPlot({
    
    # WEIBULL #################################################################
    
    # Central limit theorem for a Weibull population
    # n is number of r.v. in one set of sample
    # m is how many sets of such samples
    # shape and scale are the parameters for 2-parameter Weibull distribution
    # red line is the normal curve for reference
    
    n_ssize = input$in_ssize
    m_numsamples = input$in_numsamples
    wshape = input$in_shape  # wshape = 1 is exponential
    wscale = input$in_scale  # wscale = 1 for testing
    xlim_choice = input$in_histxlim
    
    CLT_weib_fn = function(m, n, wshapefn, wscalefn){
      bar_y = rep(0, m)
      for (i in 1:m){
        bar_y[i] = mean(rweibull(n, shape = wshapefn, scale = wscalefn))
      }
      
      par(mfrow=c(1,3))
      
      # top plot of population
      rweib = rweibull(1000000, shape = wshape, scale = wscale)
      plot(density(rweib),
           xlim = c(0, qweibull(p = 0.999, shape = wshape, scale = wscale)),
           main =  'Weibull Population Distribution')

      # bottom plot of sample means
      xmin = min(rweib); xmax = max(rweib)
      # xstdev = (xmax - xmin)/4  # rough estimate
      
      # gamma_lambda = wshape
      # gamma_k = wscale
      # gamma_mean = gamma_lambda * gamma(1 + 1/gamma_k)
      # gamma_var = (gamma_lambda^2)*( gamma(1 + 2/gamma_k) - (gamma(1 + 1/gamma_k))^2)
      # gamma_sd = sqrt(gamma_var)
      
      # xminhist = gamma_mean - 3*(gamma_sd/sqrt(n_ssize))
      # xmaxhist = gamma_mean + 3*(gamma_sd/sqrt(n_ssize))
      
      # determine xlim for sample means plot -----
      if (xlim_choice == 1){
        # compresses the sample dist plot x-axis to highlight dist of sample means
        minx = mean(bar_y) - 4*sd(bar_y)
        maxx = mean(bar_y) + 4*sd(bar_y)
      } else if(xlim_choice == 2) {
        # expands the sample dist plot x-axis to show pop dist for comparison
        minx = 0
        maxx = qweibull(p = 0.999, shape = wshape, scale = wscale)
      }

      x = seq(minx, maxx, length.out = 4000)
      y = dnorm(x, mean(bar_y), sd(bar_y))
      
      # histogram of sample means
      hist(bar_y, freq = FALSE, 
           xlim = c(minx, maxx),
           xlab = 'Sample Means',
           main = 'Distribution of Sample Means')
      
      # adds density plot of estimate norm dist for sample
      lines(x, y, 6, 14, col = "red", type = "l")
      
      # QQ normality plot with conf bands
      qqPlot(bar_y) 
      
    }
    
    CLT_weib_fn(m_numsamples, n_ssize, wshape, wscale)
    
  }) # weibull
  
  
  output$distPlotUnif <- renderPlot({
    
    # Central limit theorem for a uniform population
    # n is number of r.v. in one set of sample
    # m is how many sets of such samples
    # min and max are the parameters for uniform distribution
    # red line is the normal curve for reference
    
    n_ssize = input$in_ssize
    m_numsamples = input$in_numsamples
    umin = input$in_min 
    umax = input$in_max
    xlim_choice = input$in_histxlim
    
    CLT_unif_fn = function(m, n, min, max){
      bar_y = rep(0, m)
      for (i in 1:m){
        bar_y[i] = mean(runif(n, min = min, max = max))
      }
      
      par(mfrow=c(1,3))
      
      # top plot of population
      plot(density(runif(1000000, min = umin, max = umax)),
           xlim = c(umin, umax),
           main =  'Uniform Population Distribution')
      
      # bottom plot of sample means
      # umu = umin + ((umax - umin)/2)
      # ustdev = 1/sqrt(12*n_ssize)
      # minx = umu - 4*ustdev; maxx = umu + 4*ustdev  # expands the sample dist plot to show pop dist better
      
      # determine xlim for sample means plot -----
      if (xlim_choice == 1){
        # compresses the sample dist plot x-axis to highlight dist of sample means
        minx = mean(bar_y) - 4*sd(bar_y)
        maxx = mean(bar_y) + 4*sd(bar_y)
      } else if(xlim_choice == 2) {
        # expands the sample dist plot x-axis to show pop dist for comparison
        minx = umin
        maxx = umax
      }
      
      x = seq(minx, maxx, length.out = 4000)
      y = dnorm(x, mean(bar_y), sd(bar_y))
      # y = dnorm(x, umu, ustdev)
      
      # histogram of sample means
      hist(bar_y, freq = FALSE, 
           xlim = c(minx, maxx),
           xlab = 'Sample Means',
           main = 'Distribution of Sample Means')
      
      # adds density plot of estimate norm dist for sample
      lines(x, y, 6, 14, col = "red", type = "l")
      
      # QQ normality plot with conf bands
      qqPlot(bar_y) 
      
    }
    
    CLT_unif_fn(m_numsamples, n_ssize, umin, umax)
    
  }) # uniform
  
    
})  # shinyServer



# 
# 
# output$ui_lnorm <- renderUI({
#   
#   fluidPage(
#     
#     fluidRow(
#       column( 4, sliderInput("in_meanlog", "Mean Log:", min = 0.0001, max = 1, value = 0.5) ),
#       column( 4, sliderInput("in_sdlog", "Std Dev Log:", min = 1, max = 20, value = 5) )
#     ),  # fluidRow
#     
#     fluidRow( plotOutput("distPlotLnorm") )
#     
#   )  # end fluidPage
#   
# })  # end ui_lnorm
# 
# 
# 
# # POISSON #####################################################################
# #Central limit theorem for an uniform population
# #n is number of r.v. in one set of sample
# #m is how many sets of such samples
# #red line is the normal curve for reference
# n=10
# m=1000
# 
# CLT.unif=function(m,n){
#   bary=rep(0,m)
#   for (i in 1:m){
#     bary[i]=mean(runif(n))
#   }
#   
#   par(mfrow = c(1,2))
#   
#   hist(runif(100000))
#   
#   x=seq(min(bary),max(bary),length.out=4000)
#   y=dnorm(x,1/2,1/sqrt(12*n))
#   hist(bary, freq=F)
#   lines(x, y, 6, 14, col="red", type="l")
# }
# 
# CLT.unif(1000,10)
# 
# 
# # BINOMIAL ####################################################################
# #Central limit theorem for an Binomial population
# #n is number of r.v. in one set of sample
# #m is how many sets of such samples
# #k is number of trials for binomial R.V
# #p is probability of success
# #red line is the normal curve for reference
# 
# 
# CLT.binom=function(m,n,k,p){
#   bary=rep(0,m)
#   for (i in 1:m){
#     bary[i]=mean(rbinom(n,k,p))
#   }
#   
#   par(mfrow = c(1,2))
#   
#   hist(runif(100000))
#   
#   x=seq(min(bary),max(bary),length.out=4000)
#   y=dnorm(x,k*p,sqrt(k*p*(1-p))/sqrt(n))
#   hist(bary, freq=F)
#   lines(x,y,6, 14, col="red", type="l")
# }
# 
# CLT.binom(1000,10,10,0.99)



# END CODE ####################################################################