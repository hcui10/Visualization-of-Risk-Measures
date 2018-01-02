# hard-coded color palette
color.list <- c("#1f77b4", # muted blue
                "#9467bd", # muted purple
                "#2ca02c") # cooked asparagus green

# generate PDF, CDF, and VaR given distribution
dist.helper <- function(distribution, distribution.params) {
  switch(distribution, 
         "beta" = 
           list(ddist = function(x) dbeta(x, shape1 = distribution.params$shape1,
                                          shape2 = distribution.params$shape2),
                pdist = function(q) pbeta(q, shape1 = distribution.params$shape1,
                                          shape2 = distribution.params$shape2),
                qdist = function(p) qbeta(p, shape1 = distribution.params$shape1,
                                          shape2 = distribution.params$shape2)),
         "cauchy" = 
           list(ddist = function(x) dcauchy(x, location = distribution.params$location,
                                           scale = distribution.params$scale),
                pdist = function(q) pcauchy(q, location = distribution.params$location,
                                           scale = distribution.params$scale),
                qdist = function(p) qcauchy(p, location = distribution.params$location,
                                           scale = distribution.params$scale)),
         "chisq" = 
           list(
             ddist = function(x) dchisq(x, df= distribution.params$df),
             pdist = function(q) pchisq(q, df= distribution.params$df),
             qdist = function(p) qchisq(p, df= distribution.params$df)),
         "exp" = 
           list(
             ddist = function(x) dexp(x, rate = distribution.params$rate),
             pdist = function(q) dexp(q, rate = distribution.params$rate),
             qdist = function(p) dexp(p, rate = distribution.params$rate)),
         "f" = 
           list(
             ddist = function(x) df(x,df1 = distribution.params$df1,
                                    df2 = distribution.params$df2),
             pdist = function(q) df(q,df1 = distribution.params$df1,
                                    df2 = distribution.params$df2),
             qdist = function(p) df(p,df1 = distribution.params$df1,
                                    df2 = distribution.params$df2)),
         "gamma" = 
           list(ddist = function(x) dgamma(x, shape = distribution.params$shape,
                                           rate = distribution.params$rate),
                pdist = function(q) pgamma(q, shape = distribution.params$shape,
                                           rate = distribution.params$rate),
                qdist = function(p) qgamma(p, shape = distribution.params$shape,
                                           rate = distribution.params$rate)),
         "lnorm" = 
           list(ddist = function(x) dlnorm(x, meanlog = distribution.params$meanlog,
                                           sdlog = distribution.params$sdlog),
                pdist = function(q) plnorm(q, meanlog = distribution.params$meanlog,
                                           sdlog = distribution.params$sdlog),
                qdist = function(p) qlnorm(p, meanlog = distribution.params$meanlog,
                                           sdlog = distribution.params$sdlog)),
         "norm" = 
           list(ddist = function(x) dnorm(x, mean = distribution.params$mean, 
                                          sd = distribution.params$sd), 
                pdist = function(q) pnorm(q, mean = distribution.params$mean, 
                                          sd = distribution.params$sd),
                qdist = function(p) qnorm(p, mean = distribution.params$mean, 
                                          sd = distribution.params$sd)),
         "t" = 
           list(
             ddist = function(x) dt(x, df= distribution.params$df),
             pdist = function(q) pt(q, df= distribution.params$df),
             qdist = function(p) qt(p, df= distribution.params$df)),
         "unif" = 
           list(
             ddist = function(x) dunif(x, min = distribution.params$min,
                                       max = distribution.params$max),
             pdist = function(q) punif(q, min = distribution.params$min,
                                       max = distribution.params$max),
             qdist = function(p) qunif(p, min = distribution.params$min,
                                       max = distribution.params$max)),
         "weibull" = 
           list(
             ddist = function(x) dweibull(x, shape = distribution.params$shape,
                                          scale = distribution.params$scale),
             pdist = function(q) pweibull(x, shape = distribution.params$shape,
                                          scale = distribution.params$scale),
             qdist = function(p) qweibull(p, shape = distribution.params$shape,
                                          scale = distribution.params$scale)
           ))
}

# generate data used for plots
generate.data <- function(distribution, distribution.params, n.points) {
  c(ddist, pdist, qdist) %<-% dist.helper(distribution, distribution.params)
  x.min  <- qdist(p = 0.0001) 
  x.max  <- qdist(p = 0.9999) 
  x.grid <- seq(from = x.min, to = x.max, length.out = n.points)
  p.grid <- seq(from = 0, to = 1, length.out = n.points)[2:(n.points+1)]
  plot.data <- data.frame(x   = x.grid, p = p.grid, 
                          fx  = ddist(x.grid), 
                          Fx  = pdist(x.grid),   
                          VaR = qdist(p.grid)) 
  return(plot.data)
}

# round a number and output as a string
round.num <- function(x, digits, type = "numeric") {
  switch(type, 
         "numeric" = format(round(x, digits = digits), nsmall = digits), 
         "percent" = paste(format(round(x * 100, digits = digits), nsmall = digits), 
                           "%", sep = " "))
}

# main helper function to plot
plotly_plot <- function(plot.data, data.pt = NULL) {
  if (is.null(data.pt)) {
    subplot(
      subplot(
        # PDF Plot ---
        plot_ly(data = plot.data, x = ~x  , y = ~fx, 
                hoverinfo = "text", 
                text = ~paste("X: ", round.num(x, digits = 2), 
                              "<br> Density: ", round.num(fx, digits = 2)), 
                color = I(color.list[1])) %>% 
          add_lines(name = "PDF") %>% layout(yaxis = list(title = "$f_X(x)$")), 
        # Placeholder ---
        plot_ly(type = "scatter", mode = "lines"), # specify arguments to avoid warnings
        # CDF Plot ---
        plot_ly(data = plot.data, x = ~x  , y = ~Fx, name = "CDF", 
                hoverinfo = "text", 
                text = ~paste("X: ", round.num(x, digits = 2), 
                              "<br> Cumulative Probability: ", 
                              round.num(Fx, digits = 2, type = "percent")), 
                color = I(color.list[2])) %>% 
          add_lines() %>% 
          layout(xaxis = list(title = "$x$"), yaxis = list(title = "$p = F_X(x)$")), 
        # Rotated VaR Plot ---
        plot_ly(data = plot.data, x = ~VaR, y = ~p , name = "VaR rotated", 
                hoverinfo = "text", 
                text = ~paste("Value-at-Risk: ", round.num(VaR, digits = 2), 
                              "<br> Confidence Level: ", 
                              round.num(p, digits = 2, type = "percent")), 
                color = I(color.list[3])) %>% 
          add_lines() %>% 
          layout(xaxis = list(title = "$\\text{VaR}_X(p)$", autorange = "reversed")), 
        nrows = 2, shareX = TRUE, shareY = TRUE, which_layout = 3
      ), 
      # VaR Plot ---
      plot_ly(data = plot.data, x = ~p, y = ~VaR, name = "VaR", 
              hoverinfo = "text", 
              text = ~paste("Confidence Level: ", round.num(p, digits = 2, type = "percent"), 
                            "<br> Value-at-Risk: ", round.num(VaR, digits = 2)), 
              color = I(color.list[3])) %>% 
        add_lines() %>% 
        layout(xaxis = list(title = "$p$"), yaxis = list(title = "$\\text{VaR}_X(p)$")), 
      widths = c(0.7, 0.3), titleX = TRUE, titleY = TRUE
    ) %>% layout(showlegend = FALSE)
  } else {
    #click data
    c(x0, p0) %<-% list(x=data.pt$x,p=data.pt$y)
    #add_line data 
    al_dat <- data.frame(x = plot.data$x*(plot.data$x<=x0),
                         fx = plot.data$fx*(plot.data$x<=x0),
                         Fx = plot.data$Fx*(plot.data$x<=x0))
    al_dat[,1][which(al_dat[,1]==0)]=x0
    # plot upon click
    subplot(
      subplot(
        # PDF Plot ---
        plot_ly(data = plot.data, x = ~x  , y = ~fx, 
                hoverinfo = "text", 
                text = ~paste("X: ", round.num(x, digits = 2), 
                              "<br> Density: ", round.num(fx, digits = 2)), 
                color = I(color.list[1])) %>% 
          add_lines(x = ~al_dat$x, 
                    y = ~al_dat$fx, 
                    fill = "tonexty", name = "area under PDF") %>% 
          add_lines(name = "PDF") %>% 
          layout(yaxis = list(title = "$f_X(x)$")), 
        # Placeholder ---
        plot_ly(type = "scatter", mode = "lines"), # specify arguments to avoid warnings
        # CDF Plot ---
        plot_ly(data = plot.data, x = ~x  , y = ~Fx, name = "CDF", 
                hoverinfo = "text", 
                text = ~paste("X: ", round.num(x, digits = 2), 
                              "<br> Cumulative Probability: ", 
                              round.num(Fx, digits = 2, type = "percent")), 
                color = I(color.list[2])) %>% 
          add_lines(y = pnorm(x0), 
                    name = "area under CDF") %>% 
          add_lines(name = "CDF") %>% 
          layout(xaxis = list(title = "$x$"), yaxis = list(title = "$p = F_X(x)$")), 
        # Rotated VaR Plot ---
        plot_ly(data = plot.data, x = ~VaR, y = ~p , name = "VaR rotated", 
                hoverinfo = "text", 
                text = ~paste("Value-at-Risk: ", round.num(VaR, digits = 2), 
                              "<br> Confidence Level: ", 
                              round.num(p, digits = 2, type = "percent")), 
                color = I(color.list[3])) %>% 
          add_lines(y = pnorm(x0),name = "reversed VaR") %>% 
          layout(xaxis = list(title = "$\\text{VaR}_X(p)$", autorange = "reversed")), 
        nrows = 2, shareX = TRUE, shareY = TRUE, which_layout = 3
      ), 
      # VaR Plot ---
      plot_ly(data = plot.data, x = ~p, y = ~VaR, name = "VaR", 
              hoverinfo = "text", 
              text = ~paste("Confidence Level: ", round.num(p, digits = 2, type = "percent"), 
                            "<br> Value-at-Risk: ", round.num(VaR, digits = 2)), 
              color = I(color.list[3])) %>% 
        add_lines(x= qnorm(x0),name = "Value at Risk") %>% 
        layout(xaxis = list(title = "$p$"), yaxis = list(title = "$\\text{VaR}_X(p)$")), 
      widths = c(0.7, 0.3), titleX = TRUE, titleY = TRUE
    ) %>% layout(showlegend = FALSE)
    
    
    
          
    #return(0)
  }
}