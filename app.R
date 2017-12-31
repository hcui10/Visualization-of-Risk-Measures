library("shiny")   # UI and reactive R expressions
library("plotly")  # interavtie plot functions
library("zeallot") # unpacking assignment
source("helpers.R")

# check all distributions in the stats package
# ?stats::distribution
distribution <- "Normal"
distribution.params <- list(mu = 0, sigma = 1)
n.points <- 10000 # number of points to sample for x and p grids, hard-coded

# generate data used for plots
c(ddist, pdist, qdist) %<-% dist.helper(distribution, distribution.params)
plot.data <- generate.data(distribution, distribution.params) 

plotly_plot(plot.data)

c(x0, p0) %<-% list(x = 2, p = pdist(2))

subplot(
  subplot(
    # PDF Plot ---
    plot_ly(data = plot.data, x = ~x  , y = ~fx, 
            hoverinfo = "text", 
            text = ~paste("X: ", round.num(x, digits = 2), 
                          "<br> Density: ", round.num(fx, digits = 2)), 
            # muted blue
            color = I("#1f77b4")) %>% 
      add_lines(x = ~(x * (x <= x0)), 
                y = ~(fx * (x <= x0)), 
                fill = "tozeroy", name = "area under PDF") %>% 
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
            # muted purple
            color = I("#9467bd")) %>% 
      add_lines() %>% 
      layout(xaxis = list(title = "$x$"), yaxis = list(title = "$p = F_X(x)$")), 
    # Rotated VaR Plot ---
    plot_ly(data = plot.data, x = ~VaR, y = ~p , name = "VaR rotated", 
            hoverinfo = "text", 
            text = ~paste("Value-at-Risk: ", round.num(VaR, digits = 2), 
                          "<br> Confidence Level: ", 
                          round.num(p, digits = 2, type = "percent")), 
            # cooked asparagus green
            color = I("#2ca02c")) %>% 
      add_lines() %>% 
      layout(xaxis = list(title = "$\\text{VaR}_X(p)$", autorange = "reversed")), 
    nrows = 2, shareX = TRUE, shareY = TRUE, which_layout = 3
  ), 
  # VaR Plot ---
  plot_ly(data = plot.data, x = ~p, y = ~VaR, name = "VaR", 
          hoverinfo = "text", 
          text = ~paste("Confidence Level: ", round.num(p, digits = 2, type = "percent"), 
                        "<br> Value-at-Risk: ", round.num(VaR, digits = 2)), 
          # cooked asparagus green
          color = I("#2ca02c")) %>% 
    add_lines() %>% 
    layout(xaxis = list(title = "$p$"), yaxis = list(title = "$\\text{VaR}_X(p)$")), 
  widths = c(0.7, 0.3), titleX = TRUE, titleY = TRUE
) %>% layout(showlegend = FALSE)

