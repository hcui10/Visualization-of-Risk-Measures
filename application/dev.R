library("shiny")   # UI and reactive R expressions
library("plotly")  # interavtie plot functions
library("zeallot") # unpacking assignment
source("helpers.R")

# hard-coded color palette
color.list <- c("#1f77b4", # muted blue
                "#9467bd", # muted purple
                "#2ca02c") # cooked asparagus green

n.points <- 10000 # number of points to sample for x and p grids, hard-coded

# check all distributions in the stats package
# ?stats::distribution
distribution <- "norm"
distribution.params <- list(mean = 0, sd = 1)

# generate data used for plots
c(ddist, pdist, qdist) %<-% dist.helper(distribution, distribution.params)
plot.data <- generate.data(distribution, distribution.params, n.points) 

# plot without shading and spike lines
plotly_plot(plot.data)

# assume this is the click data
c(x0, p0) %<-% list(x = -1, p = pdist(-1))

#add_line data 
al_dat <- data.frame(x=plot.data$x*(plot.data$x<=x0),fx=plot.data$fx*(plot.data$x<=x0))
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