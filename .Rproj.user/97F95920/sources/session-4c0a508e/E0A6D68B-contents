
# This file is a generated template, your changes will not be overwritten

plotClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "plotClass",
    inherit = plotBase,
    private = list(
      .init = function() {
        image <- self$results$plot
        
        # size <- private$.plotSize()
        # image$setSize(size$width, size$height)
      },
        .run = function() {
          
          x <- self$options$xvar
          y <- self$options$yvar
          g <- self$options$group
          type <- self$options$type
          xlab <- self$options$xlab
          ylab <- self$options$ylab
          
          if ( ! is.null(x) && ! is.null(y)) {
            xCol <- jmvcore::toNumeric(self$data[[x]])
            yCol <- jmvcore::toNumeric(self$data[[y]])
            gCol <- `if`(
              is.null(g), 
              factor(rep("var", length(xCol))), 
              factor(self$data[[g]])
            )
            
            data <- data.frame(x=xCol, y=yCol, g=gCol, type = type, xlab = xlab, ylab = ylab)
            data <- jmvcore::naOmit(data)
            
            image <- self$results$plot
            image$setState(data)
          }
        },
        .plot = function(image, ...) {  # <-- the plot function
          if (is.null(image$state))
            return(FALSE)
          
          data <- image$state
          plot <- ggplot(data) +
            xlab(data$xlab[1]) +
            ylab(data$ylab[1])
          
          if(data$type[1] == "point") {
            plot <- plot +
              geom_point(aes(x = x, y = y, colour = g))
          } else if(data$type[1] == "line") {
            plot <- plot +
              geom_line(aes(x = x, y = y, colour = g))
          } else if(data$type[1] == "boxplot") {
            plot <- plot +
              geom_boxplot(aes(x = g, y = y, fill = g))
          }
            
          print(plot)
          TRUE
        })
)
