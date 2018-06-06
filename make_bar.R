#' make_bar make a bar chart
#'
#'
#' @param x - a data.frame to plot
#' @param colors - a vector that decide the group colors
#' @param title - the title of the plot
#'
#' @import ggplot2
#' @return NULL
#' @export
#'
#' @examples x<-matrix(c(500,2000,36),3,1,dimnames =list(c("a","b","c")))
#'           names(x)<-c("a","b","c")
#'            make_bar(x, c("red","blue","yellow"), "title")
#'



    make_bar <- function(x, colors, title) {

      names <- factor(names(x),names(x),ordered = TRUE)

      plotData <- data.frame(names, x)

     ggplot(data = plotData, aes(x = names, y = x )) +
        geom_bar(aes(fill = names),stat = "identity",
                 position = position_stack(reverse = TRUE)) +
        coord_flip() +
        scale_fill_manual(values = colors, guide = guide_legend(reverse = TRUE))+
        theme(legend.position = "none")+
        labs(y="Estimate",x="",
             title=title,
             subtitle="2016 American Community Survey 5-Year Estimates")
    }


