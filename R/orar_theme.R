#' Customize Theme for ggplot2
#'
#' This function `orar_theme` allows for advanced customization of ggplot2 plot themes,
#' including options for legend position, background, direction, and more.
#'
#' @param legend_position A string specifying the position of the legend. Can be one of "bottom",
#' "top", "left", "right", or corner positions like "topleft", "topright", "bottomleft", "bottomright".
#' @param legend_bg A string specifying the background color of the legend box. "transparent" is the default.
#' @param legend_direction A string indicating the orientation of the legend. Can be "horizontal" or "vertical".
#' @param grid A string that controls the appearance of the grid in the plot. It can be "all", "none", "x", or "y".
#' @param plot_ratio Numeric. A ratio for the plot dimensions.
#' @param panel_aspect_ratio Numeric. An optional parameter for setting the aspect ratio of the plot panel.
#' @param distance_from_edge Numeric. A distance measure from the edge of the plot, used when legend is placed at corners.
#' @param base_family A string specifying the font family for the plot text.
#' @param base_size Numeric. Base font size for the plot text.
#' @param ... Additional arguments passed to `theme_classic()`.
#'
#' @details
#' The function `orar_theme` enhances the visual appearance of ggplot2 plots by providing
#' extensive customization options. It is particularly useful for adjusting legend properties
#' and grid settings. When specifying corner positions for the legend, the function automatically
#' adjusts its alignment and position relative to the plot edges.
#'
#' @export
#'
#' @examples
#' # Example usage of orar_theme
#' library(ggplot2)
#' p <- ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()
#' p + orar_theme(legend_position = "topright",
#'                legend_bg = "white",
#'                legend_direction = "vertical",
#'                grid = "all",
#'                base_family = "sans",
#'                base_size = 12)
#'
#' # Using orar_theme with corner legend position
#' p + orar_theme(legend_position = "bottomright",
#'                legend_bg = "lightblue",
#'                grid = "y")
#'

orar_theme<-function(legend_position="bottom",
                            legend_bg="transparent",
                            legend_direction = "horizontal",grid="all",
                            plot_ratio=1.5,panel_aspect_ratio=NULL,
                            distance_from_edge=0.05,base_family = "sans",base_size = 20,...){
        half_line <- base_size/2

        inner_positions<-c("topleft","topright","bottomleft","bottomright")
        legend_justification<-c("center")
        if (legend_position %in% inner_positions) {

            align_v<-ifelse(grepl(pattern ="top",x=legend_position,ignore.case = TRUE),"top","bottom")
            align_h<-ifelse(grepl(pattern ="right",x=legend_position,ignore.case = TRUE),"right","left")
            legend_justification = c(align_h, align_v)

            y<-as.numeric(grepl(pattern ="top" ,x=legend_position,ignore.case = TRUE))
            leg_y=abs(y-distance_from_edge)
            x<-as.numeric(grepl(pattern ="right" ,x=legend_position,ignore.case = TRUE))
            leg_x=abs(x-distance_from_edge/plot_ratio)
            legend_position<-c(leg_x,leg_y)
        }
        require(ggplot2)
        theme_result<-theme_classic(base_size,...)+
            theme(
                panel.grid.major = element_line(colour = "grey92"),
                #Transparent Objects
                panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
                plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot

                #legend
                legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                legend.box.background = element_rect(fill = legend_bg,colour=NA), # get rid of legend panel bg
                legend.key = element_rect(fill = "transparent", colour = NA), # get rid of key legend fill, and of the surrounding


                legend.position = legend_position, legend.justification = legend_justification, legend.direction = legend_direction,

                #margins
                plot.margin= margin(half_line*2, half_line*2, half_line,half_line),#unit(c(1,1,0.5,0.5),"cm"),
                aspect.ratio = panel_aspect_ratio,

                # Caption
                plot.caption = element_text(hjust = 0, size = base_size*0.67),

                plot.title = element_text(hjust =0, face = "bold")

            )
        if (grid == "none") {
            theme_result <- theme_result +
                theme(panel.grid.major = element_blank())
        } else if (grid == "x") {
            theme_result <- theme_result +
                theme(panel.grid.major.x = element_blank())
        } else if (grid == "y") {
            theme_result <- theme_result +
                theme(panel.grid.major.y = element_blank())
        }
        if (!grid %in%  c("all","x","y","none")) {
            warning("grid must be one of all, x, y, None")
        }
        return(theme_result)
    }

