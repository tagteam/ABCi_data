plot_estimated_vs_counterfactual <- function(estimated,counterfactual){
    g <- ggprodlim(
        counterfactual,
        cause = 1,
        ylim = c(0,50),
        conf_int = FALSE
    )
    g + geom_line(
            data = estimated,
            aes(x = time, y = 100*estimate,
                group = treatment),
            linetype = "dashed"
        )
}
