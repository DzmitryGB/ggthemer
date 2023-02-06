#' Create ggthemer GUI
#'
#' Style your ggplots using a Shiny app
#'
#' @param plotObject ggplot object, optional
#'
#' @return theme or ggplot object
#'
#' @examples
#' ggObject <- ggplot(iris) + geom_bar(aes(x = Petal.Length, fill = Species)) # define plot aesthetitcs
#' myTheme <- ggthemerGUI() # Shiny module returns user theme
#' ggObject <- ggthemerGUI(ggObject) # View your plot within the Shiny app
#' ggObject <- ggObject + ggthemerGUI() # Shiny module returns user theme (data-heavy plot)
#'
#' @export
ggthemerGUI <- function(plotObject = NULL) {
    if (!is.null(plotObject)) assign(".ggthemerPlot", plotObject, .GlobalEnv)
    app_dir <- system.file("ggthemerApp", package = "ggthemer")
    userTheme <- runApp(app_dir)
    if (!is.null(plotObject)) rm(.ggthemerPlot, envir = .GlobalEnv)
    return(userTheme)
}
