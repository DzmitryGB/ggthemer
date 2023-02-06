library(ggthemer)
library(ggplot2)
library(shinyjs)
library(shiny)

options(shiny.trace = F, shiny.reactlog = F)
# Roboto font
font_dir <- system.file("fonts", package = "ggthemer")
font_cache <- list(
    Roboto = list(
        family = "Roboto",
        regular = file.path(font_dir, "Roboto-Regular.ttf"),
        bold = file.path(font_dir, "Roboto-Bold.ttf"),
        italic = file.path(font_dir, "Roboto-Italic.ttf"),
        bolditalic = file.path(font_dir, "Roboto-BoldItalic.ttf")
    )
)
# persistent storage
user_data_store <- tools::R_user_dir("ggthemer")
if (!dir.exists(user_data_store)) dir.create(user_data_store, recursive = T)
user_data_store <- file.path(user_data_store, "custom_themes.Rds")
# UI
ui <- fluidPage(
    shinyjs::useShinyjs(),
    ggthemerUI("test", font_cache),
    plotOutput("distPlot")
)
# Server
server <- function(input, output, session) {

    shinyjs::useShinyjs()

    session$onSessionEnded(function() {
        saveRDS(
            isolate(reactiveValuesToList(theme_store))[["custom_themes"]],
            user_data_store
        ) # persistence
        if (exists(".ggthemerPlot") && inherits(.ggthemerPlot, "ggplot")) {
            stopApp( .ggthemerPlot + isolate(activeTheme()$theme) )
        } else {
            stopApp( isolate(activeTheme()$theme) ) # return only theme
        }
    })

    theme_store <- reactiveValues(
        test = list(
            preview_theme = list(
                base = "grey", base_params = list(), custom_params = list()
            )
        ), # initiate theme store
        custom_themes = readRDS(ifelse(
            file.exists(user_data_store), user_data_store,
            system.file("themes/custom_themes.Rds", package = "ggthemer")
        ))
    )

    activeTheme <- ggthemerSERV("test", theme_store) # grab user defined theme

    observeEvent(activeTheme(), {
        if (!is.null(activeTheme()$name)) {
            theme_store$custom_themes[[activeTheme()$name]] <- activeTheme()$store
        }
    }) # save user theme

    output$distPlot <- renderPlot({
        if (exists(".ggthemerPlot") && inherits(.ggthemerPlot, "ggplot")) {
            .ggthemerPlot + activeTheme()$theme
        }
    })

}

shinyApp(ui, server)
