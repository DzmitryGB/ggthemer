#' Server processor for ggthemer
#'
#' @param id Character variable
#'
#' @param theme_store list
#'
#' @return reactive list
#'
#' @examples
#'
#' theme_store <- reactiveValues(
#'     myID = list(
#'         preview_theme = list(
#'             base = "grey", base_params = list(), custom_params = list()
#'         )
#'     )
#' )
#' ggthemerSERV('myID', theme_store)
#'
#' @importFrom grDevices col2rgb rgb
#' @importFrom stats setNames
#' @importFrom colourpicker updateColourInput
#' @importFrom shinyWidgets updateSliderTextInput
#' @importFrom shinyjs show hide
#' @import shinyBS
#' @import ggplot2
#' @import shiny
#' @export

ggthemerSERV <- function(id, theme_store) {
  moduleServer(
    id,
    function(input, output, session) {

        ns <- session$ns # updating bsButton requires namespace wrap!
        element_tree <- get_element_tree() # speed up calc_element2
        theme_ctrls <- get_theme_params() # for x/y UI updates

        ###########################  Theme controls  ###########################

        active <- reactiveValues( theme_selector = NULL, theme = NULL, base_theme = NULL )
        theme_out <- reactiveValues( theme = NULL, name = NULL, store = NULL ) # this is returned by the module server function

        # update custom theme selector with available themes
        observeEvent(theme_store$custom_themes, {
            updateSelectInput(session, "custom_theme_picker",
                choices = c("", names(theme_store$custom_themes)))
            updateSelectInput(session, "custom_themes",
                  choices = c("", names(theme_store$custom_themes)))
        }, ignoreNULL = T)

        observeEvent(input$theme_picker, {
            if (isTruthy(input$theme_picker)) {
                updateSelectInput(session, "custom_theme_picker", selected = "")
                active$theme_selector <- paste0("theme_picker:", input$theme_picker)
                shinyjs::show("base_ctrls")
            } else {
                shinyjs::hide("base_ctrls")
            }
        })

        observeEvent(input$custom_theme_picker, {
            if (isTruthy(input$custom_theme_picker)) {
                updateSelectInput(session, "theme_picker", selected = "")
                active$theme_selector <- paste0("custom_theme_picker:", input$custom_theme_picker)
                shinyjs::show("custom_base_ctrls")
            } else {
                shinyjs::hide("custom_base_ctrls")
            }
        }, ignoreNULL = T)

        # update theme preview reactive
        observeEvent(active$theme_selector, {
            if (grepl("^theme_picker", active$theme_selector)) {
                theme_store[[id]]$preview_theme <- list(
                    base = isolate(input$theme_picker), # built-in complete themes
                    base_params = list(),
                    custom_params = theme()
                )
            } else if (grepl("^custom_theme_picker", active$theme_selector)) {
                theme_store[[id]]$preview_theme <- isolate(theme_store$custom_themes[[
                    input$custom_theme_picker]])
                base_params <- isolate(theme_store[[id]]$preview_theme[["base_params"]])
                if (length(base_params)) {
                    sapply(names(base_params), function(x) {
                        updateSelectizeInput(
                            session, paste0("custom_", x), selected = as.character(base_params[[x]])
                        )
                    })
                }
            }
          # clear element UI as it will be out of sync (respective clear functions will be triggered)
            updateSelectizeInput(session, "text_element", selected = "")
            updateSelectizeInput(session, "line_element", selected = "")
            updateSelectizeInput(session, "rect_element", selected = "")
        }, ignoreNULL = T, ignoreInit = T)

        observeEvent(input$base_size, {
            if (input$base_size != "11") {
                theme_store[[id]]$preview_theme[["base_params"]]$base_size <- as.numeric(input$base_size)
            } else if ("base_size" %in% names(isolate(theme_store[[id]]$preview_theme[["base_params"]]))) {
                theme_store[[id]]$preview_theme[["base_params"]]$base_size <- NULL
            }
        }, ignoreInit = T)

        observeEvent(input$base_family, {
            if (isTruthy(input$base_family)) {
                theme_store[[id]]$preview_theme[["base_params"]]$base_family = input$base_family
            } else if ("base_family" %in% names(isolate(theme_store[[id]]$preview_theme[["base_params"]]))) {
                theme_store[[id]]$preview_theme[["base_params"]]$base_family <- NULL
            }
        }, ignoreInit = T)

        observeEvent(input$base_line_size, {
            if (input$base_line_size != "0.5") {
                theme_store[[id]]$preview_theme[["base_params"]]$base_line_size = as.numeric(input$base_line_size)
            } else if ("base_line_size" %in% names(isolate(theme_store[[id]]$preview_theme[["base_params"]]))) {
                theme_store[[id]]$preview_theme[["base_params"]]$base_line_size <- NULL
            }
        }, ignoreInit = T)

        observeEvent(input$base_rect_size, {
            if (input$base_rect_size != "0.5") {
                theme_store[[id]]$preview_theme[["base_params"]]$base_rect_size = as.numeric(input$base_rect_size)
            } else if ("base_rect_size" %in% names(isolate(theme_store[[id]]$preview_theme[["base_params"]]))) {
                theme_store[[id]]$preview_theme[["base_params"]]$base_rect_size <- NULL
            }
        }, ignoreInit = T)

        observeEvent(input$custom_base_size, {
            if (input$custom_base_size != "11") {
                theme_store[[id]]$preview_theme[["base_params"]]$base_size = as.numeric(input$custom_base_size)
            } else if ("base_size" %in% names(isolate(theme_store[[id]]$preview_theme[["base_params"]]))) {
                theme_store[[id]]$preview_theme[["base_params"]]$base_size <- NULL
            }
        }, ignoreInit = T)

        observeEvent(input$custom_base_family, {
            if (isTruthy(input$custom_base_family)) {
                theme_store[[id]]$preview_theme[["base_params"]]$base_family = input$custom_base_family
            } else if ("base_family" %in% names(isolate(theme_store[[id]]$preview_theme[["base_params"]]))) {
                theme_store[[id]]$preview_theme[["base_params"]]$base_family <- NULL
            }
        }, ignoreInit = T)

        observeEvent(input$custom_base_line_size, {
            if (input$custom_base_line_size != "0.5") {
                theme_store[[id]]$preview_theme[["base_params"]]$base_line_size = as.numeric(input$custom_base_line_size)
            } else if ("base_line_size" %in% names(isolate(theme_store[[id]]$preview_theme[["base_params"]]))) {
                theme_store[[id]]$preview_theme[["base_params"]]$base_line_size <- NULL
            }
        }, ignoreInit = T)

        observeEvent(input$custom_base_rect_size, {
            if (input$custom_base_rect_size != "0.5") {
                theme_store[[id]]$preview_theme[["base_params"]]$base_rect_size = as.numeric(input$custom_base_rect_size)
            } else if ("base_rect_size" %in% names(isolate(theme_store[[id]]$preview_theme[["base_params"]]))) {
                theme_store[[id]]$preview_theme[["base_params"]]$base_rect_size <- NULL
            }
        }, ignoreInit = T)

        ###########################  Theme preview  ############################

        # preview ggplot object
        themePreview <- ggplot(
            data.frame( x=-2:-1, y=-2:-1,
                colour = c("key1","key2"), shape = c("key3", "key4"),
                facet.x = "strip.x", facet.y = "strip.y"
            )
        ) +
        ggtitle("Plot title", subtitle = "Plot subtitle") +
        geom_point(
            aes(x = x, y = y, colour = colour, shape = shape)
        ) +
        facet_grid(facet.y~facet.x) +
        scale_x_continuous(name = "X axis title", limits = c(0, 2), breaks = 0:2) +
        scale_y_continuous(name = "Y axis title", limits = c(0, 2), breaks = 0:2) +
        guides(
            colour = guide_legend("Legend1 title", order = 1),
            shape = guide_legend("Legend2 title", order = 2)
        )

        # Plot output / calculated theme
        observeEvent(theme_store[[id]]$preview_theme, {
            # construct theme object
            baseTheme <- do.call(
                paste0("theme_", isolate(theme_store[[id]]$preview_theme[["base"]])),
                isolate(theme_store[[id]]$preview_theme[["base_params"]])
            )
            preTheme <- baseTheme +
                do.call(theme, isolate(theme_store[[id]]$preview_theme[["custom_params"]]))

            # SVG plot output: much faster than png-based plotOutput
            # https://github.com/rstudio/shiny/issues/2057#issuecomment-614683545
            output$theme_preview <- renderImage({
                list(src = htmltools::capturePlot({
                    suppressWarnings(print(themePreview + preTheme))
                }, tempfile(fileext = ".svg"), grDevices::svg, width = 5.5, height = 4.5)
              )
            }, deleteFile = T)

            # Base theme is required for code tab updates (findin differences)
            calc_theme <- sapply(names(baseTheme), function(el) {
                calc_element2(el, baseTheme, element_tree)
            })
            active$base_theme <- calc_theme

            # Calculate all elements of the theme (for element UI controls)
            calc_theme <- sapply(names(preTheme), function(el) {
                calc_element2(el, preTheme, element_tree)
            })
            active$theme <- calc_theme
            # Update legend UI controls
            if (is.numeric(calc_theme$legend.position)) {
                updateSelectInput(session, "legend_position", selected = "inside")
                updateSliderInput(session, "legend_position_x", value = as.character(calc_theme$legend.position[1]*100))
                updateSliderInput(session, "legend_position_y", value = as.character(calc_theme$legend.position[2]*100))
            } else {
                updateSelectInput(session, "legend_position", selected = calc_theme$legend.position)
            }
            if (calc_theme$legend.position[1] != "none") {
                updateSelectInput(session, "legend_direction", selected = calc_theme$legend.direction)
                updateSelectInput(session, "legend_box", selected = calc_theme$legend.box)
            }
            # Legend key size
            key_width <- as.numeric(calc_theme$legend.key.width)
            key_height <- as.numeric(calc_theme$legend.key.height)
            key_unit <- unique(c(
                attributes(calc_theme$legend.key.width)$unit,
                attributes(calc_theme$legend.key.height)$unit
            ))
            if (length(key_unit) == 1) {
                key_unit <- c("cm", "in", "line", "", "", "", "mm", "pt")[key_unit]
                updateTextInput(session, "legend_key_width", value = key_width)
                updateTextInput(session, "legend_key_height", value = key_height)
                updateSelectInput(session, "legend_key_unit", selected = key_unit)
            }
            # Legend margins
            marg <- as.numeric(calc_theme$legend.margin)
            marg_unit <- attributes(calc_theme$legend.margin)$unit
            marg_unit <- c("cm", "in", "line", "", "", "", "mm", "pt")[marg_unit]
            updateTextInput(session, "legend_margin_top", value = marg[1])
            updateTextInput(session, "legend_margin_right", value = marg[2])
            updateTextInput(session, "legend_margin_bottom", value = marg[3])
            updateTextInput(session, "legend_margin_left", value = marg[4])
            updateSelectInput(session, "legend_margin_unit", selected = marg_unit)

            # Update R code tab
            output$theme_code <- renderText({
                cmd <- paste0(
                    "theme_", isolate(theme_store[[id]]$preview_theme[["base"]]), "(",
                    list2code(isolate(theme_store[[id]]$preview_theme[["base_params"]])), ")"
                )
                if (length(isolate(theme_store[[id]]$preview_theme[["custom_params"]]))) {
                    cmd <- paste0(cmd, " + theme(",
                    list2code(
                        unclass(isolate(theme_store[[id]]$preview_theme[["custom_params"]])), delim = "\n    "
                    ), "\n)")
                }
                cmd
            })

            # Force element UI update (if any selected)
            if (isTruthy(isolate(input$text_element))) {
                text_element <- isolate(input$text_element)
                updateSelectInput(session, "text_element", selected = "")
                updateSelectInput(session, "text_element", selected = text_element)
            }
            if (isTruthy(isolate(input$line_element))) {
                line_element <- isolate(input$line_element)
                updateSelectInput(session, "line_element", selected = "")
                updateSelectInput(session, "line_element", selected = line_element)
            }
            if (isTruthy(isolate(input$rect_element))) {
                rect_element <- isolate(input$rect_element)
                updateSelectInput(session, "rect_element", selected = "")
                updateSelectInput(session, "rect_element", selected = rect_element)
            }

        }, ignoreInit = F, ignoreNULL = T)

        #############################  UI controls  ############################

        # Initiate without XY buttons
        shinyjs::hide( "text_ctrls" )
        shinyjs::hide( "size_rel" )
        shinyjs::hide( "text_element_axis_x" )
        shinyjs::hide( "text_element_axis_y" )
        shinyjs::hide( "line_ctrls" )
        shinyjs::hide( "line_element_axis_x" )
        shinyjs::hide( "line_element_axis_y" )
        shinyjs::hide( "rect_ctrls" )
        shinyjs::hide( "rect_element_axis_x" )
        shinyjs::hide( "rect_element_axis_y" )

        ##########################  Text elements  #############################
        # Text face
        observeEvent(input$plain, {
            if (input$plain) {
                updateButton(session, ns("bold"), value = FALSE)
                updateButton(session, ns("italic"), value = FALSE)
            }
        })
        observeEvent(c(input$bold, input$italic), {
            if (input$bold | input$italic) {
                updateButton(session, ns("plain"), value = FALSE)
            }
        })

        observeEvent(input$size_toggle, {
            if (input$size_toggle) {
                shinyjs::hide("size")
                shinyjs::show("size_rel")
            } else {
                shinyjs::hide("size_rel")
                shinyjs::show("size")
            }
        })

        # Text horizontal alignment: one button at a time
        observeEvent(input$align_left, {
            if (input$align_left) {
                updateButton(session, ns("align_center"), value = FALSE)
                updateButton(session, ns("align_right"), value = FALSE)
            }
        })
        observeEvent(input$align_center, {
            if (input$align_center) {
                updateButton(session, ns("align_left"), value = FALSE)
                updateButton(session, ns("align_right"), value = FALSE)
            }
        })
        observeEvent(input$align_right, {
            if (input$align_right) {
                updateButton(session, ns("align_center"), value = FALSE)
                updateButton(session, ns("align_left"), value = FALSE)
            }
        })

        # Text vertical alignment: one button at a time
        observeEvent(input$align_top, {
            if (input$align_top) {
                updateButton(session, ns("align_middle"), value = FALSE)
                updateButton(session, ns("align_bottom"), value = FALSE)
            }
        })
        observeEvent(input$align_middle, {
            if (input$align_middle) {
                updateButton(session, ns("align_top"), value = FALSE)
                updateButton(session, ns("align_bottom"), value = FALSE)
            }
        })
        observeEvent(input$align_bottom, {
            if (input$align_bottom) {
                updateButton(session, ns("align_top"), value = FALSE)
                updateButton(session, ns("align_middle"), value = FALSE)
            }
        })

        observeEvent(input$text_element, {
            clear_text_inputs(session) # reset controls
            if (!isTruthy(input$text_element)) {
                shinyjs::hide( "text_ctrls" )
                return()
            }
            shinyjs::show( "text_ctrls" )
            clicked$ID <- "text_element"
            clicked$values <- input$text_element
            # update controls when text element(s) selected
            if (length(input$text_element) > 1) {
                params <- intersect_elements(input$text_element, active$theme)
            } else {
                params <- active$theme[[input$text_element]]
            }
            if (length(params)) {
                if ("colour" %in% names(params)) {
                    if (!grepl("^#", params[["colour"]])) {
                        params[["colour"]] <- rgb(t(col2rgb(params[["colour"]])), maxColorValue = 255)
                    }
                    updateColourInput(session, "text_colour", value = params[["colour"]])
                }
                if ("family" %in% names(params)) {
                    updateSelectInput(session, "family", selected = params[["family"]])
                }
                if ("size" %in% names(params)) {
                    if (inherits(params$size, "rel")) {
                        updateButton(session, ns("size_toggle"), value = T)
                        updateSelectizeInput( session, "size_rel", selected = as.character(params[["size"]] * 100) )
                    } else {
                        updateButton(session, ns("size_toggle"), value = F)
                        updateSelectInput(session, "size", selected = as.character(params[["size"]]))
                    }
                }
                if ("face" %in% names(params)) {
                    if (params[["face"]] == "bold.italic") {
                        updateButton(session, ns("bold"), value = T)
                        updateButton(session, ns("italic"), value = T)
                    } else {
                        updateButton(session, ns(params[["face"]]), value = T)
                    }
                }
                if ("hjust" %in% names(params)) {
                    if (params[["hjust"]] == 0) {
                        updateButton(session, ns("align_left"), value = T)
                    } else if (params[["hjust"]] == 0.5) {
                        updateButton(session, ns("align_center"), value = T)
                    } else if (params[["hjust"]] == 1) {
                        updateButton(session, ns("align_right"), value = T)
                    }
                }
                if ("vjust" %in% names(params)) {
                    if (params[["vjust"]] == 0) {
                        updateButton(session, ns("align_bottom"), value = TRUE)
                    } else if (params[["vjust"]] == 0.5) {
                        updateButton(session, ns("align_middle"), value = TRUE)
                    } else if (params[["vjust"]] == 1) {
                        updateButton(session, ns("align_top"), value = TRUE)
                    }
                }
                if ("angle" %in% names(params)) {
                    if (params[["angle"]] %in% seq(-90, 90, by = 15)) {
                        updateSelectInput(
                            session, "angle", selected = params[["angle"]]
                        )
                    }
                }
                if ("margin" %in% names(params)) {
                    marg <- as.numeric(params[["margin"]])
                    marg_unit <- attributes(params[["margin"]])$unit
                    marg_unit <- c("cm", "in", "line", "", "", "", "mm", "pt")[marg_unit]
                    updateTextInput(session, "margin_top", value = marg[1])
                    updateTextInput(session, "margin_right", value = marg[2])
                    updateTextInput(session, "margin_bottom", value = marg[3])
                    updateTextInput(session, "margin_left", value = marg[4])
                    updateSelectInput(session, "margin_unit", selected = marg_unit)
                }
            }
        }, ignoreNULL = F, ignoreInit = T)

        # text element REMOVE button
        observeEvent(input$rm_text_element, {
            clicked$ID <- "rm_text_element"
            clicked$values <- isolate(input$text_element)
            clear_text_inputs(session) # just in case
        }, ignoreNULL = T, ignoreInit = T)

        # text element APPLY button
        observeEvent(input$apply_text_element, {
            clicked$ID <- "apply_text_element"
            clicked$values <- isolate(input$text_element)
        }, ignoreNULL = T, ignoreInit = T)

        ###########################  Line elements  ############################

        # Arrow type toggles
        observeEvent(input$arrow_open, {
            if (input$arrow_open) {
                updateButton(session, ns("arrow_closed"), value = FALSE)
            }
        })
        observeEvent(input$arrow_closed, {
            if (input$arrow_closed) {
                updateButton(session, ns("arrow_open"), value = FALSE)
            }
        })

        # Show/hide arrow controls
        observeEvent(c(input$arrow_open, input$arrow_closed), {
            if (input$arrow_open | input$arrow_closed) {
                shinyjs::show("arrow_ctrls")
            } else {
                shinyjs::hide("arrow_ctrls")
            }
        }, ignoreNULL = F, ignoreInit = F)

        observeEvent(input$line_element, {
            clear_line_inputs(session)
            if (!isTruthy(input$line_element)) {
                shinyjs::hide( "line_ctrls" )
                return()
            }
            shinyjs::show( "line_ctrls" )
            clicked$ID <- "line_element"
            clicked$values <- input$line_element
            if (length(input$line_element) > 1) {
                params <- intersect_elements(input$line_element, active$theme)
            } else {
                params <- active$theme[[input$line_element]]
            }
            if (length(params)) {
                if ("colour" %in% names(params)) {
                    if (!grepl("^#", params[["colour"]])) {
                        params[["colour"]] <- rgb(t(col2rgb(params[["colour"]])), maxColorValue = 255)
                    }
                    updateColourInput(session, "line_colour", value = params[["colour"]])
                }
                if ("linetype" %in% names(params)) {
                    if (params$linetype %in% c("0", "1", "2", "3")) {
                        params[["linetype"]] <- c("none", "solid", "dashed", "dotted")[
                            as.integer(params[["linetype"]]) + 1
                        ]
                    }
                    updateSelectizeInput(session, "linetype", selected = params[["linetype"]])
                }
                if ("linewidth" %in% names(params)) {
                    updateSelectizeInput(session, "linewidth", selected = params[["linewidth"]])
                } else if ("size" %in% names(params)) {
                    updateSelectizeInput(session, "linewidth", selected = params[["size"]])
                }
                if ("arrow" %in% names(params)) {
                    if (isTruthy(params[["arrow"]])) {
                        thisArrow <- unclass(params[["arrow"]])
                        if (thisArrow$type-1) {
                            updateButton(session, ns("arrow_closed"), value = TRUE)
                        } else {
                            updateButton(session, ns("arrow_open"), value = TRUE)
                        }
                        if (thisArrow$ends %in% c(1, 3)) {
                            updateButton(session, ns("arrow_left"), value = TRUE)
                        } else {
                            updateButton(session, ns("arrow_right"), value = TRUE)
                        }
                        if (thisArrow$angle %in% seq(15, 45, by = 5)) {
                            updateSelectInput(session, "arrow_angle", selected = thisArrow$angle)
                        }
                        if (attributes(thisArrow$length)$unit == 7) {
                            updateSelectInput(session, "arrow_length", selected = as.integer(thisArrow$length))
                        }
                    }
                }
            }
        }, ignoreNULL = F, ignoreInit = T)

        # line element remove button
        observeEvent(input$rm_line_element, {
            clicked$ID <- "rm_line_element"
            clicked$values <- isolate(input$line_element)
            clear_line_inputs(session)
        }, ignoreNULL = T, ignoreInit = T)

        # line element APPLY button
        observeEvent(input$apply_line_element, {
            clicked$ID <- "apply_line_element"
            clicked$values <- isolate(input$line_element)
        }, ignoreNULL = T, ignoreInit = T)

        ########################  Rectangle elements  ##########################

        observeEvent(input$rect_element, {
            clear_rect_inputs(session)
            if (!isTruthy(input$rect_element)) {
                shinyjs::hide( "rect_ctrls" )
                return()
            }
            shinyjs::show( "rect_ctrls" )
            clicked$ID <- "rect_element"
            clicked$values <- input$rect_element
            if (length(input$rect_element) > 1) {
                params <- intersect_elements(input$rect_element, active$theme)
            } else {
                params <- active$theme[[input$rect_element]]
            }
            if (length(params)) {
                if ("fill" %in% names(params)) {
                    if (!grepl("^#", params[["fill"]])) {
                        params[["fill"]] <- rgb(t(col2rgb(params[["fill"]])), maxColorValue = 255)
                    }
                    updateColourInput(session, "rect_fill", value = params[["fill"]])
                }
                if ("linetype" %in% names(params)) {
                    if (params$linetype %in% c("0", "1", "2", "3")) {
                        params[["linetype"]] <- c("none", "solid", "dashed", "dotted")[
                            as.integer(params[["linetype"]]) + 1
                        ]
                    }
                    updateSelectizeInput(session, "rect_linetype", selected = params[["linetype"]])
                }
                if ("linewidth" %in% names(params)) {
                    updateSelectizeInput(session, "rect_linewidth", selected = params[["linewidth"]])
                } else if ("size" %in% names(params)) {
                    updateSelectizeInput(session, "rect_linewidth", selected = params[["size"]])
                }
                if ("colour" %in% names(params)) {
                    if (!grepl("^#", params[["colour"]])) {
                        params[["colour"]] <- rgb(t(col2rgb(params[["colour"]])), maxColorValue = 255)
                    }
                    updateColourInput(session, "rect_colour", value = params[["colour"]])
                }
            }
        }, ignoreNULL = F, ignoreInit = T)

        # rect element remove button
        observeEvent(input$rm_rect_element, {
            clear_rect_inputs(session)
            clicked$ID <- "rm_rect_element"
            clicked$values <- isolate(input$rect_element)
        }, ignoreNULL = T, ignoreInit = T)

        # rect element APPLY button
        observeEvent(input$apply_rect_element, {
            clicked$ID <- "apply_rect_element"
            clicked$values <- isolate(input$rect_element)
        }, ignoreNULL = T, ignoreInit = T)


        ####################  text/line/rect_element observer  #################

        ###  Using reactive values to observe a number of similar buttons.
        # 1. Reactive values that are updated by the rm*/apply* button observers
        clicked <- reactiveValues(
            ID = NULL, # button ID
            values = NULL # elements to act on
        ) # this will be observed and code reused

        # 2. Observer function that will execute relevant code
        observeEvent(c(clicked$ID, clicked$values), {

            elements <- clicked$values # one or more elements of same type
            if (!isTruthy(elements)) return()
            # Watch element selection for X/Y axis hide and seek
            if (grepl("^(text|line|rect)_element$", clicked$ID))  {
                if (any( theme_ctrls$axis[theme_ctrls$id %in% elements] )) {
                    shinyjs::show( paste0(clicked$ID, "_axis_x") )
                    shinyjs::show( paste0(clicked$ID, "_axis_y") )
                } else {
                    shinyjs::hide( paste0(clicked$ID, "_axis_x") )
                    shinyjs::hide( paste0(clicked$ID, "_axis_y") )
                }
                return()
            }

            # Remove/apply buttons
            if (grepl("^(rm|apply)_(text|line|rect)_element$", clicked$ID))  {

                etype <- gsub("^(rm|apply)_", "", clicked$ID) # e.g. 'text_element'
                # update axis-specific values with .x or .y
                xy <- isolate(c(
                    input[[paste0(etype, "_axis_x")]], input[[paste0(etype, "_axis_y")]]
                ))
                if (xor(xy[1], xy[2])) {
                    axis_elements <- elements[theme_ctrls$axis[theme_ctrls$id %in% elements]]
                    if (length(axis_elements)) {
                        elements[ which(elements %in% axis_elements) ] <- paste0(
                            axis_elements,  c(".x", ".y")[xy]
                        )
                    }
                } # ignored if both/neither buttons are selected

                # Remove element buttons
                if (grepl("^rm_(text|line|rect)_element$", clicked$ID))  {
                    # update reactive values
                    sapply(elements, function(x) {
                        theme_store[[id]]$preview_theme$custom_params[[x]] = element_blank()
                    })

                # Apply element buttons: text
                } else if (clicked$ID == "apply_text_element") {
                    txt_inputs <- list()
                    # colour
                    if (isolate(input$text_colour) != "transparent") {
                        txt_inputs$colour  <- gsub("gray", "grey", isolate(input$text_colour))
                    }
                    if (isTruthy(isolate(input$family))) {
                        txt_inputs$family  <- isolate(input$family)
                    }
                    if (isolate(input$size_toggle)) {
                        if (isTruthy(isolate(input$size_rel))) {
                            txt_inputs$size <- rel(as.numeric(isolate(input$size_rel))/100)
                        }
                    } else {
                        # size (absolute)
                        if (isTruthy(isolate(input$size))) {
                            txt_inputs$size <- as.numeric(isolate(input$size))
                        }
                    }
                    # font face
                    face <- c("plain", "bold", "italic")[isolate(c(
                        input$plain, input$bold, input$italic
                    ))]
                    if (length(face)) {
                        if (length(face) == 2) {
                            txt_inputs$face <- "bold.italic"
                        } else { txt_inputs$face <- face }
                    }
                    # horizontal alignment
                    hjust <- c(0, 0.5, 1)[isolate(
                        c(input$align_left, input$align_center, input$align_right)
                    )]
                    if (length(hjust)) txt_inputs$hjust <- hjust
                    # vertical alignment
                    vjust <- c(0, 0.5, 1)[isolate(
                        c(input$align_bottom, input$align_middle, input$align_top)
                    )]
                    if (length(vjust)) txt_inputs$vjust <- vjust
                    # margin around text
                    marg <- isolate(list(
                        t = as.numeric(input$margin_top),
                        r = as.numeric(input$margin_right),
                        b = as.numeric(input$margin_bottom),
                        l = as.numeric(input$margin_left),
                        unit = input$margin_unit
                    ))
                    marg <- marg[!is.na(marg)]
                    if (length(marg) > 1) {
                        txt_inputs$margin <- do.call(margin, marg)
                    }
                    # Angle
                    if (isTruthy(isolate(input$angle))) {
                        txt_inputs$angle <- as.numeric(isolate(input$angle))
                    }

                    # update reactive values (using only differing params)
                    sapply(elements, function(x) {
                        # if the element is already in custom params, remove
                        if (x %in% names(isolate(theme_store[[id]]$preview_theme$custom_params))) {
                            theme_store[[id]]$preview_theme$custom_params[[x]] <- NULL
                        }
                        # find differences between base theme and current element
                        diff_params <- setdiff.list( txt_inputs,
                            unclass(active$base_theme[[x]]) )
                        if (length(diff_params)) {
                            theme_store[[id]]$preview_theme$custom_params[[x]] <- do.call(
                                element_text, diff_params
                            )
                        }
                    }) # end of apply_text_element button actions

                # Apply element buttons: line
                } else if (clicked$ID == "apply_line_element") {

                    line_inputs <- list()
                    # colour
                    if (isolate(input$line_colour) != "transparent") {
                        line_inputs$colour <- gsub("gray", "grey", isolate(input$line_colour))
                    }
                    # linetype
                    if (isolate(input$linetype) != "none") {
                        line_inputs$linetype <- isolate(input$linetype)
                    }
                    # linewidth (size previously)
                    if (isTruthy(isolate(input$linewidth))) {
                        line_inputs$linewidth <- as.numeric(isolate(input$linewidth))
                    }
                    # arrow
                    if (any(
                        isolate(c(input$arrow_open, input$arrow_closed))
                    )) {
                        arrCtrls <- list()
                        arrCtrls$type = c("open", "closed")[isolate(c(input$arrow_open, input$arrow_closed))]
                        if (isTruthy(isolate(input$arrow_angle))) {
                            arrCtrls$angle <- as.numeric(isolate(input$arrow_angle))
                        }
                        if (isTruthy(isolate(input$arrow_length))) {
                            arrCtrls$length <- unit(as.numeric(isolate(input$arrow_length)), "mm")
                        }
                        arrCtrls$ends <- "last" # default
                        ends <- c("first", "last")[isolate(c(input$arrow_left, input$arrow_right))]
                        if (length(ends) == 2) {
                            arrCtrls$ends <- "both"
                        } else if (length(ends) == 1) {
                            arrCtrls$ends <- ends
                        }
                        line_inputs$arrow = do.call(arrow, arrCtrls)
                    }

                    # update reactive values
                    sapply(elements, function(x) {
                        # if the element is already in custom params, remove
                        if (x %in% names(isolate(theme_store[[id]]$preview_theme$custom_params))) {
                            theme_store[[id]]$preview_theme$custom_params[[x]] <- NULL
                        }
                        # find differences between base theme and current element
                        diff_params <- setdiff.list( line_inputs,
                            unclass(active$base_theme[[x]]) )
                        if (length(diff_params)) {
                            theme_store[[id]]$preview_theme$custom_params[[x]] <- do.call(
                                element_line, diff_params
                            )
                        }
                    }) # end of apply_line_element button actions

                # Apply element buttons: rect
                } else if (clicked$ID == "apply_rect_element") {

                    rect_inputs <- list()
                    # Fill
                    # TODO: NA value (for no fill)
                    if (isolate(input$rect_fill) != "transparent") {
                        rect_inputs$fill <- gsub("gray", "grey", isolate(input$rect_fill))
                    }
                    # Linetype
                    if (isTruthy(isolate(input$rect_linetype))) {
                        rect_inputs$linetype <- isolate(input$rect_linetype)
                    }
                    # Linewidth
                    if (isTruthy(isolate(input$rect_linewidth))) {
                        rect_inputs$linewidth <- as.numeric(isolate(input$rect_linewidth))
                    }
                    # Line colour
                    if (isolate(input$rect_colour) != "transparent") {
                        rect_inputs$colour <- gsub("gray", "grey", isolate(input$rect_colour))
                    }
                    # update reactive values
                    sapply(elements, function(x) {
                        # if the element is already in custom params, remove
                        if (x %in% names(isolate(theme_store[[id]]$preview_theme$custom_params))) {
                            theme_store[[id]]$preview_theme$custom_params[[x]] <- NULL
                        }
                        # find differences between base theme and current element
                        diff_params <- setdiff.list( rect_inputs,
                            unclass(active$base_theme[[x]]) )
                        if (length(diff_params)) {
                            theme_store[[id]]$preview_theme$custom_params[[x]] <- do.call(
                                element_rect, diff_params
                            )
                        }
                    })
                } # end of apply_rect_element button actions
            } # end of rm/apply buttons actions

        }, ignoreInit = T)

        ###########################  Legend settings  ##########################

        observeEvent(input$legend_position, {
            if (input$legend_position == "inside") {
                shinyjs::show("legend_position_ctrls")
            } else {
                shinyjs::hide("legend_position_ctrls")
            }
            if (input$legend_position == "none") {
                shinyjs::hide("legend_ctrls")
            } else {
                shinyjs::show("legend_ctrls")
            }
        })

        observeEvent(input$apply_legend, {
            legend_els <- list()
            legend_position <- isolate(input$legend_position)
            updateSelectInput(session, "legend_position", selected = "")
            if ( isTruthy(legend_position) ) {
                if (legend_position[1] == "inside") {
                    legend_els$legend.position <- as.numeric(isolate(c(
                        input$legend_position_x, input$legend_position_y))
                    )/100
                    updateSliderInput(session, "legend_position_x", value = "0")
                    updateSliderInput(session, "legend_position_y", value = "0")
                } else {
                    legend_els$legend.position <- legend_position
                }
            }
            if (legend_position[1] != "none") {

                if ( isTruthy(isolate(input$legend_direction))) {
                    legend_els$legend.direction <- isolate(input$legend_direction)
                    updateSelectInput(session, "legend_direction", selected = "")
                }

                if ( isTruthy(isolate(input$legend_box))) {
                    legend_els$legend.box <- isolate(input$legend_box)
                    updateSelectInput(session, "legend_box", selected = "")
                }
            }
            # Key size
            if ( isTruthy(isolate(input$legend_key_width))) {
                legend_els$legend.key.width <- isolate(unit(input$legend_key_width, input$legend_key_unit))
                updateTextInput(session, "legend_key_width", value = "")
            }
            if ( isTruthy(isolate(input$legend_key_height))) {
                legend_els$legend.key.height <- isolate(unit(input$legend_key_height, input$legend_key_unit))
                updateTextInput(session, "legend_key_height", value = "")
            }
            # Margin
            marg <- isolate(list(
                t = as.numeric(input$legend_margin_top),
                r = as.numeric(input$legend_margin_right),
                b = as.numeric(input$legend_margin_bottom),
                l = as.numeric(input$legend_margin_left),
                unit = input$legend_margin_unit
            ))
            marg <- marg[!is.na(marg)]
            if (length(marg) > 1) {
                legend_els$legend.margin <- do.call(margin, marg)
                updateTextInput(session, "legend_margin_top", value = "")
                updateTextInput(session, "legend_margin_right", value = "")
                updateTextInput(session, "legend_margin_bottom", value = "")
                updateTextInput(session, "legend_margin_left", value = "")
            }
            # update reactive values
            sapply(names(legend_els), function(x) {
                # if the element is already in custom params, remove
                if (x %in% names(isolate(theme_store[[id]]$preview_theme$custom_params))) {
                    theme_store[[id]]$preview_theme$custom_params[[x]] <- NULL
                }
                if (!identical( legend_els[[x]], active$base_theme[[x]] )) {
                    theme_store[[id]]$preview_theme$custom_params[[x]] <- legend_els[[x]]
                }
            })
        })

        observeEvent(input$save_theme, {
            if (!input$save_theme || is.null(input$save_theme)) {
                shinyjs::hide("custom_themes")
                shinyjs::hide("theme_name")
            } else {
                shinyjs::show("custom_themes")
                shinyjs::show("theme_name")
            }
        }, ignoreInit = F, ignoreNULL = F)

        observeEvent(input$apply_theme, {
            preview_theme <- isolate(theme_store[[id]]$preview_theme)
            theme_out$theme <- do.call(
                paste0("theme_", preview_theme$base),
                preview_theme$base_params
            ) + do.call(theme, preview_theme$custom_params)
            # Saving theme?
            if (isTruthy(isolate(input$theme_name))) {
                theme_out$name <- isolate(input$theme_name)
            } else if (isTruthy(isolate(input$custom_themes))) {
                theme_out$name <- isolate(input$custom_themes)
            }
            if (!is.null(theme_out$name)) {
                theme_out$store <- preview_theme
            }
        })

        return(reactive(
            list(
                theme = theme_out$theme,
                name = theme_out$name,
                store = theme_out$store
            )
        ))
    }
  )
}

#' Clear text inputs
#'
#' @keywords internal
clear_text_inputs <- function(session) {
    ns <- session$ns
    updateColourInput( session, "text_colour", value = "transparent" )
    updateSelectInput(session, "family", selected = "")
    updateSelectInput(session, "size", selected = "")
    updateSelectInput(session, "size_rel", selected = "")
    updateButton(session, ns("size_toggle"), value = F)
    updateButton(session, ns("plain"), value = FALSE)
    updateButton(session, ns("bold"), value = FALSE)
    updateButton(session, ns("italic"), value = FALSE)
    updateButton(session, ns("bold"), value = FALSE)
    updateButton(session, ns("italic"), value = FALSE)
    updateButton(session, ns("align_left"), value = FALSE)
    updateButton(session, ns("align_center"), value = FALSE)
    updateButton(session, ns("align_right"), value = FALSE)
    updateButton(session, ns("align_bottom"), value = FALSE)
    updateButton(session, ns("align_middle"), value = FALSE)
    updateButton(session, ns("align_top"), value = FALSE)
    updateTextInput(session, "margin_left", value = "")
    updateTextInput(session, "margin_top", value = "")
    updateTextInput(session, "margin_right", value = "")
    updateTextInput(session, "margin_bottom", value = "")
    updateSliderTextInput(session, "angle", selected = "na")
}

#' Clear line inputs
#'
#' @keywords internal
clear_line_inputs <- function(session) {
    ns <- session$ns
    updateColourInput(session, "line_colour", value = "transparent")
    updateSelectizeInput(session, "linetype", selected = "")
    updateSelectizeInput(session, "linewidth", selected = "")
    updateButton(session, ns("arrow_open"), value = FALSE)
    updateButton(session, ns("arrow_closed"), value = FALSE)
    updateSelectInput(session, "arrow_angle", selected = "")
    updateSelectInput(session, "arrow_length", selected = "")
    updateButton(session, ns("arrow_left"), value = FALSE)
    updateButton(session, ns("arrow_right"), value = FALSE)
}

#' Clear rect inputs
#'
#' @keywords internal
clear_rect_inputs <- function(session) {
    updateColourInput(session, "rect_fill", value = "transparent")
    updateSelectizeInput(session, "rect_linetype", selected = "")
    updateSelectizeInput(session, "rect_linewidth", selected = "")
    updateColourInput(session, "rect_colour", value = "transparent")
}

