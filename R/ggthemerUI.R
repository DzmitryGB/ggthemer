#' Create ggthemer UI component
#'
#' Create user interface for the ggplot theme menu.
#'
#' @param id Character variable
#'
#' @param colour_palette character vector
#'
#' @param cached_fonts list
#'
#' @return shiny UI
#'
#' @examples
#'
#' cached_fonts <- list(
#'      Roboto = list(
#'          family = "Roboto",
#'          regular = "fonts/Roboto-Regular.ttf",
#'          bold = "fonts/Roboto-Bold.ttf",
#'          italic = "fonts/Roboto-Italic.ttf",
#'          bolditalic = "fonts/Roboto-BoldItalic.ttf"
#'     )
#' )
#' ggthemerUI('myID', cached_fonts)
#'
#' @importFrom stats setNames
#' @importFrom colourpicker colourInput
#' @importFrom shinyjs show hide
#' @importFrom shinyWidgets dropdownButton prettySwitch tooltipOptions
#' @importFrom htmltools htmlDependency
#' @importFrom sysfonts font_families font_add
#' @importFrom showtext showtext_auto
#' @import shinyBS
#' @import ggplot2
#' @import shiny
#' @export

ggthemerUI <- function(id, cached_fonts = NULL, colour_palette = NULL) {

    ns <- NS(id)

    # includeCSS(system.file("ggthemer.css", package = "ggthemer", mustWork = T))

    # Prepare element data.frame for the select inputs
    theme_ctrls <- get_theme_params()

    if (!is.null(colour_palette)) {
        colour_palette <- unique(c("transparent", colour_palette))
    }

    ###  Check available and load user-provided fonts
    available_fonts <- sysfonts::font_families()
    if (!is.null(cached_fonts)) {
        load_me <- names(cached_fonts)[!sapply(names(cached_fonts), function(x) {
            x %in% available_fonts
        })]
        if (length(load_me)) {
            sapply(load_me, function(x) {
                try({ do.call(font_add, cached_fonts[[x]]) })
            })
        }
        available_fonts <- sysfonts::font_families()
    }
    available_fonts <- sort(available_fonts[available_fonts != "wqy-microhei"])
    available_fonts <- setNames(available_fonts, tools::toTitleCase(available_fonts))
    showtext_auto()
    ###  UI
    dropdownButton(

        singleton(
            htmlDependency(
                name = "ggthemer_css",
                version = utils::packageVersion("ggthemer"),
                package = "ggthemer",
                src = ".",
                stylesheet = "ggthemer.css"
              )
        ),

        fluidRow(
            column(5,
                tabsetPanel(tabPanel(
                    ############################################################
                    "Themes", ##################################################
                    ############################################################
                    bsCollapse(
                        bsCollapsePanel(
                            ####################################################
                            "Built-in themes", #################################
                            ####################################################
                            selectInput(
                                ns("theme_picker"), "ggplot2 themes", choices = c(
                                    "", "grey", "bw", "linedraw", "light", "dark", "classic", "minimal", "void"
                                ), selected = "grey"
                            ),
                            div(
                                id = ns("base_ctrls"),
                                hr(style = "border-top: 1px solid #337ab7; margin: 20px -15px;"),
                                div(
                                    class = "ggtinline bottom", style = "margin-bottom:15px;",
                                    div(
                                        style = "display:flex;flex-flow:column wrap;",
                                        div(
                                            HTML("<div class='label-primary'>Base font</div>")
                                        ),
                                        div(
                                            class = "ggtinline bottom",
                                            selectizeInput(
                                                ns("base_size"), "Size", c("", 8:14), selected = "11",
                                                options = list(create = T), width = "60px"
                                            ),
                                            selectizeInput(
                                                ns("base_family"), "Family", width = "155px",
                                                choices = c("", available_fonts)
                                            )
                                        )
                                    ), # font controls
                                    div(
                                        style = "display:flex;flex-flow:column wrap;",
                                        div(
                                            class = "label-primary",
                                            tags$text("Line thickness, mm")
                                        ),
                                        div(
                                            class = "ggtinline bottom",
                                            selectizeInput(
                                                ns("base_line_size"), "Lines", c("", 0.25*(1:6), 2), selected = "0.5",
                                                options = list(create = T), width = "65px"
                                            ),
                                            selectizeInput(
                                                ns("base_rect_size"), "Borders", c("", 0.25*(1:6), 2), selected = "0.5",
                                                options = list(create = T), width = "65px"
                                            )
                                        )
                                    ) # line thickness controls
                                )

                            ), # theme base controls

                            style = "primary"
                        ),
                        bsCollapsePanel(
                            ####################################################
                            "Custom themes", ###################################
                            ####################################################
                            selectInput(
                                ns("custom_theme_picker"), "Available themes", choices = ""
                            ),

                            div(
                                id = ns("custom_base_ctrls"),
                                hr(style = "border-top: 1px solid #337ab7; margin: 20px -15px;"),
                                div(
                                    class = "ggtinline bottom", style = "margin-bottom:15px;",
                                    div(
                                        style = "display:flex;flex-flow:column wrap;",
                                        div(
                                            HTML("<div class='label-primary'>Base font</div>")
                                        ),
                                        div(
                                            class = "ggtinline bottom",
                                            selectizeInput(
                                                ns("custom_base_size"), "Size", c("", 8:14), selected = "11",
                                                options = list(create = T), width = "60px"
                                            ),
                                            selectizeInput(
                                                ns("custom_base_family"), "Family", width = "155px",
                                                choices = c("", available_fonts)
                                            )
                                        )
                                    ), # font controls
                                    div(
                                        style = "display:flex;flex-flow:column wrap;",
                                        div(
                                            class = "label-primary",
                                            tags$text("Line thickness, mm")
                                        ),
                                        div(
                                            class = "ggtinline bottom",
                                            selectizeInput(
                                                ns("custom_base_line_size"), "Lines", c("", 0.25*(1:6), 2), selected = "0.5",
                                                options = list(create = T), width = "65px"
                                            ),
                                            selectizeInput(
                                                ns("custom_base_rect_size"), "Borders", c("", 0.25*(1:6), 2), selected = "0.5",
                                                options = list(create = T), width = "65px"
                                            )
                                        )
                                    ) # line thickness controls
                                )

                            ), # theme base controls

                            style = "primary"
                        ),
                        open = "Built-in themes"
                    )
                ),

                tabPanel(
                    ############################################################
                    "Components", ##############################################
                    ############################################################
                    bsCollapse(
                        bsCollapsePanel(
                            ####################################################
                            "Text", ############################################
                            ####################################################
                            div(
                                class = "ggtinline bottom",
                                div(
                                    style = "min-width:220px;max-width:300px;flex-grow:2;",
                                    selectInput(
                                        ns("text_element"), "Select text element(s):",
                                        setNames(
                                            theme_ctrls$id[theme_ctrls$type == "element_text"],
                                            theme_ctrls$name[theme_ctrls$type == "element_text"]
                                        ), multiple = T, width = "100%"
                                    )
                                ),
                                div(

                                    bsButton(
                                        ns("text_element_axis_x"), NULL, icon("x"),
                                        title = "X axis", type = "toggle", value = F
                                    ),

                                    bsButton(
                                        ns("text_element_axis_y"), NULL, icon("y"),
                                        title = "Y axis", type = "toggle", value = F
                                    )
                                ),

                                div(
                                    style = "margin-left: auto;",
                                    bsButton(
                                        ns("rm_text_element"), NULL, icon("remove", lib = "glyphicon"),
                                        title = "Remove element from plot",
                                        style = "danger", type = "action"
                                    )
                                )
                            ),

                            div(
                                id = ns("text_ctrls"),
                                hr(style = "border-top: 1px solid #337ab7; margin: 20px -15px;"),
                                div(
                                    class = "ggtinline bottom", style = "margin-bottom:15px;",
                                    # Colour, family and size row
                                    div(
                                        class = "ggtinline bottom",
                                        style = "width:100%",
                                        div(
                                            class = "colourInput",
                                            colourpicker::colourInput(
                                                ns("text_colour"), "Colour", value = "transparent",
                                                showColour = "background", palette = "limited",
                                                allowedCols = colour_palette, returnName = T
                                            )
                                        ),
                                        div(
                                            style = "flex-grow:2;min-width:150px",
                                            selectInput(ns("family"), "Family", c("", available_fonts), width = "100%")
                                        ),
                                        div(
                                            style = "display:flex;flex-flow:column wrap;",
                                            div(class = "label", tags$text("Face")),
                                            div(
                                                class = "ggtinline bottom", div(
                                                bsButton( ns("plain"), "P", title = "Plain", type = "toggle" ),
                                                bsButton( ns("bold"), NULL, icon("bold", lib = "glyphicon"), title = "Bold", type = "toggle" ),
                                                bsButton( ns("italic"), NULL, icon("italic", lib = "glyphicon"), title= "Italic", type = "toggle" )
                                            ))
                                        )

                                    ),
                                    ## Size
                                    div(
                                        style = "display:flex;flex-flow:column wrap;",
                                        div(class = "label-primary", tags$text("Size")),
                                        div(
                                            class = "ggtinline bottom",
                                            selectizeInput(
                                                ns("size"), "Points", c("", 8:14), options = list(create = T), width = "60px"
                                            ),
                                            selectizeInput(
                                                ns("size_rel"), "Relative", c("", 10*(7:12)), options = list(create = T), width = "60px"
                                            ),
                                            bsButton( ns("size_toggle"), NULL, icon("percent"), title= "Relative size", type = "toggle" )
                                        )
                                    ),

                                  # Alignment controls
                                  div(
                                        style = "display:flex;flex-flow:column wrap;",
                                        div(
                                            class = "label-primary",
                                            tags$text("Alignment")
                                        ),
                                        div(
                                            class = "ggtinline bottom",
                                            div(
                                                style = "display:flex;flex-flow:column wrap;",
                                                div(
                                                    class = "label",
                                                    tags$text("Horizontal")
                                                ),
                                                div(
                                                    bsButton( ns("align_left"), NULL, icon("align-left", lib = "glyphicon"), title = "Align left", type = "toggle" ),
                                                    bsButton( ns("align_center"), NULL, icon("align-center", lib = "glyphicon"), title = "Align center", type = "toggle" ),
                                                    bsButton( ns("align_right"), NULL, icon("align-right", lib = "glyphicon"), title = "Align right", type = "toggle" )
                                                )
                                            ),

                                            div(
                                                style = "display:flex;flex-flow:column wrap;",
                                                div(
                                                    class = "label",
                                                    tags$text("Vertical")
                                                ),
                                                div(
                                                    bsButton( ns("align_top"), NULL, icon("object-align-top", lib = "glyphicon"), title = "Align top", type = "toggle" ),
                                                    bsButton( ns("align_middle"), NULL, icon("object-align-horizontal", lib = "glyphicon"), title = "Align middle", type = "toggle" ),
                                                    bsButton( ns("align_bottom"), NULL, icon("object-align-bottom", lib = "glyphicon"), title = "Align bottom", type = "toggle" )
                                                )
                                            )
                                        )
                                    ),

                                    div(
                                        style = "display:flex;flex-flow:column wrap;",
                                        div(class = "label-primary", tags$text("Rotation")),
                                        div(
                                            selectizeInput(
                                                ns("angle"), "Angle, \u00B0", c("", seq(-90, 90, by = 15)),
                                                options = list(create = T), width = "75px"
                                            )
                                        )
                                    ),

                                    div(
                                        style = "display:flex;flex-flow:column wrap;",
                                        div(
                                            class = "label-primary",
                                            tags$text("Margins")
                                        ),
                                        div(
                                            class = "ggtinline bottom",
                                            textInput(ns("margin_top"), "Top", placeholder = "top", width = "53px"),
                                            textInput(ns("margin_right"), "Right", placeholder = "right", width = "53px"),
                                            textInput(ns("margin_bottom"), "Bottom", placeholder = "bottom", width = "53px"),
                                            textInput(ns("margin_left"), "Left", placeholder = "left", width = "53px"),
                                            div(
                                                style = "flex-grow:2;min-width:60px;",
                                                selectInput(
                                                    ns("margin_unit"), "unit", choices = c("pt", "mm", "cm", "in", "line"),
                                                    selected = "pt", width = "100%"
                                                )
                                            )
                                        )
                                    ),

                                    # Apply button
                                    div(
                                        style = "margin-left: auto;",
                                        bsButton(
                                            ns("apply_text_element"), NULL, icon("ok", lib = "glyphicon"),
                                            title = "Apply text formatting", style = "success", type = "action"
                                        )
                                    )

                                ) # main flex wrap
                            ), # text_ctrls

                            style = "primary"
                        ),

                        bsCollapsePanel(
                            ####################################################
                            "Line", ############################################
                            ####################################################
                            div(
                                class = "ggtinline bottom",
                                div(
                                    style = "min-width:220px;max-width:300px;flex-grow:2;",
                                    selectInput(
                                        ns("line_element"), "Select line element(s):",
                                        setNames(
                                            theme_ctrls$id[theme_ctrls$type == "element_line"],
                                            theme_ctrls$name[theme_ctrls$type == "element_line"]
                                        ), multiple = T, width = "100%"
                                    )
                                ),
                                div(
                                    bsButton(
                                        ns("line_element_axis_x"), NULL, icon("x"),
                                        title = "X axis", type = "toggle", value = F
                                    ),
                                    bsButton(
                                        ns("line_element_axis_y"), NULL, icon("y"),
                                        title = "Y axis", type = "toggle", value = F
                                    )
                                ),
                                div(
                                    style = "margin-left: auto;",
                                    bsButton(
                                        ns("rm_line_element"), NULL, icon("remove", lib = "glyphicon"),
                                        title = "Remove element from plot",
                                        style = "danger", type = "action"
                                    )
                                )
                            ),

                            div(
                                id = ns("line_ctrls"),
                                hr(style = "border-top: 1px solid #337ab7; margin: 15px -15px;"),
                                div(
                                    class = "ggtinline bottom",
                                    div(
                                        class = "colourInput",
                                        colourpicker::colourInput(
                                            ns("line_colour"), "Colour", value = "transparent",
                                            showColour = "background", palette = "limited",
                                            allowedCols = colour_palette, returnName = T
                                        )
                                    ),
                                    div(
                                        style = "flex-grow:2;min-width:80px;",
                                        selectizeInput(
                                            ns("linetype"), "Line type", setNames(
                                                c("none", "solid", "dashed", "dotted"),
                                                c("", "solid", "dashed", "dotted")
                                            ),
                                            width = "100%", options = list( render = I(
                                                "{ item: function(item, escape) {
                                                return '<hr style=\"border:none;border-top: 2px ' +
                                                item.label + ' black;width:70%;margin:5px auto;\"/>'; },
                                                option: function(item, escape) {
                                                return '<hr style=\"border:none;border-top: 2px ' +
                                                item.label + ' black;width:60%;margin:15px auto;\"/>'; } }"
                                            ))
                                        )
                                    ),
                                    selectizeInput(
                                        ns("linewidth"), "Thickness", c("", 0.25*(1:8)), options = list(create = T), width = "75px"
                                    ),
                                    div(
                                      HTML("<label class = 'control-label'>Arrow:</label>"),
                                      div(
                                          bsButton(
                                              ns("arrow_open"), NULL, icon("menu-right", lib = "glyphicon"),
                                              title = "Open arrow", type = "toggle", value = F
                                          ),
                                          bsButton(
                                              ns("arrow_closed"), NULL, icon("triangle-right", lib = "glyphicon"),
                                              title = "Closed arrow", type = "toggle", value = F
                                          )
                                      )
                                    ),
                                    div(
                                        style = "margin-left: auto;",
                                        bsButton(
                                            ns("apply_line_element"), NULL, icon("ok", lib = "glyphicon"),
                                            title = "Apply line formatting", style = "success", type = "action"
                                        )
                                    )
                                ),


                                div(
                                    class = "ggtinline bottom",
                                    div(
                                        id = ns("arrow_ctrls"),
                                        style = "display:flex;flex-flow:column wrap;margin-top:20px;margin-bottom:15px;width:250px;",
                                        div(class = "label-primary", tags$text("Arrow")),
                                        div(
                                            class = "ggtinline bottom",
                                            selectInput(
                                                ns("arrow_angle"), "Angle, \u00B0", c("", paste0(seq(15, 45, by = 5))), width = "70px"
                                            ),
                                            selectizeInput(
                                                ns("arrow_length"), "Size, mm", c("", 1:8), options = list(create = T), width = "70px"
                                            ),
                                            div(
                                                HTML("<label>End(s)</label><br>"),
                                                bsButton(
                                                    ns("arrow_left"), NULL, icon("arrow-left", lib = "glyphicon"),
                                                    title = "Arrow at the start", type = "toggle", value = F
                                                ),
                                                bsButton(
                                                    ns("arrow_right"), NULL, icon("arrow-right", lib = "glyphicon"),
                                                    title = "Arrow at the end", type = "toggle", value = F
                                                )
                                            )
                                        )
                                    )
                                )
                            ),

                            style = "primary"
                        ),

                        bsCollapsePanel(
                            ####################################################
                            "Rectangle", #######################################
                            ####################################################
                            div(
                                class = "ggtinline bottom",
                                div(
                                    style = "min-width:220px;max-width:300px;flex-grow:2;",
                                    selectInput(
                                        ns("rect_element"), "Select rectangle element(s):",
                                        setNames(
                                            theme_ctrls$id[theme_ctrls$type == "element_rect"],
                                            theme_ctrls$name[theme_ctrls$type == "element_rect"]
                                        ), multiple = T, width = "100%"
                                    )
                                ),

                                div(
                                    bsButton(
                                        ns("rect_element_axis_x"), NULL, icon("x"),
                                        title = "X axis", type = "toggle", value = F
                                    ),
                                    bsButton(
                                        ns("rect_element_axis_y"), NULL, icon("y"),
                                        title = "Y axis", type = "toggle", value = F
                                    )
                                ),

                                div(
                                    style = "margin-left: auto;",
                                    bsButton(
                                        ns("rm_rect_element"), NULL, icon("remove", lib = "glyphicon"),
                                        title = "Remove element from plot",
                                        style = "danger", type = "action"
                                    )
                                )
                            ),

                            div(
                                id = ns("rect_ctrls"),
                                hr(style = "border-top: 1px solid #337ab7; margin: 15px -15px;"),
                                div(
                                    class = "ggtinline bottom",
                                    div(
                                        class = "colourInput",
                                        colourpicker::colourInput(
                                            ns("rect_fill"), "Fill", value = "transparent",
                                            showColour = "background", palette = "limited",
                                            allowedCols = colour_palette, returnName = T
                                        )
                                    ),
                                    div(
                                        style = "display:flex;flex-flow:column wrap;flex-grow:2;min-width:200px;",
                                        div( class = "label-primary", tags$text("Border") ),
                                        div(
                                            class = "ggtinline bottom",
                                            div(
                                                style = "flex-grow:2;min-width:80px;",
                                                selectizeInput(
                                                    ns("rect_linetype"), "Line type", setNames(
                                                        c("none", "solid", "dashed", "dotted"),
                                                        c("", "solid", "dashed", "dotted")
                                                    ), width = "100%", options = list( render = I(
                                                      "{ item: function(item, escape) {
                                                      return '<hr style=\"border:none;border-top: 2px ' +
                                                      item.label + ' black;width:70%;margin:5px auto;\"/>'; },
                                                      option: function(item, escape) {
                                                      return '<hr style=\"border:none;border-top: 2px ' +
                                                      item.label + ' black;width:60%;margin:15px auto;\"/>'; } }"
                                                    ))
                                                )
                                            ),
                                            selectizeInput(
                                                ns("rect_linewidth"), "Thickness", c("", 0.25*(1:8)), options = list(create = T), width = "75px"
                                            ),
                                            div(
                                                class = "colourInput",
                                                colourpicker::colourInput(
                                                    ns("rect_colour"), "Colour", value = "transparent",
                                                    showColour = "background", palette = "limited",
                                                    allowedCols = colour_palette, returnName = T
                                                )
                                            )
                                        )
                                    ),
                                    div(
                                        style = "margin-left: auto;",
                                        bsButton(
                                            ns("apply_rect_element"), NULL, icon("ok", lib = "glyphicon"),
                                            title = "Apply rectangle formatting", style = "success", type = "action"
                                        )
                                    )
                                )
                            ),

                            style = "primary"
                        ),

                        bsCollapsePanel(
                            "Legend",
                            div(
                                class = "ggtinline bottom",
                                selectInput(
                                    ns("legend_position"), "Position", choices = c(
                                        "", "none", "left", "top", "right", "bottom", "inside"
                                    ), selected = "", width = "100px"
                                ),
                                div(
                                    id = ns("legend_position_ctrls"),
                                    class = "ggtinline bottom",
                                    HTML("<label>X</label>"),
                                    sliderInput(
                                        ns("legend_position_x"), NULL, min = 0, max =  100, step = 5,
                                        value = "0", ticks = F, width = "100px", post = "%"
                                    ),
                                    HTML("<label>Y</label>"),
                                    sliderInput(
                                        ns("legend_position_y"), NULL, min = 0, max =  100, step = 5,
                                        value = "0", ticks = F, width = "100px", post = "%"
                                    )
                                )
                            ),

                            div(
                                id = ns("legend_ctrls"),
                                hr(style = "border-top: 1px solid #337ab7; margin: 15px -15px;"),

                                div(
                                    class = "ggtinline bottom",
                                    div(
                                        style = "display:flex;flex-flow:column wrap;",
                                        div(
                                            class = "label-primary",
                                            tags$text("Element flow")
                                        ),
                                        div(
                                            class = "ggtinline bottom",
                                            selectInput(
                                                ns("legend_direction"), "Keys", choices = c(
                                                  "", "horizontal", "vertical"
                                                ), width = "100px"
                                            ),
                                            selectInput(
                                                ns("legend_box"), "Legends", choices = c(
                                                  "", "horizontal", "vertical"
                                                ), width = "100px"
                                            )
                                        )
                                    ),
                                    # legend.key.height, legend.key.width
                                    div(
                                        style = "display:flex;flex-flow:column wrap;",
                                        div(
                                            class = "label-primary",
                                            tags$text("Key size")
                                        ),
                                        div(
                                            class = "ggtinline bottom",
                                            textInput(ns("legend_key_width"), "Width", placeholder = "width", width = "48px"),
                                            textInput(ns("legend_key_height"), "Height", placeholder = "height", width = "48px"),
                                            div(
                                                style = "flex-grow:2;min-width:55px;",
                                                selectInput(
                                                    ns("legend_key_unit"), "unit", choices = c("pt", "mm", "cm", "in", "line"),
                                                    selected = "pt", width = "100%"
                                                )
                                            )
                                        )
                                    ),
                                    # Margins
                                    div(
                                        style = "display:flex;flex-flow:column wrap;",
                                        div(
                                            class = "label-primary",
                                            tags$text("Margins")
                                        ),
                                        div(
                                            class = "ggtinline bottom",
                                            textInput(ns("legend_margin_top"), "Top", placeholder = "top", width = "53px"),
                                            textInput(ns("legend_margin_right"), "Right", placeholder = "right", width = "53px"),
                                            textInput(ns("legend_margin_bottom"), "Bottom", placeholder = "bottom", width = "53px"),
                                            textInput(ns("legend_margin_left"), "Left", placeholder = "left", width = "53px"),
                                            div(
                                                style = "flex-grow:2;min-width:60px;",
                                                selectInput(
                                                    ns("legend_margin_unit"), "unit", choices = c("pt", "mm", "cm", "in", "line"),
                                                    selected = "pt", width = "100%"
                                                )
                                            )
                                        )
                                    ),
                                    # Apply legend button
                                    div(
                                        style = "margin-left: auto;",
                                        bsButton(
                                            ns("apply_legend"), NULL, icon("ok", lib = "glyphicon"),
                                            title = "Apply legend settings", style = "success", type = "action"
                                        )
                                    )

                                )

                            ),

                            style = "primary"
                        ),

                        open = "Text"
                    )

                ))

            ), column(7,

                tabsetPanel(

                    tabPanel(
                        ########################################################
                        "Theme preview", #######################################
                        ########################################################
                        HTML("<center>"),
                        imageOutput(ns("theme_preview"), width = "528px", height = "432px"),
                        HTML("</center>")
                    ),
                    tabPanel(
                        ########################################################
                        "R code", ##############################################
                        ########################################################
                        verbatimTextOutput(ns("theme_code"))
                    ),
                    footer = div(
                        class = "ggtinline bottom",
                        style = "margin:10px;width:95%;",
                        div(
                            class = "ggtinline center",
                            prettySwitch(ns("save_theme"), "Save", status = "primary", width = "100px"),
                            selectInput(ns("custom_themes"), "Overwrite", choices = "", width = "150px"),
                            textInput(ns("theme_name"), "New name", width = "150px")
                        ),
                        div(
                            actionButton(ns("apply_theme"), "Apply")
                        )
                    )
                )
        )),

        circle = T, status = "primary",
        icon = icon("palette"), width = "1050px",
        tooltip = tooltipOptions(title = "Modify plot appearance")
    )
}
