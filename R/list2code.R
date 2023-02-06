#' Generate code from params named list
#'
#' @keywords internal

list2code <- function(lst, delim = "") {
    paste0(sapply(
        names(lst), function(x) {
        paste0(delim, x, " = ", param2code(lst[[x]]))
    }, USE.NAMES = F), collapse = ", ")
}

param2code <- function(x) UseMethod("param2code")

param2code.default <- function(x) {
    if (length(x) == 1) {
        paste0(x)
    } else if (length(x > 1)) {
        paste0("c(", paste(x, collapse = ", "), ")")
    } else {
        paste0(class(x), "()")
    }
}

param2code.logical <- function(x) {
    if (length(x)) {
        if (length(x) == 1) {
            if (is.na(x)) return("NA")
            else if (x) return("T") else return("F")
        } else {
            return(paste0("c(", paste(sapply(x, ifelse, "T", "F"), collapse = ", "), ")"))
        }
    } else return("logical()")
}

param2code.character <- function(x) {
    if (length(x) == 1) {
      if (is.na(x)) {paste("NA")} else {paste0("'", x, "'")}
    } else if (length(x > 1)) {
        paste0(
            "c(",
            paste0(sapply(x, function(y) {
                paste0("'"[!is.na(y)], y, "'"[!is.na(y)])
            }, USE.NAMES = F), collapse = ", "),
            ")"
        )
    } else {
        paste0("character()")
    }
}

param2code.NULL <- function(x) {
    "NULL"
}

param2code.unit <- function(x) {
    paste0(
        "unit(", as.numeric(x), ", '",
        c("cm", "in", "ln", "", "", "", "mm", "pt")[attributes(x)$unit],
        "')"
    )
}

param2code.rel <- function(x) {
    paste0("rel(", unclass(x), ")")
}

param2code.arrow <- function(x) {
    x <- unclass(x)
    x$ends <- c("first", "last", "both")[x$ends]
    x$type <- c("open", "closed")[x$type]
    paste0( "arrow(", list2code(x), ")" )
}

param2code.margin <- function(x) {
    xn <- as.numeric(x)
    paste0(
        "margin(t = ", xn[1], ", r = ", xn[2],
        ", b = ", xn[3], ", l = ", xn[4],
        ", unit = '",
        c("cm", "in", "ln", "", "", "", "mm", "pt")[attributes(x)$unit],
        "')"
    )
}

param2code.element <- function(x) {
    el_type <- class(x)[1]
    x <- shiny:::dropNulls(unclass(x))
    x$inherit.blank <- NULL
    if ("arrow" %in% names(x) && is.logical(x$arrow)) {
        x$arrow <- NULL # NULL and FALSE are same for arrow
    }
    paste0( el_type, "(", list2code(x), ")" )
}
