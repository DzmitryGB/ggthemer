#' Element functions
#'
#' @keywords internal

# function to extract common elements' properties
# used to update relevant UI when more than one element is selected
intersect_elements <- function(elements, theme) {
    els <- theme[elements]
    el_types <- unique(unlist(lapply(els, class))) # element and element_type
    if (any(el_types == "element_blank")) {
        return(list())
    } # if any element is blank, there won't be any common features
    if (length(el_types) > 2) {
        warning("Elements of different types! Ignored")
        return(list())
    }
    params <- names(els[[1]])
    mxID <- sapply(params, function(x) {
        unlist(lapply(els, function(y) { identical( els[[1]][x], y[x] ) }))
    }) # matrix of identity (elements by features)
    common_params <- params[apply(mxID, 2, all)]
    return(els[[1]][common_params])
}

# return different in new list items
setdiff.list <- function(new, old) {
    diffs <- new[sapply(names(new), function(x) {
        !identical(new[[x]], old[[x]])
    })]
    return(diffs)
}

el2list <- function(el) {
    if (!inherits(el, "unit") && !inherits(el, "element_text")) {
        return(unclass(el))
    }
    if (inherits(el, "margin")) {
        eln <- as.numeric(el)
        return(
            list(
                margin = list(
                    t = eln[1], r = eln[2], b = eln[3], l = eln[4],
                    unit = c("cm", "in", "ln", "", "", "", "mm", "pt")[attributes(el)$unit]
                )
            )
        )
    }
    if (inherits(el, "unit")) {
        eln <- as.numeric(el)
        return(
            list(
                x = eln[1],
                unit = c("cm", "in", "ln", "", "", "", "mm", "pt")[attributes(el)$unit]
            )
        )
    }
    # element_text. Deal with the margin
    el_out <- unclass(el)
    el_out$margin <- NULL
    el_out <- c(el_out, el2list(el$margin))
    return(el_out)
}


#' @importFrom tools toTitleCase
# Prepare element data.frame for the select inputs
get_theme_params <- function() {
    els <- ggplot2::get_element_tree()
    theme_ctrls <- data.frame(
        id = names(els),
        type = unlist(lapply(els, `[[`, "class"))
    ) # element names and classes
    axis <- gsub( "[.]x$", "", names(els)[grepl("[.]x$", names(els))] ) # axis-specific els
    theme_ctrls$id <- gsub("[.]x([.]top|[.]bottom)?$|[.]y([.]left|[.]right)?$", "", theme_ctrls$id)
    theme_ctrls <- unique(theme_ctrls[grepl("^element", theme_ctrls$type), ])
    theme_ctrls$name <- sapply(theme_ctrls$id, function(x) {
        gsub("[.]", " ", tools::toTitleCase(x))
    })
    theme_ctrls$axis <- theme_ctrls$id %in% axis
    theme_ctrls <- theme_ctrls[order(nchar(theme_ctrls$id), theme_ctrls$id), ]
    return(theme_ctrls)
}

### Replacement functions for the calc_element
# calc_element: replace call to merge_elements
calc_element2 <- function(element, theme, element_tree = get_element_tree(), verbose = FALSE, skip_blank = FALSE,
                         call = caller_env()) {
  if (verbose) message(element, " --> ", appendLF = FALSE)

  el_out <- theme[[element]]

  # If result is element_blank, we skip it if `skip_blank` is `TRUE`,
  # and otherwise we don't inherit anything from parents
  if (inherits(el_out, "element_blank")) {
    if (isTRUE(skip_blank)) {
      el_out <- NULL
    } else {
      if (verbose) message("element_blank (no inheritance)")
      return(el_out)
    }
  }

  # If the element is defined (and not just inherited), check that
  # it is of the class specified in element_tree
  if (!is.null(el_out) &&
      !inherits(el_out, element_tree[[element]]$class)) {
    # allow legend.position to be numeric
    if (element != "legend.position" && inherits(el_out, "numeric")) {

      cli::cli_abort("Theme element {.var {element}} must have class {.cls {ggplot_global$element_tree[[element]]$class}}", call = call)
  }}

  # Get the names of parents from the inheritance tree
  pnames <- element_tree[[element]]$inherit

  # If no parents, this is a "root" node. Just return this element.
  if (is.null(pnames)) {
    if (verbose) message("nothing (top level)")

    # Check that all the properties of this element are non-NULL
    nullprops <- vapply(el_out, is.null, logical(1))
    if (!any(nullprops)) {
      return(el_out) # no null properties, return element as is
    }

    # if we have null properties, try to fill in from ggplot_global$theme_default
    el_out <- combine_elements(el_out, ggplot_global$theme_default[[element]])
    nullprops <- vapply(el_out, is.null, logical(1))
    if (!any(nullprops)) {
      return(el_out) # no null properties remaining, return element
    }

    cli::cli_abort("Theme element {.var {element}} has {.val NULL} property without default: {.field {names(nullprops)[nullprops]}}", call = call)
  }

  # Calculate the parent objects' inheritance
  if (verbose) message(paste(pnames, collapse = ", "))
  parents <- lapply(
    pnames,
    calc_element,
    theme,
    verbose = verbose,
    # once we've started skipping blanks, we continue doing so until the end of the
    # recursion; we initiate skipping blanks if we encounter an element that
    # doesn't inherit blank.
    skip_blank = skip_blank || (!is.null(el_out) && !isTRUE(el_out$inherit.blank)),
    call = call
  )

  # Combine the properties of this element with all parents
  Reduce(combine_elements2, parents, el_out)
}

# skip calculating relative sizes
combine_elements2 <- function(e1, e2) {
  # If e2 is NULL, nothing to inherit
  if (is.null(e2) || inherits(e1, "element_blank")) {
    return(e1)
  }
  # If e1 is NULL inherit everything from e2
  if (is.null(e1)) {
    return(e2)
  }
  # If neither of e1 or e2 are element_* objects, return e1
  if (!inherits(e1, "element") && !inherits(e2, "element")) {
    return(e1)
  }
  # If e2 is element_blank, and e1 inherits blank inherit everything from e2,
  # otherwise ignore e2
  if (inherits(e2, "element_blank")) {
    if (e1$inherit.blank) {
      return(e2)
    } else {
      return(e1)
    }
  }
  # If e1 has any NULL properties, inherit them from e2
  n <- names(e1)[vapply(e1, is.null, logical(1))]
  e1[n] <- e2[n]
  return(e1)
}
