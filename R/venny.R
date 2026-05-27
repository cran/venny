#' Venn diagram
#'
#' @param data A list with 2 to 4 vectors
#' @param detail Logical (default: TRUE).
#'      If TRUE, output a list contains the venn diagram and summary table.
#'      Otherwise, output only venn diagram.
#' @param ellipse.line A list. See `ellipse_line()`.
#' @param ellipse.fill A list. See `ellipse_fill()`.
#' @param ellipse.density An integer (default: 200L). Higher value yield smoother ellipse.
#' @param set.label Logical (default: TRUE). If TRUE, show the set labels.
#' @param set.label.position A list. See `set_label_position()`.
#' @param set.label.font A list. See `set_label_font()`.
#' @param subset.label Logical (default: TRUE). If TRUE, show the subset labels.
#'      If a named list is provided, then the selected subset name will be renamed.
#'      For example, `list(AB = "new_AB")` will change the original subset AB name to "new_AB".
#' @param subset.label.position A list. See `subset_label_position()`.
#' @param subset.label.font A list. See `subset_label_font()`.
#' @param subset.count Logical (default: TRUE). If TRUE, show the element counts for each subset.
#' @param subset.count.position A list. See `subset_count_position()`.
#' @param subset.count.font A list. See `subset_count_font()`.
#' @param subset.percentage Logical (default: TRUE). If TRUE, show the percentages of the counts.
#' @param subset.percentage.position A list. See `subset_percentage_position()`.
#' @param subset.percentage.font A list. See `subset_percentage_font()`.
#' @param subset.percentage.rounding An integer (default: 1L). How many decimal points.
#'
#' @returns A venn diagram which is a `ggplot` object.
#' @export
#'
#' @examples
#' lst <- LGL23$DEGs
#' setLabelPosition <- set_label_position(hjust = c(0.2, -0.5, 0.5, -0.2))
#' venny(lst, set.label.position = setLabelPosition)
venny <- function(
        data,
        detail = FALSE,
        ellipse.line = ellipse_line(),  # from ./params.R
        ellipse.fill = ellipse_fill(),  # from ./params.R
        ellipse.density = 200L,
        set.label = TRUE,
        set.label.position = set_label_position(),  # from ./params.R
        set.label.font = set_label_font(),  # from ./params.R
        subset.label = TRUE,
        subset.label.position = subset_label_position(),  # from ./params.R
        subset.label.font = subset_label_font(),  # from ./params.R
        subset.count = TRUE,
        subset.count.position = subset_count_position(),  # from ./params.R
        subset.count.font = subset_count_font(),  # from ./params.R
        subset.percentage = TRUE,
        subset.percentage.position = subset_count_position(vjust = -0.3),  # from ./params.R
        subset.percentage.font = subset_percentage_font(),  # from ./params.R
        subset.percentage.rounding = 1L
) {
    if (is.null(data) || length(data) < 2 || length(data) > 4 || !is.list(data))
        stop("`data` should be a list with 2 to 4 vectors.")
    
    n_sets <- length(data)  # should be 2-4
    n_subsets <- how_many_subsets(seq_along(data))  # should be 15
    
    p0 <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::coord_fixed()
    class(p0) <- c(class(p0), "venny")
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Draw ellipses ====
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ellipse.line <- lapply(ellipse.line, function(`_`) fixed_length(`_`, n_sets))
    ellipse.fill <- lapply(ellipse.fill, function(`_`) fixed_length(`_`, n_sets))
    ellipse.density <- fixed_length(ellipse.density, n_sets)
    
    pos <- ellipse_position()[[n_sets - 1]]
    
    ellipse_path <- lapply(
        seq_along(pos),
        function(i)
        {
            generate_ellipse_path(
                x0 = pos[[i]]["x0"],
                y0 = pos[[i]]["y0"],
                a = pos[[i]]["a"],
                b = pos[[i]]["b"],
                angle = pos[[i]]["angle"],
                density = ellipse.density[i]
            )
        }
    )
    
    x <- NULL  # prevent warning message: no visible binding variables x y
    y <- NULL  # prevent warning message: no visible binding variables x y
    for (i in seq_along(ellipse_path))
    {
        p0 <- p0 +
            ggplot2::geom_polygon(
                data = ellipse_path[[i]],
                mapping = ggplot2::aes(x, y), # no visible binding variables x y
                fill = ellipse.fill[["color"]][[i]],
                alpha = ellipse.fill[["alpha"]][[i]],
                color = ellipse.line[["color"]][[i]],
                linetype = ellipse.line[["linetype"]][[i]],
                linewidth = ellipse.line[["linewidth"]][[i]]
            )
    }
    
    lst0 <- venn_summary(data)
    df0 <- lst0[["table"]]
    
    `_set_label` <- names(data)
    if (is.null(names(data))) `_set_label` <- set_label_default(n_sets)
    
    `_subset_label` <- subset_label_default(n_sets)  # from ./params.R
    
    df0 <- df0[match(names(`_subset_label`), rownames(df0)), ]
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Set label ====
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(set.label) || is.recursive(set.label))
    {
        if (is.null(set.label.font[["angle"]])) {
            set.label.font[["angle"]] <- switch(
                n_sets,
                NA,
                0,
                c(0, 45, -45),
                c(-48, 0, 0, 48)
            )
        }
        
        set.label.font <- lapply(
            set.label.font,
            function(`_`) fixed_length(`_`, n_sets)
        )
        
        pos <- set.label.position[[n_sets - 1]]
        
        for (i in seq_along(pos))
        {
            nm <- names(pos[i])
            xy <- pos[[i]]
            if (is.null(xy) | any(is.na(xy)) | length(xy) != 2) next
            
            name <- `_set_label`[i]
            
            p0 <- p0 +
                ggplot2::annotate(
                    geom = "text",
                    x = xy[1],
                    y = xy[2],
                    label = name,
                    family = set.label.font[["family"]][i],
                    fontface = set.label.font[["face"]][i],
                    size = set.label.font[["size"]][i],
                    color = set.label.font[["color"]][i],
                    angle = set.label.font[["angle"]][i]
                )
        }
    }
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Subset label ====
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(subset.label) || is.recursive(subset.label))
    {
        subset.label.font <- lapply(
            subset.label.font,
            function(`_`) fixed_length(`_`, n_subsets)
        )
        
        if (is.recursive(subset.label))
        {
            subset.label <- subset.label[names(subset.label) %in% `_subset_label`]
            `_subset_label`[names(subset.label)] <- subset.label
            df0[, "subset"] <- unlist(`_subset_label`)
        }
        
        pos <- subset.label.position[[n_sets - 1]]
        
        for (i in seq_along(pos))
        {
            nm <- names(pos[i])
            xy <- pos[[i]]
            if (is.null(xy) | any(is.na(xy)) | length(xy) != 2) next
            
            name <- df0[rownames(df0) == nm, ][["subset"]]
            
            p0 <- p0 +
                ggplot2::annotate(
                    geom = "text",
                    x = xy[1],
                    y = xy[2],
                    label = name,
                    family = subset.label.font[["family"]][i],
                    fontface = subset.label.font[["face"]][i],
                    size = subset.label.font[["size"]][i],
                    color = subset.label.font[["color"]][i],
                    angle = subset.label.font[["angle"]][i]
                )
        }
    }
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Subset count ====
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(subset.count))
    {
        subset.count.font <- lapply(
            subset.count.font,
            function(`_`) fixed_length(`_`, n_subsets)
        )
        
        pos <- subset.count.position[[n_sets - 1]]
        
        for (i in seq_along(pos))
        {
            nm <- names(pos[i])
            xy <- pos[[i]]
            if (is.null(xy) | any(is.na(xy)) | length(xy) != 2) next
            
            count <- df0[rownames(df0) == nm, ][["n_elements"]]
            
            p0 <- p0 +
                ggplot2::annotate(
                    geom = "text",
                    x = xy[1],
                    y = xy[2],
                    label = count,
                    family = subset.count.font[["family"]][i],
                    fontface = subset.count.font[["face"]][i],
                    size = subset.count.font[["size"]][i],
                    color = subset.count.font[["color"]][i],
                    angle = subset.count.font[["angle"]][i]
                )
        }
    }
    
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Subset percentage ====
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(subset.percentage))
    {
        subset.percentage.font <- lapply(
            subset.percentage.font,
            function(`_`) fixed_length(`_`, n_subsets)
        )
        
        pos <- subset.percentage.position[[n_sets - 1]]
        
        for (i in seq_along(pos))
        {
            nm <- names(pos[i])
            xy <- pos[[i]]
            if (is.null(xy) | any(is.na(xy)) | length(xy) != 2) next
            
            percentage <- df0[rownames(df0) == nm, ][["percentage"]]
            percentage <- round(percentage, subset.percentage.rounding)
            perc_format <- paste0("(%.", subset.percentage.rounding, "f%%)")
            percentage <- sprintf(perc_format, percentage)
            # percentage <- sprintf("(%.2f%%)", round(percentage, subset.percentage.rounding))
            
            p0 <- p0 +
                ggplot2::annotate(
                    geom = "text",
                    x = xy[1],
                    y = xy[2],
                    label = percentage,
                    family = subset.percentage.font[["family"]][i],
                    fontface = subset.percentage.font[["face"]][i],
                    size = subset.percentage.font[["size"]][i],
                    color = subset.percentage.font[["color"]][i],
                    angle = subset.percentage.font[["angle"]][i]
                )
        }
    }
    
    if (isTRUE(detail))
    {
        names(ellipse_path) <- `_set_label`
        ret <- list(
            venn = p0,
            ellipse_path = ellipse_path,
            table = df0,
            subset_elements = lst0[["subset_elements"]]
        )
        return(ret)
    } else {
        return(p0)
    }
}
