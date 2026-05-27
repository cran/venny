#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Ellipses ====
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#' Ellipse positions
#'
#' This is an internal function used to generate the ellipse parameters that
#' was used by the `generate_ellipse_path()` function.
#' It returns a list encompassing 3 lists.
#' Each list consists of the x0, y0, long-arm, short-arm, and angle default values
#' for generating 2, 3, or 4 ellipses.
#'
#' @returns A two-layer nested list.
#' @export
#'
#' @examples
#' ellipse_position()
ellipse_position <- function() {
    list(
        list(
            e1 = c(x0 = -0.45, y0 = 0, a = 1, b = 1, angle = 0),
            e2 = c(x0 =  0.45, y0 = 0, a = 1, b = 1, angle = 0)
        ),
        list(
            e1 = c(x0 =  0.00, y0 =  0.45, a = 1, b = 1, angle = 0),
            e2 = c(x0 = -0.45, y0 = -0.45, a = 1, b = 1, angle = 0),
            e3 = c(x0 =  0.45, y0 = -0.45, a = 1, b = 1, angle = 0)
        ),
        list(
            # e1 = c(x0 = -1.1, y0 = -0.569, a = 2.5, b = 1.35, angle = -45),
            e1 = c(x0 = -1.09, y0 = -0.569, a = 2.5, b = 1.35, angle = -45),
            e2 = c(x0 = 0, y0 = 0, a = 2.5, b = 1.2, angle = -50),
            e3 = c(x0 = 0, y0 = 0, a = 2.5, b = 1.2, angle = 50),
            # e4 = c(x0 = 1.1, y0 = -0.569, a = 2.5, b = 1.35, angle = 45)
            e4 = c(x0 = 1.09, y0 = -0.569, a = 2.5, b = 1.35, angle = 45)
        )
    )
}


#' Control the ellipse textures
#'
#' This is used to generate the ellipse line color and transparency parameters,
#' for passing into the `venny()`s `ellipse.line` argument.
#'
#' @param linetype A character.
#'      The options are: blank, solid, dashed, dotted, dotdash, longdash, twodash.
#' @param linewidth A number (default: 0.5).
#' @param color A character vector (default: `c("#009E73", "#E69F00", "#CC79A7", "#56B4E9")`).
#' @param alpha A number range from 0 to 1 (default: 0.5). Set the line transparency.
#'
#' @returns A list contains "color" and "alpha" values.
#' @export
#' 
#' @examples
#' ellipse_line()
#' @seealso [ellipse_fill()]
ellipse_line <- function(
        linetype = "blank",
        linewidth = 0.5,
        color = c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"),
        alpha = 0.5
) {
    list(
        linetype = linetype,
        linewidth = linewidth,
        color = color,
        alpha = alpha
    )
}

#' @rdname ellipse_line
#' @export
ellipse_fill <- function(
        color = c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"),
        alpha = 0.2
){
    list(
        color = color,
        alpha = alpha
    )
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Set label ====
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#' Set labels
#'
#' This is an internal function for `venny()` to automatically generate
#' set labels, when the input list is unnamed.
#'
#' @param n_sets An integer.
#'
#' @returns A list.
#' @export
#'
#' @examples
#' set_label_default(3)
set_label_default <- function(n_sets) {
    switch(
        n_sets,
        NA,
        list(A = "Set_A", B = "Set_B"),
        list(A = "Set_A", B = "Set_B", C = "Set_C"),
        list(A = "Set_A", B = "Set_B", C = "Set_C", D = "Set_D")
    )
}


#' Set label position
#'
#' Generate the coordinates for the set labels. This is passed into the `venny()`'s
#' `set.label.position` arguments.
#'
#' @param hjust A numeric vector (default: 0).
#'      Horizontal adjustment, adjust the x-axis coordinates of the set labels.
#' @param vjust A numeric vector (default: 0).
#'      Vertical adjustment, adjust the y-axis coordinates of the set labels.
#' @param show A character vector (default: NULL).
#'      Show only the specified set labels. By default, show all labels.
#' @param hide A character vector (default: NULL).
#'      Do not show the specified set labels. By default, show all labels.
#'
#' @returns A list contains 3 named list.
#'      Each named list contains the x, y coordinates of the set labels for
#'      different venn diagram (2, 3, or 4 ellipses), respectively.
#'
#' @export
#'
#' @examples
#' set_label_position()
set_label_position <- function(
        hjust = 0,
        vjust = 0,
        show = NULL,
        hide = NULL
) {
    pos <- list(
        list(
            "A"  = c(-0.5, 1.15),
            "B"  = c( 0.5, 1.15)
        ),
        list(
            "A"   = c( 0.0, 1.65),
            "B"   = c(-1.4, 0.30),
            "C"   = c( 1.4, 0.30)
        ),
        list(
            # "A" = c(-2.45, 1.75),
            "A" = c(-2.45, -1.5),
            "B" = c(-1.3,  2.4),
            "C" = c( 1.3,  2.4),
            # "D" = c(2.45, 1.75)
            "D" = c( 2.45, -1.5)
        )
    )

    tmp_pos <- lapply(
        pos,
        function(lst)
        {
            n_subsets <- length(lst)

            if (length(hjust) > 1)
                hjust <- fixed_length(x = hjust, len = n_subsets, fill_with = 0)
            else
                hjust <- fixed_length(x = hjust, len = n_subsets)

            if (length(vjust) > 1)
                vjust <- fixed_length(x = vjust, len = n_subsets, fill_with = 0)
            else
                vjust <- fixed_length(x = vjust, len = n_subsets)

            for (i in seq_along(lst))
            {
                set_label <- names(lst[i])
                if ( ! is.null(show) & ! set_label %in% show )
                    lst[set_label] <- list(NULL)
                if ( ! is.null(hide) & set_label %in% hide )
                    lst[set_label] <- list(NULL)
                if (is.null(show) & is.null(hide))
                    lst[[set_label]] <- lst[[i]] + c(hjust[i], vjust[i])
            }
            return(lst)
        }
    )

    pos <- structure(
        .Data = tmp_pos,
        hjust = hjust,
        vjust = vjust,
        show = show,
        hide = hide
    )

    return(pos)
}


#' Set label font
#'
#' Generate a list of the available set label font parameters.
#'
#' @param family Character (default: "sans").
#' @param face Character (default: "bold").
#' @param size Numeric (default: 5).
#' @param color Character (default: c("#009E73", "#E69F00", "#CC79A7", "#56B4E9")).
#' @param angle Numeric | NULL (default: NULL).
#'
#' @returns A list.
#' @export
#'
#' @examples
#' set_label_font()
set_label_font <- function(
        family = "sans",
        face = "bold",
        size = 5,
        color = c("#CC79A7", "#009E73", "#E69F00", "#56B4E9"),
        angle = NULL
) {
    list(family = family,
         face = face,
         size = size,
         color = color,
         angle = angle)
}


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Subset label ====
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#' @rdname set_label_default
#' @export
subset_label_default <- function(n_sets) {
    switch(
        n_sets,
        NA,
        list(
            A = "A", AB = "AB", B = "B"
        ),
        list(
            A = "A", B = "B", C = "C",
            AB = "AB", AC = "AC", BC = "BC",
            ABC = "ABC"
        ),
        list(
            A = "A", B = "B", C = "C", D = "D",
            AB = "AB", BC = "BC", CD = "CD",
            AC = "AC", AD = "AD", BD = "BD",
            ABC = "ABC", BCD = "BCD", ACD = "ACD", ABD = "ABD",
            ABCD = "ABCD"
        )
    )
}


#' @rdname set_label_position
#' @export
subset_label_position <- function(
        hjust = 0,
        vjust = 0,
        show = NULL,
        hide = NULL
) {
    pos <- list(
        list(
            "A"  = c(-1, 0.3),
            "B"  = c( 1, 0.3),
            "AB" = c( 0, 0.3)
        ),
        list(
            "A"   = c( 0.0,  1.30),
            "B"   = c(-1.2, -0.20),
            "C"   = c( 1.2, -0.20),
            "AB"  = c(-0.6,  0.40),
            "AC"  = c( 0.6,  0.40),
            "BC"  = c( 0.0, -0.65),
            "ABC" = c( 0.0,  0.20)
        ),
        list(
            "A"    = c(-2.4,  1.15),
            "B"    = c(-1.3,  1.90),
            "C"    = c( 1.3,  1.90),
            "D"    = c( 2.4,  1.15),
            "AB"   = c(-1.6,  1.15),
            "BC"   = c( 0.0,  1.40),
            "CD"   = c( 1.6,  1.15),
            "AC"   = c(-1.4, -0.50),
            "AD"   = c( 0.0, -1.80),
            "BD"   = c( 1.4, -0.50),
            "ABC"  = c(-0.7,  0.50),
            "BCD"  = c( 0.7,  0.50),
            "ACD"  = c(-0.7, -1.10),
            "ABD"  = c( 0.7, -1.10),
            "ABCD" = c( 0.0, -0.30)
        )
    )

    tmp_pos <- lapply(
        pos,
        function(lst)
        {
            n_subsets <- length(lst)

            if (length(hjust) > 1)
                hjust <- fixed_length(x = hjust, len = n_subsets, fill_with = 0)
            else
                hjust <- fixed_length(x = hjust, len = n_subsets)

            if (length(vjust) > 1)
                vjust <- fixed_length(x = vjust, len = n_subsets, fill_with = 0)
            else
                vjust <- fixed_length(x = vjust, len = n_subsets)

            for (i in seq_along(lst))
            {
                subset_label <- names(lst[i])
                if ( ! is.null(show) & ! subset_label %in% show )
                    lst[subset_label] <- list(NULL)
                if ( ! is.null(hide) & subset_label %in% hide )
                    lst[subset_label] <- list(NULL)
                if (is.null(show) & is.null(hide))
                    lst[[subset_label]] <- lst[[i]] + c(hjust[i], vjust[i])
            }
            return(lst)
        }
    )

    pos <- structure(
        .Data = tmp_pos,
        hjust = hjust,
        vjust = vjust,
        show = show,
        hide = hide
    )

    return(pos)
}


#' @rdname set_label_font
#' @export
subset_label_font <- function(
        family = "sans",
        face = "bold.italic",
        size = 4,
        color = "black",
        angle = 0
) {
    list(family = family,
         face = face,
         size = size,
         color = color,
         angle = angle)
}


#' @rdname set_label_position
#' @export
subset_count_position <- function(
        hjust = 0,
        vjust = 0,
        show = NULL,
        hide = NULL
) {
    pos <- list(
        list(
            "A"  = c(-1, -0.05),
            "B"  = c( 1, -0.05),
            "AB" = c( 0, -0.05)
        ),
        list(
            "A"   = c( 0.00,  1.00),
            "B"   = c(-0.90, -0.50),
            "C"   = c( 0.90, -0.50),
            "AB"  = c(-0.65,  0.15),
            "AC"  = c( 0.65,  0.15),
            "BC"  = c( 0.00, -0.90),
            "ABC" = c( 0.00, -0.10)
        ),
        list(
            "A"    = c(-2.4,  0.5),
            "B"    = c(-0.9,  1.7),
            "C"    = c( 0.9,  1.7),
            "D"    = c( 2.4,  0.5),
            "AB"   = c(-1.5,  0.8),
            "BC"   = c( 0.0,  1.1),
            "CD"   = c( 1.5,  0.8),
            "AC"   = c(-1.4, -0.8),
            "AD"   = c( 0.0, -2.1),
            "BD"   = c( 1.4, -0.8),
            "ABC"  = c(-0.7,  0.2),
            "BCD"  = c( 0.7,  0.2),
            "ACD"  = c(-0.7, -1.4),
            "ABD"  = c( 0.7, -1.4),
            "ABCD" = c( 0.0, -0.6)
        )
    )

    tmp_pos <- lapply(
        pos,
        function(lst)
        {
            n_subsets <- length(lst)

            if (length(hjust) > 1)
                hjust <- fixed_length(x = hjust, len = n_subsets, fill_with = 0)
            else
                hjust <- fixed_length(x = hjust, len = n_subsets)

            if (length(vjust) > 1)
                vjust <- fixed_length(x = vjust, len = n_subsets, fill_with = 0)
            else
                vjust <- fixed_length(x = vjust, len = n_subsets)

            for (i in seq_along(lst))
            {
                subset_label <- names(lst[i])
                lst[[subset_label]] <- lst[[i]] + c(hjust[i], vjust[i])  # fixing bug
                if ( ! is.null(show) & ! subset_label %in% show )
                    lst[subset_label] <- list(NULL)
                if ( ! is.null(hide) & subset_label %in% hide )
                    lst[subset_label] <- list(NULL)
                # if (is.null(show) & is.null(hide))
                #     lst[[subset_label]] <- lst[[i]] + c(hjust[i], vjust[i])
            }
            return(lst)
        }
    )

    pos <- structure(
        .Data = tmp_pos,
        hjust = hjust,
        vjust = vjust,
        show = show,
        hide = hide
    )

    return(pos)
}


#' @rdname set_label_font
#' @export
subset_count_font <- function(
        family = "sans",
        face = "plain",
        size = 4,
        color = "grey20",
        angle = 0
) {
    list(family = family,
         face = face,
         size = size,
         color = color,
         angle = angle)
}


#' @rdname set_label_position
#' @export
subset_percentage_position <- function(
        hjust = 0,
        vjust = 0,
        show = NULL,
        hide = NULL
) {
    pos <- list(
        list(
            "A"  = c(-1, -0.35),
            "B"  = c( 1, -0.35),
            "AB" = c( 0, -0.35)
        ),
        list(
            "A"   = c( 0.00,  0.7),
            "B"   = c(-0.90, -0.80),
            "C"   = c( 0.90, -0.80),
            "AB"  = c(-0.65, -0.15),
            "AC"  = c( 0.65, -0.15),
            "BC"  = c( 0.00, -1.20),
            "ABC" = c( 0.00, -0.40)
        ),
        list(
            "A"    = c(-2.4,  0.2),
            "B"    = c(-0.9,  1.4),
            "C"    = c( 0.9,  1.4),
            "D"    = c( 2.4,  0.2),
            "AB"   = c(-1.5,  0.5),
            "BC"   = c( 0.0,  0.8),
            "CD"   = c( 1.5,  0.5),
            "AC"   = c(-1.4, -1.1),
            "AD"   = c( 0.0, -2.4),
            "BD"   = c( 1.4, -1.1),
            "ABC"  = c(-0.7, -0.1),
            "BCD"  = c( 0.7, -0.1),
            "ACD"  = c(-0.7, -1.7),
            "ABD"  = c( 0.7, -1.7),
            "ABCD" = c( 0.0, -0.9)
        )
    )
    
    tmp_pos <- lapply(
        pos,
        function(lst)
        {
            n_subsets <- length(lst)
            
            if (length(hjust) > 1)
                hjust <- fixed_length(x = hjust, len = n_subsets, fill_with = 0)
            else
                hjust <- fixed_length(x = hjust, len = n_subsets)
            
            if (length(vjust) > 1)
                vjust <- fixed_length(x = vjust, len = n_subsets, fill_with = 0)
            else
                vjust <- fixed_length(x = vjust, len = n_subsets)
            
            for (i in seq_along(lst))
            {
                subset_label <- names(lst[i])
                lst[[subset_label]] <- lst[[i]] + c(hjust[i], vjust[i])  # fixing bug
                if ( ! is.null(show) & ! subset_label %in% show )
                    lst[subset_label] <- list(NULL)
                if ( ! is.null(hide) & subset_label %in% hide )
                    lst[subset_label] <- list(NULL)
                # if (is.null(show) & is.null(hide))
                #     lst[[subset_label]] <- lst[[i]] + c(hjust[i], vjust[i])
            }
            return(lst)
        }
    )
    
    pos <- structure(
        .Data = tmp_pos,
        hjust = hjust,
        vjust = vjust,
        show = show,
        hide = hide
    )
    
    return(pos)
}


#' @rdname set_label_font
#' @export
subset_percentage_font <- function(
        family = "sans",
        face = "plain",
        size = 4,
        color = "grey20",
        angle = 0
) {
    list(family = family,
         face = face,
         size = size,
         color = color,
         angle = angle)
}
