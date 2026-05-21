#' Set operations
#'
#' Perform intersection (`intersect()`), union (`union()`), and difference (`setdiff()`)
#' for multiple `venny_ep` objects representing sets. They are handy wrappers of
#' the `polyclip::polyclip()` function.
#'
#' These functions override the base versions to make them generic so that `venny`
#' can provide methods to proceed the `venny_ep` class object. The default methods
#' call the base versions.
#'
#' @name setops
#'
#' @param x A two-column matrix consists of the point coordinates (x, y) used to construct the ellipse.
#'      This is the reference-ellipse used to be clipped by the others.
#' @param y A two-column matrix consists of the point coordinates (x, y) used to construct the ellipse.
#'      This is the object-ellipse used to clip the reference ellipse, i.e. `x`.
#' @param ... The other object-ellipses.
#'
#' @returns A list contains one or multiple two-column dataframe.
#'      Each dataframe is the point coordinates (x, y) used to construct a polygon.
#'
#' @examples
#' data <- list(
#'     Set_A = c(10:100, 500:600),
#'     Set_B = c(5:150, 550:650),
#'     Set_C = c(80:180, 580:680),
#'     Set_D = c(120:220, 520:620)
#' )
#' out <- venny(data, detail = TRUE)
#' p0 <- out$venn
#' ep <- out$ellipse_path
#'
#' #------------- Intersection -------------#
#' res <- intersect(ep$Set_A, ep$Set_B, ep$Set_D)
#' highlight(p0, res)
#'
#' #----------------- Union ----------------#
#' res <- union(ep$Set_A, ep$Set_C, ep$Set_D)
#' highlight(p0, res)
#'
#' #--------------- Difference -------------#
#' res <- setdiff(ep$Set_B, ep$Set_D, ep$Set_A, ep$Set_C)
#' highlight(p0, res)
#'
#' #---------- Multiple operations ---------#
#' res <- union(ep$Set_A, ep$Set_B, ep$Set_C) |>
#'     intersect(ep$Set_D) |>
#'     setdiff(ep$Set_B)
#' highlight(p0, res)
NULL


#' @rdname setops
#' @export
intersect <- function(x, y, ...) UseMethod("intersect")
#' @rdname setops
#' @export
union <- function(x, y, ...) UseMethod("union")
#' @rdname setops
#' @export
setdiff <- function(x, y, ...) UseMethod("setdiff")

#' @export
intersect.default <- function(x, y, ...) base::intersect(x, y)
#' @export
union.default <- function(x, y, ...) base::union(x, y)
#' @export
setdiff.default <- function(x, y, ...) base::setdiff(x, y)


#' @rdname setops
#' @export
intersect.venny_setops <- function(x, y, ...)
{
    # arg_names <- as.list(match.call())[-1]
    ellipses <- list(x, y, ...)
    # ellipses <- lapply(ellipses, \(e) list(list(x = e[, "x"], y = e[, "y"])))
    ellipses <- lapply(ellipses, .tidy_data_for_polyclip)
    ref <- ellipses[[1]]
    for (i in seq_along(ellipses))
    {
        if (i == 1)
            ret <- ref
        else {
            clipper <- ellipses[[i]]
            ret <- polyclip::polyclip(ret, clipper, op = "intersection")
        }
    }
    ret <- lapply(ret, \(`_`) as.matrix(as.data.frame(`_`)))
    class(ret) <- c("list", "venny_setops")
    return(ret)
}


#' @rdname setops
#' @export
union.venny_setops <- function(x, y, ...)
{
    ellipses <- list(x, y, ...)
    ellipses <- lapply(ellipses, .tidy_data_for_polyclip)
    ref <- ellipses[[1]]
    for (i in seq_along(ellipses))
    {
        if (i == 1)
            ret <- ref
        else {
            clipper <- ellipses[[i]]
            ret <- polyclip::polyclip(ret, clipper, op = "union")
        }
    }
    ret <- lapply(ret, \(`_`) as.matrix(as.data.frame(`_`)))
    class(ret) <- c("list", "venny_setops")
    return(ret)
}


#' @rdname setops
#' @export
setdiff.venny_setops <- function(x, y, ...)
{
    ellipses <- list(x, y, ...)
    ellipses <- lapply(ellipses, .tidy_data_for_polyclip)
    ref <- ellipses[[1]]
    for (i in seq_along(ellipses))
    {
        if (i == 1)
            ret <- ref
        else {
            clipper <- ellipses[[i]]
            ret <- polyclip::polyclip(ret, clipper, op = "minus")
        }
    }
    ret <- lapply(ret, \(`_`) as.matrix(as.data.frame(`_`)))
    class(ret) <- c("list", "venny_setops")
    return(ret)
}


.tidy_data_for_polyclip <- function(venny_setops)
{
    if (inherits(venny_setops, "venny_ep") & is.matrix(venny_setops))
        return(list(x = venny_setops[, "x"], y = venny_setops[, "y"]))

    if (inherits(venny_setops, "venny_setops") & is.recursive(venny_setops))
    {
        lst <- vector("list", length(venny_setops))
        for (i in seq_along(venny_setops))
        {
            lst_tmp <- venny_setops[[i]]
            lst[[i]] <- list(x = lst_tmp[, "x"], y = lst_tmp[, "y"])
        }
        ret <- lst[ ! unlist(lapply(lst, is.null)) ]
        return(ret)
    }
}


#' Draw polygons to highlight subsets
#'
#' A handy wrapper for the `ggplot2::geom_polygon()` used to represent the
#' result of set operations.
#'
#' @param venn Venn diagram produced from `venny::venny()`.
#' @param setops The result of set operations.
#' @param color Character (default: "transparent"). The polygon line color.
#' @param linewidth Numeric (default: 1.5). The polygon linewidth.
#' @param linetype Character (default: "solid"). The polygon linetype.
#' @param fill Character (default: "black"). The polygon color.
#' @param alpha Numeric (default: 0.4). The polygon transparency (0-1).
#' @param ... The other arguments passed into `ggplot2::geom_polygon`.
#'
#' @returns A ggplot object.
#' @export
#'
#' @examples
#' data <- list(
#'     Set_A = c(10:100, 500:600),
#'     Set_B = c(5:150, 550:650),
#'     Set_C = c(80:180, 580:680),
#'     Set_D = c(120:220, 520:620)
#' )
#' out <- venny(data, detail = TRUE)
#' p0 <- out$venn
#' ep <- out$ellipse_path
#' res <- intersect(ep$Set_A, ep$Set_B, ep$Set_D)
#' highlight(p0, res)
highlight <- function(
        venn,
        setops,
        color = "transparent",
        linewidth = 1.5,
        linetype = "solid",
        fill = "black",
        alpha = 0.4,
        ...
) {
    x <- NULL
    y <- NULL
    for (i in seq_along(setops)) {
        ggpoly <- ggplot2::geom_polygon(
            data = setops[[i]],
            mapping = ggplot2::aes(x, y),
            color = color,
            linewidth = linewidth,
            linetype = linetype,
            fill = fill,
            alpha = alpha,
            ...
        )
        p0 <- if (i == 1) venn + ggpoly else p0 + ggpoly
    }
    return(p0)
}
