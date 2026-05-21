# These are internal functions. Just a note for myself.
# The users can't access to them.

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Color palettes for color blindness ====
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
colorblind_Wong <- c(
    # Wong, B. Points of view: Color blindness. Nat Methods 8, 441 (2011).
    # https://doi.org/10.1038/nmeth.1618
    "Black"           = "#000000", # rgb_to_hexadecimal(  0,   0,   0)
    "Orange"          = "#E69F00", # rgb_to_hexadecimal(230, 159,   0)
    "Sky-blue"        = "#56B4E9", # rgb_to_hexadecimal( 86, 180, 233)
    "Bluish-green"    = "#009E73", # rgb_to_hexadecimal(  0, 158, 115)
    "Yellow"          = "#F0E442", # rgb_to_hexadecimal(240, 228,  66)
    "Blue"            = "#0072B2", # rgb_to_hexadecimal(  0, 114, 178)
    "Vermillion"      = "#D55E00", # rgb_to_hexadecimal(213,  94,   0)
    "Reddish-purple" = "#CC79A7"  # rgb_to_hexadecimal(204, 121, 167)
)

colorblind_Kelly_22 <- c(
    white = "#fdfdfd",
    black = "#1d1d1d",
    yellow = "#ebce2b",
    purple = "#702c8c",
    orange = "#db6917",
    `light blue` = "#96cde6",
    red = "#ba1c30",
    buff = "#c0bd7f",
    grey = "#7f7e80",
    green = "#5fa641",
    `purplish pink` = "#d485b2",
    blue = "#4277b6",
    `yellowish pink` = "#df8461",
    violet = "#463397",
    `orange yellow` = "#e1a11a",
    `purplish red` = "#91218c",
    `greenish yellow` = "#e8e948",
    `reddish brown` = "#7e1510",
    `yellow green` = "#92ae31",
    `yellowish brown` = "#6f340d",
    `reddish orange` = "#d32b1e",
    `olive green` = "#2b3514"
)

colorblind_Tol_muted <- c(
    ## Paul Tol's Muted
    "#DDDDDD",
    "#332288",
    "#117733",
    "#44AA99",
    "#88CCEE",
    "#DDCC77",
    "#CC6677",
    "#AA4499",
    "#882255"
) # Paul Tol

colorblind_Okabe <- c(
    ## Okabe and Ito
    "#000000",
    "#009E73",
    "#0072B2",
    "#56B4E9",
    "#F0E442",
    "#E69F00",
    "#D55E00",
    "#CC79A7"
)

colorblind_IBM <- c(
    ## IBM Design Library
    "#648FFF",
    "#785EF0",
    "#DC267F",
    "#FE6100",
    "#FFB000"
)






rgb_to_hexadecimal <- function(R, G, B, alpha = NULL)
{
    R <- R / 255
    G <- G / 255
    B <- B / 255
    ret <- grDevices::rgb(R, G, B, alpha)
    return(ret)
}
