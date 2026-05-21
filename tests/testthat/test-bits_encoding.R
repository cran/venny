test_that("Encode all possible combination with 0 and 1.", {
    x <- bits_encoding(c("A", "B", "C", "D"), rownames = FALSE)
    res <- matrix(
        data = c(
            1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L,
            0L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L,
            0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L,
            0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
        ),
        ncol = 4,
        dimnames = list(NULL, c("A", "B", "C", "D"))
    )
    expect_identical(x, res)
})
