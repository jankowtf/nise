test_that("Handle NSE name: chr", {
    result <- rlang::quo("mpg") %>% handle_nse_name()
    expected <- rlang::sym("mpg")
    expect_identical(result, expected)
})

test_that("Handle NSE name: sym", {
    result <- rlang::sym("mpg") %>% handle_nse_name()
    expected <- rlang::sym("mpg")
    expect_identical(result, expected)
})

test_that("Handle NSE name: quo", {
    result <- rlang::quo(mpg) %>% handle_nse_name()
    expected <- rlang::sym("mpg")
    expect_identical(result, expected)
})

test_that("Handle NSE name: var", {
    col <- rlang::quo(mpg)
    result <- col %>% handle_nse_name()
    expected <- rlang::sym("mpg")
    expect_identical(result, expected)

    col <- "mpg"
    result <- col %>% handle_nse_name()
    expected <- rlang::sym("mpg")
    expect_identical(result, expected)
})

test_that("Handle NSE name: empty value", {
    col <- character()
    result <- col %>% handle_nse_name()
    expected <- character()
    expect_identical(result, expected)
})

test_that("Handle NSE name: within function", {
    foo <- function(col) {
        handle_nse_name(col, env = env)
    }

    result <- foo(mpg)
    expected <- rlang::sym("mpg")
    expect_identical(result, expected)

    result <- foo(rlang::quo(mpg))
    expected <- rlang::sym("mpg")
    expect_identical(result, expected)

    result <- foo("mpg")
    expected <- rlang::sym("mpg")
    expect_identical(result, expected)

    col <- rlang::quo(mpg)
    result <- foo(col)
    expected <- rlang::sym("mpg")
    expect_identical(result, expected)

    col <- "mpg"
    result <- foo(col)
    expected <- rlang::sym("mpg")
    expect_identical(result, expected)
})

# Handle multiple inputs --------------------------------------------------

test_that("Handle NSE names", {
    col_c <- rlang::sym("c")
    col_d <- "d"
    col_d_enquo <- rlang::enquo(col_d)
    result <- handle_nse_names(b, rlang::quo(a), "color", col_d_enquo, col_c)
    expected <- structure(list(as.name("b"), as.name("a"), as.name("color"),
        as.name("d"), as.name("c")), .Names = c("", "", "", "",
        ""))
    expect_identical(result, expected)
})

test_that("Handle NSE names: empty values", {
    result <- handle_nse_name(character(), character())
    expected <- character()
    expect_identical(result, expected)

    col_c <- rlang::sym("c")
    col_d <- "d"
    col_d_enquo <- rlang::enquo(col_d)
    result <- handle_nse_names(b, rlang::quo(a), "color", col_d_enquo, col_c, character())
    expected <- structure(list(as.name("b"), as.name("a"), as.name("color"),
        as.name("d"), as.name("c"), character()), .Names = c("", "", "", "",
            "", ""))
    expect_identical(result, expected)
})

test_that("Handle NSE names: within function", {
    tbl <- tibble::tribble(
        ~color,     ~a, ~b, ~c, ~d,
        "blue",      1,  2, TRUE, FALSE,
        "green",     6,  2, TRUE, FALSE,
        "purple",    3,  3, TRUE, FALSE,
        "red",       2,  3, TRUE, FALSE,
        "yellow",    5,  1, TRUE, FALSE
    )

    foo <- function(data, ...) {
        cols <- handle_nse_names(...)
        data %>% dplyr::select(!!!cols)
    }
    col_c <- rlang::sym("c")
    col_d <- "d"
    col_d_enquo <- rlang::enquo(col_d)
    result <- tbl %>% foo(b, rlang::quo(a), "color", col_d_enquo, col_c)
    expected <- structure(list(b = c(2, 2, 3, 3, 1), a = c(1, 6, 3, 2, 5), color = c("blue",
        "green", "purple", "red", "yellow"), d = c(FALSE, FALSE, FALSE,
            FALSE, FALSE), c = c(TRUE, TRUE, TRUE, TRUE, TRUE)), row.names = c(NA,
                -5L), class = c("tbl_df", "tbl", "data.frame"))
    expect_identical(result, expected)
})

test_that("Handle NSE names: within function: empty value", {
    tbl <- tibble::tribble(
        ~color,     ~a, ~b, ~c, ~d,
        "blue",      1,  2, TRUE, FALSE,
        "green",     6,  2, TRUE, FALSE,
        "purple",    3,  3, TRUE, FALSE,
        "red",       2,  3, TRUE, FALSE,
        "yellow",    5,  1, TRUE, FALSE
    )

    foo <- function(data, ...) {
        cols <- handle_nse_names(...)
        data %>% dplyr::select(!!!cols)
    }
    col_c <- rlang::sym("c")
    col_d <- "d"
    col_d_enquo <- rlang::enquo(col_d)
    result <- tbl %>% foo(b, rlang::quo(a), "color", col_d_enquo, col_c, character())
    expected <- structure(list(b = c(2, 2, 3, 3, 1), a = c(1, 6, 3, 2, 5), color = c("blue",
        "green", "purple", "red", "yellow"), d = c(FALSE, FALSE, FALSE,
            FALSE, FALSE), c = c(TRUE, TRUE, TRUE, TRUE, TRUE)), row.names = c(NA,
                -5L), class = c("tbl_df", "tbl", "data.frame"))
    expect_identical(result, expected)
})
