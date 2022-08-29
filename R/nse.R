#' Handle NSE name
#'
#' @param x [[character] or [name] or [quosure]]
#' @param env [[environment]] Caller environment
#'
#' @return [name] or `character()` in case `x` was `character()`
#' @export
#' @examples
#' data_tbl <- tibble::tribble(
#'     ~color,     ~a, ~b, ~c, ~d,
#'     "blue",      1,  2, TRUE, FALSE,
#'     "green",     6,  2, TRUE, FALSE,
#'     "purple",    3,  3, TRUE, FALSE,
#'     "red",       2,  3, TRUE, FALSE,
#'     "yellow",    5,  1, TRUE, FALSE
#' )
#' foo <- function(data, col) {
#'     col <- handle_nse_name(col)
#'     data %>% dplyr::select(!!col)
#' }
#'
#' data_tbl %>% foo(color)
#' data_tbl %>% foo("color")
#' data_tbl %>% foo(rlang::quo(color))
#'
#' tmp <- rlang::sym("color")
#' col <- rlang::enquo(tmp)
#' data_tbl %>% foo(col)
handle_nse_name <- function(
    x,
    env = rlang::caller_env()
    # ref = character()
) {
    # value_quo <- rlang::quo_squash(x)
    # Doesn't work for names/symbols as they are not 'enquo'ed yet

    value_quo <- try(rlang::quo_squash(x), silent = TRUE)
    # Fall back to 'character' in case of names/symbols
    if (inherits(value_quo, "try-error")) {
        value_quo <- value_quo %>%
            stringr::str_extract("'.*'") %>%
            stringr::str_remove_all("'")
    }
    # Works but seems very inefficient -> check if there's something better!
    # Essentially, I would need a way to call 'rlang::enquo()' "after the fact",
    # i.e. in the enclosing environment/frame. Something like
    # 'withr::with_environment(<frame>, rlang::enquo(x))'`- which doesn't work

    # Early exit
    if (!length(value_quo)) {
        return(value_quo)
    }

    if (inherits(value_quo, "character")) {
        value_quo %>% rlang::sym()
    } else if (inherits(value_quo, "call")) {
        res <- try(rlang::eval_tidy(value_quo, env = env), silent = TRUE)
        if (inherits(res, "try-error")) {
            value_quo[[2]] %>% rlang::eval_tidy(env = env)
        } else {
            if (res %>% rlang::is_quosure()) {
                res %>% rlang::quo_squash()
            } else {
                if (!length(res)) {
                    return(res)
                }
                res %>% rlang::sym()
            }
        }
    } else {
        # Previous approach via 'ref'
        # if (missing(ref)) {
        #     return(x)
        # }

        # eval <- (!(value_quo %>% as.character()) %in% ref)
        # if (eval) {
        #     try(rlang::eval_tidy(x, env = env), silent = TRUE) %>%
        #         handle_nse_name(env = env)
        # } else {
        #     x
        # }

        # Current best-shot approach
        # TODO: works, not satisfied yet - still feels too hacky -> check on how
        # to do proper XE (i.e. standard and non-standard execution) the RIGHT
        # way ;-)
        value_tidy <- try(rlang::eval_tidy(x, env = env), silent = TRUE)
        if (inherits(value_tidy, "try-error")) {
            return(value_quo)
        } else {
            if (value_tidy %>% rlang::is_quosure()) {
                value_tidy %>%
                    handle_nse_name(env = env)
            } else {
                if ((value_quo != value_tidy) && !inherits(value_tidy, "character"))  {
                    return(value_tidy)
                } else {
                    value_tidy %>%
                        handle_nse_name(env = env)
                }
            }
        }
    }
}

#' Handle NSE names
#'
#' @param ... [[character] or [name] or [quosure]]
#' @param env [[environment]] Caller environment
#'
#' @return
#' @export
#' @examples
#' bar <- function(data, ...) {
#'     cols <- handle_nse_names(...)
#'     data %>% dplyr::select(!!!cols)
#' }
#' col_c <- rlang::sym("c")
#' col_d <- "d"
#' col_d_enquo <- rlang::enquo(col_d)
#'
#' data_tbl %>% bar(b, rlang::quo(a), "color", col_d_enquo, col_c)
handle_nse_names <- function(
    ...,
    env = rlang::caller_env()
) {
    rlang::enquos(...) %>% purrr::map(handle_nse_name)
}
