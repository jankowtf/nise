deparse_and_clip <- function(x) {
    x %>% deparse() %>% clipr::write_clip()
}
