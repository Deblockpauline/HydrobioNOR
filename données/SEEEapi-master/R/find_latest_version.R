find_latest_version <- function(x) {
  x_split <- strsplit(x, split = ".", fixed = TRUE) %>%
    map(as.numeric)                                 %>%
    map(as_tibble)                                  %>%
    bind_cols()                                     %>%
    t()                                             %>%
    as.data.frame()                                 %>%
    (function(df) {
      colnames(df) <- c("major", "minor", "dev")
      df
    })

  filter(x_split,
         major == max(major))   %>%
    filter(minor == max(minor)) %>%
    filter(dev   == max(dev))   %>%
    paste(collapse = ".")
}
