describe_wide_dataset <- function(x){

  sum_table <- x %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), list)) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      type = paste0(class(value), collapse = " "),
      n_missing = sum(is.na(value) | is.null(value)) / length(value)
    ) %>%
    dplyr::mutate(
      Mean = ifelse(type %in% c("double", "integer", "numeric"), mean(value, na.rm = TRUE), NA),
      Median = ifelse(type %in% c("double", "integer", "numeric"), median(value, na.rm = TRUE), NA),
      SD = ifelse(type %in% c("double", "integer", "numeric"), sd(value, na.rm = TRUE), NA)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(type, name, dplyr::everything())
  sum_table


}
