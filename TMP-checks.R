library(tidyida)
data(adlbh_baseline)

describe_long_dataset()

a <- adlbh_baseline |> describe_long_dataset()


a |> dplyr::glimpse()

a

adlbh_baseline |> dplyr::filter(PARAMCD == "ANISO") |> describe_long_dataset()


## by statistic
adlbh_baseline |> describe_long_dataset() |> dplyr::filter(STATISTIC  == "n_distinct")


## by parameter
adlbh_baseline |> describe_long_dataset() |> dplyr::filter(PARAMCD  == "ANISO")

## by condition
adlbh_baseline |>
  describe_long_dataset() |>
  dplyr::filter(STATISTIC  == "kurtosis" & RESULT > 2)


data(adsl)
ard <- describe_wide_dataset(adsl)

ard

a <- ard %>% dplyr::glimpse()

a |> dplyr::filter(STATISTIC == "n_distinct" & RESULT <= 3)



ard %>%
  dplyr::filter(type %in% c("double", "integer", "numeric")) |>
  tidyr::pivot_longer(cols = c(-type, -name, -value), names_to = "STATISTIC", values_to = "RESULT") |>
  dplyr::rename(PARAMCD = name ,
                VARTYPE = type) |>
  dplyr::select(PARAMCD, STATISTIC, RESULT, VARTYPE)


a <- adsl %>%
  dplyr::summarise(dplyr::across(dplyr::everything(), list)) %>%  ## put all vars in to a list
  tidyr::pivot_longer(dplyr::everything()) %>%  ## long format
  dplyr::rowwise()  %>% ## see here https://dplyr.tidyverse.org/articles/rowwise.html
  dplyr::mutate(
    type = paste0(class(value), collapse = " "),  ## identify type
    n_missing = sum(is.na(value) | is.null(value)) / length(value) ## calculate # missing
  ) |>
  dplyr::mutate(
    Mean = ifelse(type %in% c("double", "integer", "numeric"), mean(value, na.rm = TRUE), NA),
    Median = ifelse(type %in% c("double", "integer", "numeric"), median(value, na.rm = TRUE), NA),
    SD = ifelse(type %in% c("double", "integer", "numeric"), sd(value, na.rm = TRUE), NA)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(type, name, dplyr::everything())

a
