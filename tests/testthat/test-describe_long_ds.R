test_that("verification: describe returns expected numbers", {

  ##################################
  ##  Test code
  ##################################

  #1. check if there are missing
  #2. variable types
  #3. check valid measurements
  #4. how to handle missingness

  data(adlbh_baseline)
  output <- describe_long_dataset(adlbh_baseline)

  # dimensions
  expect_n_rows(output, 240)
  expect_n_columns(output, 4)

  })


#a

#.n_extreme_values(adlbh_baseline) |>
#  dplyr::filter(statistics == "bottom_n")


#df_hi_lo |> filter(top_n == 5)
#df_hi_lo |> filter(bottom_n == 5)

#min_rank(na.omit(x))

#ADLB |>
#  group_by(PARAM, PARAMCD) |>
#  dense_rank() |>
#  mutate(rank = dense_rank(AVAL))




