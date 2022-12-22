
### see expand https://tidyr.tidyverse.org/reference/complete.html


wide_data <-
  adlbh_baseline %>%
  tidyr::pivot_wider(id_cols = USUBJID, names_from = "PARAMCD", values_from = "AVAL", values_fill = NA)

wide_data %>%
  dplyr::group_by(USUBJID) %>%
  dplyr::filter(dplyr::n()>1)


long_data <-
  wide_data %>%
  tidyr::pivot_longer(cols = -USUBJID, names_to = "PARAMCD", values_to = "AVAL")




## determine which is missing
all <- adlbh_baseline %>% tidyr::expand(PARAMCD, USUBJID)
all1 <- adlbh_baseline %>% dplyr::right_join(all) %>% dplyr::ungroup()

all1 %>% dplyr::filter(PARAMCD == "ANISO") %>% dplyr::tally()

# Use with `group_by()` to expand within each group
all2 <- adlbh_baseline %>% dplyr::group_by(PARAMCD) %>% tidyr::expand(USUBJID)

all3 <- adlbh_baseline %>% dplyr::right_join(all2)


all1 %>% dplyr::group_by(PARAM, PARAMCD) %>% dplyr::summarise(n = dplyr::n())
all2 %>% dplyr::group_by(PARAMCD) %>% dplyr::tally()
all3 %>% dplyr::group_by(PARAMCD) %>% dplyr::tally()



## check number of measured variables across the groups
check <- adlbh_baseline %>% dplyr::group_by(PARAMCD) %>% dplyr::tally()


check <- adlbh_baseline %>% dplyr::filter(PARAMCD == "ANISO")


#install.packages("tableone")
library(tableone)
data("adsl")
tableone::CreateTableOne(data = adsl %>% dplyr::select(-USUBJID, -SUBJID))



