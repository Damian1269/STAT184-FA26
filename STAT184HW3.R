install.packages(c(
  "dplyr",
  "tidyr",
  "tibble",
  "readr",
  "stringr",
  "forcats",
  "purrr"
))

library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

sales_clean <- sales_raw |>
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )

sales_clean |>
  select(
    sale_id,
    item_clean,
    amount,
    discount_rate,
    net_amount,
    size
  )

# Output
# sale_id item_clean amount discount_rate net_amount size      
# <int> <chr>       <dbl>         <dbl>      <dbl> <chr>     
# 1       1 pen            10          0          10   small     
# 2       2 pen            20          0.1        18   small     
# 3       3 notebook       NA          0.05       NA   unrecorded
# 4       4 pen            15          0          15   small     
# 5       5 folder         30          0.2        24   large     
# 6       6 folder         25          0.1        22.5 large     
# 7       7 eraser         NA          0          NA   unrecorded
# 8       8 pen            20          0          20   large  

# item_clean removes extra spaces and makes all item names lowercase.
# parse_number() removes symbols such as $ and % and extracts the numbers.
# discount_rate is divided by 100 so that it is stored as a proportion.
# Missing amount values remain missing, so their net_amount is also NA.


sales_codes <- sales_clean |>
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(
      code_clean,
      "^(ST|ON)-\\d{3}$"
    )
  )

invalid_codes <- sales_codes |>
  filter(!valid_code) |> 
  select(sale_id, code_raw, code_clean)
invalid_codes

valid_code_parts <- sales_codes |>
  filter(valid_code) |> 
  select(sale_id, code_clean) |>
  separate_wider_delim(
    code_clean,
    delim = "-",
    names = c("channel", "number")
  )
valid_code_parts

sale_tags <- sales_clean |>
  select(sale_id, tags) |>
  separate_longer_delim(
    tags,
    delim = ";"
  )
sale_tags

# Output
# sale_id code_raw code_clean
# <int> <chr>    <chr>     
# 1       5 bad      BAD       
# 2       7 ON-107X  ON-107X   
# sale_id channel number
# <int> <chr>   <chr> 
# 1       1 ST      101   
# 2       2 ON      102   
# 3       3 ST      103   
# 4       4 ON      104   
# 5       6 ST      106   
# 6       8 ON      108   
# sale_id tags  
# <int> <chr> 
# 1       1 gift  
# 2       1 urgent
# 3       2 online
# 4       3 gift  
# 5       4 member
# 6       5 online
# 7       5 member
# 8       6 gift  
# 9       7 member
# 10       8 gift  
# 11       8 member
# separate_wider_delim() splits one value into multiple columns, while separate_longer_delim() splits one value into multiple rows.

customer_summary <- sales_clean |>
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if_else(
      known_n == 0,
      NA_real_,
      large_n / known_n
    ),
    .by = customer_id
  ) |>
  arrange(customer_id)
customer_summary

sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(
      factor(item_clean),
      n = 2,
      other_level = "Other"
    )
  )
levels(sales_factors$item_factor)
levels(sales_factors$item_group)

sales_factors |>
  count(item_factor)

sales_factors |>
  count(item_group)

# Output
# customer_id sales_n known_n large_n large_share
# <chr>         <int>   <int>   <int>       <dbl>
# 1 C1                2       1       0       0    
# 2 C2                3       3       2       0.667
# 3 C3                1       1       0       0    
# 4 C4                1       1       1       1    
# 5 C5                1       0       0      NA    
# For C1, the numerator of large_share is 0 because none of C1's known sales have a net amount of at least 20.
# The denominator is 1 because C1 has one known net amount.
# Therefore C1's large_share is 0 / 1 = 0.


reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10,-999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()

reports_labeled


reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

reports_clean |>
  select(
    form,
    line,
    customer_id,
    round,
    primary_amount,
    backup_amount,
    amount
  )

reports_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |>
  complete(
    customer_id,
    round = 1:3,
    fill = list(recorded_row = FALSE)
  )

reports_complete


original_rows <- sum(reports_complete$recorded_row)
added_rows <- sum(!reports_complete$recorded_row)

missing_before <- sum(is.na(reports_clean$amount))
missing_after <- sum(is.na(reports_complete$amount))

original_rows
added_rows
missing_before
missing_after

#Output
# form   line customer_id round primary_amount backup_amount amount
# <chr> <dbl> <chr>       <dbl>          <dbl>         <dbl>  <dbl>
# 1 A         1 C1              1             10            NA     10
# 2 A         2 C1              3           -999            15     15
# 3 A         3 C2              1             20            25     20
# 4 B         1 C3              1             NA            NA     NA
# 5 B         2 C3              2              0            99      0
# 6 B         3 C4              1             30            NA     30
# coalesce() uses primary_clean whenever it contains a real value. It uses backup_amount only when primary_clean is missing.
# Therefore a recorded zero is preserved because 0 is a valid value and is not considered missing.

# An explicit missing amount is C3, round 1. The report exists, but its amount is NA.
#An implicit missing row is C1, round 2. There was no row for that customer-round combination before complete(). complete() creates the missing customer-round combinations but leaves their amounts as NA.

customer_id_check <- customers |>
  summarise(
    missing_ids = sum(is.na(customer_id)),
    duplicate_ids = sum(duplicated(customer_id))
  )

customer_id_check

# missing_ids duplicate_ids
# <int>         <int>
#   1           0             0

duplicate_customer_rounds <- reports_clean |>
  count(customer_id, round) |>
  filter(n > 1)

duplicate_customer_rounds

customers_marked <- customers |>
  mutate(customer_found = TRUE)

reports_joined <- reports_clean |>
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) |>
  mutate(
    customer_found = replace_na(customer_found, FALSE)
  )

reports_joined |>
  select(
    customer_id,
    round,
    amount,
    region,
    customer_found
  )

reports_only <- reports_clean |>
  distinct(customer_id) |>
  anti_join(
    customers |>
      distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never"
  )

reports_only

customers_only <- customers |>
  distinct(customer_id) |>
  anti_join(
    reports_clean |>
      distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never"
  )

customers_only

# C3 has customer_found = TRUE, meaning a matching customer record exists even though the region itself is missing.
# C4 has customer_found = FALSE, meaning there is no matching customer record at all. Therefore, a missing region alone cannot tell us whether the customer exists in the customers table.


two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  
  nx <- length(x)
  ny <- length(y)
  
  x_bar <- mean(x)
  y_bar <- mean(y)
  
  sx2 <- var(x)
  sy2 <- var(y)
  
  if (equal_var) {
    
    sp2 <- (
      (nx - 1) * sx2 +
        (ny - 1) * sy2
    ) / (nx + ny - 2)
    
    se <- sqrt(
      sp2 * (1 / nx + 1 / ny)
    )
    
    df <- nx + ny - 2
    method <- "Pooled"
    
  } else {
    
    se <- sqrt(
      sx2 / nx +
        sy2 / ny
    )
    
    df <- (
      (sx2 / nx + sy2 / ny)^2
    ) / (
      ((sx2 / nx)^2 / (nx - 1)) +
        ((sy2 / ny)^2 / (ny - 1))
    )
    
    method <- "Welch"
  }
  
  t_stat <- (x_bar - y_bar) / se
  
  p_value <- 2 * pt(
    abs(t_stat),
    df = df,
    lower.tail = FALSE
  )
  
  reject <- p_value < alpha
  
  list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = reject
  )
}


x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)


welch_result <- two_sample_t(
  x_check,
  y_check
)

pooled_result <- two_sample_t(
  x_check,
  y_check,
  equal_var = TRUE
)

welch_result
pooled_result

welch_reference <- t.test(
  x_check,
  y_check,
  var.equal = FALSE
)

pooled_reference <- t.test(
  x_check,
  y_check,
  var.equal = TRUE
)


welch_reference
pooled_reference


welch_comparison <- c(
  t_stat =
    abs(
      welch_result$t_stat -
        unname(welch_reference$statistic)
    ) < 1e-8,
  
  df =
    abs(
      welch_result$df -
        unname(welch_reference$parameter)
    ) < 1e-8,
  
  p_value =
    abs(
      welch_result$p_value -
        welch_reference$p.value
    ) < 1e-8
)

welch_comparison


pooled_comparison <- c(
  t_stat =
    abs(
      pooled_result$t_stat -
        unname(pooled_reference$statistic)
    ) < 1e-8,
  
  df =
    abs(
      pooled_result$df -
        unname(pooled_reference$parameter)
    ) < 1e-8,
  
  p_value =
    abs(
      pooled_result$p_value -
        pooled_reference$p.value
    ) < 1e-8
)

pooled_comparison


# All three Welch comparisons should be TRUE.
# All three pooled comparisons should be TRUE.
# Therefore, the manually constructed function agrees with R's t.test() to within 1e-8 for t-statistic, degrees of freedom, and p-value.


B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c(
    "equal_null",
    "unequal_null",
    "equal_shift",
    "unequal_shift"
  ),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

set.seed(2026)


total_rows <- nrow(scenarios) * B * 2

simulation_results <- data.frame(
  scenario = character(total_rows),
  replicate = integer(total_rows),
  method = character(total_rows),
  t_stat = numeric(total_rows),
  df = numeric(total_rows),
  p_value = numeric(total_rows),
  reject = logical(total_rows),
  stringsAsFactors = FALSE
)


row_index <- 1


for (i in seq_len(nrow(scenarios))) {
  
  for (b in seq_len(B)) {
    
    x_sim <- rnorm(
      n_x,
      mean = scenarios$mean_x[i],
      sd = scenarios$sd_x[i]
    )
    
    y_sim <- rnorm(
      n_y,
      mean = scenarios$mean_y[i],
      sd = scenarios$sd_y[i]
    )
    
    
    welch_test <- two_sample_t(
      x_sim,
      y_sim,
      equal_var = FALSE,
      alpha = alpha
    )
    
    pooled_test <- two_sample_t(
      x_sim,
      y_sim,
      equal_var = TRUE,
      alpha = alpha
    )
    
    
    simulation_results[row_index, ] <- list(
      scenarios$scenario[i],
      b,
      welch_test$method,
      welch_test$t_stat,
      welch_test$df,
      welch_test$p_value,
      welch_test$reject
    )
    
    row_index <- row_index + 1
    
    
    simulation_results[row_index, ] <- list(
      scenarios$scenario[i],
      b,
      pooled_test$method,
      pooled_test$t_stat,
      pooled_test$df,
      pooled_test$p_value,
      pooled_test$reject
    )
    
    row_index <- row_index + 1
  }
}


head(simulation_results, 6)

nrow(simulation_results)

simulation_summary <- simulation_results |>
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  )

simulation_summary


simulation_summary_rounded <- simulation_summary |>
  mutate(
    mean_t = round(mean_t, 3),
    sd_t = round(sd_t, 3),
    rejection_rate = round(rejection_rate, 3)
  )

simulation_summary_rounded

plot_table <- xtabs(
  rejection_rate ~ scenario + method,
  data = simulation_summary
)

barplot(
  t(plot_table),
  beside = TRUE,
  legend.text = TRUE,
  main = "Rejection Rates by Scenario and Test",
  xlab = "Scenario",
  ylab = "Rejection Rate"
)

abline(
  h = 0.05,
  lty = 2
)


# Short Report

# When the population means and variances are equal, both tests have rejection rates that are close to 0.05.
# When the variances are different, Welch's test does a better job of keeping the Type I error under control, while the pooled test can reject too much.
# When the means are different, the rejection rates go up because the tests are more likely to detect the difference.
# Having higher power is not always better if the test is also making too many Type I errors when the null hypothesis is actually true.