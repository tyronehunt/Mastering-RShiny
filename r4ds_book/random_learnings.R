library(tidyverse)
library(nycflights13)

airlines

airports

planes

weather

planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2



flights2 %>%
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

flights2 %>% 
  left_join(airports, c("dest" = "faa"))

airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)

flights %>% count(tailnum)

sum(!is.na(flights$tailnum))


?'"'

x <- c("apple", "banana", "pear", "gym")
str_view(x, "an")

str_view(x, "a.")

str_view(c("abc", "deaf", "def", "abcdeaf", "abcdxd"), "abc|d..f")


x <- "1888 is the longest year in Roman numerals: MDCCCCCLXXXXXVIII"
str_view(x, 'CLX*')

str_view(c("colour", "color"), "colou?r")

str_view(x, 'C(LX)*')

fruit <- c("apple", "banana", "pear", "coconut")
str_view(fruit, "(..)\\1")

no_vowels_1 <- !str_detect(x, "[aeiou]")

no_vowels_2 <- str_detect(no_vowels_1, "^[^aeiou]+$")
no_vowels_2

df <- tibble(
  word = words, 
  i = seq_along(word)
)


df <- tibble(
  word = words, 
  i = seq_along(word)
)
df %>% 
  filter(str_detect(word, "x$"))

length(sentences)
head(sentences)


colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")
colour_match
has_colour <- str_subset(sentences, colour_match)
has_colour
matches <- str_extract(has_colour, colour_match)
head(matches)

more <- sentences[str_count(sentences, colour_match) > 1]
more
str_view_all(more, colour_match)

str_extract(more, colour_match)

str_extract_all(more, colour_match, simplify=TRUE)

has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)

has_noun

has_noun %>% 
  str_extract(noun)

tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)", 
    remove = FALSE
  )

x <- c("1 house", "2 cars", "22 people")
str_replace_all(x, c("1" = "one","22" = "TWO", "2" = "two"))

head(sentences, 5)

sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1  \\3  \\2") %>% 
  head(5)

sentences %>%
  head(5) %>% 
  str_split(" ")

"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]]


x <- "This is a sentence.  This is another sentence."
str_view_all(x, boundary("T"))

x <- "Line 1\nLine 2\nLine 3"

x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
y1 <- factor(x1, levels = month_levels)
y1
sort(y1)
y2 <- factor(x2, levels = month_levels)
y2
y2 <- parse_factor(x2, levels = month_levels)

f1 <- factor(x1, levels = unique(x1))
f1
f2 <- x1 %>% factor() %>% fct_inorder()
f2

gss_cat

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

relig_summary

relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()


rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

rincome_summary

ggplot(rincome_summary, aes(age, fct_relevel(rincome, "No answer"))) +
  geom_point()


by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))
by_age

ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

gss_cat %>%
  mutate(marital = marital %>% fct_infreq())

gss_cat %>%
  mutate(marital = marital %>% fct_infreq()) %>%
  ggplot(aes(marital)) +
  geom_bar()

gss_cat$partyid

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
  )) %>%
  count(partyid)


gss_cat %>%
  count(relig, sort = TRUE)

gss_cat %>%
  mutate(relig = fct_lump(relig, n=10))  %>%
  count(relig, sort = TRUE)

library(lubridate)

ymd("2017-01-31")
ymd(now())


flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))


make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

make_datetime_100

flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% view()

?update

flights_dt %>% 
  mutate(dep_hour = update(dep_time, yday = 1)) %>% 
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 300)

ymd("2016-12-07") %>% 
  update(yday = 2)

flights_dt %>% 
  filter(arr_time < dep_time) %>%
  select(arr_time, dep_time, everything())


flights_dt <- flights_dt %>% 
  mutate(
    overnight = arr_time < dep_time,
    arr_time = arr_time + days(overnight * 1),
    sched_arr_time = sched_arr_time + days(overnight * 1)
  ) %>%
  select (overnight, arr_time, sched_arr_time, everything())

flights_dt

years(1) / days(1)

next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)

Sys.timezone()

library(magrittr)
foo_foo <- little_bunny()
foo_foo_1 <- hop(foo_foo, through = forest)

diamonds <- ggplot2::diamonds
diamonds2 <- diamonds %>% 
  dplyr::mutate(price_per_carat = price / carat)

library(pryr)
pryr::object_size(diamonds)
pryr::object_size(diamonds2)

env <- environment()
"x" %>% assign(100, envir = env)
x

mtcars

mtcars %$%
  cor(disp, mpg)

df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df
x <- df$a
x
(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))


is_directory <- function(x) file.info(x)$isdir
is_directory("r4ds.Rproj")

file.info("untitled folder")

is_readable <- function(x) file.access(x, 4) == 0
is_readable("r4ds.Rproj")


# Functions (use cmd + shift + r) ---------------------------------------------------

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}

x <- c("tyrone", "test", NA, 3)
x <- mtcars$disp
has_name(x)


function(x, y, op) {
switch(op,
plus = x + y,
minus = x - y,
times = x * y,
divide = x / y,
stop("Unknown op!")
)
}

mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)
x
mean_ci(x)


# checking_function_values ------------------------------------------------

wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}
wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w * (x - mu) ^ 2) / sum(w)
}
wt_sd <- function(x, w) {
  sqrt(wt_var(x, w))
}

wt_mean <- function(x, w) {
  if (length(x) != length(w)) {
    stop("x and w must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}

wt_mean(1:6, 1:3)


wt_mean <- function(x, w, na.rm = TRUE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}

wt_mean(1:6, 1:4)

commas <- function(x) stringr::str_c(x, collapse = ", ")
commas(letters[1:10])

stringr::str_c(commas(letters[1:10]), collapse = ", ")


rule <- function(..., pad = "-") {
  title <- print(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
  list(...)
}

rule("Important output")

complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(2)
  }
  
  # Complicated code here
}
complicated_function("", "", 2)

show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ",n, "\n", sep = "")
  
  invisible(df)
}

show_missings(flights)

if (length(c(1, 2, 3))) {
  print("what")
}

1:10 + 1:10

tibble(x = 1:4, y = 1:2)

x <- list(1, 2, 3)
x

x_named <- list(a = 1, b = 2, c = 3)
str(x_named)
typeof(x_named)

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
str(a)
str(a[[4]][1])

x <- 1:10
x
typeof(x)
attr(x, "greeting")
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)

as.Date

methods("as.Date")

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)


output <- vector("double", ncol(df))  
for (i in df) {           
  i
}
output

means <- c(0, 1, 2)

out <- vector("list", length(means))
str(out)
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}

str(unlist(out))


df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", length(df))

for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

map_dbl(df, mean)
map_dbl(df, mean, trim = 0.2)


z <- list(x = 1:3, y = 4:5)
z
map_int(z, length)


models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

models %>% 
  map(summary) %>% 
  map_dbl(~.$r.squared)

safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))

y <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)

is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]

y$result[is_ok] %>% flatten_dbl()

x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))
x

mu <- list(5, 10, -3)
mu %>% 
  map(rnorm, n = 5) %>% 
  str()

n <- list(1, 3, 5)
args1 <- list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>% 
  str()

library(ggplot2)
plots <- mtcars %>% 
  split(.$cyl) %>% 
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")

pwalk(list(paths, plots), ggsave, path = tempdir())


# MODELS ------------------------------------------------------------------

library(tidyverse)

library(modelr)
options(na.action = na.warn)

ggplot(sim1, aes(x, y)) + 
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 

# Linear function
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}

model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )
