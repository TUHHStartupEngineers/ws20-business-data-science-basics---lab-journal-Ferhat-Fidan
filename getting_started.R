iris %>% head(n=3)

tibble(
  x = 1:50,
  y = runif(50),
  Z = x + y^2,
  outcome = rnorm(50)
)


class(cars)
cars_tbl <- as_tibble(cars)
class(cars_tbl)

vehicles <- as_tibble(cars[1:5,])
vehicles
vehicles[['speed']]
vehicles[[1]]
vehicles$speed

vehicles %>% .$dist
vehicles %>% .[['dist']]
vehicles %>% .[[2]]
write_csv(cars_tbl, "cars_test.csv")


library("tidyverse")
diamonds2 <- readRDS("diamonds2.rds")

diamonds2 %>% head(n=5)
diam<-diamonds2 %>% pivot_longer(cols = c("2008", "2009"), 
                           names_to = 'year', 
                           values_to = 'price') %>% head(n=5)

diam

model <- lm(price ~.,data = diam)
model

diamonds3 <- readRDS("diamonds3.rds")
diamonds3 %>% head(n=5)

diam3<-diamonds3 %>% pivot_wider(names_from = "dimension", values_from = "measurement") %>% head(n=5)

diamonds4 <- readRDS("diamonds4.rds")
diamonds4
diam4 <- diamonds4 %>% separate(col = dim,
                                into = c("x","y","z"),
                                sep = "/",
                                convert = T)
diamonds5 <- readRDS("diamonds5.rds")
diamonds5
diam5 <- diamonds5 %>% unite(clarity, clarity_prefix, clarity_suffix, sep = '')
diam5
diamonds
diamonds %>% filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% head(5)
diamonds %>% filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% slice(3:4)
diamonds %>% arrange(cut, carat, desc(price))
diamonds %>% select(color, clarity, x:z) %>% head(n=5)
diamonds %>% select(-(x:z)) %>% head(n=5)
diamonds %>% select((x:z),everything()) %>% head(n=5)
diamonds %>% select(starts_with('c')) %>% head(n=5)
diamonds %>% select(contains('cu')) %>% head(n=5)
diamonds %>% rename(var_x = x) %>% head(n=5)
diamonds %>% mutate(p = x + z, q = p + y) %>% 
  select(-(depth:price)) %>% head(n=5)
diamonds %>% transmute(carat, cut, sum = x+y+z) %>% head(n=5)
diamonds %>% group_by(cut)
by_cyl <- mtcars %>% group_by(cyl)
by_cyl
mtcars
mtcars %>% summarise(
  disp = mean(disp),
  hp = mean(hp)
)
diamonds %>% group_by(cut) %>% summarize(max_price = max(price),
                                         mean_price = mean(price),
                                         min_price = min(price), standard_deviation = sd(price))
glimpse(diamonds)
typeof(diamonds)
