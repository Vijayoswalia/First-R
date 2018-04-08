library(dplyr)
library(nycflights13)
flights
iris
dplyr::tbl_df(iris)
print(dplyr::tbl_df(mtcars), n=30)
tbl_df(iris) %>% print(n=Inf)
tbl_df(iris) %>% print(width=Inf)
tbl_df(iris) %>% as.data.frame(iris)

glimpse(iris)
head(iris)
str(iris)

mtcars['mpg']
mtcars %>% select(mpg, vs, wt)
mtcars %>% group_by(cyl) %>% summarise(avg= mean(wt), mean(hp)) %>% arrange(avg)
names(mtcars)
filter(mtcars,mpg>23 | wt< 2)
mtcars %>% select(mpg,wt) %>% filter(mpg>23)


names(iris)
filter(iris, Sepal.Length>7 | Petal.Length<7)
filter(mtcars, cyl ==4)
distinct(mtcars)
sample_frac(mtcars, 0.5, replace=T)
sample_n(mtcars, 2, replace=T) %>% select(mpg)
slice(mtcars, 1:4)
top_n(mtcars, 2, mpg)

select(mtcars, mpg) %>% arrange(mpg)

dplyr::summarise(iris, avg=mean(Sepal.Length))
dplyr::summarise_all(iris, fun(mean))


a = c(x1=c('A','B','C'), x2=c(1,2,3))
a = c(x3=c('A','B','D'),x2=c(T,F,T))
a
