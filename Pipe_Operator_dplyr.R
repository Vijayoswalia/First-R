View(iris)
iris %>%
  group_by(Species)%>%
  summarise(avg = mean(Sepal.Width)) %>%
  arrange(avg)
