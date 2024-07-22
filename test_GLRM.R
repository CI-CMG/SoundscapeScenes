# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for data visualization
library(tidyr)    # for data reshaping
library(readr)
library(gridExtra)

# https://bradleyboehmke.github.io/HOML/GLRM.html
# https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glrm.html#:~:text=Low%20rank%20models%20facilitate%20the,numeric%20matrices%20X%20and%20Y.
# https://www.nowpublishers.com/article/Details/MAL-055

url <- "https://koalaverse.github.io/homlr/data/my_basket.csv"
my_basket <- readr::read_csv(url)

head(mtcars)
# Modeling packages
library(h2o)  # for fitting GLRMs
h2o.init()

h2o.no_progress()  # turn off progress bars
h2o.init(max_mem_size = "5g")  # connect to H2O instance
my_basket.h2o <- as.h2o(my_basket)
# run basic GLRM
basic_glrm <- h2o.glrm(
  training_frame = my_basket.h2o,
  k = 20, 
  loss = "Quadratic",
  regularization_x = "None", 
  regularization_y = "None", 
  transform = "STANDARDIZE", 
  max_iterations = 2000,
  seed = 123
)
summary(basic_glrm)
plot(basic_glrm)

p1 <- t(basic_glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, reorder(feature, Arch1))) +
  geom_point()

p2 <- t(basic_glrm@model$archetypes) %>% 
  as.data.frame() %>% 
  mutate(feature = row.names(.)) %>%
  ggplot(aes(Arch1, Arch2, label = feature)) +
  geom_text()

gridExtra::grid.arrange(p1, p2, nrow = 1)


predict(k8_glrm_regularized, my_basket.h2o)[1:5, 1:5]
