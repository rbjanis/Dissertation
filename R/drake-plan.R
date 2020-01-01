library(drake)
library(tidyverse)

plan <- drake_plan(
  raw_pre_data = load("data/2013-2015_TI_2019-02-05.RDa"),
  raw_post_data = load("data/2016-2018_TI_2019-03-12.rda")
  # data = raw_data %>%
  #   mutate(Species = forcats::fct_inorder(Species)),
  # hist = create_plot(data),
  # fit = lm(Sepal.Width ~ Petal.Width + Species, data),
  # report = rmarkdown::render(
  #   knitr_in("report.Rmd"),
  #   output_file = file_out("report.html"),
  #   quiet = TRUE
  # )
)
