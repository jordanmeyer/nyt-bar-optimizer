library("readr")
library("dplyr")
library("reshape2")
library("tidyr")
library("rhandsontable")
library("shinyBS")
library("shinydashboard")
library("shinyjs")
library("V8")
library("lazyeval")

source("functions.R")
source("www/my-network.R")

ingredient.df <- read_csv("data/ingredient-df.csv")
recipe.df <- read_csv("data/recipe-df.csv")
recipe.ingredient.df <- read_csv("data/recipe-ingredient-df.csv")
similar.drinks.df <- read_csv("data/similar-drinks-df.csv")
load("./data/all_optimal_list.rda")

## Empty seed inventory for online use. If running locally, can pull from
## a saved "my inventory" csv as below to avoid loading manually:
## seed.inventory <- read_csv("data/my-inventory.csv")
seed.inventory <-
  ingredient.df %>%
  filter(ingredient != "DROP") %>%
  inner_join(recipe.ingredient.df, by="ingredient") %>%
  group_by(ingredient.group, ingredient.subgroup, ingredient) %>%
  summarise(total.drink.count = n())%>%
  select(total.drink.count, ingredient.group, ingredient.subgroup, ingredient) %>%
  ungroup() %>%
  arrange(desc(total.drink.count))

seed.inventory$in.inventory <- FALSE

## shinyjs extension to show about modal on startup
showModal <- 'shinyjs.showInfo = function(){$("#aboutModal").modal()}'
