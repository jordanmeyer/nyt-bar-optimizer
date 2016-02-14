library("parallel")
library("rvest")
library("readr")
library("dplyr")
library("stringr")
library("reshape2")
library("rgenoud")

## Extract functions for cooking.nytimes.com
source("functions.R")

## ingredient-lookup.csv has all unique cocktail ingredient descriptions from
## NYT as of Feb 8, 2016 with cleaned ingredient name and grouping
## NB: Modify this file if you want different substution rules or if new
## cocktails have unaccounted for ingredient descriptions
ingredient.lookup <- read_csv("../data/ingredient-lookup.csv")

################################################################################
## Scrape the data from cooking.nytimes.com

## Get all cocktail recipe URLs
cocktail.recipe.links <- get_recipe_links("", "dish_types", "cocktails")

## Get a list of all cocktail recipes
cocktail.recipe.list <- get_recipes(cocktail.recipe.links$recipe.link)

## Get all images
img.link.split <- str_split(cocktail.recipe.links$recipe.image.link, "/")
img.names <- sapply(img.link.split, function(x) x[[length(x)]])
img.names <- str_replace(img.names, "\\?1", "")
## for(i in 1:length(cocktail.recipe.links$recipe.image.link)){
##   download.file(cocktail.recipe.links$recipe.image.link[i],
##                 paste0("../www/images/", img.names[i]))
## }

################################################################################
## recipe.df - One row per recipe uniquely identified by the URL

## Recipe URL
recipe.df <-
  data.frame(recipe.link = cocktail.recipe.links$recipe.link,
             stringsAsFactors = FALSE)

## Recipe name
recipe.df$recipe.name <- sapply(cocktail.recipe.list, function(x) x$recipe.name)

## Recipe Author
recipe.df$recipe.author <- sapply(cocktail.recipe.list, function(x) x$recipe.author.name)

## Recipe description
recipe.df$recipe.description <-
  sapply(cocktail.recipe.list,
         function(x) paste(unlist(x$recipe.topnote),
                           collapse = " "))

recipe.df$recipe.description <-
  clean_text(recipe.df$recipe.description)

## Recipe instructions
recipe.df$recipe.instructions <-
  sapply(cocktail.recipe.list,
         function(x) paste(unlist(x$recipe.steps),
                           collapse = " "))

recipe.df$recipe.instructions <-
  clean_text(recipe.df$recipe.instructions)

recipe.df$img <- img.names

recipe.df$img.css <- cocktail.recipe.links$recipe.image.classes

write.csv(recipe.df, file="../data/recipe-df.csv", row.names = FALSE)

################################################################################
## ingredient.df - One row per ingredient uniquely identified by name
ingredient.df <-
  ingredient.lookup %>%
  select(ingredient.group, ingredient.subgroup, ingredient) %>%
  unique()

write.csv(ingredient.df, file="../data/ingredient-df.csv", row.names = FALSE)

################################################################################
## recipe.ingredient.df - One row per recipe ingredient
## uniquely identified by recipe.link and ingredient.name

## Extract recipe.link and ingredient description from cocktail.recipe.list
recipe.ingredient.df <-
  rbind_all(lapply(cocktail.recipe.list, extract_recipe_ingredient_df))

## Join to ingredient.lookup to get cleaned ingredient name
recipe.ingredient.df <-
  recipe.ingredient.df %>%
  mutate(join.ingredient = tolower(ingredient.as.written)) %>%
  left_join(ingredient.lookup, c("join.ingredient"="as.written")) %>%
  select(recipe.link, ingredient, ingredient.quantity, ingredient.as.written)

## CAUTION:
## If there are new cocktail recipes in the NYT DB with new
## ingredient.desc.names since the last time this was run (400 cocktails), the
## ingredient-lookup.csv file should be updated. The commented line below
## tests for unaccounted descriptions:
## recipe.ingredient.df$ingredient.as.written[is.na(recipe.ingredient.df$ingredient)]

write.csv(recipe.ingredient.df,
          file="../data/recipe-ingredient-df.csv", row.names = FALSE)

################################################################################
## Create similar drink network
## Similar drinks share one or more alcoholic ingredients
bottle.names <- filter(ingredient.df,
                       ingredient.group == "Bottle")$ingredient

similar.drinks.df <-
  rbind_all(lapply(1:(nrow(recipe.df)-1),
                   function(i) get_pairs(i,
                                         recipe.df,
                                         recipe.ingredient.df,
                                         bottle.names)))

write.csv(similar.drinks.df,
          file="../data/similar-drinks-df.csv", row.names = FALSE)

################################################################################
## Create optimal bars

## Optimize bottles only
recipe.bottles <-
  recipe.ingredient.df %>%
  subset(ingredient %in% bottle.names)

## ingredients by usage
recipe.bottles <- group_by(recipe.bottles, ingredient)
ingredient.count <- summarise(recipe.bottles,
                              count = length(ingredient))
ingredient.usage <- ingredient.count[order(-ingredient.count$count),]

## cast as 1 row per drink, 1 column for ingredient
recipe.mat <- dcast(data = recipe.bottles, recipe.link ~ ingredient,
                    value.var = "ingredient", fun.aggregate = length)
recipe.mat.bool <- recipe.mat[-1]

recipe.mat.bool <- data.matrix(recipe.mat.bool)
rownames(recipe.mat.bool) <- recipe.mat[,1]
mode(recipe.mat.bool) <- "logical"

## genoud
set.seed(1234)
all.optimal <- mclapply(1:40, all_genoud, mc.cores = 4)

plot(1:40, sapply(all.optimal, get_total_possible_drinks))

all.optimal.list <-
  lapply(all.optimal,
         function(x) colnames(recipe.mat.bool[,x,drop=FALSE]))

save(all.optimal.list, file="../data/all_optimal_list.rda")
