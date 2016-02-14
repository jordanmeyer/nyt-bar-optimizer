######################################################################
## functions

## get_recipe_matrix returns a boolean matrix with one row per
## recipe and one column per ingredient. Matrix can include bottles
## only or bottles + pantry depending on user selection.
get_recipe_matrix <- function(ingredient.df, recipe.ingredient.df,
                              user.ingredient.groups){
  bottle.names <-
  filter(ingredient.df,
         ingredient.group %in% user.ingredient.groups)$ingredient

  bottles.only.f <- filter(recipe.ingredient.df, ingredient %in% bottle.names)

  recipe.mat <- dcast(data = bottles.only.f,
                      recipe.link ~ ingredient,
                      value.var = "ingredient",
                      fun.aggregate = length)

  recipe.mat.bool <- recipe.mat[-1]
  recipe.mat.bool <- data.matrix(recipe.mat.bool)
  mode(recipe.mat.bool) <- "logical"

  rownames(recipe.mat.bool) <- recipe.mat$recipe.link
  return(recipe.mat.bool)
}

## get_drink_possibility returns a list with:
## 1) A vector of possible drinks
## 2) A dataframe of next best bottles names and the number of
## drinks they unlock
## 3) A dataframe of next best pantry item names and the number of
## drinks they unlock
get_drink_possibility <- function(my.inventory, recipe.mat, recipe.ingredient.df,
                                  ingredient.df, user.ingredient.groups,
                                  allow.subs){

  if(allow.subs){
    my.inventory.types <-
      unique(ingredient.df$ingredient.subgroup[ingredient.df$ingredient %in%
                                               my.inventory])
    my.inventory <-
      ingredient.df$ingredient[ingredient.df$ingredient.subgroup %in%
                               my.inventory.types]

    group.var <- "ingredient.subgroup"
    filter.criteria <- interp(~ !(which_column %in% my.inventory.types),
                              which_column = as.name(group.var))
  } else {
    group.var <- "ingredient"
    filter.criteria <- interp(~ !(which_column %in% my.inventory),
                              which_column = as.name(group.var))
  }

  ## Set up inventory vector
  inventory <- rep(0, NCOL(recipe.mat))
  names(inventory) <- colnames(recipe.mat)
  inventory[names(inventory) %in% my.inventory] <- 1

  ## What can we make with what we have
  ingredient.matches <- recipe.mat %*% inventory
  ingredients.in.recipe <- rowSums(recipe.mat)
  possible.drinks <-
    rownames(recipe.mat)[ingredient.matches == ingredients.in.recipe]

  ## What are we 1 ingredient away from
  one.away <-
    rownames(recipe.mat)[ingredient.matches+1 == ingredients.in.recipe]

  ## What are the ingredients we're missing?
  ## ingredients by usage
  recipe.ingredient.df <-
    recipe.ingredient.df %>%
    inner_join(ingredient.df, by="ingredient")

  if(length(one.away) > 0){
    bottle.count <-
      recipe.ingredient.df %>%
      filter(recipe.link %in% one.away,
             ingredient.group %in% "Bottle") %>%
      group_by_(group.var) %>%
      summarise(count = n()) %>%
      filter_(filter.criteria) %>%
      arrange(desc(count))

    pantry.count <-
      recipe.ingredient.df %>%
      filter(recipe.link %in% one.away,
             ingredient.group %in% "Pantry") %>%
      group_by_(group.var) %>%
      summarise(count = n()) %>%
      filter_(filter.criteria) %>%
      arrange(desc(count))
  } else {
    bottle.count <- NA
    pantry.count <- NA
  }

  return(list(possible.recipe.links=possible.drinks,
              bottle.count=bottle.count,
              pantry.count=pantry.count))

}
