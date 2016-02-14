################################################################################
## Functions to scrape cooking.nytimes.com
## NB: NYT occasionally changes the structure of their HTML
## Check the get_recipes function if a list has no values for an expected field
## All field extracts working as of Feb 8, 2016 - 400 cocktails
################################################################################

################################################################################
## get_recipe_links accepts an optional search term and an INDIVIDUAL optional
## filter field and value. For the cocktail dataset, one filter is sufficient.
## get_recipe_links returns a vector of urls for the resulting recipes

get_recipe_links <- function(search.term="", filter.field="", filter.value=""){
  recipe.links <- character()
  recipe.image.links <- character()
  recipe.image.classes <- character()

  query.string <-
    sprintf("http://cooking.nytimes.com/search?filters[%s][]=%s&q=%s",
            filter.field, filter.value, search.term)

  search.page <- read_html(query.string)

  total.recipes <-
    search.page %>%
    html_nodes(".search-content #search-results #search-count") %>%
    html_attr("data-count") %>%
    as.numeric()

  for(page in 1:ceiling(total.recipes/48)){
    url <- paste0(query.string, "&page=", page)

    search.page <- try(read_html(url))

    recipe.links <-
      search.page %>%
      html_nodes(".card-recipe-info") %>%
      html_attr("href") %>%
      c(recipe.links, .)

    recipe.image.links <-
      search.page %>%
      html_nodes(".recipe-card-list .image-anchor .image img") %>%
      html_attr("data-large") %>%
      c(recipe.image.links, .)

    recipe.image.classes <-
      search.page %>%
      html_nodes(".recipe-card-list .image-anchor .image img") %>%
      html_attr("class") %>%
      c(recipe.image.classes, .)

    Sys.sleep(1) ## be nice
  }

  recipe.image.links <-
  ifelse(str_detect(recipe.image.links, "^/static"),
         paste0("http://cooking.nytimes.com", recipe.image.links),
         recipe.image.links)

  recipe.links.df <- data.frame(recipe.link = recipe.links,
                                recipe.image.link = recipe.image.links,
                                recipe.image.classes = recipe.image.classes,
                                stringsAsFactors = FALSE)

  return(recipe.links.df)
}

################################################################################
## get_recipes accepts a vector of recipe urls from cooking.nytimes.com
## get_recipes returns a list of recipes
## NB: NYT occasionally changes the structure of their HTML
## All field extracts working as of Feb 8, 2016

get_recipes <- function(recipe.links) {
  recipes <- list()

  for(recipe.num in 1:length(recipe.links)){
    recipe.link <- recipe.links[recipe.num]

    try(recipe.page <- read_html(paste0("http://cooking.nytimes.com",
                               recipe.link)))

    recipes[[recipe.num]] <- list()

    ## Link (id)
    recipes[[recipe.num]]$link <- recipe.link

    ## Image
    recipes[[recipe.num]]$recipe.image <-
      recipe.page %>%
      html_nodes(".recipe-intro .media-container img") %>%
      html_attr("src")

    ## Name
    recipes[[recipe.num]]$recipe.name <-
      recipe.page %>%
      html_nodes(".recipe-title") %>%
      html_attr("data-name")

    ## Author
    recipes[[recipe.num]]$recipe.author.name <-
      recipe.page %>%
      html_nodes(".recipe-subhead .author") %>%
      html_attr("data-author")

    ## Author Link
    recipes[[recipe.num]]$recipe.author.link <-
      recipe.page %>%
      html_nodes(".recipe-subhead .author") %>%
      html_attr("href")

    ## Yield
    recipes[[recipe.num]]$recipe.yield <-
      recipe.page %>%
      html_nodes(".recipe-subhead span[itemprop='recipeYield']") %>%
      html_text()

    ## Topnote
    recipes[[recipe.num]]$recipe.topnote <-
      recipe.page %>%
      html_nodes(".topnote p:not(.related-article)") %>%
      html_text()

    ## tags
    recipes[[recipe.num]]$recipe.tags <-
      recipe.page %>%
      html_nodes(".recipe-metadata .tags-nutrition-container .tag-block a") %>%
      html_text()

    ## nutritional info
    recipes[[recipe.num]]$recipe.nutrition.header <-
      recipe.page %>%
      html_nodes(".nutrition-tooltip .header") %>%
      html_text()

    recipes[[recipe.num]]$recipe.nutrition.calories <-
      recipe.page %>%
      html_nodes(".nutrition-tooltip span[itemprop='calories']") %>%
      html_text()

    recipes[[recipe.num]]$recipe.nutrition.fat <-
      recipe.page %>%
      html_nodes(".nutrition-tooltip span[itemprop='fatContent']") %>%
      html_text()

    recipes[[recipe.num]]$recipe.nutrition.saturated.fat <-
      recipe.page %>%
      html_nodes(".nutrition-tooltip span[itemprop='saturatedFatContent']") %>%
      html_text()

    recipes[[recipe.num]]$recipe.nutrition.trans.fat <-
      recipe.page %>%
      html_nodes(".nutrition-tooltip span[itemprop='transFatContent']") %>%
      html_text()

    recipes[[recipe.num]]$recipe.nutrition.carbs <-
      recipe.page %>%
      html_nodes(".nutrition-tooltip span[itemprop='carbohydrateContent']") %>%
      html_text()

    recipes[[recipe.num]]$recipe.nutrition.fiber <-
      recipe.page %>%
      html_nodes(".nutrition-tooltip span[itemprop='fiberContent']") %>%
      html_text()

    recipes[[recipe.num]]$recipe.nutrition.protein <-
      recipe.page %>%
      html_nodes(".nutrition-tooltip span[itemprop='proteinContent']") %>%
      html_text()

    recipes[[recipe.num]]$recipe.nutrition.cholesterol <-
      recipe.page %>%
      html_nodes(".nutrition-tooltip span[itemprop='cholesterolContent']") %>%
      html_text()

    recipes[[recipe.num]]$recipe.nutrition.sodium <-
      recipe.page %>%
      html_nodes(".nutrition-tooltip span[itemprop='sodiumContent']") %>%
      html_text()

    ## ratings
    recipes[[recipe.num]]$recipe.rating <-
      recipe.page %>%
      html_nodes(".recipe-metadata span[itemprop='ratingValue']") %>%
      html_text() %>%
      as.numeric()

    recipes[[recipe.num]]$recipe.num.reviews <-
      recipe.page %>%
      html_nodes(".recipe-metadata span[itemprop='ratingCount']") %>%
      html_text() %>%
      as.numeric()

    ## Ingredient quantities
    recipes[[recipe.num]]$ingredient.quantities <-
      recipe.page %>%
      html_nodes(".recipe-instructions .recipe-ingredients li[itemprop='recipeIngredient'] .quantity") %>%
      html_text()

    ## Ingredient names
    recipes[[recipe.num]]$ingredient.names <-
      recipe.page %>%
      html_nodes(".recipe-instructions .recipe-ingredients li[itemprop='recipeIngredient'] .ingredient-name span") %>%
      html_text()

    ## Ingredient descriptive names
    recipes[[recipe.num]]$ingredient.desc.names <-
      recipe.page %>%
      html_nodes(".recipe-instructions .recipe-ingredients li[itemprop='recipeIngredient'] .ingredient-name") %>%
      html_text()

    ## Steps vector
    recipes[[recipe.num]]$recipe.steps <-
      recipe.page %>%
      html_nodes(".recipe-instructions .recipe-steps li") %>%
      html_text()

    Sys.sleep(1)
  }

  return(recipes)
}


################################################################################
## Helper function to clean recipe fields
clean_text <- function(s) {
  s <- str_replace_all(s, "\n", " ")
  s <- str_replace_all(s, "  ", " ")
  s <- str_replace_all(s, "\\\"", "'")
  s
}

################################################################################
## extract_recipe_ingredient_df accepts a single recipe list as input
## extract_recipe_ingredient_df returns a dataframe with
## one row per recipe ingredient
extract_recipe_ingredient_df <- function(recipe){
  link <- recipe$link
  quantities <- recipe$ingredient.quantities
  ingredients <- recipe$ingredient.desc.names

  ## clean ingredient description carriage returns
  ingredients <- clean_text(ingredients)

  data.frame(recipe.link = link,
             ingredient.quantity = quantities,
             ingredient.as.written = ingredients,
             stringsAsFactors = FALSE)
}

################################################################################
## Create Network
get_pair <- function(i, j, recipe.df, recipe.ingredient.df, ingredient.names) {
  recipe.1.ingredients <-
    filter(recipe.ingredient.df,
           recipe.link==recipe.df$recipe.link[i])$ingredient %>%
    intersect(ingredient.names)

  recipe.2.ingredients <-
    filter(recipe.ingredient.df,
           recipe.link==recipe.df$recipe.link[j])$ingredient %>%
    intersect(ingredient.names)

  if(length(recipe.1.ingredients) == 0 ||
     length(recipe.2.ingredients) == 0) return(NA)

  if(length(intersect(recipe.1.ingredients,recipe.2.ingredients)) > 0){
    return(data.frame(source = recipe.df$recipe.link[i],
                      target = recipe.df$recipe.link[j],
                      value = length(intersect(recipe.1.ingredients,
                                               recipe.2.ingredients)),
                      stringsAsFactors = FALSE))
  } else return(NA)
}

get_pairs <- function(i, recipe.df, recipe.ingredient.df, ingredient.names) {
  pairs <- lapply((i+1):nrow(recipe.df),
                  function(j) get_pair(i,j,
                                       recipe.df,
                                       recipe.ingredient.df,
                                       ingredient.names))

  return(rbind_all(pairs[!is.na(pairs)]))
}


################################################################################
## Optimization functions

get_total_possible_drinks <- function(ingredients.in.inventory) {
  inventory <- rep(0, NCOL(recipe.mat.bool))
  inventory[floor(ingredients.in.inventory)] <- 1
  ingredient.matches <- recipe.mat.bool %*% inventory
  ingredients.in.recipe <- rowSums(recipe.mat.bool)
  return(sum(ingredient.matches == ingredients.in.recipe))
}

all_genoud <- function(n){
  genoud(get_total_possible_drinks, pop.size = 1000, max.generations = 800,
         wait.generations = 100,
         max = TRUE,
         data.type.int = TRUE,
         nvars = n,
         boundary.enforcement = 2,
         Domains = cbind(rep(1,n), rep(NCOL(recipe.mat.bool), n)),
         print.level=0,
         optim.method="Nelder-Mead"
  )$par
}
