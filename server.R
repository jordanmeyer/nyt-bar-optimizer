server <- function(input, output, clientData, session) {
  ##############################################################################
  ## Reactives and Observers

  ## global value for resetting rhandsontable with new inventory uploads
  new.upload <- FALSE

  ## inventory
  inventory <- reactiveValues(active = seed.inventory,
                              my.inventory = seed.inventory,
                              first.load = TRUE)

  ## Include Bottles only or Pantry
  user.ingredient.groups <- reactive(
    if(input$cfgPantry) c("Bottle", "Pantry")
    else("Bottle")
  )

  ## Keep active inventory and my.inventory in sync
  ## my.inventory can only be modified when active
  observeEvent(inventory$my.inventory, {
    inventory$active <- inventory$my.inventory
  })

  ## Update my.inventory and reset rhandsontable on upload
  observeEvent(input$uploadInv, {
    inventory$my.inventory <- read_csv(input$uploadInv$datapath)
    new.upload <<- TRUE
    hide("uploadInv")
  })

  ## Set active inventory and inputs when inventory preset changes
  observeEvent(input$cfgPreset, {
    inventory$active$in.inventory <- FALSE

    if(input$cfgPreset == "My Bar"){
      updateCheckboxInput(session, "cfgSubs", value = TRUE)
      updateCheckboxInput(session, "cfgPantry", value = TRUE)
      show("cfgSubs"); show("cfgPantry"); show("editInv"); show("saveInv")
      show("uploadToggle")
      updateTabsetPanel(session, "bottleTabset", selected = "Next Best")
      updateTabsetPanel(session, "pantryTabset", selected = "Next Best")
      inventory$active <- inventory$my.inventory
    } else if(input$cfgPreset == "Optimal Bar") {
      updateCheckboxInput(session, "cfgSubs", value = FALSE)
      updateCheckboxInput(session, "cfgPantry", value = FALSE)
      hide("cfgSubs"); hide("cfgPantry"); hide("editInv"); hide("saveInv");
      hide("uploadInv"); hide("uploadToggle")
      updateTabsetPanel(session, "bottleTabset", selected = "Next Best")
      updateTabsetPanel(session, "pantryTabset", selected = "Most Used")
      inventory$active$in.inventory[inventory$active$ingredient %in%
                                  all.optimal.list[[input$cfgBarSize]]] <- TRUE
    } else {
      updateCheckboxInput(session, "cfgSubs", value = FALSE)
      updateCheckboxInput(session, "cfgPantry", value = TRUE)
      hide("cfgSubs"); hide("cfgPantry"); hide("editInv"); hide("saveInv")
      hide("uploadInv"); hide("uploadToggle")
      updateTabsetPanel(session, "bottleTabset", selected = "Most Used")
      updateTabsetPanel(session, "pantryTabset", selected = "Most Used")
      inventory$active$in.inventory <- TRUE
    }
  })

  ## Update inventory for user selected optimal bar size
  observeEvent(input$cfgBarSize, {
    if(input$cfgPreset == "Optimal Bar") {
      inventory$active$in.inventory <- FALSE
      inventory$active$in.inventory[inventory$active$ingredient %in%
                                  all.optimal.list[[input$cfgBarSize]]] <- TRUE
      }
  })

  ## Toggle the upload inventory dialog
  observeEvent(input$uploadToggle, {
    toggle("uploadInv")
  })

  ## Recipe Matrix: Depends on whether pantry is included
  recipe.mat <- reactive(get_recipe_matrix(ingredient.df,
                                           recipe.ingredient.df,
                                           user.ingredient.groups()))

  ## List of Possible recipes: Depends on Recipe Matrix
  possible.recipe.list <-
  reactive({
    ingredients.in.inventory <-
      inventory$active$ingredient[inventory$active$in.inventory]

    get_drink_possibility(ingredients.in.inventory,
                          recipe.mat(),
                          recipe.ingredient.df,
                          ingredient.df,
                          user.ingredient.groups(),
                          input$cfgSubs)
  })

  ##############################################################################
  ## Tile view tab
  output$tiles <- renderUI({
    if(inventory$first.load){
      js$showInfo()
      inventory$first.load = FALSE
    }

    possible.recipe.links <- possible.recipe.list()$possible.recipe.links

    if(length(possible.recipe.links) == 0)
      return(h2("Inventory has no possible drinks"))

    possible.recipes <-
      filter(recipe.df, recipe.link %in% possible.recipe.links)

    subbed.recipe.ingredient.df <- recipe.ingredient.df

    if(input$cfgSubs){
      subbed.recipe.ingredient.df <-
        subbed.recipe.ingredient.df %>%
        inner_join(ingredient.df, by="ingredient")

      subbed.recipe.ingredient.df$ingredient.as.written <-
        ifelse(subbed.recipe.ingredient.df$ingredient %in%
               inventory$active$ingredient[inventory$active$in.inventory] |
               subbed.recipe.ingredient.df$ingredient.group %in% c("Pantry",
                                                                   "DROP"),
               subbed.recipe.ingredient.df$ingredient.as.written,
               paste0('<span style="font-size: 8pt; color: #666">(',
                      subbed.recipe.ingredient.df$ingredient.as.written,
                      ")</span> ",
                      subbed.recipe.ingredient.df$ingredient.subgroup))
    }

    ingredient.text <-
      subbed.recipe.ingredient.df %>%
      filter(recipe.link %in% possible.recipes$recipe.link) %>%
      group_by(recipe.link) %>%
      summarise(ingredients = paste(unique(paste(ingredient.quantity,
                                                 ingredient.as.written)),
                                    collapse="<br>"))

    ingredient.text <-
      recipe.df %>%
      select(recipe.link, recipe.instructions) %>%
      inner_join(ingredient.text, by="recipe.link")

    ingredient.text$print.recipe <-
      paste(ingredient.text$ingredients,
            ingredient.text$recipe.instructions, sep="<p><hr>")

    fluidRow(
      column(12, id="columns",
        lapply(possible.recipes$recipe.link, function(i) {
          a(box(width=NULL,
            title = HTML(paste0("<div class='image-wrap'><img src='/images/",
                                recipe.df$img[recipe.df$recipe.link == i],
                                "' class='",
                                recipe.df$img.css[recipe.df$recipe.link == i],
                                "'></div>", "<strong>",
                                recipe.df$recipe.name[recipe.df$recipe.link == i],
                                "</strong><br>",
                                '<span style="font-size: 11pt;">By ',
                                recipe.df$recipe.author[recipe.df$recipe.link == i],
                                '</span>'))
            ), href= paste0("http://cooking.nytimes.com", i), target="_blank")
         })
       )
    )
  })

  ##############################################################################
  ## Network tab
  output$networkPlot <- renderPrint({
    possible.recipe.links <- possible.recipe.list()$possible.recipe.links

    if(length(possible.recipe.links) == 0)
      return(h2("Inventory has no possible drinks"))

    possible.recipes <- filter(recipe.df,
                               recipe.link %in% possible.recipe.links)

    possible.pairs <- filter(similar.drinks.df,
                             source %in% possible.recipes$recipe.link,
                             target %in% possible.recipes$recipe.link)

    possible.pairs$source <-
    sapply(possible.pairs$source,
           function(x) which(possible.recipes$recipe.link == x) - 1)

    possible.pairs$target <-
    sapply(possible.pairs$target,
           function(x) which(possible.recipes$recipe.link == x) - 1)

    if(nrow(possible.pairs) == 0)
      possible.pairs <- data.frame(source = 0, target = 0, value = 0)

    subbed.recipe.ingredient.df <- recipe.ingredient.df

    if(input$cfgSubs){
      subbed.recipe.ingredient.df <-
        subbed.recipe.ingredient.df %>%
        inner_join(ingredient.df, by="ingredient")

      subbed.recipe.ingredient.df$ingredient.as.written <-
        ifelse(subbed.recipe.ingredient.df$ingredient %in%
               inventory$active$ingredient[inventory$active$in.inventory] |
               subbed.recipe.ingredient.df$ingredient.group %in% c("Pantry",
                                                                   "DROP"),
               subbed.recipe.ingredient.df$ingredient.as.written,
               paste0("<strike>",
                      subbed.recipe.ingredient.df$ingredient.as.written,
                      "</strike> ",
                      subbed.recipe.ingredient.df$ingredient.subgroup))
    }

    ingredient.text <-
      subbed.recipe.ingredient.df %>%
      filter(recipe.link %in% possible.recipes$recipe.link) %>%
      group_by(recipe.link) %>%
      summarise(ingredients = paste(unique(paste(ingredient.quantity,
                                                 ingredient.as.written)),
                                    collapse="<br>"),
                size=length(unique(ingredient)))

    possible.recipes$order <- 1:nrow(possible.recipes)
    possible.recipes <-
      inner_join(possible.recipes, ingredient.text, by="recipe.link") %>%
      arrange(order)

    possible.recipes$group <- 1
    create_network(Links = as.data.frame(possible.pairs),
                   Nodes = as.data.frame(possible.recipes),
                   Source = "source", Target = "target", Size = "size",
                   Value = "value", NodeID = "recipe.link",
                   Group = "group", Ingredients = "ingredients",
                   width = 800, height = 800, DisplayName = "recipe.name",
                   parentElement = '#networkPlot')
  })

  ##############################################################################
  ## Configure tab
  ## Bottle inventory table for display
  output$bottlesTable <- renderDataTable({
    inventory$active %>%
    filter(in.inventory) %>%
    filter(ingredient.group=="Bottle") %>%
    select(ingredient.subgroup, ingredient) %>%
    arrange(ingredient.subgroup, ingredient)
  }, options = list(pageLength = 10))

  ## Pantry inventory table for display
  output$pantryTable <- renderDataTable({
    inventory$active %>%
    filter(in.inventory) %>%
    filter(ingredient.group=="Pantry") %>%
    select(ingredient.subgroup, ingredient) %>%
    arrange(ingredient.subgroup, ingredient)
  }, options = list(pageLength = 10))

  ## Editable inventory table
  output$inventoryTable <- renderRHandsontable({
    if(new.upload){
      print(new.upload)
      new.upload <<- FALSE
     } else if(!is.null(input$inventoryTable)) {
      inventory$my.inventory <- hot_to_r(input$inventoryTable)
    }

    rhandsontable(inventory$my.inventory, readOnly = TRUE) %>%
      hot_cols(columnSorting = TRUE) %>%
      hot_col("in.inventory", readOnly = FALSE)
  })

  ## Drink stats tile
  output$drinkStats <- renderUI({
    possible.recipe.links <- possible.recipe.list()$possible.recipe.links
    valueBox(length(possible.recipe.links), "Drinks",
             icon = icon("glass"), width = NULL)
  })

  ## Ingredient stats tile
  output$ingredientStats <- renderUI({
    num.ingredients <-
      inventory$active %>%
      filter(in.inventory) %>%
      nrow()

    valueBox(num.ingredients, "Ingredients", icon = icon("shopping-basket"),
      width = NULL)
  })

  ## Most used bottles
  output$mostUsedBottles <- renderText({
    possible.recipe.links <- possible.recipe.list()$possible.recipe.links
    if(length(possible.recipe.links) == 0) return("No possible drinks")

    if(input$cfgSubs) group.var <- "ingredient.subgroup"
    else group.var <- "ingredient"

    ingredient.usage <-
      recipe.ingredient.df %>%
      inner_join(ingredient.df, by="ingredient") %>%
      filter(recipe.link %in% possible.recipe.links,
             ingredient.group == "Bottle") %>%
      group_by_(group.var) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      top_n(5)

    ingredient.usage$prettyprint <-
      paste("<span style='font-weight:strong'>",
            ingredient.usage[[group.var]],
            ": </span>", ingredient.usage$count,
            " drink(s)")

    paste(ingredient.usage$prettyprint, collapse="<br>")
  })

  ## Most used pantry items
  output$mostUsedPantry <- renderText({
    possible.recipe.links <- possible.recipe.list()$possible.recipe.links
    if(length(possible.recipe.links) == 0) return("No possible drinks")

    ingredient.usage <-
      recipe.ingredient.df %>%
      inner_join(ingredient.df, by="ingredient") %>%
      filter(recipe.link %in% possible.recipe.links,
             ingredient.group == "Pantry") %>%
      group_by(ingredient) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      top_n(5)

    ingredient.usage$prettyprint <-
      paste("<span style='font-weight:strong'>",
            ingredient.usage$ingredient,
            ": </span>", ingredient.usage$count,
            " drink(s)")

    paste(ingredient.usage$prettyprint, collapse="<br>")
  })

  ## Next best bottles to purchase
  output$nextBestBottles <- renderText({
    if(input$cfgPreset == "All Ingredients") return("All bottles selected")
    if(!("data.frame" %in% class(possible.recipe.list()$bottle.count)))
      return("No drinks possible with only one additional ingredient.")

    next.best.bottles <- possible.recipe.list()$bottle.count[1:5,]

    next.best.bottles$prettyprint <-
      paste("<span style='font-weight:strong'>",
            next.best.bottles$ingredient,
            ": </span>", next.best.bottles$count,
            " drink(s)")

    paste(next.best.bottles$prettyprint, collapse="<br>")
  })

  ## Next best pantry items to purchase
  output$nextBestPantry <- renderText({
    if(input$cfgPreset == "All Ingredients") return("All pantry items selected")
    if(input$cfgPreset == "Optimal Bar") return("Optimal bar is bottles only")
    if(!input$cfgPantry) return("Include Pantry not selected")
    if(!("data.frame" %in% class(possible.recipe.list()$pantry.count)))
      return("No drinks possible with only one additional ingredient.")

    next.best.pantry <- possible.recipe.list()$pantry.count[1:5,]
    next.best.pantry$prettyprint <-
      paste("<span style='font-weight:strong'>",
            next.best.pantry$ingredient,
            ": </span>", next.best.pantry$count,
            " drink(s)")
      paste(next.best.pantry$prettyprint, collapse="<br>")
  })

  output$saveInv <- downloadHandler(
    filename = function() { "my-inventory.csv"},
    content = function(file) {
      write.csv(inventory$active, file, row.names = FALSE)
    }
  )
}
