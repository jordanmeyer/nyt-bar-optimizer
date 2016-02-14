## About this app
The NYT Bar Optimizer limits cocktail recipes pulled from <a href="http://cooking.nytimes.com/" target="_blank">The New York Times Cooking Website</a> based on ingredients from your bar's inventory.
It recommends additional ingredient purchases that unlock the largest number of new drinks.

It is built using <a href="http://www.r-project.org/" target="_blank">R</a> and <a href="http://shiny.rstudio.com/" target="_blank">Shiny</a>. The <a href="http://github.com/jordanmeyer/nyt-bar-optimizer" target="_blank">code</a> is available on github.

### Configure
The optimizer starts with the **All Ingredients** bar preset, which causes the app to show the full set of NYT cocktail recipes.

The **Optimal Bar** bar preset shows n-ingredient bars that enable the most drink recipes for the number of ingredients.

To use the **My Inventory** bar preset for the first time, enter the liquors and other ingredients that you have on hand to see what drinks you can make. The Bar Stats panel on the right recommends additional ingredients that let you create the largest number of new drinks.
Enabling **Include Pantry** will only show recipes if you have the pantry items.
Enabling **Allow Substitutions** will show recipes if you have any ingredient in the *ingredient.subgroup*.
You can save your bar as a csv file and upload it for future use.

### Network View
The network view shows drink recipes as nodes linked by shared bar ingredients. The sizes of the nodes correspond to the number of ingredients needed to make the drink.
Try hovering over the nodes to see information about the drinks. Drag and zoom to navigate the network.
