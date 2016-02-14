dashboardPage(
  skin = "black",

  dashboardHeader(
    title = "NYT Bar Optimizer"
  ),

  dashboardSidebar(
    includeCSS("www/styles.css"),
    includeScript("www/d3.v3.min.js"),
    includeScript("www/d3-tip.js"),
    useShinyjs(),
    extendShinyjs(text = showModal),
    sidebarMenu(
      menuItem("Recipe View", tabName = "recipeView",
        icon = icon("glass", lib="font-awesome")
      ),
      menuItem("Network View", tabName = "networkView",
        icon = icon("share-alt", lib="font-awesome")
      ),
      menuItem("Configure", tabName = "configure",
        icon = icon("gear", lib="font-awesome")
      ),
      menuItem("About", tabName = "about",
        icon = icon("info-circle", lib="font-awesome")
      )
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "recipeView",
        uiOutput("tiles"),
        bsModal("aboutModal", h3(" "), "", size = "large",
                includeMarkdown("www/about.md")
        )
      ),

      tabItem(tabName = "networkView",
        fluidRow(
          column(12,
            htmlOutput('networkPlot')
          )
        )
      ),

      tabItem(tabName = "configure",
        fluidRow(
          column(width = 9,
            box(width = NULL,
              fluidRow(
                column(width = 5,
                  selectInput("cfgPreset", "Bar Presets ",
                              choices = c("My Bar", "Optimal Bar",
                                          "All Ingredients"),
                              selected = "All Ingredients"),
                  conditionalPanel("input.cfgPreset == 'Optimal Bar'",
                    numericInput("cfgBarSize",
                                 "Bar Size (Up to 40 Bottles)",
                                 value = 5, min = 1, max = 40)
                  )
                ),
                column(width = 4,
                  checkboxInput("cfgSubs", "Allow Substitutions", value=FALSE),
                  checkboxInput( "cfgPantry", "Include Pantry", value=TRUE)
                ),
                column(width = 3,
                       actionLink("editInv", label="Edit Bar"),
                       HTML("&nbsp;&nbsp;"),
                       downloadLink("saveInv", label="Save Bar"),
                       HTML("&nbsp;&nbsp;"),
                       actionLink("uploadToggle", label="Upload Bar"),
                       fileInput("uploadInv", NULL,
                                 accept=c('text/csv',
                                          'text/comma-separated-values,text/plain',
                                          '.csv'), width = "100%")
                )
              ),
              tabsetPanel(
                tabPanel("Bar", h2(''), dataTableOutput("bottlesTable")),
                tabPanel("Pantry", h2(''), dataTableOutput("pantryTable"))
              ),
              bsModal("invModal", "Edit Inventory - Click headers to sort",
                      "editInv", size = "large",
                rHandsontableOutput("inventoryTable")
              )
            )
          ),
          column(width = 3,
            uiOutput("drinkStats"),
            uiOutput("ingredientStats"),
            box(width = NULL, title = "Bar Stats",
              tabsetPanel(id="bottleTabset",
                tabPanel("Next Best", h4(''), htmlOutput("nextBestBottles")),
                tabPanel("Most Used", h4(''), htmlOutput("mostUsedBottles"))
              )
            ),
            box(width = NULL, title = "Pantry Stats",
              tabsetPanel(id="pantryTabset",
                tabPanel("Next Best", h4(''), htmlOutput("nextBestPantry")),
                tabPanel("Most Used", h4(''), htmlOutput("mostUsedPantry"))
              )
            )
          )
        )
      ),

      tabItem(tabName = "about",
        box(width = 12,
            includeMarkdown("www/about.md")
        )
      )

    ) ## end tabItems
  ) ## end dashboardBody
) ## end dashboardPage
