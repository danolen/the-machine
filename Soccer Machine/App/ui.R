navbarPage(
  title = "Shiny Application",
  theme = bslib::bs_theme(4),
  tabPanel(
    title = "Bets",
    value = "tab_vf8gukmk2z",
    textOutput(
      outputId = "Suggested Bets"
    ),
    dataTableOutput(
      outputId = "Suggested Bets"
    )
  ),
  tabPanel(
    title = "Performance",
    value = "tab_1b7co4lyda",
    dataTableOutput(
      outputId = "Bet Types Table"
    )
  )
)