
library(plotly)
library(rjson)


# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig <- plot_geo(extended_data, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = ~Percent_byStateParty, locations = ~state,
  color = ~Percent_byStateParty, colors = 'Purples'
)
fig <- fig %>% colorbar(title = "Millions USD")
fig <- fig %>% layout(
  title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
  geo = g
)

fig

