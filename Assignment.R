# Assignment 6
# This is just a R file made to test my map functionality



# setwd("C:/Users/Soichi/Documents/info201/a6-mapping-shootings-Soichit")
# install.packages("plotly")
# install.packages("dplyr")

library(plotly)
library(dplyr)
shootings.df <- read.csv("data/Mass-Shooting-Data.csv")
# View(shootings.df)

# Summary Information
# states affected/ unaffected
# states with more than 5 incidents

num.shootings <- nrow(shootings.df)
num.injured <- sum(shootings.df$injured)
num.killed <- sum(shootings.df$killed)

# grouped by states
state.shootings <- shootings.df %>%
  group_by(state) %>%
  summarise( num_total = n(),
             injured_total = sum(injured),
             killed_total = sum(killed))
# adding state abbreviations column
abr <- state.abb[match(state.shootings$state,state.name)]
state.shootings <- mutate(state.shootings, abr)

#View(state.shootings)

# finding states that weren't affected by shootings
states.affected <- state.shootings$state
data("state")
setdiff(state.name, states.affected)

# finding states with at least 10 incidents
states.high.incidents <- state.shootings %>% filter(num_total > 10)





# Description of a particular incident
worst.shooting <- filter(shootings.df, killed == max(killed))
new.shootings.df <- filter(shootings.df, killed != max(killed))
select(worst.shooting, city, killed)

worst.shooting$date
worst.shooting$city
worst.shooting$state
worst.shooting$injured
worst.shooting$killed




# An Interactive Map
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

shootings.map <- plot_geo(new.shootings.df, lat = ~lat, lon = ~lng, colors=c("red","black")) %>%
  add_markers(
    text = ~paste(date, city, state, paste("Injuries:", injured), paste("Deaths:", killed), sep = "<br />"),
    color = ~injured,
    size = I(8),
    hoverinfo = "text"
  ) %>%
  colorbar(title = "# of Injuries") %>%
  layout(
    title = 'Mass Shootings in the United States 2016', 
    geo = g
  )


# An Interactive Map 2





# An Interactive Plot
#plot
bar.states <- state.shootings$state
bar.states.abr <- state.abb[match(bar.states,state.name)]
bar.injured <- state.shootings$injured_total
bar.killed <- state.shootings$killed_total
df <- data.frame(bar.states, bar.injured, bar.killed)


plot_ly(df, x = ~bar.states, y = ~bar.injured, type = 'bar', name = 'injured') %>%
  add_trace(y = ~bar.killed, name = 'killed') %>%
  layout(xaxis = list(title = 'States'),
         yaxis = list(title = 'Number of Injuries/Deaths'), barmode = 'group')


# An Interactive Plot 2
# state.shootings
#df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")

View(state.shootings)



state.shootings$hover <- with(state.shootings, paste(state, '<br>', "Incidents:", num_total,
                             '<br>', "Injuries:", injured_total, "<br>", "Deaths:", killed_total))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)

# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_geo(state.shootings, locationmode = 'USA-states',  colors=c("white","red")) %>%
  add_trace(
    z = ~num_total,
    text = ~hover,
    locations = ~abr,
    color = ~num_total
  ) %>%
  colorbar(title = "Number of incidents") %>%
  layout(
    title = 'Number of Shooting Incidents by State',
    geo = g
  )




