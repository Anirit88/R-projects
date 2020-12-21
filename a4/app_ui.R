



co2data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

#US data
filtered_US <- select(co2data, country, year, co2, co2_growth_prct, population, gdp) %>%
    filter(country == "United States")%>%
    arrange(-year)%>%
    subset(year > "1999" )
#PORTUGAL DATA
filtered_Port <- select(co2data, country, year, co2, co2_growth_prct, population, gdp) %>%
    filter(country == "Portugal")%>%
    arrange(-year)%>%
    subset(year > "1999" )
combined <- full_join(filtered_Port, filtered_US)

# Introduction Page

intro_page <- tabPanel(
    "Introduction",
    mainPanel(
        h2("CO2 emmison comparisson between US vs Portugal"),
        p("For this assignment i deceided to analyze the co2 emmsion rates amongst America and Portugal.
      The reason i chose these 2  countries is because, currently as we know America is one  of the 
      many countries in the world that has the worst climate situation. As per enviormentolists,
      and the world clock we have approximately 7 hours 102 days left for humanity of something 
      isnt done soon. And as we can see from the data collected, in the  past 20 years America's
      co2 emmison rates have  rised as  high up as to  'r USHIghest_co2' and fallen as low as 
      'r USLowest_co2', with an average emmison percent going up 'r USavg_co2' percentover the years."),
        p("The reason i chose to compare ameriuca to portugal is because accprding to the 
      Global carbon project, portugal is said to have the lowest carbon emmison rates.
      Some might argue that having a lower population plays a huge factor in  this. Although,
      that being true another reason for  so is because of the policies the government  has 
      in place to control this and to prevent being at this position. We can see this as we
      notice the maximum number of co2 emmison in portugal being 'r PORTHIghest_co2' and 
      lowest being 'r PORTLowest_co2', with an average emmison percent going down
      'r PORTavg_co2' percent over the years.")
    ))

category_names <- unique(combined$country)

category_input <- selectInput(
    inputId = "category_input",
    label = "Choose country",
    choices = category_names
)

bar_page <- tabPanel(
    "co2 emmision rates",
    sidebarLayout(
        sidebarPanel(
            category_input
        ),
        mainPanel(
            h2("co2 emmision comparison US vs Portugal"),
            plotlyOutput("bar"),
            p("The purpose of this bar chart  is  to show a comparison amongst  US
              and portugal for their  respective co2 emmsion rates. From this chart we 
              notice  how small of a  fraction portugal comrpireses  of in co2  emmsion 
              rates as compared to US, with US having  almost 100 times more emmsions 
              comparitivly")
        )
    )
)

ui <- fluidPage(
  h1("CO2 emmisions: United States VS Portugal"),
  navbarPage(
    inverse = TRUE,
    "assignment4",
    intro_page,
    bar_page
  )
)
.