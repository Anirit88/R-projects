

library(shiny)

#loading data and filtering the data thats going to be used

co2data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

#US data
filtered_US <- select(co2data, country, year, co2, co2_growth_prct, population, gdp) %>%
    filter(country == "United States")%>%
    arrange(-year)%>%
    subset(year > "1999" )

#relevant values of interest, calculated using your DPLYR skills
#1, which year had the highest emmison for CO2 
USHIghest_co2 <-  max(filtered_US$co2_growth_prct)

#2, which year had the lowest emmison for CO2
USLowest_co2 <-  min(filtered_US$co2)

#3, average amount of co2 emmison over the years
USavg_co2 <- mean(filtered_US$co2_growth_prct)

#4,co2 emmison rate in 2018
USco2_2018 <- filtered_US %>%
    filter(year == "2018")%>%
    select(co2)

#5,co2 emmison rate in 2000
USco2_2000 <- filtered_US %>%
    filter(year == "2000")%>%
    select(co2)

#6, difference in co2 emmison rates form 2000-2018
USco2_diff <- USco2_2000-USco2_2018
USco2_diff_percent <- (USco2_diff/USco2_2000)*100


#PORTUGAL DATA
filtered_Port <- select(co2data, country, year, co2, co2_growth_prct, population, gdp) %>%
    filter(country == "Portugal")%>%
    arrange(-year)%>%
    subset(year > "1999" )

#relevant values of interest, calculated using your DPLYR skills
#1, which year had the highest emmison for CO2 
PORTHIghest_co2 <-  max(filtered_Port$co2)

#2, which year had the lowest emmison for CO2
PORTLowest_co2 <-  min(filtered_Port$co2)

#3, average amount of co2 emmison over the years
PORTavg_co2 <- mean(filtered_Port$co2)

#4,co2 emmison rate in 2018
PORTco2_2018 <- filtered_Port %>%
    filter(year == "2018")%>%
    select(co2)

#5,co2 emmison rate in 2000
PORTco2_2000 <- filtered_Port %>%
    filter(year == "2000")%>%
    select(co2)

#6, difference in co2 emmison rates form 2000-2018
PORTco2_diff <- PORTco2_2000-PORTco2_2018
PORTco2_diff_percent <- (PORTco2_diff/PORTco2_2000)*100

combined <- full_join(filtered_Port, filtered_US)

server <- function(input, output) {
    output$bar <- renderPlotly({
        chart <- ggplot(data = combined) +
            geom_col(mapping = aes(x = year, y = co2, fill = country) )
    })
}

