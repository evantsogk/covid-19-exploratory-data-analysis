library(data.table)
library(ggplot2)
library(lubridate)
library(dplyr)
library(DT)

# --------------------- INITIAL TASKS --------------------- 

# read data
confirmed_raw <- fread("time_series_covid19_confirmed_global.csv")
deaths_raw <- fread("time_series_covid19_deaths_global.csv")

# remove columns
dt_confirmed <- confirmed_raw[, -c("Province/State", "Lat", "Long")]
dt_deaths <- deaths_raw[, -c("Province/State", "Lat", "Long")]

# convert data from wide to long format
dt_confirmed <- melt(dt_confirmed)
dt_deaths <- melt(dt_deaths)

# rename variables
setnames(dt_confirmed, c("country", "date", "confirmed"))
setnames(dt_deaths, c("country", "date", "deaths"))

# convert date from character to a date object  
dt_confirmed$date <- mdy(dt_confirmed$date)
dt_deaths$date <- mdy(dt_deaths$date)

# group counts by country and date 
dt_confirmed <- dt_confirmed[, .(confirmed=sum(confirmed)), by = .(country, date)]
dt_deaths <- dt_deaths[, .(deaths=sum(deaths)), by = .(country, date)]

# merge the two data-sets
covid19_data <- merge(dt_confirmed, dt_deaths, by=c("country", "date"))

# Calculate counts for the whole world
last_date <- tail(covid19_data$date, 1)
total_confirmed <- covid19_data[date==last_date, sum(confirmed)]
total_deaths <- covid19_data[date==last_date, sum(deaths)]

# create variables confirmed.ind and deaths.ind with daily cases
covid19_data <- cbind(covid19_data, 
                      covid19_data[, .(confirmed.ind = confirmed - c(0, lag(confirmed)[-1]),
                                       deaths.ind = deaths - c(0, lag(deaths)[-1])), by=country]
                      [, c("confirmed.ind", "deaths.ind")])

# --------------------- WORLD WIDE --------------------- 

# line plot of cumulative confirmed cases in linear scale
world_confirmed <- covid19_data[, .(confirmed=sum(confirmed)), by=date]
# axis labels and breaks
x_brks <- world_confirmed$date[seq(1, length(world_confirmed$date), 30)]
x_lbls <- paste(month.abb[month(x_brks)], lubridate::day(x_brks))
y_brks <- c(seq(0, 8e+7, 2e+7))
y_lbls <- c(0, paste(y_brks[-1] / 1e+6, 'M', sep=''))
# plot
ggplot(world_confirmed, aes(x = date, y=confirmed)) +
  geom_line(size=3, color="#636efa") +
  labs(title="Total confirmed cases over time", subtitle="(Linear Scale)", 
       x="Date", y="Total confirmed cases") +
  scale_x_date(labels = x_lbls, breaks = x_brks) +
  scale_y_continuous(labels = y_lbls, breaks = y_brks) +
  theme(axis.text.x = element_text(angle = 50, vjust=0.5), 
        panel.grid.minor = element_blank())


# line plot of cumulative confirmed cases in logarithmic scale
y_brks <- c(1e+3, 1e+4, 1e+5, 1e+6,  1e+7, 1e+8)
y_lbls <- c("1K", "10K", "100K", "1M", "10M", "100M")

ggplot(world_confirmed, aes(x = date, y=confirmed)) +
  geom_line(size=3, color="#636efa") +
  labs(title="Total confirmed cases over time", subtitle="(Logarithmic Scale)", 
       x="Date", y="Total confirmed cases") +
  scale_x_date(labels = x_lbls, breaks = x_brks) +
  scale_y_log10(labels = y_lbls, breaks = y_brks) +
  theme(axis.text.x = element_text(angle = 50, vjust=0.5), 
        panel.grid.minor = element_blank())


# line plot of cumulative deaths in linear scale
world_deaths <- covid19_data[, .(deaths=sum(deaths)), by=date]
y_brks <- c(0, 5e+5, 1e+6, 1.5e+6)
y_lbls <- c('0', '500K', '1M', '1.5M')

ggplot(world_deaths, aes(x = date, y=deaths)) +
  geom_line(size=3, color="#D63230") +
  labs(title="Total deaths over time", subtitle="(Linear Scale)", 
       x="Date", y="Total deaths") +
  scale_x_date(labels = x_lbls, breaks = x_brks) +
  scale_y_continuous(labels = y_lbls, breaks = y_brks) +
  theme(axis.text.x = element_text(angle = 50, vjust=0.5), 
        panel.grid.minor = element_blank())

# bar plot of daily cases
world_daily_confirmed <- covid19_data[, .(confirmed.ind=sum(confirmed.ind)), by=date]

y_brks <- c(0, 5e+5, 1e+6, 1.5e+6)
y_lbls <- c('0', '500K', '1M', '1.5M')
ggplot(world_daily_confirmed, aes(x = date, y=confirmed.ind)) +
  geom_bar(stat="identity", width=1, fill="#636efa") +
  labs(title="Daily new cases", x="Date", y="Number of cases") +
  scale_x_date(labels = x_lbls, breaks = x_brks) +
  scale_y_continuous(labels = y_lbls, breaks = y_brks) +
  theme(axis.text.x = element_text(angle = 50, vjust=0.5), 
        panel.grid.minor = element_blank())


# bar plot of daily deaths
world_daily_deaths <- covid19_data[, .(deaths.ind=sum(deaths.ind)), by=date]

y_brks <- c(0, 5e+3, 10e+3, 15e+3)
y_lbls <- c('0', '5K', '10K', '15K')
ggplot(world_daily_deaths, aes(x = date, y=deaths.ind)) +
  geom_bar(stat="identity", width=1, fill="#D63230") +
  labs(title="Daily deaths", x="Date", y="Number of deaths") +
  scale_x_date(labels = x_lbls, breaks = x_brks) +
  scale_y_continuous(labels = y_lbls, breaks = y_brks) +
  theme(axis.text.x = element_text(angle = 50, vjust=0.5), 
        panel.grid.minor = element_blank())


# --------------------- COUNTRIES--------------------- 

# bar plot of top 15 countries with most confirmed cases
most_confirmed <- covid19_data[, .(confirmed=sum(confirmed.ind)), by=country][order(-confirmed)][1:15]

ggplot(most_confirmed, aes(x=confirmed, y=country)) +
  geom_bar(stat="identity", width=0.8, fill="#636efa") +
  labs(title="Top 15 countries with most confirmed cases", x="Total confirmed cases", y="Country") +
  scale_x_continuous(labels = c('0', '5M', '10M', '15M', '20M'), breaks = c(0, 5e+6, 1e+7, 1.5e+7, 2e+7), 
                     limits = c(0, 2e+7)) +
  scale_y_discrete(limits = rev(most_confirmed$country)) +
  geom_text(aes(label = format(confirmed, big.mark=","), hjust = - 0.1), color = "black") +
  theme(panel.grid.minor = element_blank())


# bar plot of top 15 countries with most deaths
most_deaths <- covid19_data[, .(deaths=sum(deaths.ind)), by=country][order(-deaths)][1:15]
x_brks <- c(seq(0, 4e+5, 1e+5))
x_lbls <- c(0, paste(x_brks[-1] / 1e+3, 'K', sep=''))

ggplot(most_deaths, aes(x=deaths, y=country)) +
  geom_bar(stat="identity", width=0.8, fill="#D63230") +
  labs(title="Top 15 countries with most deaths", x="Total deaths", y="Country") +
  scale_x_continuous(labels = x_lbls, breaks =x_brks, limits = c(0, 4e+5)) +
  scale_y_discrete(limits = rev(most_deaths$country)) +
  geom_text(aes(label = format(deaths, big.mark=","), hjust = - 0.1), color = "black") +
  theme(panel.grid.minor = element_blank())


# mortality rate
mortality_rate <- covid19_data[date==last_date, .(country, confirmed, deaths, 
                  mortality_rate=round(deaths/confirmed*100, 2))]

# top 10 countries with highest mortality rate 
# (only countries with more than 1000 confirmed cases considered)
highest_mortality <- head(mortality_rate[confirmed>1000][order(-mortality_rate)], 10)

# format style
brks <- quantile(highest_mortality$mortality_rate, probs = seq(.05, .95, .05))
clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}

datatable(highest_mortality, rownames = FALSE) %>% 
            formatStyle('mortality_rate', backgroundColor = styleInterval(brks, clrs))

# top 10 countries with lowest mortality rate
# (only countries with more than 1000 confirmed cases considered)
lowest_mortality <- head(mortality_rate[confirmed>1000][order(mortality_rate)], 10)

# format style
brks <- quantile(lowest_mortality$mortality_rate, probs = seq(.05, .95, .05))
clrs <- round(seq(40, 255, length.out = length(brks) + 1), 0) %>%
  {paste0("rgb(", ., ",255,", ., ")")}

datatable(lowest_mortality, rownames = FALSE) %>% 
  formatStyle('mortality_rate', backgroundColor = styleInterval(brks, clrs))


# --------------------- GREECE --------------------- 

# line plot of cumulative confirmed cases in Greece
greece_confirmed <- covid19_data[country=='Greece', .(date, confirmed)]

# total confirmed
tail(greece_confirmed$confirmed, 1)

# axis labels and breaks
x_brks <- greece_confirmed$date[seq(1, length(greece_confirmed$date), 15)]
x_lbls <- paste(month.abb[month(x_brks)], lubridate::day(x_brks))
y_brks <- c(seq(0, 150e+3, 25e+3))
y_lbls <- c(0, paste(y_brks[-1] / 1e+3, 'K', sep=''))
# plot
ggplot(greece_confirmed, aes(x = date, y=confirmed)) +
  geom_line(size=3, color="#636efa") +
  labs(title="Total confirmed cases in Greece over time", 
       x="Date", y="Total confirmed cases") +
  scale_x_date(labels = x_lbls, breaks = x_brks) +
  scale_y_continuous(limits = c(0, 150e+3), labels = y_lbls, breaks = y_brks) +
  theme(axis.text.x = element_text(angle = 60, vjust=0.5), 
        panel.grid.minor = element_blank())


# line plot of cumulative deaths in Greece
greece_deaths <- covid19_data[country=='Greece', .(date, deaths)]

# total deaths
tail(greece_deaths$deaths, 1)

# axis labels and breaks
x_brks <- greece_deaths$date[seq(1, length(greece_deaths$date), 15)]
x_lbls <- paste(month.abb[month(x_brks)], lubridate::day(x_brks))
y_brks <- c(seq(0, 5e+3, 1e+3))
y_lbls <- c(0, paste(y_brks[-1] / 1e+3, 'K', sep=''))
# plot
ggplot(greece_deaths, aes(x = date, y=deaths)) +
  geom_line(size=3, color="#D63230") +
  labs(title="Total deaths in Greece over time", 
       x="Date", y="Total deaths") +
  scale_x_date(labels = x_lbls, breaks = x_brks) +
  scale_y_continuous(limits = c(0, 5e+3), labels = y_lbls, breaks = y_brks) +
  theme(axis.text.x = element_text(angle = 60, vjust=0.5), 
        panel.grid.minor = element_blank())


# bar plot of daily cases in Greece
greece_daily_confirmed <- covid19_data[country=='Greece', .(date, confirmed.ind)]

y_brks <- c(seq(0, 4e+3, 1e+3))
y_lbls <- c(0, paste(y_brks[-1] / 1e+3, 'K', sep=''))
ggplot(greece_daily_confirmed, aes(x = date, y=confirmed.ind)) +
  geom_bar(stat="identity", width=1, fill="#636efa") +
  labs(title="Daily new cases in Greece", x="Date", y="Number of cases") +
  scale_x_date(labels = x_lbls, breaks = x_brks) +
  scale_y_continuous(limits = c(0, 4e+3), labels = y_lbls, breaks = y_brks) +
  theme(axis.text.x = element_text(angle = 60, vjust=0.5), 
        panel.grid.minor = element_blank())


# bar plot of daily deaths in Greece
greece_daily_deaths <- covid19_data[country=='Greece', .(date, deaths.ind)]

ggplot(greece_daily_deaths, aes(x = date, y=deaths.ind)) +
  geom_bar(stat="identity", width=1, fill="#D63230") +
  labs(title="Daily deaths in Greece", x="Date", y="Number of deaths") +
  scale_x_date(labels = x_lbls, breaks = x_brks) +
  theme(axis.text.x = element_text(angle = 60, vjust=0.5), 
        panel.grid.minor = element_blank())


# Greece mortality rate
mortality_rate[country=='Greece', mortality_rate]

# World wide mortality rate
round(total_deaths / total_confirmed * 100, 2)



