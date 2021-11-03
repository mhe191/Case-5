# Henter data
library(data.table)
kpi_csv <- fread("https://data.ssb.no/api/v0/dataset/1086.csv?lang=no")
kpi_csv

library(rjstat)
url <- "https://data.ssb.no/api/v0/dataset/1086.json?lang=no"
kpi_json <- fromJSONstat(url)
str(kpi_json)

# trekker ut informajoen fra listen 
tabell <- kpi_json[[1]]
str(tabell)

head(tabell)

# velger ut bestemte rader
suppressPackageStartupMessages(library(tidyverse))

tabell %>% 
  group_by(statistikkvariabel) %>% 
  summarise(n=n())

# trekker ut rader om kpi
kpi <- 
  tabell %>% 
  filter(statistikkvariabel=="Konsumprisindeks (2015=100)") %>% 
  as_tibble()

kpi

# formatere måned som dato
library(lubridate)

kpi <- 
  kpi %>% 
  separate(måned, into=c("år", "måned"), sep = "M") %>% 
  mutate(dato=ymd(paste(år, måned, "1"))) %>% 
  select(dato, konsumgruppe, statistikkvariabel, value)
head(kpi)

# lager plot
kpi %>% 
  ggplot(aes(x=dato, y=value)) +
  geom_line(col="dark blue") +
  labs(title="Konsumprisindeks - KPI \n Totalindeks (2015=100)",
       x =" ",
       y = "Totalindeks") +
  theme_bw()

# se på tallene til 2015
kpi %>% 
  filter(dato >= "2015-01-01" & dato <= "2015-12-01")

kpi %>% 
  mutate(year=year(dato)) %>% 
  filter(year==2015) %>% 
  summarise(mean(value))

# endre basis år til 2010
kpi %>% 
  mutate(year=year(dato)) %>% 
  filter(year==2010) %>%
  summarise(mean(value))

b2010 <- kpi %>%
  mutate(year=year(dato)) %>% 
  filter(year==2010) %>%
  summarise(ny_basis_2010=mean(value))

kpi <- 
  kpi %>%
  mutate(KPI_2010=100*value/b2010$ny_basis_2010)

# lager plot med begge indeksene 
kpi %>% 
  rename(KPI_2015=value) %>% 
  select(dato, KPI_2010, KPI_2015) %>% 
  pivot_longer(-dato,
               names_to = "KPI",
               values_to = "indeks") %>% 
  ggplot(aes(x=dato, y=indeks, col=KPI)) +
  geom_line() +
  labs(title="Konsumprisindeks - KPI",
       x =" ",
       y = "Totalindeks") +
  theme_bw()

# Oppgave 1
kpi %>% 
  mutate(year=year(dato)) %>% 
  filter(year==2019) %>% 
  summarise(mean(value))
 

bn2019 <- kpi %>%
  mutate(year=year(dato)) %>% 
  filter(year==2019) %>%
  summarise(ny_basis_n2019=mean(value))

kpi <- 
  kpi %>%
  mutate(KPI_n2019=100*value/bn2019$ny_basis_n2019)
head(kpi)

kpi %>% 
  rename(KPI_2015=value) %>%
  select(dato, KPI_2010, KPI_2015, KPI_n2019) %>% 
  pivot_longer(-dato,
               names_to = "KPI",
               values_to = "indeks") %>% 
  ggplot(aes(x=dato, y=indeks, col=KPI)) +
  geom_line() +
  labs(title="Konsumprisindeks - KPI",
       x =" ",
       y = "Totalindeks") +
  theme_bw()


# Oppgave 2

Pris2020 <- print(mean(3519.72, 3520.36, 3534.44, 3821.36, 3832.89, 3944.75))
Pris2018 <- print(mean(3315.45, 3326.76, 3259.30, 3731.57, 3597.78, 3666.59))
Prosend <- print((Pris2020 - Pris2018) / Pris2018 * 100)

# Oppgave 3
url <- "https://data.ssb.no/api/v0/dataset/1094.json?lang=no"
kpi_json2 <- fromJSONstat(url)
str(kpi_json2)

tabell2 <- kpi_json2[[1]]
str(tabell)

Priser2020 <- tabell2 %>% 
  filter(konsumgruppe == "Leskedrikker", måned=="2020M09") %>% 
  print

Priser2018 <- tabell2 %>% 
  filter(konsumgruppe == "Leskedrikker", måned=="2018M10") %>%
  print


Priser2020 <- Priser2020[1, 4] %>%
  print

Priser2018 <- Priser2018[1, 4] %>%
  print()
# Fra tabell
Prosend <-
  100*((Priser2020-Priser2018)/Priser2018) %>% 
  print()

# Fra VG
Pris2018_2 <- print(mean(338.10,	338.10,	338.77,	359.40,	378.20,	360.80))
Pris2020_2 <- print(mean(342.55,	342.60,	342.57,	355.85,	364.75,	372.13))

Prosend2 <-
  100*((Pris2020_2-Pris2018_2)/Pris2018_2) %>% 
  print()

###

tabell2 <-
  tabell %>%
  filter(statistikkvariabel != "12-måneders endring (prosent)") %>% 
  separate(måned, into = c("år", "måned"), sep="M") %>% 
  mutate(dato = ymd(paste(år, måned, "1"))) %>% 
  select(dato, statistikkvariabel, value) %>% 
  pivot_wider(names_from = "statistikkvariabel") %>% 
  rename(KPI = "Konsumprisindeks (2015=100)",
         SSB_dp ="Månedsendring (prosent)") %>% 
  mutate(dp  = 100*(KPI - lag(KPI))/lag(KPI),
         lndp.v1 = 100*(log(KPI) - log(lag(KPI))),
         lndp.v2 = c(NA, 100*diff(log(KPI))))

head(tabell2)

tabell2 %>%
  filter(dato >= "1979-02-01") %>%
  select(dato, lndp.v2) %>% 
  mutate(kumulativKPI=cumsum(lndp.v2)) %>% 
  ggplot(aes(x=dato, y=kumulativKPI)) +
  geom_line(col="dark green") +
  labs(title="Kumulativ endring i konsumprisindeksen \n Totalindeks (2015=100)",
       x = " ",
       y = "Prosent") +
  theme_bw()

# Oppgave 4
tabell2 %>% 
  count(dp < 0)

# Prosent 0 <
100*(417/(417+95))

# Prosent 0 >
100*(95/(417+95))

tabell2 %>%
  filter(dato >= "1979-02-01") %>%
  select(dato, lndp.v2) %>% 
  mutate(kumulativKPI=cumsum(lndp.v2)) %>% 
  ggplot(aes(x=dato, y=kumulativKPI)) +
  geom_line(col="dark green") +
  labs(title="Kumulativ endring i konsumprisindeksen \n Totalindeks (2015=100)",
       x = " ",
       y = "Prosent") +
  theme_bw()

  

# Oppgave 5
tabell3 <-
  tabell2 %>%
  filter(dato >= "1979-02-01") %>%
  select(dato, lndp.v2) %>% 
  mutate(kumulativKPI=cumsum(lndp.v2)) %>% 
  print()
  
# 50
tabell3 %>% 
  count(kumulativKPI <= 50)
# 100
tabell3 %>% 
  count(kumulativKPI <= 100)
# 150
tabell3 %>% 
  count(kumulativKPI <= 150)  
  
  

