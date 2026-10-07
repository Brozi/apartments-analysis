#===============================================
#Biblioteki (do pobrania jeśli nie masz)
#===============================================
install.packages("nortest")
install.packages("ggspatial")
install.packages("sf")
install.packages("leaflet")
install.packages("viridis")
install.packages("dummy")
install.packages("fastDummies")
install.packages("openxlsx")
install.packages("e1071")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("car")
install.packages("moments")
#===============================================
#Biblioteki
#===============================================
library(tidyverse)
library(openxlsx)
library(nortest)
library(e1071)
library(cluster)
library(factoextra)
library(ggspatial)
library(sf)
library(leaflet)
library(viridis)
library(ggcorrplot)
library(dummy)
library(car)
library(moments)
library(fastDummies)
#===============================================
#Baza
#===============================================
baza <- read.xlsx("C:/WAD/Dane/apartments.xlsx")

#===============================================
#Opis bazy
#===============================================
#Baza zawiera dane wszystkich ofert mieszkań na sprzedaż w Krakowie z otodom.pl 
#z dnia 04.01.2026. Zebrane dane i ich opis:
#id - unikalny identyfikator oferty
#source - link do oferty
#city - stała wartość "Cracow"
#district - nazwa dzielnicy (18 różnych dzielnic)
#latitude - szerokość geograficzna
#longitude - szerokość geograficzna
#total_price - całkowita cena mieszkania (zł)
#area - powierzchnia w m^2
#rooms - ilość pokoi
#floor - piętro
#total_floors - łączna liczba pięter bloku
#finishing_state - stan wykończenia. Jedna z 3 wartości (ready_to_use, to_completion, to_renovate)
#market_type - Rynek piertowny bądź wtórny. Jedna z 2 wartości (primary, secondary)
#advertiser_type - Rodzaj sprzedawcy. Jenda z 3 wartości (developer, agency, private)
#build_year - rok budowy
#has_elevator - czy jest winda. Wartości true or false
#heating_type - rodzaj ogrzewania
#avaiable_from - data od kiedy mieszkanie jest dostępne
#created_at - Data zapisania oferty do bazy

#Dane dodane w trakcie analizy:
#price_per_m - cena za m^2
#log_price - zlogarytmowana cena całkowita
#log_price_per_m - zlogarytmowana cena za metr kwadratowy
#log_area - zlogarytmowana powierzchnia

#===============================================
#Wstępna eksploracja
#===============================================

#1. Sprawdzenie typów danych
typy_danych <- data.frame(
  kolumna = names(baza),
  typ = sapply(baza, class)
)

print(typy_danych)

#Ad. 1. wszystkie dane poza has_elevator (logical) mają typ character

#2. Decyzja odnośnie dostosowania typów w bazie w celu umożliwienia dalszej analizy
# numeric -> latidute, longitude, total_price, area, rooms, floor, total_floors
# build_year, has elevator
# factor -> district, finishing_state, market_type, advertiser_type, heating_type
baza <- baza %>%
  mutate(across(
    c(latitude, longitude, total_price, area, rooms, floor, total_floors, build_year), 
    as.numeric
  )) %>%
  mutate(across(
    c(district, finishing_state, market_type, advertiser_type, heating_type),
    as.factor
  ))

#3. Czyszczenie bazy ze zbędnych danych
#id - wystraczy zostawić source które też jest unikalne i pozwala podejrzeć ofertę
#city - stała wartość "Cracow" <=> nic nie wnosi
#available_from - dużo braków i dziwnych wartości dla niektórych mieszkań jak
#dostępność od 2024 roku

baza <- baza %>%
  select(-id, -city, -avaiable_from, -created_at)

#4. Dodanie nowych zmiennych
#price_per_m, log_price, log_price_m
baza <- baza %>%
  mutate(price_per_m = total_price / area) %>% 
  mutate(log_price = log(total_price)) %>% 
  mutate(log_price_m = log(price_per_m)) %>%
  mutate(log_area = log(area))

#5. Analiza zmiennej total_price i log_price
summary(baza$total_price)
#Min.     1st Qu.  Median   Mean    3rd Qu. Max. 
#160000   670000   819000   954493  1030000 16500000
#Mediana dużo mniejsza od średniej <=> problem z wartościami odstającymi
#3 kwartyl ma wartość 1mln, a wartość maksymalna ponad 16mln
#To zaburza średnią, ale podejrzyjmy jeszcze histogram

summary(baza$log_price)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#11.98   13.42   13.62   13.67   13.85   16.62

hist(baza$total_price,
     breaks = 30,
     main = "Histogram cen mieszkań",
     xlab = "Cena")
#Oferty powyżej 3 milionów to pojedyńcze oferty. Można je odfiltrować i przeprowadzać
#dalszą analizę dla mieszkań z ceną do 3mln
hist(baza$total_price[baza$total_price <= 3000000],
     breaks = 30,
     main = "Histogram cen mieszkań do 3mln zł",
     xlab = "")
mtext(
  paste("Cena"),
  side = 1, line = 2, cex = 1.2
)
mtext(
  paste("Skosność:", round(skewness(baza$total_price), 4)),
  side = 1, line = 3, cex = 0.8
)
mtext(
  paste("Kurtoza:", round(kurtosis(baza$total_price), 4)),
  side = 1, line = 4, cex = 0.8
)
#Wyraźna prawoskośność. Większość mieszkań w cenach 0.5mln - 1mln
#brak rozkładu normalnego => problem z wykonaniem testów parametrycznych

hist(baza$log_price,
     breaks = 30,
     main = "Histogram zlogarytmowanych cen mieszkań",
     xlab = "Zlogarytmowana cena")
#Wygląda to lepiej. Zwłaszcza, że nie odfiltrowaliśmy jeszcze outlinerów.
#Dalej jesteśmy daleko od rozkładu normalnego, ale wykres daje przestrzeń
#do podjęcia walki

#Przeprowadzimy jeszcze testy do ostatecznego potwierdzenia

#shapriro.test jest ograniczony do 5000 argumetnów więc trzeba posłużyć się innym

#Test Kolmogorowa-Smirnowa
ks.test(baza$total_price, "pnorm")
#Problem z wybranym testem - nie jest odporny na powtarzające się wartości

#Test Lillieforsa
#dla total_price
lillie.test(baza$total_price)
#D = 0.20929, p-value < 2.2e-16

#dla log_price
lillie.test(baza$log_price)
#D = 0.088214, p-value < 2.2e-16
#p value w obu przypadkach znacząco mniejsze od 0.05. Nawet nie jesteśmy blisko
#rozkładu normalnego.

#Sprawdzenie skośności
skewness(baza$total_price)
#[1] 8.579692
#Wynik potwierdza to co pokazał histogram. silna skośność dodatnia (prawostronna)
#Co oznacza większą ilość mieszkań w niższej, standardowej cenie
skewness(baza$log_price)
#[1] 1.191421
#Skośność w dalszym ciągu jest silna, ale widać duży spadek w porównaniu
#skośnośći dla niezlogarytmowanej ceny

#6. Analiza zmiennej area (zlogarytmowane wartości dopiero później po odfiltrowaniu
#bazy)
summary(baza$area)
#Oj... 3 kwartyl to 66m^2, a max to 1415 m^2. Zdecydowanie jest to kolejna zamienna
#która wymaga odfiltrowania ze skrajnych wartości po skończonym etapie analizy
#pojedyńczych zmiennych. Co ciekawe najmniejsze mieszkanie ma powierzchnię
#12m^2 <=> patologia (moim zdaniem. Niekórzy mają większą cele. Mam nadzieje, że
#to biuro przypadkiem wrzucone do kategorii mieszkań)

hist(baza$area,
     breaks = 30,
     main = "Histogram powierzchni mieszkań",
     xlab = "powierzchnia")
#ciężkie do interetacji. Powyżej około 250m^2 znikoma ilość ofert

hist(baza$area[baza$area <= 250],
     breaks = 30,
     main = "Histogram powierzchni mieszkań do 250m^2",
     xlab = "")

#Nawet powyżej 150m^2 znikoma ilość ofert
hist(baza$area[baza$area <= 150],
     breaks = 30,
     main = "Histogram powierzchni mieszkań do 150m^2",
     xlab = "")
mtext(
  paste("Cena"),
  side = 1, line = 2, cex = 1.2
)
mtext(
  paste("Skosność:", round(skewness(baza$area), 4)),
  side = 1, line = 3, cex = 0.8
)
mtext(
  paste("Kurtoza:", round(kurtosis(baza$area), 4)),
  side = 1, line = 4, cex = 0.8
)
#Widać wyraźną dominacje mieszkań z powierzchnią 30-70m^2. Poza tym zakresem
#ilosć ofert jest niższa, ale nie są to pojedyńczne przypadki. Trzeba będzie się
#zastanowić nad wyborem zakresu danych w finalnej bazie.

#7. Sprawdzenie ilości ofert na dzielnice
oferty_na_dzielnice <- baza %>%
  group_by(district) %>%
  summarise(liczba_ofert = n()) %>%
  arrange(desc(liczba_ofert))
print(oferty_na_dzielnice)
#duże zrożnicowanie. Zakres 105-2115 🥷🤙
summary(oferty_na_dzielnice)

#8. Analiza cen mieszkań ze względu na dzielnicę:
analiza_dzielnic_total_price <- baza %>%
  group_by(district) %>%
  summarise(
    liczba_ofert = n(),
    Min = min(total_price, na.rm = TRUE), #Nie ma braków w cenie, ale niech zostanie
    `1st Qu.` = quantile(total_price, 0.25, na.rm = TRUE),
    Median = median(total_price, na.rm = TRUE),
    Mean = mean(total_price, na.rm = TRUE),
    `3rd Qu.` = quantile(total_price, 0.75, na.rm = TRUE),
    Max = max(total_price, na.rm = TRUE)
  ) %>%
  arrange(desc(Median))
print(analiza_dzielnic_total_price)
#Jest problem z odstającymi wartościami więc posłużę się medianą
#Top 3 mediany (jedyne które przekroczyły 1mln):
#a) Zwierzyniec: 1732745
#b) Grzegórzki: 1100000
#c) Stare Miasto: 1084038
#Min 3 mediany:
#a) Bieńczyce: 589000
#b) Nowa Huta: 628894
#c) Mistrzejowice: 

#9. Analiza ceny za m^2 mieszkań ze względu na dzielnicę:
analiza_dzielnic_price_per_m <- baza %>%
  group_by(district) %>%
  summarise(
    liczba_ofert = n(),
    Min = min(price_per_m, na.rm = TRUE),
    `1st Qu.` = quantile(price_per_m, 0.25, na.rm = TRUE),
    Median = median(price_per_m, na.rm = TRUE),
    Mean = mean(price_per_m, na.rm = TRUE),
    `3rd Qu.` = quantile(price_per_m, 0.75, na.rm = TRUE),
    Max = max(price_per_m, na.rm = TRUE)
  ) %>%
  arrange(desc(Median))
print(analiza_dzielnic_price_per_m)
#Top 3 mediany:
#Stare Miasto: 21557
#Grzegórzki: 21243
#Zwierzyniec: 19302
#Min 3 mediany:
#swoszowice: 13163
#Bieżanów-Prokocim: 13175
#Nowa Huta: 13362

#10. Analiza powierzchni mieszkań ze względu na dzielnicę:
analiza_dzielnic_area <- baza %>%
  group_by(district) %>%
  summarise(
    liczba_ofert = n(),
    Min = min(area, na.rm = TRUE), #Nie ma braków w powierzchni, ale niech zostanie
    `1st Qu.` = quantile(area, 0.25, na.rm = TRUE),
    Median = median(area, na.rm = TRUE),
    Mean = mean(area, na.rm = TRUE),
    `3rd Qu.` = quantile(area, 0.75, na.rm = TRUE),
    Max = max(area, na.rm = TRUE)
  ) %>%
  arrange(desc(Median))
print(analiza_dzielnic_area)
#Bardzo ciekawy wynik. Mediana powierzchni mieszkań w Zwierzyńcu to aż 91.3m^2
#na 201 ofert. Kolejne są mieszkania na Bieżanowie 57m^2. Jest drastyczna 
#róznica, aż 24.3m^2. Najmniejsze mieszkania mamy w Bieńczycach: 43.5m^2

#11. Ilość ofert ze względu na rodzaj sprzedawcy (osoba prywatna, agencja, deweloper)
oferty_na_sprzedawce <- baza %>%
  group_by(advertiser_type) %>%
  summarise(liczba_ofert = n()) %>%
  arrange(desc(liczba_ofert))
print(oferty_na_sprzedawce)
#:OOOOO
#1 developer               5395
#2 agency                  5036
#3 private                  430
#Deweloperzy w formie. Odnośnie sporej ilości ofert od agencji, może to wynikać 
#z kosztów jakie ponosi osoba prywatna wystawiając mieszkanie na sprzedaż na 
#otodom bądź po prostu wygody (agencja ich wyręcza)

#12. Analiza ceny za m^2 ze względu na sprzedawcę
analiza_sprzedawcow_price_per_m <- baza %>%
  group_by(advertiser_type) %>%
  summarise(
    liczba_ofert = n(),
    Min = min(price_per_m, na.rm = TRUE),
    `1st Qu.` = quantile(price_per_m, 0.25, na.rm = TRUE),
    Median = median(price_per_m, na.rm = TRUE),
    Mean = mean(price_per_m, na.rm = TRUE),
    `3rd Qu.` = quantile(price_per_m, 0.75, na.rm = TRUE),
    Max = max(price_per_m, na.rm = TRUE)
  ) %>%
  arrange(desc(liczba_ofert))
print(analiza_sprzedawcow_price_per_m)
#brak większych różnic w średniej i medianie
#średnia w agencji może być zawyżona prze oferty odstające. Np. Mają oni to 
#nieszczęsne mieszkanie z ceną 79800 za m^2
#Median   Mean 
#15781. 16421. <- developer 
#16000  17078. <- agency
#15777. 16748. <- private

#===============================================
#Wykresy - baza przed filtrowaniem (ewentualne filtrowanie wpisane w kod
#odpowiedzialny za utworzenie wykresu)
#===============================================

#1. Boxplot cen mieszkań do 3mln ze względu na dzielnice
baza %>%
  filter(total_price <= 3000000) %>%
  ggplot(aes(x = reorder(district, total_price, FUN = median), y = total_price)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  coord_flip() + #dla lepszej czytelności wykresu
  scale_y_continuous(labels = scales::comma) + #żeby w cenach nie było np. 1e+06
  labs(
    title = "Rozkład cen mieszkań do 3mln w dzielnicach",
    x = "Dzielnica",
    y = "Cena"
  ) +
  theme_minimal()
#Zwierzyniec wydaje się być dzielnicą "Premium", pudełko zaczyna się od ceny
# 1 000 000 i wyraźnie odstaje od reszty. To wynika z tego co sprawdziliśmy
# wcześniej (mediana mieszkań w Zwierzyńcu to 91.3m^2).
# Różnice w cenach również są największe (najdłuższe pudełko). 
# Najkorzystniejsze oferty pod względem ceny znajdują się w Bieńczycach,
# Nowej Hucie i Mistrzejowicach

#2. Boxplot cen za m^2 mieszkań (do 25000 za m^2) ze względu na dzielnice
baza %>%
  filter(price_per_m >= 7000 & price_per_m <= 25000) %>%
  ggplot(aes(x = reorder(district, price_per_m , FUN = median), y = price_per_m)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  coord_flip() +
  labs(
    title = "Rozkład cen za m^2 (do 25000zł) mieszkań w dzielnicach",
    x = "Dzielnica",
    y = "Cena za m^2"
  ) +
  theme_minimal()
#Najdroższe mieszkania w Starym mieście. Jak podniesiemy ograniczenie
#w cenie bądź nawet usuniemy to możemy zaobserować sporo outlinerów w Starym Mieście
#gdzie ceny sięgają nawet powyżej 50000zł za m^2. 3 Mieszkania z najwyższą ceną
#za m^2 znajdują się w Zwierzyńcu 62000-70000 i jedno 79800 które
#zobaczyliśmy w summary(baza$price_per_m)

#3. Wykres rozrzutu ceny i powierzchni z podziałem na dzielnice
# cena do 3 000 000 i powierzchnia do 200 m^2
baza %>%
  filter(total_price <= 3000000, area <= 200) %>%
  ggplot (aes(x=area, y=total_price, colour = district)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 3mln) od powierzchni (do 200m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()
#Trochę nieczytelne, ale można zaobserwować, że w dzielnicy Grzegórzki linia
#trendu jest najbardziej stroma, a w swoszowicach najmniej

#Ad.3 Spróbujmy jeszcze bez podziału
baza %>%
  filter(total_price <= 3000000, area <= 200) %>%
  ggplot (aes(x=area, y=total_price)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 3mln) od powierzchni (do 200m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()
#piękna chmurka. Możemy zaobserwować, że wraz ze wzrostem powierzchni cena,
#staje się coraz bardziej zróżnicowana/nieprzewidalna (odstaje od lini trendu)

baza %>%
  filter(total_price <= 3000000, area <= 200) %>%
  ggplot (aes(x=area, y=total_price)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 3mln) od powierzchni (do 200m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()

# Ad. 5 Wykres rozrzutu ceny za m^2 od roku budowy z wygładzaniem LOESS (Mikołaj Dyjakowski)
baza_filtered %>%
  filter(build_year >= 1865) %>%   # <--- Ograniczenie zakresu czasowego
  ggplot(aes(x = build_year, y = price_per_m)) +
  geom_point(alpha = 0.3, color = "darkblue") +  # Punkty z przezroczystością
  geom_smooth(method = "loess", color = "red", se = TRUE) + # Linia trendu LOESS
  scale_y_continuous(labels = scales::comma) +   # Formatowanie osi Y (bez notacji naukowej)
  labs(
    title = "Zależność ceny za m^2 od roku budowy",
    subtitle = "Metoda wygładzania: LOESS",
    x = "Rok budowy",
    y = "Cena za m^2 (zł)"
  ) +
  theme_minimal()


#===============================================
#Ad.6 Mapa cen (Heatmapa punktowa) (Mikołaj Dyjakowski)
#===============================================

# Wykres geograficzny: Kolor punktu zależy od ceny za m^2
baza_filtered %>%
  ggplot(aes(x = longitude, y = latitude, color = price_per_m)) +
  geom_point(alpha = 0.6, size = 1) +  
  scale_color_viridis_c(option = "magma", direction = -1, labels = scales::comma) + 
  coord_quickmap() +  
  labs(
    title = "Geograficzny rozkład cen mieszkań w Krakowie",
    subtitle = "Im jaśniejszy kolor, tym wyższa cena za m^2",
    x = "Długość geograficzna",
    y = "Szerokość geograficzna",
    color = "Cena/m^2"
  ) +
  theme_dark()


#===============================================
#Heatmapa - leaflet
#===============================================
mieszkania_sf <- st_as_sf(baza, coords = c("longitude", "latitude"), crs = 4326)

ggplot(mieszkania_sf) +
  annotation_map_tile(type = "osm", zoom = 12) + 
  geom_sf(aes(color = price_per_m), size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Mapa cen mieszkań w Krakowie",
       subtitle = "Stan na 04.01.2026",
       caption = "Źródło: otodom.pl")

baza_heatmap_price_per_m <- baza %>%
  filter(price_per_m >= 7000 & price_per_m <= 30000)

leaflet(baza_heatmap_price_per_m) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude,
    radius = 5,
    color = ~colorNumeric("YlOrRd", price_per_m)(price_per_m),
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~paste0("Dzielnica: ", district, "<br>",
                    "Cena/m2: ", round(price_per_m, 2), " zł<br>",
                    "Powierzchnia: ", area, " m2")
  ) %>%
  addLegend(pal = colorNumeric("YlOrRd", baza_heatmap_price_per_m$price_per_m), 
            values = ~price_per_m, 
            title = "Cena za m^2", 
            position = "bottomright")

baza_heatmap_total_price <- baza %>%
  filter(total_price >= 500000 & total_price <= 2000000)

leaflet(baza_heatmap_total_price) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude,
    radius = 5,
    color = ~colorNumeric("YlOrRd", total_price)(total_price),
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = ~paste0("Dzielnica: ", district, "<br>",
                    "Cena: ", round(total_price, 2), " zł<br>",
                    "Powierzchnia: ", area, " m2")
  ) %>%
  addLegend(pal = colorNumeric("YlOrRd", baza_heatmap_total_price$total_price), 
            values = ~total_price, 
            title = "Cena", 
            position = "bottomright")
#===============================================
#Filtrowanie bazy. Walka o normalność
#===============================================
#1.Filtrowanie bazy
baza_filtered <- baza %>%
  filter(
    total_price >= 500000 & total_price <= 1380000,
    area >= 25 & area <= 110,
    price_per_m >= 7000 & price_per_m <= 28000,
    build_year > 1550 & build_year <= 2026,
    rooms != 0,
    total_floors >= floor,
  )
#stracone 3950 ofert

#2.Analiza log_price po filtrowaniu
summary(baza_filtered$log_price)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#13.12   13.42   13.59   13.60   13.77   14.14 
#Wygląda to obiecująco

hist(baza_filtered$log_price,
     breaks = 30,
     main = "Histogram zlogarytmowanych cen mieszkań",
     xlab = "Zlogarytmowana cena")
#Wygląda to już dużo lepiej

lillie.test(baza_filtered$log_price)
#p-value < 2.2e-16
#Daleko od rozkładu normalnego, ale może to wynikać z:
#niestyntetycznych danych
#dużej ilości argumentów
#sprawdżmy skośność
skewness(baza_filtered$log_price)
#[1] 0.1461568
#Udało się zbić skośność z ponad 1 do 0.146. Jest to spore zbliżenie do rozkładu
#symetrycznego. Mieści się w zakresie 0-0.5
#sprawdźmy jeszcze kurtozę
kurtosis(baza_filtered$log_price)
#[1] -0.6931759
#kurtoza ujemna, ale dosyć bliska zera. Rozkład jest lekko spłaszczony

#3.Analiza log_area po filtrowaniu
summary(baza_filtered$log_area)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.219   3.748   3.932   3.930   4.130   4.700 

hist(baza_filtered$log_area,
     breaks = 30,
     main = "Histogram zlogarytmowanej powierzchni mieszkań",
     xlab = "Zlogarytmowana powierzchnia")
#Histogram ma się jednak nieco gorzej. Wartość około 4.25 "psuje" rozkład

lillie.test(baza_filtered$log_area)
#p-value < 2.2e-16
#nie spodziewałem się niczego innego...

skewness(baza_filtered$log_area)
#[1] -0.05447489
#:000000
#Praktycznie zero. Blisko symetryczności

kurtosis(baza_filtered$log_area)
#[1] -0.4003116
#Rozkład wciąż lekko spłaszczony, ale mniej niż w przypadku log_price.

#===============================================
#Powtórzone wykresy dla odfiltrowanej bazy
#===============================================
#1. Boxplot cen mieszkań ze względu na dzielnice
baza_filtered %>%
  ggplot(aes(x = reorder(district, total_price, FUN = median), y = total_price)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  coord_flip() + #dla lepszej czytelności wykresu
  scale_y_continuous(labels = scales::comma) + #żeby w cenach nie było np. 1e+06
  labs(
    title = "Rozkład cen mieszkań do 1.38mln w dzielnicach",
    x = "Dzielnica",
    y = "Cena"
  ) +
  theme_minimal()

#2. Boxplot cen za m^2 mieszkań ze względu na dzielnice
baza_filtered %>%
  ggplot(aes(x = reorder(district, price_per_m , FUN = median), y = price_per_m)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  coord_flip() +
  labs(
    title = "Rozkład cen za m^2 mieszkań w dzielnicach",
    x = "Dzielnica",
    y = "Cena za m^2"
  ) +
  theme_minimal()

#3. Wykres rozrzutu ceny i powierzchni z podziałem na dzielnice
baza_filtered %>%
  ggplot (aes(x=area, y=total_price, colour = district)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 1.38mln) od powierzchni (do 110m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()

#Ad.3 Spróbujmy jeszcze bez podziału
baza_filtered %>%
  ggplot (aes(x=area, y=total_price)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 1.38mln) od powierzchni (do 110m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()

#4. Wykres rozrzutu zlogarytmowanej ceny i powierzchni z podziałem na dzielnice
baza_filtered %>%
  ggplot (aes(x=log_area, y=log_price, colour = district)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 1.38mln) od powierzchni (do 110m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()

#Ad.4 Spróbujmy jeszcze bez podziału

baza_filtered %>%
  ggplot (aes(x=log_area, y=log_price)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 1.38mln) od powierzchni (do 110m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()

# Ad. 5 Wykres rozrzutu ceny za m^2 od roku budowy z wygładzaniem LOESS (Mikołaj Dyjakowski)
baza_filtered %>%
  filter(build_year >= 1865) %>%   # <--- Ograniczenie zakresu czasowego
  ggplot(aes(x = build_year, y = price_per_m)) +
  geom_point(alpha = 0.3, color = "darkblue") +  # Punkty z przezroczystością
  geom_smooth(method = "loess", color = "red", se = TRUE) + # Linia trendu LOESS
  scale_y_continuous(labels = scales::comma) +   # Formatowanie osi Y (bez notacji naukowej)
  labs(
    title = "Zależność ceny za m^2 od roku budowy",
    subtitle = "Metoda wygładzania: LOESS",
    x = "Rok budowy",
    y = "Cena za m^2 (zł)"
  ) +
  theme_minimal()


#===============================================
#Ad.6 Mapa cen (Heatmapa punktowa) (Mikołaj Dyjakowski)
#===============================================

# Wykres geograficzny: Kolor punktu zależy od ceny za m^2
baza_filtered %>%
  ggplot(aes(x = longitude, y = latitude, color = price_per_m)) +
  geom_point(alpha = 0.6, size = 1) +  
  scale_color_viridis_c(option = "magma", direction = -1, labels = scales::comma) + 
  coord_quickmap() +  
  labs(
    title = "Geograficzny rozkład cen mieszkań w Krakowie",
    subtitle = "Im jaśniejszy kolor, tym wyższa cena za m^2",
    x = "Długość geograficzna",
    y = "Szerokość geograficzna",
    color = "Cena/m^2"
  ) +
  theme_dark() 


#===============================================
#Modele docelowe
#===============================================

#1. Regresja liniowa
#   a) zmienna wyjaśniana: log_price

#2. Analiza skupień

#===============================================
#Relacje pomiędzy parami zmiennych
#===============================================
#1. Korelacja ceny całkowitej i powierzchni
cor.test(baza_filtered$total_price, baza_filtered$area, method = "spearman", exact = FALSE)
#rho 0.7536825 p-value < 2.2e-16
#Pierwszy przykład oczywisty, ale no... Wyraźna korelacja dodatnia =>
# powierzchnia jest głównym czynnikiem całkowitą cene

#2. Korelacja ceny za m^2 i roku budowy
cor.test(baza_filtered$price_per_m, baza_filtered$build_year, method = "spearman", exact = FALSE)
#rho 0.07401119 p-value = 0.3856
#Niska korelacja, wysoka wartośc p => wynik nieistotny statystycznie.
#Problem przez metode. Rok budowy wiele razy się powtórzył więc spearman który
#opiera się na nadawaniu "rang" mógł zgłupieć... Jest obawa, że to źle dobrana
#metoda do "zadania".

#3.Zależność ceny za m^2 od stanu wykończenia
kruskal.test(price_per_m ~ finishing_state, data = baza_filtered)
#chi-squared = 226.18, p-value < 2.2e-16
#p < 0.05 => Ceny za m^2 w zależności od stanu wykończenia się różnią

#===============================================
#Regresja liniowa
#Za tą część odpowiadał: Jan Barchanowicz
#===============================================
baza <- read.xlsx("C:/Users/jasiu/OneDrive - Akademia Górniczo-Hutnicza im. Stanisława Staszica w Krakowie/Studia/IFS1-3/Analiza danych/Koło naukowe/Apartments project/apartments (2).xlsx")

baza <- baza %>%
  mutate(across(
    c(latitude, longitude, total_price, area, rooms, floor, total_floors, build_year), 
    as.numeric
  ))
#zamiana typów character na numeric, niezbędna do dalszej analizy

baza <- baza %>%
  mutate (price_per_m = total_price / area)
#dodanie zmiennej price_per_m w celu ułatwienia zobrazowania cen

baza <- baza %>% 
  filter(price_per_m < 25000)

#odfiltrowanie obserwacji z ceną za m^2 powyzej 25000 zł
#Nie było ich dużo, a znacząco zaburzały rozkład tej zmiennej
#Oraz skośność, co mogłoby prowadić do późniejszych błędów w modelu
#tu możnaby pokazac histogram tej wartosci
baza<- baza %>% 
  mutate(log_price = log(total_price)) %>% 
  mutate(log_price_m = log(price_per_m))
#Dodanie logarytmu ceny całkowitej oraz ceny za m^2, dzięki czemu 
#rozkład będzie znacznie stabilniejszy i bliższy normalnemu
#Ułatwia to dalszą analizę


Q1 <- quantile(baza$area, 0.25)
Q3 <- quantile(baza$area, 0.75)
IQR <- Q3 - Q1
#definicja zakresów potrzebnych do następnego etapu filtrowania
#Zakresy te wzięły się z wykresu pudełkowego powierzchni

boxplot(baza$area)

#na którym widać, iż jest sporo obserwacji wykraczających poza 
#wartości mediany

baza<- baza %>%  
  filter(log_price < 14.75 & log_price > 12.5 ) %>% 
  #odfiltrowanie obserwacji z mocno odstającą ceną
  # w celu redukcji skośności i poprawy kurtozy
  filter(log_price >= 12.75) %>% 
  #tutaj to samo
  #możnaby przy tym gdzieś pokazac znowu ten histogram
  filter(log_price_m >=9.2) %>%
  filter(log_price < 14.35) %>% 
  filter(total_price <= 1500000) %>% 
  #Odfiltrowanie skrajnie wysokich cen
  #mogących zaburzać dokładność modelu
  filter(price_per_m < 21500 | price_per_m > 22000) %>% 
  #znowu odfiltrowanie odstających cen na m^2
  dplyr::select(-id, -source, -city, -avaiable_from, -created_at) %>% 
  #usunięcie niepotrzebnych w dalszej analizie kolumn
  filter(area < Q3 + 1.5 * IQR | area > Q1 - 1.5 * IQR)
  #filtrowanie na podstawie odstawania obserwacji o więcej niż
#1.5 IQR


#histogram log price w celu pokazania osiągniętych wartosci 
#kurtozy i skośności
hist(baza$log_price,
     breaks = 30,
     main = "Histogram cen całkowitych",
     xlab = "",
     ylab = "Częstotliwość")



mtext("Logarytm ceny za metr kwadratowy", side = 1, line = 2, cex = 0.9)
#ładniejszy opis osi X
mtext(
  paste("Skosność:", round(skewness(baza$log_price), 4)),
  side = 1, line = 3, cex = 0.8
)
# Skosność

mtext(
  paste("Kurtoza:", round(kurtosis(baza$log_price), 4)),
  side = 1, line = 4, cex = 0.8
)
# Kurtoza


Q1 <- quantile(baza$log_price_m, 0.25, na.rm = TRUE)
Q3 <- quantile(baza$log_price_m, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

#Przygotowanie zakresów w celu usunięcia odstających 
#obserwacji znalezionych na wykresie pudełkowym 
#logarytmu ceny za m^2

num_vars <- baza %>% dplyr::select(where(is.numeric))

par(mfrow = c(1, 1))
for (v in names(num_vars)) {
  boxplot(num_vars[[v]], main = v)
}
#utworzenie wykresów pudełkowych dla każdej zmiennej numerycznej
#w bazie

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
baza <- baza %>% 
  dplyr::filter(log_price_m > lower_bound, log_price_m < upper_bound)

#usunięcie odstających obserwacji w logarytmie ceny za m^2

Q1 <- quantile(baza$log_price, 0.25, na.rm = TRUE)
Q3 <- quantile(baza$log_price, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

baza <- baza %>%
  filter(log_price >= lower_bound, log_price <= upper_bound)


#To samo dla logarytmu ceny całkowitej

Q1 <- quantile(baza$build_year, 0.25, na.rm = TRUE)
Q3 <- quantile(baza$build_year, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

sum(baza$build_year < lower_bound | 
      baza$build_year > upper_bound, 
    na.rm = TRUE)

#Usunięcie obserwacji odstających dla roku zbudowania

sum(baza$build_year == 0, na.rm = TRUE)
#sprawdzenie ile jest zerowych wartości roku budowy

baza <- baza %>%
  mutate(build_year = ifelse(build_year == 0, NA, build_year))
#zamiana tych wartosci na NA

baza <- baza %>%
  filter(!is.na(build_year))
#usunięcie wartości NA z build year

baza$build_year[which(baza$build_year == 980)] <- 1980
#Poprawa prawdopobobnego błędu znalezionego w jednej obserwacji
baza$build_year[which(baza$build_year == 1)] <- NA
#zamiana wartości 1 w roku wybudowania na NA
baza$build_year[which(baza$build_year == 0)] <- NA
#To samo dla wartości 0

baza <- baza %>%
  filter(!is.na(build_year))
#odfiltrowanie wartosci NA z build year (krok powtórzony dla pewności)
baza <- baza %>%
  filter(build_year != 1) %>% 
  filter(build_year != 0) %>% 
  filter(build_year != 2)
#Odfiltrowanie wartości year build równych 0, 1 lub 2 

colSums(is.na(baza)) > 0
#sprawdzenie które kolumny jeszcze wciąż mają NA
sum(is.na(baza$finishing_state))
#sprawdzenie ile NA ma konkretna kolumna

baza <- baza %>%
  filter(!is.na(finishing_state))
#odfiltrowanie obserwacji z wartościami NA w finishing state
sum(is.na(baza$heating_type))
#sprawdzenie ilosci NA w heating type

mode_val <- names(sort(table(baza$heating_type), decreasing = TRUE))[1]

baza <- baza %>%
  mutate(heating_type = ifelse(is.na(heating_type), mode_val, heating_type))
#z powodu ogromnej ilości NA, brakujące wartości wypełniliśmy
#modą

baza <- baza %>%
  mutate(has_elevator = as.numeric(has_elevator))
#przekształcenie zmiennej logicznej has_elevator na binarną

baza$heating_type <- as.factor(baza$heating_type)
baza$finishing_state <- as.factor(baza$finishing_state)
baza$district <- as.factor(baza$district)
baza$market_type <- as.factor(baza$market_type)
baza$advertiser_type <- as.factor(baza$advertiser_type)
#przekształcenie zmiennych kategorialnych na factory

model1 <- lm(formula = log_price ~ . + area * district - price_per_m - log_price_m, data = baza)
summary(model1)
par(mfrow = c(2, 2))  # Dzieli ekran na 2 wiersze i 2 kolumny
plot(model1)
par(mfrow = c(1,1))
#pierwszy model z relacją
#area_log - district
#decyzja o kodowaniu hot one aby 
#ułatwić interpretację modelu
#Mimo niewiarygodnie wysokiego R^2
#Jakość modelu nie jest najlepsza, prawdopodobnie z powodu zbyt dużej
#ilości zmiennych wyjaśniających, oraz z powodu obecności zmiennych
#kategorialnych
install.packages("fastDummies")
library(fastDummies)
lapply(baza[c("heating_type", "finishing_state", "district", "market_type", "advertiser_type")],
       function(x) sort(unique(x))[1])

#sprawdzenie które kolumny zostaną użyte jako referencje 
#Heating_type - boiler_room
#Finishing_state - ready_to_use
#district - Bieńczyce
#market_type - primary
#advertiser_type - agency

# wyniku działania pakietu fastDummies
baza <- dummy_cols(
  baza,
  select_columns = c("heating_type", "finishing_state", "district", "market_type", "advertiser_type"),
  remove_first_dummy = TRUE,   # usuwa kategorię referencyjną
  remove_selected_columns = TRUE
)
#kodowanie jeden do wielu zmiennych kategorialnych

baza <- baza %>% 
  dplyr::select(-c(latitude, longitude, total_price, price_per_m, log_price_m))
#usunięcie kolumn zbędnych w dalszej analizie
#Z racji iż model price_per_m okazał się być niezwykle ciężki
#do dalszego użycia (trudno było poprawiać jego jakość)
#zdecydowaliśmy się na model wyjaśniający total_price 
# w formie logarytmowanej
names(baza) <- gsub(" ", "_", names(baza))
#pozybcie się spacji w nazwach zmiennych
#wykresy diagnostyczne modelu
reszty <- resid(model1)
baza <- baza[abs(reszty) <= 0.4, ]
#usuniecie reszt wiekszych niz 0.4
leverage_values <- hatvalues(model1)
id_punktu <- which.max(leverage_values)

leverage_values[id_punktu]

baza[id_punktu, ]
#ustalenie punktu o najwyższym leverage value
#Jest to prawdopodobnie blad - jak mieszkanie z 2008 roku moze mieć 
#piec kaflowy?
#Usuwamy

baza <- baza[-id_punktu, ]

baza$area <- log(baza$area)
#decyzja o logarytmizacji powierzchni
#w celu poprawy rozkladu, a tym samym jakości modelu
baza <- rename(baza, area_log = area)
#zmiana nazwy kolumny w celu poprawy czytelności


#podejrzenie silnej korelacji market_type_secondary z year_build
cor_matrix <- cor(baza[ , c("market_type_secondary", "build_year")])
corrplot::corrplot(
  cor_matrix,
  method = "color",
  addCoef.col = "black",
  tl.col = "black",
  type = "upper",
  diag = FALSE,
  main = "Korelacja: market_type_secondary vs build_year"
)
#potwierdzenie tej korelacji

baza$build_year <- cut(baza$build_year, 
                                   breaks = c(-Inf, 1945, 1989, 2010, Inf), 
                                   labels = c("Kamienica", "PRL", "Transformacja", "Nowe"))
#zamiana build year na przedzialy w celu 
#zanegowania efektu tej korelacji
#ponadto relacja log_price z build_year
# nie jest liniowa
#dzieki temu model lepiej moze zrozumieć że
#stare kamienice i nowe apartamenty mogą być jednocześnie drogie 
#mimo rożnicy wieku

model2 <- lm(formula = log_price ~ ., data = baza)
summary(model2)
par(mfrow = c(2, 2))  # Dzieli ekran na 2 wiersze i 2 kolumny
plot(model2)
par(mfrow = c(1,1))

v_model_m <- vif(model2)
subset(v_model_m, v_model_m > 5)
#wyświetlenie czynników o VIF >5)
#Zmienne dzielnicowe niestety muszą zostać, mimo tak
#silnego wpływu na model
#natomiast usunięciu podległa zmienna heating_type_urban, 
#gdyż niewiele wnosi ona do wytłumaczenia ceny
ggplot(baza, aes(x = factor(heating_type_urban), y = log_price)) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(
    x = "Heating type: urban",
    y = "Log price",
    title = "Średnie log_price z przedziałami ufności"
  ) +
  theme_minimal()

#jak widać tutaj na wykresie


#ponowne sprawdzenie modelu
v_model_m <- vif(model2)
subset(v_model_m, v_model_m > 5)


#kolejny etap - usunięcie zmiennych nie wnoszących nic do modelu
#tutaj będą to zmienne advertiser_type oraz market_type_secondary
#i heating type gas, other i tiled stove, oraz total floors

model3 <- lm(formula = log_price ~ .-advertiser_type_developer -advertiser_type_private -market_type_secondary-heating_type_gas-heating_type_other-heating_type_tiled_stove, data = baza)
summary(model3)
par(mfrow = c(2, 2))  # Dzieli ekran na 2 wiersze i 2 kolumny
plot(model3)
par(mfrow = c(1,1))
#kolejna poprawa jakosci oraz dopasowania modelu, 
#qq residuals są lepsze, zarowno jak i residuals vs leverage
reszty <- resid(model3)
print(which.max(reszty))
baza <- baza[rownames(baza) != "171", ]
baza <- baza[rownames(baza) != "169", ]
#usuwanie maksymalnej reszty

reszty_std <- rstandard(model3)
indeksy_qq <- order(abs(reszty_std), decreasing = TRUE)[1:10]
top10_qq <- baza[indeksy_qq, ]
print(top10_qq)
#wybor oraz podejrzenie obserwacji o największych błędach
# w celu decyzji co nalezy zrobić
#Wiersze 171 oraz 3321 to duplikaty
# nalezy je usunąć
#Wiersz 4423 - anomalia w postaci jednopokojowego 50m mieszkania
#usunięcie duplikatu
baza <- baza[which(rownames(baza) != "171"), ]
#


cooks <- cooks.distance(model3)
indeksy_leverage <- order(cooks, decreasing = TRUE)[1:10]
top10_leverage <- baza[indeksy_leverage, ]
print(top10_leverage)
#wyswietlenie 10 obserwacji o największym wpływie na model
#wiersz 4497 - dziwna kombinacja stanu i epoki
#do wykończenia z lat 1990-2015?
#nietypowy przypadek -> do usuniecia
#4884 -> prawdopodobinie dom. Heating type other oraz wysoka powierzchnia
#moze oznaczać pompę 
#ciepła - jest to opcja premium, stąd cena
#Do usunięcia
for_deletion <- c("4884", "4497")
baza <- baza[rownames(baza) != "4884", ]
baza <- baza[rownames(baza) != "4497", ]
#usunięcie wspomnianych przypadków
model3 <- lm(formula = log_price ~ .-advertiser_type_developer -advertiser_type_private -market_type_secondary-heating_type_gas-heating_type_other-heating_type_tiled_stove, data = baza)
summary(model3)
par(mfrow = c(2, 2))  # Dzieli ekran na 2 wiersze i 2 kolumny
plot(model3)
par(mfrow = c(1,1))
#końcowy model

reszty <- resid(model3)

hist(reszty)
#pokazanie rozkładu reszt modelu

coefs <- coef(model3)[-1]
#Pobranie zmiennych bez interceptu

tabela_final <- data.frame(
  Zmienna = names(coefs),
  Beta = coefs
) %>%
  #utworzenie tabeli zawierającej zmienne oraz ich wpływ w procentach
  mutate(
    Wplyw_Procentowy = ifelse(
      Zmienna == "area_log",
      round(Beta * 1, 2), 
      round((exp(Beta) - 1) * 100, 2) 
    )
    #logika obliczania wpływu procentowego
    #Nie obliczamy logarytmu dla area_log
  ) %>%
  select(Zmienna, Wplyw_Procentowy) %>%
  arrange(desc(abs(Wplyw_Procentowy))) # Sortowanie od najsilniejszych czynników
#wybieramy kolumny oraz sortujemy


referencje <- data.frame(
  Zmienna = c(
    "heating_type_boiler_room",
    "finishing_state_ready_to_use",
    "district_Bieńczyce", "build_year"
  ),
  Wplyw_Procentowy = 0
)
#utworzenie tabeli ze zmiennymi referencyjnymi

tabela_final <- bind_rows(tabela_final, referencje) %>%
  mutate(
    Zmienna = gsub("district_", "Dzielnica: ", Zmienna),
    Zmienna = gsub("heating_type_", "Ogrzewanie: ", Zmienna),
    Zmienna = gsub("finishing_state_", "Standard: ", Zmienna),
    Zmienna = gsub("market_type_", "Rynek: ", Zmienna),
    Zmienna = gsub("advertiser_type_", "Ogłoszeniodawca: ", Zmienna),
    Zmienna = gsub("build_year", "Epoka: ", Zmienna)
  ) %>%
  #utworzenie tabeli finalnej 
  arrange(Zmienna, desc(abs(Wplyw_Procentowy)))

tabela_final$Zmienna[tabela_final$Zmienna == "Epoka: "] <- "Epoka: Kamienica"
#Zmiana nazwy "Epoka" na "Epoka: Kamienica" dla czytelności
#grupowanie po zmiennej
print(tabela_final, row.names = FALSE)
#wyświetlanie tabeli

#===================================
#To samo dla pozostałych dwóch modeli
#===================================
coefs <- coef(model2)[-1]
#Pobranie zmiennych bez interceptu

tabela_final <- data.frame(
  Zmienna = names(coefs),
  Beta = coefs
) %>%
  #utworzenie tabeli zawierającej zmienne oraz ich wpływ w procentach
  mutate(
    Wplyw_Procentowy = ifelse(
      Zmienna == "area_log",
      round(Beta * 1, 2), 
      round((exp(Beta) - 1) * 100, 2) 
    )
    #logika obliczania wpływu procentowego
    #Nie obliczamy logarytmu dla area_log
  ) %>%
  select(Zmienna, Wplyw_Procentowy) %>%
  arrange(desc(abs(Wplyw_Procentowy))) # Sortowanie od najsilniejszych czynników
#wybieramy kolumny oraz sortujemy


referencje <- data.frame(
  Zmienna = c(
    "heating_type_boiler_room",
    "finishing_state_ready_to_use",
    "district_Bieńczyce", "build_year", "Rynek, Ogłoszeniodawca:"
  ),
  Wplyw_Procentowy = 0
)
#utworzenie tabeli ze zmiennymi referencyjnymi

tabela_final <- bind_rows(tabela_final, referencje) %>%
  mutate(
    Zmienna = gsub("district_", "Dzielnica: ", Zmienna),
    Zmienna = gsub("heating_type_", "Ogrzewanie: ", Zmienna),
    Zmienna = gsub("finishing_state_", "Standard: ", Zmienna),
    Zmienna = gsub("market_type_", "Rynek: ", Zmienna),
    Zmienna = gsub("advertiser_type_", "Ogłoszeniodawca: ", Zmienna),
    Zmienna = gsub("build_year", "Epoka: ", Zmienna)
  ) %>%
  #utworzenie tabeli finalnej 
  arrange(Zmienna)

tabela_final$Zmienna[tabela_final$Zmienna == "Epoka: "] <- "Epoka: Kamienica"
#Zmiana nazwy "Epoka" na "Epoka: Kamienica" dla czytelności
#grupowanie po zmiennej
print(tabela_final, row.names = FALSE)

coefs <- coef(model3)[-1]
#Pobranie zmiennych bez interceptu

tabela_final <- data.frame(
  Zmienna = names(coefs),
  Beta = coefs
) %>%
  #utworzenie tabeli zawierającej zmienne oraz ich wpływ w procentach
  mutate(
    Wplyw_Procentowy = ifelse(
      Zmienna == "area_log",
      round(Beta * 1, 2), 
      round((exp(Beta) - 1) * 100, 2) 
    )
    #logika obliczania wpływu procentowego
    #Nie obliczamy logarytmu dla area_log
  ) %>%
  select(Zmienna, Wplyw_Procentowy) %>%
  arrange(desc(abs(Wplyw_Procentowy))) # Sortowanie od najsilniejszych czynników
#wybieramy kolumny oraz sortujemy


referencje <- data.frame(
  Zmienna = c(
    "heating_type_boiler_room",
    "finishing_state_ready_to_use",
    "district_Bieńczyce", "build_year"
  ),
  Wplyw_Procentowy = 0
)
#utworzenie tabeli ze zmiennymi referencyjnymi

tabela_final <- bind_rows(tabela_final, referencje) %>%
  mutate(
    Zmienna = gsub("district_", "Dzielnica: ", Zmienna),
    Zmienna = gsub("heating_type_", "Ogrzewanie: ", Zmienna),
    Zmienna = gsub("finishing_state_", "Standard: ", Zmienna),
    Zmienna = gsub("market_type_", "Rynek: ", Zmienna),
    Zmienna = gsub("advertiser_type_", "Ogłoszeniodawca: ", Zmienna),
    Zmienna = gsub("build_year", "Epoka: ", Zmienna)
  ) %>%
  #utworzenie tabeli finalnej 
  arrange(Zmienna, desc(abs(Wplyw_Procentowy)))

tabela_final$Zmienna[tabela_final$Zmienna == "Epoka: "] <- "Epoka: Kamienica"
#Zmiana nazwy "Epoka" na "Epoka: Kamienica" dla czytelności
#grupowanie po zmiennej
print(tabela_final, row.names = FALSE)

#===================================
#Testowanie modeli - Mikołaj Grabowski
#===================================
#1. Wczytanie bazy testowej
baza_test <- read.xlsx("C:/WAD/Dane/apartments-test.xlsx")

#2. Wstępne przygotowanie - wspólne dla wszystkich modeli
# Zamiana typów i obsługa braków danych
baza_test <- baza_test %>%
  mutate(across(c(latitude, longitude, total_price, area, rooms, floor, total_floors, build_year), as.numeric)) %>%
  mutate(log_price = log(total_price),
         has_elevator = as.numeric(has_elevator),
         price_per_m = total_price / area,
         log_price_m = log(total_price / area))

baza_test <- baza_test %>% 
  filter(!is.na(build_year),
         !is.na(finishing_state),
         !is.na(heating_type),
         build_year > 1000,
         total_price <= 1500000)

#3. Przygotowanie bazy dla modelu 1
baza_m1 <- baza_test %>%
  mutate(across(c(district, finishing_state, market_type, advertiser_type, heating_type), as.factor))

#4. Przygotowanie bazy dla modelu 2
#dodanie logarytmu powierzchni, podziału na epoki i ręcznego dummy encodingu
baza_m2 <- baza_test %>%
  mutate(area_log = log(area)) %>%
  mutate(build_year = cut(build_year, 
                          breaks = c(-Inf, 1945, 1989, 2010, Inf), 
                          labels = c("Kamienica", "PRL", "Transformacja", "Nowe")))

#Generowanie zmiennych Dummy
baza_m2 <- dummy_cols(
  baza_m2,
  select_columns = c("heating_type", "finishing_state", "district", "market_type", "advertiser_type"),
  remove_first_dummy = TRUE,
  remove_selected_columns = TRUE
)

# Usunięcie zbędnych kolumn numerycznych, których Janek nie użył w 2 modelu
names(baza_m2) <- gsub(" ", "_", names(baza_m2))

#Dodatkowe działania w celu zgrania bazy testowej z modelem
#a) Nazwy kolumn których oczekuje model
required_cols <- names(coef(model2))[-1]

#b) Jakich kolumn brakuje w bazie testowej?
missing_cols <- setdiff(required_cols, names(baza_m2))

#c) Wypełnienie brakujących kolumn zerami
if(length(missing_cols) > 0) {
  baza_m2[missing_cols] <- 0
}

#5. Przygotowanie bazy dla modelu 3
#Model 3 to model 2 po usunięciu konkretnych zmiennych, ale to już jest wpisane
#w formule modelu
baza_m3 <- baza_m2

#6. Testowanie modeli
pred1 <- predict(model1, newdata = baza_m1)
pred2 <- predict(model2, newdata = baza_m2)
pred3 <- predict(model3, newdata = baza_m3)

print(pred1)

#7. Wyniki (exp żeby wrócić z logarytmu do "normalnej" wartości)
wyniki <- data.frame(
  Actual = baza_test$total_price,
  Pred_M1 = exp(pred1),
  Pred_M2 = exp(pred2),
  Pred_M3 = exp(pred3)
)

#8. Przygotowanie zmiennych do wykresu
wyniki_long_1 <- wyniki %>%
  pivot_longer(cols = starts_with("Pred"), 
               names_to = "Model", 
               values_to = "Prediction")

wyniki_long_2 <- wyniki_long %>%
  mutate(Reszty = Actual - Prediction)

#9. Wykresy
df_m1 <- data.frame(
  Actual = baza_test$total_price,
  Predicted = exp(pred1)
)

ggplot(df_m1, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#0fd149", alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", color = "#0f25d1", fill = "lightblue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#d10f1c", size = 1) +
  scale_x_continuous(labels = scales::label_number(suffix = " zł", big.mark = " ")) +
  scale_y_continuous(labels = scales::label_number(suffix = " zł", big.mark = " ")) +
  labs(
    title = "Analiza dokładności Modelu 1",
    subtitle = "Linia czerwona: idealna predykcja | Linia niebieska: trend predykcji",
    x = "Cena rzeczywista",
    y = "Cena przewidziana"
  ) +
  theme_minimal()

df_m2 <- data.frame(
  Actual = baza_test$total_price,
  Predicted = exp(pred2)
)

ggplot(df_m2, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#0fd149", alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", color = "#0f25d1", fill = "lightblue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#d10f1c", size = 1) +
  scale_x_continuous(labels = scales::label_number(suffix = " zł", big.mark = " ")) +
  scale_y_continuous(labels = scales::label_number(suffix = " zł", big.mark = " ")) +
  labs(
    title = "Analiza dokładności Modelu 2",
    subtitle = "Linia czerwona: idealna predykcja | Linia niebieska: trend predykcji",
    x = "Cena rzeczywista",
    y = "Cena przewidziana"
  ) +
  theme_minimal()

df_m3 <- data.frame(
  Actual = baza_test$total_price,
  Predicted = exp(pred3)
)

ggplot(df_m3, aes(x = Actual, y = Predicted)) +
  geom_point(color = "#0fd149", alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", color = "#0f25d1", fill = "lightblue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#d10f1c", size = 1) +
  scale_x_continuous(labels = scales::label_number(suffix = " zł", big.mark = " ")) +
  scale_y_continuous(labels = scales::label_number(suffix = " zł", big.mark = " ")) +
  labs(
    title = "Analiza dokładności Modelu 3",
    subtitle = "Linia czerwona: idealna predykcja | Linia niebieska: trend predykcji",
    x = "Cena rzeczywista",
    y = "Cena przewidziana"
  ) +
  theme_minimal()
#WNIOSKI
#Model pierwszy to jakiś asior. Był przeszkolony przez dostępne zmienne poza ceną za metr
#(i zmiennymi które nic nie wnosiły - zostały odfiltrowane)
#i jest niemal nieomylny do pewnej kwoty (1.5mln) Warto też zaznaczyć, że sam model
#był liczony na odfiltrowanych danych. Zastanawiający jest fakt wykresów które
#są daleko od tego co moglibyśmy uznać za książkowe
#Model 2 i 3 osiągna ma niemal identyczne predykcje. W przypadku tych modeli
#wykresy były dużo lepsze jednak same predykcje są znacznie gorsze w porównaniu
#z modelem pierwszym

#===============================================
#Analiza skupień - metoda PAM
#===============================================
#1. Przygotowanie bazy do analizy
baza_cluster_analysis <- baza %>%
  filter(
    total_price >= 350000 & total_price <= 3000000,
    area >= 20 & area <= 150,
    price_per_m >= 7000 & price_per_m <= 40000,
    build_year > 1550 & build_year <= 2026,
    rooms != 0
  ) %>%
  select(log_price_m, area, rooms, build_year, latitude, longitude) %>%
  na.omit() %>% #raczej braków nie ma, ale na wszelki
  scale()

# 2. Ile klastrów wybrać?
fviz_nbclust(baza_cluster_analysis, pam, method = "wss")

gap <- clusGap(baza_cluster_analysis, pam, K.max = 8, B = 100)
fviz_gap_stat(gap)

#4. Wersja dla 4 klastrów
wynik_4_klastry <- pam(baza_cluster_analysis,6)
fviz_cluster(wynik_4_klastry,data = baza_cluster_analysis)

#5. Wersja dla 6 klastrów
wynik_6_klastry <- pam(baza_cluster_analysis,6)
fviz_cluster(wynik_4_klastry,data = baza_cluster_analysis)

#6. Wersja dla 7 klastrów
wynik_7_klastry <- pam(baza_cluster_analysis,6)
fviz_cluster(wynik_4_klastry,data = baza_cluster_analysis)

#7. Jak interpretować wymiary
res.pca <- prcomp(baza_cluster_analysis)
fviz_pca_var(res.pca, col.var = "black")
