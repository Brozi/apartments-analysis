# Analiza i predykcja cen mieszkań w Krakowie

## Opis projektu
Kompleksowy projekt analityczny i badawczy w języku R. Jego celem jest eksploracja, czyszczenie, wizualizacja oraz modelowanie danych dotyczących rynku nieruchomości w Krakowie (na podstawie ofert z serwisu otodom.pl). Projekt zawiera budowę modeli regresji liniowej do przewidywania cen oraz analizę skupień.

## Główne funkcjonalności i etapy analizy
* **Eksploracyjna Analiza Danych (EDA):** Badanie rozkładów zmiennych, skosności i kurtozy. Analiza rozkładu ofert w podziale na dzielnice, lata budowy oraz typ sprzedawcy.
* **Czyszczenie danych:** Skomplikowany proces usuwania wartości odstających (wykorzystanie rozstępu ćwiartkowego IQR i odległości Cooka), logarytmizacja prawoskośnych rozkładów cen i powierzchni, oraz obsługa braków danych.
* **Wizualizacje:** Tworzenie zaawansowanych wykresów, w tym boxplotów, histogramów oraz wykresów rozrzutu z wygładzaniem liniowym i LOESS.
* **Analiza Przestrzenna:** Generowanie interaktywnych map (heatmap) z rozkładem cen na mapie Krakowa.
* **Testy statystyczne:** Weryfikacja normalności rozkładów (test Lillieforsa), analiza korelacji (Spearman) oraz test Kruskala-Wallisa.
* **Modelowanie predykcyjne:** Budowa trzech modeli wielokrotnej regresji liniowej przewidujących logarytm ceny całkowitej. Wdrożenie kodowania zmiennych kategorialnych (One-Hot Encoding).
* **Uczenie nienadzorowane:** Przeprowadzenie analizy skupień (klasteryzacji) z wykorzystaniem algorytmu PAM (Partitioning Around Medoids).

## Technologie i biblioteki
* **R**
* **Przetwarzanie danych:** `tidyverse`, `fastDummies`, `openxlsx`.
* **Wizualizacja:** `ggplot2`, `ggcorrplot`, `factoextra`.
* **Analiza przestrzenna:** `sf`, `leaflet`, `viridis`, `ggspatial`.
* **Statystyka i modele:** `car`, `e1071`, `nortest`, `cluster`, `moments`.

## Struktura skryptu
Skrypt został podzielony na logiczne sekcje:
1. Konfiguracja i import bibliotek.
2. Wstępna eksploracja i transformacja typów danych.
3. Czyszczenie i filtrowanie bazy (redukcja wartości odstających).
4. Generowanie wykresów statystycznych i przestrzennych.
5. Przygotowanie danych do uczenia maszynowego.
6. Trening, diagnostyka modeli regresyjnych i walidacja na zbiorze testowym.
7. Analiza skupień (PAM).
