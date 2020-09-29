Jak testować aplikację w RStudio?

W konsoli R:
1. Jeżeli nie mamy pakietu "remotes" to instalujemy - install.packages("remotes")
2. Instalujemy wersję deweloperską Shiny - remotes::install_github("rstudio/shiny")
3. Potrzebne są też pakiety "shinyjs" i "shinythemes"

Następnie:
4. Uruchamiamy dowolny z plików - app.R, server.R bądź ui.r
5. Klikamy Run App > Run External
6. Powinna uruchomić się nasza przeglądarka a w niej komunikat o błędzie (ale to nie jest powód do smutku!)
7. Do adresu URL (jakiś ciąg cyfr) doklejamy np. /?id=1234&form=WS&type=word&lang=Polish
8. Klikamy Enter czy tam odśwież i powinna uruchomić się część ze słowami IRMiKa SiZ

Dostępne aktualnie wartości dla parametrów:

- form: WS, WG
- type:
  - Dla form WS: word, combine, wielowyrazowe, najdluzsze, nasladownictwo (te ostatnie są po PL bo nie znalazłem angielskich odpowiedników w wordbanku)
  - Dla form WG: first_signs, word, phrases, combine, wielowyrazowe, nasladownictwo
- lang: Polish
- id: 1234, 5678 (narazie wpisane w kodzie, zakładam, że będzie się skądś pobierała w przyszłości lista możliwych)

