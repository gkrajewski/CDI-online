Jak testować aplikację w RStudio?
1. Uruchamiamy dowolny z plików - app.R, server.R bądź ui.r
2. Klikamy Run App > Run External
3. Powinna uruchomić się nasza przeglądarka a w niej komunikat o błędzie (ale to nie jest powód do smutku!)
4. Do adresu URL (jakiś ciąg cyfr) doklejamy np. /?id=1234&form=WS&type=word&lang=Polish
5. Klikamy Enter czy tam odśwież i powinna uruchomić się część ze słowami IRMiKa SiZ

Dostępne aktualnie wartości dla parametrów:

- form: WS, WG
- type:
  - Dla form WS: word, combine, wielowyrazowe, najdluzsze, nasladownictwo (te ostatnie są po PL bo nie znalazłem angielskich odpowiedników w wordbanku)
  - Dla form WG: word, phrases, combine, wielowyrazowe, nasladownictwo
- lang: Polish
- id: 1234, 5678 (narazie wpisane w kodzie, zakładam, że będzie się skądś pobierała w przyszłości lista możliwych)

