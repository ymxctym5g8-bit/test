# Beispiel für ein ganz normales Skript ohne Anpassung für die App.
# `stichtag` wird von der App gesetzt, `ergebnis` wird live angezeigt.
ergebnis <- data.frame()

for (i in 1:15) {
  Sys.sleep(0.7)
  neu <- data.frame(
    Zeit     = format(Sys.time(), "%H:%M:%S"),
    Stichtag = format(stichtag, "%d.%m.%Y"),
    Schritt  = i,
    Wert     = round(rnorm(1, 100, 15), 2)
  )
  ergebnis <- rbind(ergebnis, neu)
}
