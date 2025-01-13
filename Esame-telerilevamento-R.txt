# Per prima cosa importiamo le librerie che ci serviranno per lavorare con dati raster e grafici.
# "terra" è utilizzata per gestire immagini geospaziali e analisi raster, mentre "imageRy" è utile per la visualizzazione.
library(terra)
library(imageRy)

# Importiamo anche "ggplot2" per creare grafici, "patchwork" per combinare più grafici insieme,
# e "viridis" per utilizzare palette di colori di alta qualità per le nostre visualizzazioni.
library(ggplot2)
library(patchwork)
library(viridis)

# Fissiamo un seed randomico con la funzione set.seed(). Questo è importante per evitare che le operazioni di classificazione,
# che includono elementi di randomizzazione, producano risultati diversi ad ogni esecuzione.
set.seed(12345)

# Impostiamo la directory di lavoro, cioè la cartella dove R cercherà i file delle immagini da analizzare.
# Cambia questo percorso con la posizione dei tuoi file.
setwd("C:/Users/utente/Desktop/Immagini-esame-AlessandroRustignoli")

# Importiamo le immagini raster. Ogni anno ha due immagini: una in True Color e una in False Color (che include il NIR).
# Le immagini rappresentano una zona specifica e sono divise in bande (rosso, verde, blu, NIR).

tc_2017 <- rast("Eremo_truecolor_2017-04-21.tiff")  # Immagine True Color per il 2017.
nir_2017 <- rast("Eremo_falsecolor_2017-04-21.tiff")  # Immagine False Color per il 2017.

tc_2020 <- rast("Eremo_truecolor_2020-04-23.tiff")  # Immagine True Color per il 2020.
nir_2020 <- rast("Eremo_falsecolor_2020-04-23.tiff")  # Immagine False Color per il 2020.

tc_2023 <- rast("Eremo_truecolor_2023-04-03.tiff")  # Immagine True Color per il 2023.
nir_2023 <- rast("Eremo_falsecolor_2023-04-03.tiff")  # Immagine False Color per il 2023.

# Estraggo le singole bande dalle immagini importate. Ogni banda rappresenta un canale di colore specifico:
# - Banda rossa (b1)
# - Banda verde (b2)
# - Banda blu (b3)
# - Banda NIR (b4, presente solo nelle immagini False Color).

b17_r <- tc_2017[[1]]  # Banda rossa per il 2017.
b17_g <- tc_2017[[2]]  # Banda verde per il 2017.
b17_b <- tc_2017[[3]]  # Banda blu per il 2017.
b17_nir <- nir_2017[[1]]  # Banda NIR per il 2017.

b20_r <- tc_2020[[1]]  # Banda rossa per il 2020.
b20_g <- tc_2020[[2]]  # Banda verde per il 2020.
b20_b <- tc_2020[[3]]  # Banda blu per il 2020.
b20_nir <- nir_2020[[1]]  # Banda NIR per il 2020.

b23_r <- tc_2023[[1]]  # Banda rossa per il 2023.
b23_g <- tc_2023[[2]]  # Banda verde per il 2023.
b23_b <- tc_2023[[3]]  # Banda blu per il 2023.
b23_nir <- nir_2023[[1]]  # Banda NIR per il 2023.

# Combino le bande di ciascun anno in un'unica immagine raster composita. Questo permette di analizzare
# ogni anno considerando tutte le informazioni delle bande.

forest_2017 <- c(b17_r, b17_g, b17_b, b17_nir)  # Immagine combinata per il 2017.
forest_2020 <- c(b20_r, b20_g, b20_b, b20_nir)  # Immagine combinata per il 2020.
forest_2023 <- c(b23_r, b23_g, b23_b, b23_nir)  # Immagine combinata per il 2023.

# Visualizzo le immagini per confrontare visivamente le differenze tra gli anni. 
# Uso il sistema RGB (colori naturali) per rappresentare i dati in modo intuitivo.

par(mfrow = c(1, 3))  # Creo un layout con 3 colonne per affiancare le immagini.
im.plotRGB(forest_2017, 4, 2, 3)  # Immagine RGB per il 2017.
im.plotRGB(forest_2020, 4, 2, 3)  # Immagine RGB per il 2020.
im.plotRGB(forest_2023, 4, 2, 3)  # Immagine RGB per il 2023.

# Classifico le immagini in tre categorie: foresta densa, copertura rada e suolo esposto.
# Questo aiuta a quantificare i cambiamenti nelle aree analizzate.

forest_2017_class <- im.classify(forest_2017, 3)  # Classificazione per il 2017.
forest_2020_class <- im.classify(forest_2020, 3)  # Classificazione per il 2020.
forest_2023_class <- im.classify(forest_2023, 3)  # Classificazione per il 2023.

# Calcolo il numero di pixel appartenenti a ciascuna classe in ogni anno. 
# Questo passaggio serve per analizzare quanto spazio occupa ogni classe nelle immagini classificate.

freq_2017 <- freq(forest_2017_class)  # Frequenza pixel per il 2017.
freq_2020 <- freq(forest_2020_class)  # Frequenza pixel per il 2020.
freq_2023 <- freq(forest_2023_class)  # Frequenza pixel per il 2023.

# Calcolo il totale dei pixel per ogni immagine classificata. Questo è necessario per calcolare le percentuali.

total_2017 <- ncell(forest_2017_class)  # Totale pixel per il 2017.
total_2020 <- ncell(forest_2020_class)  # Totale pixel per il 2020.
total_2023 <- ncell(forest_2023_class)  # Totale pixel per il 2023.

# Calcolo le percentuali di copertura per ogni classe rispetto al totale dei pixel.
# Questo rende più comprensibili i risultati, mostrando la distribuzione delle classi.

perc_2017 <- (freq_2017$count / total_2017) * 100  # Percentuali delle classi per il 2017.
perc_2020 <- (freq_2020$count / total_2020) * 100  # Percentuali delle classi per il 2020.
perc_2023 <- (freq_2023$count / total_2023) * 100  # Percentuali delle classi per il 2023.

# Creo una tabella dei dati per organizzare le percentuali di ciascuna classe nei tre anni.
# Questo rende i dati pronti per essere visualizzati in grafici comparativi.

class_labels <- c("foresta densa", "copertura rada", "suolo esposto")  # Nomi delle classi.
data <- data.frame(class_labels, perc_2017, perc_2020, perc_2023)  # Tabella dei dati.

# Creo grafici a barre per rappresentare visivamente la distribuzione delle classi nei tre anni.

plot_2017 <- ggplot(data, aes(x = class_labels, y = perc_2017, fill = class_labels)) +
  geom_bar(stat = "identity", color = "black") +  # Grafico a barre per il 2017.
  ylim(0, 100) +  # Limito l'asse y al 100%.
  scale_fill_manual(values = c("#2E8B57", "#FFD700", "#D2691E")) +  # Colori per le classi.
  labs(title = "Distribuzione delle classi - 2017")

plot_2020 <- ggplot(data, aes(x = class_labels, y = perc_2020, fill = class_labels)) +
  geom_bar(stat = "identity", color = "black") +  # Grafico a barre per il 2020.
  ylim(0, 100) +
  scale_fill_manual(values = c("#2E8B57", "#FFD700", "#D2691E")) +
  labs(title = "Distribuzione delle classi - 2020")

plot_2023 <- ggplot(data, aes(x = class_labels, y = perc_2023, fill = class_labels)) +
  geom_bar(stat = "identity", color = "black") +  # Grafico a barre per il 2023.
  ylim(0, 100) +
  scale_fill_manual(values = c("#2E8B57", "#FFD700", "#D2691E")) +
  labs(title = "Distribuzione delle classi - 2023")

# Combino i tre grafici per confrontarli in un'unica visualizzazione.

plot_2017 + plot_2020 + plot_2023

# Riorganizzo i dati in formato lungo per confrontare l'andamento delle classi vegetative nel tempo.
# Questo formato è utile per creare un grafico che mostri come cambiano le percentuali di ciascuna classe tra i tre anni.

data_long <- data.frame(
  Anno = rep(c(2017, 2020, 2023), each = 3),  # Ripeto gli anni per ogni classe
  Classe = rep(class_labels, times = 3),  # Ripeto le classi per ogni anno
  Percentuale = c(perc_2017, perc_2020, perc_2023)  # Percentuali combinate
)

# Creo un grafico a linee per visualizzare l'andamento delle classi vegetative dal 2017 al 2023.

ggplot(data_long, aes(x = Anno, y = Percentuale, color = Classe, group = Classe)) +
  geom_line(size = 1.2) +  # Linee per ogni classe
  geom_point(size = 3) +  # Punti per evidenziare gli anni
  scale_color_manual(values = c("#2E8B57", "#FFD700", "#D2691E")) +  # Colori personalizzati per ogni classe
  labs(
    title = "Andamento della vegetazione dal 2017 al 2023",
    x = "Anno",
    y = "Percentuale (%)",
    color = "Classe"
  ) +
  theme_minimal()  # Applico un tema semplice e pulito
