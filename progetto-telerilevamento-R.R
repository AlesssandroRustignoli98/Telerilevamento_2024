# Importiamo le librerie necessarie per lavorare con dati raster e grafici.
# "terra" serve per manipolare dati geospaziali e immagini raster, mentre "imageRy" aiuta nella visualizzazione e gestione delle immagini.
library(terra)       # Pacchetto R specializzato per l'analisi geospaziale e manipolazione di immagini raster.
library(imageRy)     # Pacchetto R per la gestione e visualizzazione di immagini raster.

# Aggiungiamo "ggplot2" per creare visualizzazioni statistiche, "patchwork" per combinare più grafici, 
# e "viridis" per palette di colori inclusive per daltonici.
library(ggplot2)     # Pacchetto R per creare grafici statistici avanzati.
library(patchwork)   # Pacchetto R per combinare più grafici in un'unica visualizzazione.
library(viridis)     # Pacchetto R per palette di colori di alta qualità e daltonismo-friendly.

# Fissiamo il seed per garantire risultati riproducibili, 
# in modo che operazioni casuali producano sempre gli stessi output.
set.seed(12345)      # Fissa il seme per i processi randomici garantendo coerenza nei risultati.

# Impostiamo la directory di lavoro dove sono salvate le immagini da analizzare.
# Modifica questo percorso in base alla posizione dei tuoi file.
setwd("C:/Users/utente/Desktop/Immagini-esame-AlessandroRustignoli")  # Cambia percorso in base alla tua struttura di file.

# Importiamo le immagini raster per tre anni diversi. Ogni anno include due immagini:
# una "True Color" con bande RGB e una "False Color" con il NIR (vicino infrarosso).
tc_2017 <- rast("Eremo_truecolor_2017-04-21.tiff")  # Importa immagine True Color 2017 (bande b4, b3, b2: rosso, verde, blu).
nir_2017 <- rast("Eremo_falsecolor_2017-04-21.tiff")  # Importa immagine False Color 2017 (bande b8, b4, b3: NIR, rosso, verde).

tc_2020 <- rast("Eremo_truecolor_2020-04-23.tiff")  # Importa immagine True Color 2020 (bande b4, b3, b2).
nir_2020 <- rast("Eremo_falsecolor_2020-04-23.tiff")  # Importa immagine False Color 2020 (bande b8, b4, b3).

tc_2023 <- rast("Eremo_truecolor_2023-04-03.tiff")  # Importa immagine True Color 2023 (bande b4, b3, b2).
nir_2023 <- rast("Eremo_falsecolor_2023-04-03.tiff")  # Importa immagine False Color 2023 (bande b8, b4, b3).

# Estraiamo le bande dalle immagini. Ogni banda rappresenta una componente specifica:
# Rosso (b1), Verde (b2), Blu (b3) e NIR (b4 solo per le immagini False Color).
b17_r <- tc_2017[[1]]  # Banda 1 (rosso) estratta dall'immagine 2017 True Color.
b17_g <- tc_2017[[2]]  # Banda 2 (verde) estratta dall'immagine 2017 True Color.
b17_b <- tc_2017[[3]]  # Banda 3 (blu) estratta dall'immagine 2017 True Color.
b17_nir <- nir_2017[[1]]  # Banda 1 (NIR) estratta dall'immagine 2017 False Color.

b20_r <- tc_2020[[1]]  # Banda 1 (rosso) estratta dall'immagine 2020 True Color.
b20_g <- tc_2020[[2]]  # Banda 2 (verde) estratta dall'immagine 2020 True Color.
b20_b <- tc_2020[[3]]  # Banda 3 (blu) estratta dall'immagine 2020 True Color.
b20_nir <- nir_2020[[1]]  # Banda 1 (NIR) estratta dall'immagine 2020 False Color.

b23_r <- tc_2023[[1]]  # Banda 1 (rosso) estratta dall'immagine 2023 True Color.
b23_g <- tc_2023[[2]]  # Banda 2 (verde) estratta dall'immagine 2023 True Color.
b23_b <- tc_2023[[3]]  # Banda 3 (blu) estratta dall'immagine 2023 True Color.
b23_nir <- nir_2023[[1]]  # Banda 1 (NIR) estratta dall'immagine 2023 False Color.

# Combiniamo le bande in un'unica immagine raster per ogni anno. Questo permette di 
# analizzare ogni anno considerando tutte le informazioni disponibili nelle bande.
forest_2017 <- c(b17_r, b17_g, b17_b, b17_nir)  # Immagine combinata 2017: rosso, verde, blu e NIR.
forest_2020 <- c(b20_r, b20_g, b20_b, b20_nir)  # Immagine combinata 2020: rosso, verde, blu e NIR.
forest_2023 <- c(b23_r, b23_g, b23_b, b23_nir)  # Immagine combinata 2023: rosso, verde, blu e NIR.

# Visualizziamo le immagini RGB per confrontare le differenze tra gli anni.
# Usiamo il sistema di colori naturali (RGB) per rendere i dati più comprensibili.
par(mfrow = c(1, 3))  # Divide la finestra grafica in 1 riga e 3 colonne per mostrare più immagini insieme.
im.plotRGB(forest_2017, 4, 2, 3)  # Visualizzazione immagine 2017 con banda NIR sul rosso.
im.plotRGB(forest_2020, 4, 2, 3)  # Visualizzazione immagine 2020 con banda NIR sul rosso.
im.plotRGB(forest_2023, 4, 2, 3)  # Visualizzazione immagine 2023 con banda NIR sul rosso.

# Classifichiamo le immagini in tre categorie: foresta densa, copertura rada e suolo esposto.
# Questo processo aiuta a quantificare i cambiamenti tra le diverse aree.
forest_2017_class <- im.classify(forest_2017, 3)  # Classificazione con 3 classi (pacchetto imageRy) per il 2017.
forest_2020_class <- im.classify(forest_2020, 3)  # Classificazione con 3 classi (pacchetto imageRy) per il 2020.
forest_2023_class <- im.classify(forest_2023, 3)  # Classificazione con 3 classi (pacchetto imageRy) per il 2023.

# Calcoliamo la frequenza dei pixel per ciascuna classe in ogni anno.
# Questo permette di analizzare la distribuzione delle classi.
freq_2017 <- freq(forest_2017_class)  # Frequenza dei pixel per classe (pacchetto terra) nel 2017.
freq_2020 <- freq(forest_2020_class)  # Frequenza dei pixel per classe (pacchetto terra) nel 2020.
freq_2023 <- freq(forest_2023_class)  # Frequenza dei pixel per classe (pacchetto terra) nel 2023.

# Determiniamo il numero totale di pixel per ogni immagine classificata.
# Questa informazione è necessaria per calcolare le percentuali.
total_2017 <- ncell(forest_2017_class)  # Totale pixel nell'immagine classificata del 2017 (pacchetto terra).
total_2020 <- ncell(forest_2020_class)  # Totale pixel nell'immagine classificata del 2020 (pacchetto terra).
total_2023 <- ncell(forest_2023_class)  # Totale pixel nell'immagine classificata del 2023 (pacchetto terra).

# Calcoliamo la percentuale di pixel per ciascuna classe rispetto al totale.
# Questo rende i risultati più facili da interpretare.
perc_2017 <- (freq_2017$count / total_2017) * 100  # Percentuali delle classi nel 2017 (conteggio / totale * 100).
perc_2020 <- (freq_2020$count / total_2020) * 100  # Percentuali delle classi nel 2020.
perc_2023 <- (freq_2023$count / total_2023) * 100  # Percentuali delle classi nel 2023.

# Creiamo una tabella dei dati per organizzare le percentuali delle classi nei tre anni.
# Questo facilita l'analisi e la visualizzazione.
class_labels <- c("foresta densa", "copertura rada", "suolo esposto")  # Etichette per le 3 classi definite.
data <- data.frame(class_labels, perc_2017, perc_2020, perc_2023)  # Dataframe per contenere le percentuali.

# Creiamo grafici a barre per rappresentare le percentuali delle classi nei tre anni.
plot_2017 <- ggplot(data, aes(x = class_labels, y = perc_2017, fill = class_labels)) +
  geom_bar(stat = "identity", color = "black") +  # Grafico a barre per il 2017, dove stat="identity" usa i dati esistenti senza fare calcoli; color="black" aggiunge un bordo nero alle barre.
  ylim(0, 100) +  # Limiti dell'asse y da 0 a 100%.
  scale_fill_manual(values = c("#2E8B57", "#FFD700", "#D2691E")) +  # Colori definiti manualmente per le classi.
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

# Combiniamo i grafici per un confronto visivo immediato.
plot_2017 + plot_2020 + plot_2023  # Combina i tre grafici a barre in un'unica visualizzazione.

# Riorganizziamo i dati in formato lungo per creare un grafico che mostri l'evoluzione temporale delle classi.
data_long <- data.frame(
  Anno = rep(c(2017, 2020, 2023), each = 3),  # Ripetiamo gli anni per ogni classe.
  Classe = rep(class_labels, times = 3),  # Ripetiamo le classi per ogni anno.
  Percentuale = c(perc_2017, perc_2020, perc_2023)  # Percentuali combinate in un unico vettore.
)

# Creiamo un grafico a linee per visualizzare i cambiamenti nelle classi vegetative nel tempo.
ggplot(data_long, aes(x = Anno, y = Percentuale, color = Classe, group = Classe)) +
  geom_line(size = 1.2) +  # Linee per rappresentare ogni classe lungo il tempo.
  geom_point(size = 3) +  # Punti per evidenziare i valori nei diversi anni.
  scale_color_manual(values = c("#2E8B57", "#FFD700", "#D2691E")) +  # Palette di colori personalizzata.
  labs(
    title = "Andamento della vegetazione dal 2017 al 2023",
    x = "Anno",
    y = "Percentuale (%)",
    color = "Classe"
  ) +
  theme_minimal()  # Applichiamo un tema minimale per una grafica chiara.

