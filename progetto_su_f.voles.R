# Stampiamo i dati del dataset
plot(f.voles)
# La forma dei dati è elissoidale, è quindi più pertinente usare la distanza di Mahalanobis.


# 1 - Calcolare varie distanze --------------------------------------------


# Usiamo prima la distanza di Mahalanobis
library(StatMatch)
californicus <- subset (f.voles, Species == "californicus") 
californicus <- californicus[,-1]
ochrogaster <- subset (f.voles, Species == "ochrogaster")
ochrogaster <- ochrogaster[,-1]
d <- mahalanobis.dist (data.x = californicus, data.y = ochrogaster)
d
plot (d, pch = 19, col = rainbow (100)[cut (d, breaks = 100)], main="Plot della matrice derivata dal metodo di Mahalanobis", xlab = "Indice di californicus", ylab = "Indice di ochrogaster")
# Questa matrice misura la distanza tra ogni osservazione di californicus e la media di ochrogaster. Il fatto che i punti siano distribuiti uniformemente sulla diagonale implica che i gruppi di dati sono abbastanza diversi tra loro e non c'è correlazione tra le variabili.

# Visualizziamo i dati in uno spazio bidimensionale con le componenti principali
pca <- prcomp(rbind(californicus, ochrogaster))
plot(pca$x[, 1:2], col = factor(f.voles$Species), pch = 19, main = "Plot dei dati con le componenti principali")
# I punti neri corrispondono alle osservazioni di californicus e quelli rossi a ochrogaster, siccome i punti sono molto disposti in modo molto diverso, concludiamo che le due specie sono in generale molto diverse, e che le principali componenti riescono a catturare queste differenze.

# Ora usiamo la distanza di Euclide
library(fields)
e <- rdist (californicus, ochrogaster)
as.matrix(e)
plot (e, pch = 19, col = rainbow (100)[cut (e, breaks = 100)], main="Plot della matrice derivata dal metodo di Euclide", xlab = "Indice di californicus", ylab = "Indice di ochrogaster")
# La matrice derivata dalla distanza di Euclide misura la distanza tra ogni osservazione di californicus ed ogni osservazione di ochrogaster, usando la formula della radice quadrata della somma dei quadrati della differenza delle due variabili.
# Nel grafico della distanza di Euclide i punti sono più schiacciati verso l’origine, mentre nel grafico della distanza di Mahalanobis i punti sono più distribuiti lungo l’asse diagonale. Questo potrebbe indicare che la distanza di Mahalanobis è più sensibile alle differenze tra i due gruppi, mentre la distanza di Euclide è più influenzata dalla varianza complessiva dei dati.


# Ora usiamo la distanza di Manhattan
m <- dist(rbind(californicus, ochrogaster), method = "manhattan")
as.matrix(m)
plot(m, pch = 19, col = rainbow(100)[cut(m, breaks = 100)],
     main = "Plot della matrice derivata dal metodo di Manhattan",
     xlab = "Indice di californicus", ylab = "Indice di ochrogaster")
# La matrice derivata dalla distanza di Manhattan misura la distanza tra ogni osservazione di californicus ed ogni osservazione di ochrogaster, usando la somma dei valori assoluti delle differenze tra le variabili.
# Il fatto che il grafico stampi punti molto sparsi ed irregolari significa che la distanza di Manhattan non è adeguata perché non tiene conto della varianza complessiva e della forma elissoidale dei dati.



# Usiamo la distanza di Minkowski con p = 3 e p = 100
p <- 3  # Ordine della distanza di Minkowski
r <- dist(rbind(californicus, ochrogaster), method = "Minkowski", p = p)
r <- as.matrix(r)
r
plot(r, pch = 19, col = rainbow(100)[cut(as.vector(r), breaks = 100)],
     main = paste("Plot della matrice derivata dal metodo di Minkowski (p =", p, ")"),
     xlab = "Indice di californicus", ylab = "Indice di ochrogaster")



p <- 100  # Ordine della distanza di Minkowski
o <- dist(rbind(californicus, ochrogaster), method = "Minkowski", p = p)
o <- as.matrix(o)
o
plot(o, pch = 19, col = rainbow(100)[cut(as.vector(o), breaks = 100)],
     main = paste("Plot della matrice derivata dal metodo di Minkowski (p =", p, ")"),
     xlab = "Indice di californicus", ylab = "Indice di ochrogaster")
# Questa matrice misura la distanza tra ogni osservazione di ochrogaster e californicus, usando la formula della radice p-esima della somma dei valori assoluti della somma delle differenze tra le variabili elevate alla potenza di p.
# Questo significa che ci sono alcune osservazioni che hanno una distanza molto bassa tra loro, il che indica una grande similarità, mentre altre hanno una distanza molto alta tra loro, il che indica una grande dissimilarità. Questo potrebbe riflettere la variabilità delle caratteristiche morfologiche delle due specie.


# 2 - Usare criteri di raggruppamento gerarchici --------------------------
# Usiamo il legame singolo
# Calcoliamo la distanza minima tra le due specie usando la distanza di Mahalanobis
min(d)
# Questo significa che ci sono due osservazioni, una da ogni specie, che hanno una distanza di Mahalanobis vicina ad 1, il che indica grande similarità fra loro.
# Identifichiamoli con la funzione which()
which(d == min(d), arr.ind = TRUE)
# Questo significa che l'osservazione 18 di californicus e quella 21 di ochrogaster hanno distanza minima tra due specie.


# Usiamo il legame di Ward
r <- dist(rbind(californicus, ochrogaster))
w <- hclust(r, method="ward.D2")
plot(w, main="Dendrogramma con il legame di Ward")
# Possiamo anche tagliare il dendrogramma per ottenere un numero desiderato di cluster, per esempio ad altezza 100
h <- 100
cutree(w, h = h)
rect.hclust(w, h=h)



# Applichiamo il legame completo
c <- hclust(r, method = "complete")
plot(c, main = "Dendrogramma con il legame completo")
h <- 100
cutree(c, h = h)
rect.hclust(c, h = h) # Evidenziamo tutti i cluster che si formano ad altezza 100
# l legame di Ward ha suddiviso i dati in 7 cluster, mentre il legame completo ha suddiviso i dati in 3 cluster. Questo significa che il legame di Ward ha creato dei sottogruppi all’interno delle due specie di arvicole, mentre il legame completo ha mantenuto le due specie separate e ha creato un gruppo intermedio. Questo potrebbe indicare che il legame di Ward ha rilevato una maggiore eterogeneità tra le osservazioni, mentre il legame completo ha privilegiato una maggiore omogeneità.


# 3 - Usare k-means -------------------------------------------------------
library(cluster)
f.voles <- f.voles[,-1]
f.voles <- scale(f.voles)
fv.ss <- sapply(1:10, function(k){kmeans(f.voles, k, nstart = 25)$tot.withinss})
plot(1:10, fv.ss, type = "b", xlab = "Numero di cluster", ylab = "Somma dei quadrati delle distanze", main = "Grafico del punto di flesso") 
# Notiamo che il punto di flesso si trova in k = 2, quindi dobbiamo usare 2 cluster
fv.km <- kmeans(f.voles,2,nstart=25)
fv.clusplot <- clusplot(f.voles, fv.km$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0, main="Due cluster ed i loro dati")
# In questo grafico notiamo che i dati sono suddividi in 2 gruppi: in un gruppo i dati sono similissimi tra di loro così come nell'altro gruppo, ma poche osservazioni sono simili tra i due gruppi, il grafico poi è accurato al 80.73% nella partizione dei dati, una percentuale abbastanza alta.
fv.silplot <- silhouette(fv.km$cluster, dist(f.voles))
plot(fv.silplot, main="Grafico del valore di Silhouette per ogni dato con 2 cluster")
# Il coefficiente di Silhouette misura quanto il clustering ha partizionato bene i dati in funzione della loro somiglianza, senza usare alcuna informazione a priori sulle classi. Il coefficiente varia da -1 ad 1 per ogni dato. Il fatto che pochi dati siano vicini ad 1 nel grafico indica che i dati avevano molta variabilità interna, anche se più o meno simili.


# Proviamo ad usare il k-means con k = 4
fv.km2 <- kmeans(f.voles, 4, nstart=25)
fv.clusplot2 <- clusplot(f.voles, fv.km2$cluster, color=TRUE, shade=TRUE, labels=2, lines = 0, main="Quattro cluster ed i loro dati")
# Notiamo che anche in questo caso l'accuratezza della suddivisione è accurata al 80,73%
fv.silplot2 <- silhouette(fv.km2$cluster, dist(f.voles))
plot(fv.silplot2, main="Grafico del valore di Silhouette per ogni dati con 4 cluster")
# Come previsto, quattro cluster non sono adeguati, infatti sebbene alcuni dati si avvicinino ad 1, alcuni sono anche dietro lo zero.


# 4 - Estrapoliamo altre informazioni dal metodo k-means con 2 e 4 cluster --------
# Questi sono i valori della devianza di ciascun gruppo
df <- data.frame(Devianza = fv.km$withinss, Cluster = 1:2)
barplot(df$Devianza, names.arg = df$Cluster, xlab = "Cluster", ylab = "Devianza", main = "Devianza per cluster")

# Adesso adottiamo il modello di regressione tra le devianza e i 4 cluster
df <- data.frame(Devianza = fv.km$withinss, Cluster = 1:4)
x <- df$Cluster
y <- df$Devianza
modello <- lm(formula = y ~ x, x = TRUE, y = TRUE)
res <- summary.lm(object=modello, correlation = TRUE)
res

plot(modello <- lm(formula = y ~ x, x = TRUE, y = TRUE))
# Grafico 1 - Il primo grafico serve per verificare se ci sono relazioni non lineari tra le variabili, o se ci sono dei residui atipici. Nel nostro caso, vi è una linea a zig zag con quattro punti che rappresentano i cluster anziché una linea retta, questo dimostra che il modello non riesce a spiegare bene la varianza della devianza.
# Grafico 2 - Il Secondo grafico serve per verificare se i residui seguono una distribuzione normale, se il grafico mostra una linea retta con dei punti molto vicini ad essa, questo significa che il modello rispetta l'assunzione di normalità dei residui.
# Grafico 3 - Il terzo grafico serve a verificare se i residui del modello hanno una varianza costante, che è una assunzione della regressione lineare, nel nostro caso il grafico mostra altro rispetto ad una linea orizzontale senza pattern evidenti, ciò significa che il modello viola l’assunzione di omoschedasticità dei residui, perché la varianza dei residui dipende dai valori predetti.
# Grafico 4 - Questo grafico ti serve per identificare le osservazioni influenti, cioè quelle che hanno un grande impatto sul modello e possono alterarne i risultati. Nel caso, il grafico mostra quattro punti che non superano la soglia della distanza di Cook. Questo significa che non ci sono osservazioni influenti nel tuo modello.
res <- influence.measures(model = modello)
res
# Il modello ha due osservazioni influenti, la prima e la quarta, che hanno un valore di leva alto e una distanza di Cook alta. Questo significa che queste osservazioni hanno un grande impatto sul tuo modello.
res$is.inf
# I dati estrapolati indicano quali osservazioni sono influenti nel modello di regressione: la tabella mostra che "dfb.1_", cioè il cambiamento del coefficiente di regressione quando l'osservazione è eliminata, è falsa per tutte e quattro osservazioni, "dffit", cioè il cambiamento del valore predetto quando l'osservazione è eliminata, è falsa per tutti; "cov.r" misura il cambiamento della matrice di covarianza quando l'osservazione è eliminata, è vero per la prima osservazione e per la quarta; "cook.d" misura il cambiamento nell'intero vettore dei valori predetti quando l'osservazione è eliminata, è falsa per tutte e 4 le osservazioni, così come la leva.


# Supponendo che i cluster siano 24
df <- data.frame(Devianza = fv.km$withinss, Cluster = 1:24)
x <- df$Cluster
y <- df$Devianza
modello <- lm(formula = y ~ x, x = TRUE, y = TRUE)
res <- summary.lm(object=modello, correlation = TRUE)
res
plot(modello <- lm(formula = y ~ x, x = TRUE, y = TRUE))
# Adesso con 24 cluster notiamo che il modello riesce a spiegare bene la varianza della devianza perché la linea è quasi retta, che l'omoschedasticità dei residui è soddisfatta, cioè i residui hanno varianza costante, e neanche in questo caso ci sono osservazioni influenti che possono alterare il modello. I residui non seguono una distribuzione normale.

