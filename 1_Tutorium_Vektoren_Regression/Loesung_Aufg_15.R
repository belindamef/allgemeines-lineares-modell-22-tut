"Dieses Skript betimmt die ML Parameterschätzer des einfachen linearen Modells
für den Beispieldatensatz '1_Regression.csv' und visualisiert die 
Regressionsgeraden.

Lösung zu Aufg. 15 der Selbstkontrollfragen für 
1_Regression (ALM SoSe 2022)"

# Laden von R packages
library(car)  # Enthält Funktionen wie lm() zur Parameterschäztung
library(latex2exp)  # Enthält TeX() für Schriftformatierung

# Einlesen des Beispieldatensatzes
fname       = file.path(getwd(), "1_Daten", "1_Regression.csv")
D           = read.table(fname, sep = ",", header = TRUE)

# -------- Lösungsmöglichkeit A) mit Formeln--------------------------
# Stichprobenstatistiken
n           = length(D$y_i)                                      # Anzahl Datenpunkte
x_bar       = mean(D$x_i)                                        # Stichprobenmittel der x_i-Werte
y_bar       = mean(D$y_i)                                        # Stichprobenmittel der y_i-Werte
s2x         = var(D$x_i)                                         # Stichprobenvarianz der  x_i-Werte
cxy         = cov(D$x_i, D$y_i)                                  # Stichprobenkovarianz der (x_i,y_i)-Werte

# Parameteterschätzer (nach dem Theorem der ML Schätzung)
beta_1_hat  = cxy/s2x                                            # \hat{\beta}_1, Steigungsparameter
beta_0_hat  = y_bar - beta_1_hat*x_bar                           # \hat{\beta}_0, Offset Parameter
sigsqr_hat  = (1/n)*sum((D$y_i-(beta_0_hat+beta_1_hat*D$x_i))^2) # Varianzparameter

# Ausgabe
cat("beta_0_hat:"  , beta_0_hat,
    "\nbeta_1_hat:", beta_1_hat,
    "\nsigsqr_hat:", sqrt(sigsqr_hat))


# -------- Lösungsmöglichkeit B) mit "car" package------------------------------
# Analyse mit lm()
model       = lm(formula = D$y_i ~ D$x_i, data = D)
print(model)


# --------Visualisierung -------------------------------------------------------
# Datenwerte
plot(
  D$x_i,
  D$y_i,
  pch         = 16,
  xlab        = "Anzahl Therapiestunden (x)",
  ylab        = "Symptomreduktion (y)",
  xlim        = c(0,21),
  ylim        = c(-10, 40),
  main        = TeX("$\\hat{\\beta}_0 =  -6.19, \\hat{\\beta}_1 = 1.66$"))

# Ausgleichsgerade
abline(
  coef        = c(beta_0_hat, beta_1_hat),
  lty         = 1,  # linetype (0: Punkt, 1:Linie, 2:gestrichelte Linie, usw.)
  col         = "black")

# Legende
legend(
  "topleft",
  c(TeX("$(x_i,y_i)$"), TeX("$f(x) = \\hat{\\beta}_0 + \\hat{\\beta}_1x$")),
  lty        = c(0,1),
  pch        = c(16, NA),  # plot character (0: Vierreck, 1:Kringel, ... 16: ausgefüllter Kreis, usw.)
  bty        = "n")  # boxtype ("o": komplette box, "n": keine box, "7": top + right, L: bottom + left, usw.)

# Plot speichern
dev.copy2pdf(
  file        = file.path(getwd(), "plot_aufg_15.pdf"),
  width       = 4,
  height      = 4)