"Dieses Skript berechnet Korrelation r_xy und Bestimmtheitsmaß R^2 
für 'Beispieldatensatz.csv'.

Lösung zu Aufg. 13 der Selbstkontrollfragen für 
2_Korrelation (ALM SoSe 2022)"

# Laden des Beispieldatensatzes
fname = file.path(getwd(), "Daten", "Beispieldatensatz.csv")
D     = read.table(fname, sep = ",", header = TRUE)                             
n     = length(D$x_i)                                     # n (= length(D$y_i))

# ------Stichprobenstatistiken (incl. Korrelation)------------------------------
x_bar = (1/n)*sum(D$x_i)                                  # Stichprobenmittel x
y_bar = (1/n)*sum(D$y_i)                                  # Stichprobenmittel y
s_x   = sqrt(1/(n-1)*sum((D$x_i - x_bar)^2))              # Stichprobenstandardabweichung x
s_y   = sqrt(1/(n-1)*sum((D$y_i - y_bar)^2))              # Stichprobenstandardabweichung y
c_xy  = 1/(n-1) * sum((D$x_i - x_bar) * (D$y_i - y_bar))  # Stichprobenkovarianz
r_xy  = c_xy/(s_x * s_y)                                  # Stichprobenkorrelation 

# ------Erklärte Werte der Ausgleichsgeraden (y_hat)----------------------------
beta_1_hat = c_xy / s_x^2                                 # beta_0 (Parameter der Ausgleichsgerade)
beta_0_hat = y_bar - beta_1_hat * x_bar                   # beta_1 (Parameter der Ausgleichsgerade)
D$y_hat = beta_0_hat + beta_1_hat * D$x_i                 # Funktionswerte der Ausgleichsgeraden/erklärte Werte (y_hat)

# ------Berechnung R^2 nach Definition------------------------------------------
SQT = sum((D$y_i - y_bar)^2)                              # Total sum of squares
SQE = sum((D$y_hat - y_bar)^2)                            # Explained sum of sqaures
R_sqr_def = SQE/SQT                                       # Bestimmtheitsmaß

# ------Berechnung R^2 über Stichprobenkorrelationskoeffizienten----------------
R_sqr = r_xy^2                                            # Bestimmtheitsmaß

# ------Ausgabe-----------------------------------------------------------------
cat("r_xy", r_xy, 
    "\n SQT", SQT, 
    "\n SQE", SQE, 
    "\n R_sqr wie Def", R_sqr_def,
    "\n R_sqr mit r_xy", R_sqr)