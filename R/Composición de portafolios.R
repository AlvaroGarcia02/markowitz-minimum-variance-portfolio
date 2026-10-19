#==============================================================================
#===                CÓDIGO R PARA COMPOSICIÓN DE PORTAFOLIOS                ===
#==============================================================================
#===          Teoría estadística del riesgo - Álvaro A. García G.           ===
#===                                 2026-1                                 ===
#==============================================================================

rm(list = ls())
library(quantmod) #para descargar los datos de Yahoo Finance
library(openxlsx) #para exportar archivos .xlsx
library(ggplot2) #para los violin plots

# Cargamos los datos de Yahoo Finance:
getSymbols("AAPL", #(Apple)
           src = "yahoo",
           from = "2017-01-01",
           to = "2018-12-31")
getSymbols("KO", #(Coca-Cola)
           src = "yahoo",
           from = "2017-01-01",
           to = "2018-12-31")
getSymbols("JPM", #(JP Morgan Chase)
           src = "yahoo",
           from = "2017-01-01",
           to = "2018-12-31")
getSymbols("CVX", #(Chevron)
           src = "yahoo",
           from = "2017-01-01",
           to = "2018-12-31")

# Calculamos los retornos logarítmicos con los datos de cierre (close):
AAPL_ret <- na.omit(diff(log(Cl(AAPL))))
KO_ret <- na.omit(diff(log(Cl(KO))))
JPM_ret <- na.omit(diff(log(Cl(JPM)))) 
CVX_ret <- na.omit(diff(log(Cl(CVX))))

# Gráficos:
ggplot(AAPL_ret,
       aes(x = "", y = AAPL_ret)) +
  geom_violin(fill = "gray30", alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Retorno"
  ) +
  theme_minimal()
ggplot(KO_ret,
       aes(x = "", y = KO_ret)) +
  geom_violin(fill = "firebrick2", alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Retorno"
  ) +
  theme_minimal()
ggplot(JPM_ret,
       aes(x = "", y = JPM_ret)) +
  geom_violin(fill = "steelblue4", alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Retorno"
  ) +
  theme_minimal()
ggplot(CVX_ret,
       aes(x = "", y = CVX_ret)) +
  geom_violin(fill = "darkolivegreen3", alpha = 0.5) +
  geom_boxplot(width = 0.12) +
  geom_jitter(width = 0.03, alpha = 0.25) +
  labs(
    x = "",
    y = "Retorno"
  ) +
  theme_minimal()

# Almacenamos en dataframes los retornos y los pasamos a .xlsx:
Apple_ret <- data.frame(Apple = AAPL_ret)
CocaCola_ret <- data.frame(CocaCola = KO_ret)
JPMorgan_ret <- data.frame(JPMorgan = JPM_ret)
Chevron_ret <- data.frame(Chevron = CVX_ret)
write.xlsx(
  Apple_ret,
  file = "C:/Users/Álvaro García/OneDrive/Documentos/R/a62 Composición de portafolios - TER - 2026-1/Apple.xlsx",
  rowNames = FALSE
)
write.xlsx(
  CocaCola_ret,
  file = "C:/Users/Álvaro García/OneDrive/Documentos/R/a62 Composición de portafolios - TER - 2026-1/CocaCola.xlsx",
  rowNames = FALSE
)
write.xlsx(
  JPMorgan_ret,
  file = "C:/Users/Álvaro García/OneDrive/Documentos/R/a62 Composición de portafolios - TER - 2026-1/JPMorgan.xlsx",
  rowNames = FALSE
)
write.xlsx(
  Chevron_ret,
  file = "C:/Users/Álvaro García/OneDrive/Documentos/R/a62 Composición de portafolios - TER - 2026-1/Chevron.xlsx",
  rowNames = FALSE
)

# Almacenamos los retornos en un solo dataframe:
retornos <- data.frame(
  Apple = AAPL_ret,
  CocaCola = KO_ret,
  JPMorgan = JPM_ret,
  Chevron = CVX_ret
) 

# Matriz de varianzas y covarianzas:
Sigma <- cov(retornos)

# Cálculo del portafolio de mínima varianza:
uno <- rep(1, 4)
delta = as.numeric(1/(t(uno)%*%solve(Sigma)%*%uno))
omega_c = delta*solve(Sigma)%*%uno; omega_c

# Varianza de la cartera equiponderada vs. cartera diversificada:
omega = rep(1/4, 4)
t(omega)%*%Sigma%*%omega
t(omega_c)%*%Sigma%*%omega_c

# Para las referencias:
citation()
