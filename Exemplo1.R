#--------------------------- Exemplo do TileStats ----------------------------------#

install.packages(c("dplyr", "ggplot2", "lme4", "lmerTest"))
lapply(c("dplyr", "ggplot2", "lme4", "lmerTest"), library, character.only = TRUE)

# Data frame 
df <- data.frame(
  Weight = c(102, 96, 83, 79, 97, 93, 79, 77, 95, 87, 78, 75, 93, 85, 74, 72),
  Subjects = rep(1:4, times = 4),
  Diet = rep(rep(c("A", "B"), each = 2), times = 4),
  Weeks = rep(0:3, each = 4))

# Ajustar modelos de regressão linear usando modelos mistos
M0 <- lmer(Weight ~ Weeks + (1+Weeks|Subjects), REML=F, data=df)  # Intercepto aleatório e inclinação fixa
summary(M0)

M1 <- lmer(Weight ~ Weeks + (1|Subjects), REML=F, data=df)  # Intercepto aleatório e inclinação aleatória
summary(M1)

ranef(M0)  # Extrai os efeitos aleatórios ajustados (individuais)
coef(M0)   # Extrai os coeficientes 
predict(M0) # Extrai os valores preditos

# plot dos residuos 
plot(M0)

# Extrair o intercepto e inclinação dos efeitos fixos para o plot
intercepto = fixef(M0)["(Intercept)"]; intercepto
inclinacao = fixef(M0)["Weeks"]; inclinacao

# Plot das diferentes linhas de regressão
ggplot(df, aes(Weeks, Weight)) +
  geom_point(size=1) +               
  geom_line(aes(y=predict(M0), group=Subjects, color=factor(Subjects)), size=1) + 
  geom_point(aes(color=factor(Subjects)), size=1) +
  geom_abline(intercept=intercepto, slope=inclinacao, color="black", linetype="solid", size = 1) +          
  labs(x="Weeks", y="Weight (kg)", color="Subjects") +
  theme_gray()
