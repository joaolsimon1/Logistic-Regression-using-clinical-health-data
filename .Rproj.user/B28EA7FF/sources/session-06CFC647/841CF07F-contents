########## IMPORTANDO DADOS

library(haven)
library(tidyverse)
library(pastecs)
library(knitr)
library(viridis)
library(hrbrthemes)



##### IMPORTANDO DADOS
dados_tabagismo <- read_sas("~/Pessoal/Bioestatística/exercicio_shhs_180321.sas7bdat", NULL)



####### VISUALIZANDO OS DADOS
kable(head(dados_tabagismo))


##### SEPARANDO AS VARIÁVEIS NUMÉRICAS
var_num <- dados_tabagismo %>% select(-c("smoking", "activity", "chd", "id"))


#### DESCRITIVA
aux <- stat.desc(var_num)

aux <- aux[c(4, 5, 8, 9, 12:14), ]
rownames(aux) <- c("Mínimo", "Máximo", "Mediana", "Média", "Variância", "Desvio Padrão", "Coeficiente de Variação")
kable(aux, digits = 3, align = "c", caption = "Principais Estatísticas descritivas do banco de dados")


aux2 <- gather(var_num)
colnames(aux2) <- c("Variável", "Valor")

# creating a plot
ggplot(aux2) +
  geom_boxplot(aes(x = Variável, y = Valor, colour = Variável)) +
  facet_wrap(~Variável, scale = "free") +
  ggtitle("Variáveis contínuas") +
  theme_ipsum()






##### TRANSFORMANDO AS VARIAVEIS EM CATEGORICAS
var_cat <- c("smoking", "activity", "chd")

aux1 <- dados_tabagismo %>% select(c(var_cat))

aux1 <- lapply(aux1, function(x) as.factor(x))

aux1 <- as.data.frame(aux1)

dados_tabagismo[, var_cat] <- aux1


####### GRÁFICO VARIÁVEIS CATEGÓRICAS
dados_tabagismo %>%
  select("smoking", "activity", "chd") %>%
  gather(key = "variavel", value = "valor", smoking, activity, chd) %>%
  ggplot() +
  geom_bar(mapping = aes(x = valor, fill = variavel)) +
  scale_fill_viridis(discrete = T) +
  ggtitle("Studying 4 species..") +
  theme_ipsum() +
  xlab("")



str(dados_tabagismo)



modelo1 <- glm(chd ~ totchol, family = binomial(logit), data = dados_tabagismo)
summary(modelo1)


modelo2 <- glm(chd ~ totchol + age, family = binomial(logit), data = dados_tabagismo)
summary(modelo2)


anova(modelo1, modelo2, test = "Chisq") # teste da razão de verossimilhança entre dois modelos encaixados utilizando a distribuição Qui - Quadrado

cbind(RC = exp(log_mod$coefficients), exp(confint(log_mod)))


exp(coef(modelo2))
exp(confint(modelo2))


family(modelo1)$linkinv



modelo3 <- glm(chd ~ totchol + age + systol + smoking + activity, family = binomial(logit), data = dados_tabagismo)

summary(modelo3)





dados_tabagismo %>%
  select(-c("id")) %>%
  cor()

cor(dados_tabagismo[, 2:8])

unique(dados_tabagismo$chd)




### categorização da variável colesterol
quantile(dados_tabagismo$totchol, )[5]
totchol_cat <- NULL


quantis <- quantile(dados_tabagismo$totchol)


dados_tabagismo$totchol_cat <- ifelse(dados_tabagismo$totchol <= quantis[2], 1,
  ifelse(dados_tabagismo$totchol <= quantis[3], 2,
    ifelse(dados_tabagismo$totchol <= quantis[4], 3, 4)
  )
)

