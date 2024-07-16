rm(list=ls(all=TRUE))

install.packages("sommer")
library(sommer)
  
vignette("v1.sommer.quick.start")
vignette("v2.sommer.changes.and.faqs")
vignette("v3.sommer.qg")
vignette("v4.sommer.gxe")

# Package sommer: https://cran.r-project.org/package=sommer

#------------------- Quick start for the sommer package ----------------------#

# 1) Diferentes modelos no pacote, ajustado por REML:

## 1.1 - Modelos univariados de variância homogênea: modelo de simetria composta (CS).

# Este modelo refere-se a modelos de resposta única, onde uma variável de interesse (como genótipos) deve ser analisada em interação com um segundo efeito aleatório (por exemplo, ambientes). 
# A premissa é que, através dos diferentes ambientes, os genótipos apresentam o mesmo componente de variância.

# dados
data(DT_example)
DT <- DT_example
head(DT); tail(DT)

# solving for r records
# ajuste do modelo
ans1r <- mmer(Yield~Env, random= ~ Name + Env:Name, rcov= ~ units, data=DT, verbose = FALSE)   # função "mmer" para resíduos
summary(ans1r)           # resumo geral 
summary(ans1r)$varcomp   # para obter apenas os componentes de variância-covariância estimados pelo modelo

# solving for c coefficients
# ajuste do modelo
ans1c <- mmec(Yield~Env, random= ~ Name + Env:Name, rcov= ~ units, data=DT, verbose = FALSE)   # função "mmec" para coeficientes (parâmetros fixos e aleatórios do modelo.)
summary(ans1c) 

# Ambos os modelos estão ajustando uma estrutura de simetria composta para dados com efeitos fixos (Env) e aleatórios (Name e Env:Name), 
# mas diferem na abordagem utilizada para resolver as equações do modelo misto: "mmer" para resíduos e "mmec" para coeficientes.

# VarComp: Componente de Variância (representa a variabilidade associada ao efeito aleatório específico)
# VarCompSE: Erro Padrão do Componente de Variância (quanto menor o valor, maior a precisão da estimativa)
# Zratio: razão Z calculada para o componente de variância correspondente (valores maiores indicam uma maior discrepância em relação ao erro padrão e, portanto, sugerem uma maior significância estatística.)
# Constraint: indica restrições para obter as variâncias durante a estimativa (para garantir que os componentes de variância permaneçam positivos)

#-----------------------------------------------------------------------------#
## 1.2 - Modelos univariados de variância heterogênea: modelo CS+DIAG (também chamado de modelo CS heterogêneo).

# Muitas vezes, em ensaios multiambientais, a suposição de que a variância genética ou a variância residual é a mesma em todos os locais pode ser errônea. 
# Por isso, especificar um componente genético geral e uma variância genética específica de cada local é o caminho a seguir.

# ajuste do modelo (resíduos)
ans2r <- mmer(Yield~Env, random= ~Name + vsr(dsr(Env),Name), rcov= ~ vsr(dsr(Env),units), data=DT, verbose = FALSE)
summary(ans2r)

# ajuste do modelo (coeficientes)
DT=DT[with(DT, order(Env)), ]  # reorganiza o conjunto de dados
ans2c <- mmec(Yield~Env, random= ~Name + vsc(dsc(Env),isc(Name)), rcov= ~ vsc(dsc(Env),isc(units)), data=DT, verbose = FALSE)
summary(ans2c)

# A função especial "atr" ou "diag" pode ser usada para indicar que há uma variância diferente para os genótipos em cada ambiente. 
# A diferença entre "atr" e "diag" é que a função "atr" pode ser usada para especificar os níveis ou ambientes específicos onde a variância é diferente.

#-----------------------------------------------------------------------------#
## 1.3 - Modelos de variância não estruturada: modelo não estruturado (US)

# O modelo US assume que entre os níveis de um certo fator (ou seja, Ambientes) há uma estrutura de covariância de um segundo efeito aleatório (ou seja, Genótipos).

# ajuste do modelo (resíduos)
ans3r <- mmer(Yield~Env, random=~ vsr(usr(Env),Name), rcov=~vsr(dsr(Env),units), data=DT, verbose = FALSE)
summary(ans3r)

# ajuste do modelo (coeficientes)
DT=DT[with(DT, order(Env)), ]
ans3c <- mmec(Yield~Env, random=~ vsc(usc(Env),isc(Name)), rcov=~vsc(dsc(Env),isc(units)), data=DT, verbose = FALSE)
summary(ans3c)

# Como pode ser visto, "usr(Env)" indica que os genótipos (Nome) podem ter uma estrutura de covariância entre ambientes (Env).
  
#-----------------------------------------------------------------------------#
## 1.4 - Modelos multivariados de variância homogênea

# Atualmente, há um grande impulso para modelos de resposta múltipla. Isso é motivado pela correlação que certas variáveis escondem e que poderiam beneficiar na perspectiva de previsão. 
# Para especificar modelos multivariados, a resposta requer o uso da função "cbind()" na resposta, e das funções "usr(trait)", "diag(trait)" ou "atr(trait)" na parte aleatória do modelo.

DT$EnvName <- paste(DT$Env,DT$Name); DT$EnvName
DT$Yield <- as.vector(scale(DT$Yield)); DT$Yield
DT$Weight <- as.vector(scale(DT$Weight)); DT$Weight 

# ajuste do modelo (resíduos)
ans4r <- mmer(cbind(Yield, Weight) ~ Env, random= ~ vsr(Name, Gtc=unsm(2)), rcov= ~ vsr(units, Gtc=diag(2)), data=DT, verbose = FALSE)
summary(ans4r)

DT2 <- reshape(DT, idvar = c("Name","Env","Block"), varying = list(6:7), v.names = "y", direction = "long", timevar = "trait", times = colnames(DT)[6:7])
DT2$trait <- as.factor(DT2$trait)

# ajuste do modelo (coeficientes)
# ans4c <- mmec(y ~ Env:trait,random= ~ vsc(usc(trait),isc(Name)), rcov= ~ vsc(dsc(trait),isc(units)), returnParam = T, data=DT2, verbose = T)
# summary(ans4c)

# Pode-se notar que foi adicionado o "usr(trait)" após os efeitos aleatórios, isso serve para indicar a estrutura que deve ser assumida no modelo multivariado. 
# O "diag(trait)" utilizado após um efeito aleatório (por exemplo, Nome) indica que, para os traços modelados (Rendimento e Peso), não há componente de covariância a ser estimado. 
# Por outro lado, o "usr(trait)" assume que, para esse efeito aleatório, há um componente de covariância a ser estimado (ou seja, a covariância entre Rendimento e Peso para o efeito aleatório Nome). 
# O mesmo se aplica para a parte residual (rcov). Qualquer número de efeitos aleatórios pode ser especificado com diferentes estruturas.

#-----------------------------------------------------------------------------#
## 2) Funções especiais para modelos especiais

# Modelos de regressão aleatória: Para ajustar modelos de regressão aleatória, o usuário pode usar a função leg() para ajustar polinômios de Legendre. 
# Isso pode ser combinado com outras estruturas especiais de covariância, como dsr() / dsc(), usr() / usc(), etc.

# install.packages("orthopolynom")
library(orthopolynom)

# dados
data(DT_legendre)
DT <- DT_legendre
head(DT); tail(DT)

# ajuste do modelo (resíduos)
mRR2r<-mmer(Y~ 1 + Xf, random=~ vsr(usr(leg(X,1)),SUBJECT), rcov=~vsr(units), data=DT, verbose = FALSE)
summary(mRR2r)

# ajuste do modelo (coeficientes)
mRR2c<-mmec(Y~ 1 + Xf, random=~ vsc(usc(leg(X,1)),isc(SUBJECT)), rcov=~vsc(isc(units)), data=DT, verbose = FALSE)
summary(mRR2c)

# Aqui, uma covariável numérica X é usada para explicar a trajetória dos SUJEITOS e combinada com uma matriz de covariância não estruturada. Os detalhes podem ser encontrados na teoria.

#------------------------------------------------------------------------------#
## 3) Testes de razão de verossimilhança

# O teste de razão de verossimilhança (LRT) é uma boa maneira de investigar a significância de efeitos aleatórios ou componentes específicos de variância-covariância.

## 3.1 - Testando a significância de um componente de variância:

# Por exemplo, imagine que um pesquisador gostaria de saber se seu modelo melhora ao adicionar o efeito de um kernel espacial para capturar a tendência espacial no campo. 
# Seu modelo base pode se parecer com isto:

# dados
data(DT_cpdata)

DT <- DT_cpdata; head(DT); tail(DT)
GT <- GT_cpdata; GT
MP <- MP_cpdata; head(MP); tail(MP)

# mimic two fields
A <- A.mat(GT); A
mix1 <- mmer(Yield~1, random=~vsr(id, Gu=A) + 
               vsr(Rowf) + 
               vsr(Colf), rcov=~vsr(units), nIters=3, data=DT, verbose = FALSE)

# E o modelo com o kernel espacial é o seguinte:
mix2 <- mmer(Yield~1, random=~vsr(id, Gu=A) +
               vsr(Rowf) +
               vsr(Colf) +
               spl2Da(Row,Col), rcov=~vsr(units), nIters=3, data=DT,verbose = FALSE)

# Para testar se o segundo modelo traz valor, vamos ajustar o teste de razão de verossimilhança da seguinte forma:
lrt <- anova(mix1, mix2)
# Como pode ser visto, o teste não se mostrou muito significativo apesar do aumento na verossimilhança.

#------------------------------------------------------------------------------#
## 3.2 - Testando a significância de um componente de covariância:

# Às vezes, o pesquisador está mais interessado em saber se uma estrutura de covariância é relevante ou não. Suponha que temos dois modelos multi-características: 
# 1) ajustando sem covariância (independente) entre características, e 
# 2) ajustando a covariância genética entre rendimento e cor na seguinte população:

data(DT_example)
DT <- DT_example
DT$EnvName <- paste(DT$Env,DT$Name)
head(DT); tail(DT)

# ajuste do modelo
modelBase <- mmer(cbind(Yield, Weight) ~ Env,
                  random= ~ vsr(Name, Gtc=diag(2)),   # aqui está diag()
                  rcov= ~ vsr(units, Gtc=unsm(2)), nIters=3,
                  data=DT,verbose = FALSE)

modelCov <- mmer(cbind(Yield, Weight) ~ Env,
                 random= ~ vsr(usr(Env),Name, Gtc=unsm(2)),   # aqui está unsm()
                 rcov= ~ vsr(dsr(Env),units, Gtc=unsm(2)), nIters=3,
                 data=DT,verbose = FALSE)

lrt <- anova(modelBase, modelCov)

# Como pode ser visto, neste caso, ajustar a covariância entre os genótipos melhora consideravelmente o ajuste do modelo e a probabilidade da distribuição qui-quadrado é < 0,05. 
# Portanto, o modelo com a covariância é o modelo preferido.

#------------------------------------------------------------------------------#
## 4) Predição de médias

# O pacote sommer está equipado com uma função "predict()" que pode ser usada para calcular médias e erros padrão para efeitos fixos e aleatórios especificados nos modelos ajustados com as funções "mmer" e "mmec". 
# Usando o conjunto de dados "yatesoats", ajustaremos alguns efeitos fixos e aleatórios.

# dados
data(DT_yatesoats)
DT <- DT_yatesoats
head(DT); tail(DT)

# ajuste do modelo
m3 <- mmer(fixed=Y ~ V + N + V:N, random = ~ B + B:MP, rcov=~units, data = DT, verbose=FALSE)
summary(m3)

# Agora, o modelo pode ser usado junto com o argumento "classify" para obter as médias desejadas.
# o modelo inclui na fórmula fixa os termos "V" para variedade, "N" para tratamentos de nitrogênio e "V" para a interação entre variedade e nitrogênio.
# No exemplo a seguir, as médias para os tratamentos de nitrogênio são obtidas da seguinte forma:

Dt <- m3$Dtable; Dt

# primeiro efeito fixo apenas média
Dt[1,"average"] = TRUE

# segundo efeito fixo incluir
Dt[2,"include"] = TRUE

# terceiro efeito fixo incluir e média
Dt[3,"include"] = TRUE
Dt[3,"average"] = TRUE
Dt

pp=predict(object=m3, Dtable=Dt, D="N")
pp$pvals

#------------------- Quantitative genetics using the sommer package ----------------------#

## Tópicos em genética quantitativa

# 1) Cálculo de herdabilidade baseado em marcadores e não-marcadores:

# exemplo A

# O conjunto de dados a seguir possui 41 linhas de batata avaliadas em 5 locais ao longo de 3 anos em um desenho RCBD (Randomized Complete Block Design). 
# Mostramos como ajustar o modelo e extrair os componentes de variância para calcular h².

# dados
data(DT_example)
DT <- DT_example; DT
A <- A_example; A

# ajuste do modelo
ans1 <- mmec(Yield~1, random= ~ Name + Env + Env:Name + Env:Block, rcov= ~ units, nIters=3, data=DT, verbose = FALSE)
summary(ans1)

# Número de ambientes
(n.env <- length(levels(DT$Env)))
# vpredict(ans1, h2 ~ V1 / ( V1 + (V3/n.env) + (V5/(2*n.env))))  # Isso é uma estimativa da herdabilidade de sentido amplo.

# exemplo B

# Suponha que você tenha uma população (mesmo que não replicada) no campo, mas além disso, tenha marcadores genéticos. 
# Agora podemos ajustar o modelo e estimar a herdabilidade genômica que explica uma parte da variância genética aditiva (com alta densidade de marcadores σ²_Aditiva = σ²_marcadores).

# dados
data(DT_cpdata)
DT <- DT_cpdata
GT <- GT_cpdata
MP <- MP_cpdata
DT$idd <-DT$id; DT$ide <-DT$id

# olhar para os dados
A <- A.mat(GT); A    # matriz de relação aditiva
D <- D.mat(GT); D    # matriz de relação de dominância
E <- E.mat(GT); E    # matriz de relação epistática

# ajustar o modelo
ans.ADE <- mmer(color~1, random=~vsr(id,Gu=A) + vsr(idd,Gu=D), rcov=~units, nIters=3, data=DT,verbose = FALSE)
(summary(ans.ADE)$varcomp)

vpredict(ans.ADE, h2 ~ (V1) / (V1+V3))   # herdabilidade de sentido restrito
vpredict(ans.ADE, h2 ~ (V1+V2) / (V1+V2+V3))   # herdabilidade de sentido amplo

# Neste exemplo acima, mostramos como estimar os componentes de variância aditiva (σ²_A) e dominância (σ²_D) com base em marcadores, e como estimar a herdabilidade de sentido amplo (H²) e de sentido restrito (h²). 
# Note que utilizamos a função vsr(), onde o efeito aleatório dentro dos parênteses (ou seja, id, idd ou ide) possui uma matriz de covariância (A, D, ou E), que será especificada no argumento Gu da função vsr(). 
# Por favor, NÃO forneça o inverso, mas sim a matriz de covariância original.

#------------------------------------------------------------------------------#
# 2) Especificando variâncias heterogêneas em modelos univariados:

# Frequentemente, em ensaios multiambientes, a suposição de que a variância genética é a mesma em todos os locais pode ser muito ingênua. 
# Por isso, especificar um componente genético geral e uma variância genética específica para cada local é o caminho a seguir.
# Estimamos os componentes de variância para GCA2 e SCA especificando a estrutura de variância.

# dados
data(DT_cornhybrids)
DT <- DT_cornhybrids
DTi <- DTi_cornhybrids
GT <- GT_cornhybrids

# ajustar o modelo
modFD <- mmec(Yield~1, random=~ vsc(atc(Location,c("3","4")),isc(GCA2)), rcov= ~ vsc(dsc(Location),isc(units)), nIters=3, returnParam = F, data=DT, verbose = FALSE)
summary(modFD)

# No exemplo anterior, mostramos como a função atr() é usada no solver mmer(). Ao usar a função atr(), você pode especificar que, por exemplo, o GCA2 tem uma variância diferente em diferentes Locais, neste caso, nos locais 3 e 4, além de uma variância principal de GCA. 
# Isso é considerado um modelo CS + DIAG (simetria composta + diagonal).

# Além disso, outras funções podem ser adicionadas para ajustar modelos com estruturas de covariância, como o argumento Gu da função vsr() para indicar uma matriz de covariância (A, pedigree ou matriz de relacionamento genômico).

# dados
data(DT_cornhybrids)
DT <- DT_cornhybrids
DTi <- DTi_cornhybrids
GT <- as(GT_cornhybrids, Class = "dgCMatrix")
GT[1:4,1:4]
DT=DT[with(DT, order(Location)), ]

# ajustar o modelo
modFD <- mmec(Yield~1,random=~ vsc(atc(Location,c("3","4")),isc(GCA2),Gu=GT), rcov= ~ vsc(dsc(Location),isc(units)), nIters=3, data=DT, verbose = FALSE)
summary(modFD)

#------------------------------------------------------------------------------#
# 3) Usando o calculador "vpredict":

# Às vezes, o usuário precisa calcular razões ou funções de componentes de variância-covariância específicos e obter os erros padrão para tais parâmetros. 
# Exemplos desses parâmetros são as correlações genéticas, herdabilidades, etc. Usando o "CPdata", mostraremos como estimar a herdabilidade e o erro padrão usando a função "vpredict()" que utiliza o método delta para calcular esses parâmetros. 
# Isso pode ser estendido para qualquer combinação linear dos componentes de variância.

# dados 
data(DT_cpdata)
DT <- DT_cpdata
GT <- GT_cpdata
MP <- MP_cpdata

# olhando os dados
A <- A.mat(GT); A   # matriz de relação aditiva

# ajustar o modelo
ans <- mmer(color~1, random=~vsr(id,Gu=A), rcov=~units, nIters=3, data=DT, verbose = FALSE, dateWarning = FALSE)
summary(ans)

vpredict(ans, h2 ~ (V1) / (V1+V2))   # herdabilidade de sentido restrito

# O mesmo pode ser usado para modelos multivariados, consulte a documentação da função vpredict para ver mais exemplos.

#------------------- Fitting genotype by enviroment models in sommer ----------------------#

# O objetivo deste exemplo é mostrar como ajustar diferentes modelos de interação genótipo por ambiente (GxA) usando o pacote sommer:

# 1) Modelo de ambiente único (modelo simples)

# Um modelo de ambiente único é aquele que é ajustado quando o programa de melhoramento só pode se dar ao luxo de usar uma única localização, 
# deixando de fora as possíveis informações disponíveis de outros ambientes. Este será usado para expandir ainda mais para modelos GxA.

# dados
data(DT_example)
DT <- DT_example
A <- A_example

# ajustar o modelo
ansSingle <- mmer(Yield~1, random= ~ vsr(Name, Gu=A), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansSingle)

# ou
Ai <- as(solve(A), Class="dgCMatrix")
ansSingle <- mmec(Yield~1, random= ~ vsc(isc(Name), Gu=Ai), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansSingle)

# Neste modelo, o único termo a ser estimado é o do germoplasma (aqui chamado de Name). 
# Para fins de exemplo, adicionamos uma matriz de relacionamento entre os níveis do efeito aleatório "Name". 
# Esta é apenas uma matriz diagonal com tantas linhas e colunas quanto níveis presentes no efeito aleatório "Name", mas qualquer outra matriz de relacionamento não diagonal poderia ser usada.

#------------------------------------------------------------------------------#
# 2) Modelo de múltiplos ambientes: Modelo de efeito principal

# Um modelo de múltiplos ambientes é aquele que é ajustado quando o programa de melhoramento pode se dar ao luxo de usar mais de uma localização. 
# O modelo de efeito principal assume que a GxA não existe e que o principal efeito genótipo, mais o efeito fixo do ambiente, é suficiente para prever o efeito genótipo em todas as localidades de interesse.

# ajustar o modelo
ansMain <- mmer(Yield~Env, random= ~ vsr(Name, Gu=A), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansMain)

# ou
Ai <- as(solve(A), Class="dgCMatrix")
ansMain <- mmec(Yield~Env, random= ~ vsc(isc(Name), Gu=Ai), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansMain)

#------------------------------------------------------------------------------#
# 3) Modelo de múltiplos ambientes: Modelo diagonal (DG)

# Um modelo de múltiplos ambientes é aquele que é ajustado quando o programa de melhoramento pode se dar ao luxo de usar mais de uma localização. 
# O modelo diagonal assume que a GxA existe e que a variação genotípica é expressa de maneira diferente em cada local, portanto ajustando um componente de variância para o efeito genotípico em cada local. 
# A principal desvantagem é que este modelo assume que não há covariância entre os locais, como se os genótipos fossem independentes (apesar de serem os mesmos genótipos). 
# O efeito fixo do ambiente, mais o BLUP específico de cada local, é usado para prever o efeito genotípico em cada local de interesse.

# ajustar o modelo
ansDG <- mmer(Yield~Env, random= ~ vsr(dsr(Env),Name, Gu=A), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansDG)

# ou
Ai <- as(solve(A), Class="dgCMatrix")
ansDG <- mmec(Yield~Env, random= ~ vsc(dsc(Env),isc(Name), Gu=Ai), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansDG)

#------------------------------------------------------------------------------#
# 4) Modelo de múltiplos ambientes: Modelo de simetria composta (CS)

# O modelo de simetria composta assume que a GxA existe e que um componente principal de variância-covariância do genótipo é expresso em todos os locais. 
# Além disso, assume que uma variância principal de genótipo por ambiente é expressa em todos os locais. A principal desvantagem é que o modelo assume a mesma variância e covariância entre os locais. 
# O efeito fixo do ambiente, mais o efeito principal do BLUP, mais o efeito de genótipo por ambiente são usados para prever o efeito genotípico em cada local de interesse.

E <- diag(length(unique(DT$Env)))
rownames(E) <- colnames(E) <- unique(DT$Env)
EA <- kronecker(E,A, make.dimnames = TRUE)

# ajustar o modelo
ansCS <- mmer(Yield~Env, random= ~ vsr(Name, Gu=A) + vsr(Env:Name, Gu=EA), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansCS)

# ou
E <- diag(length(unique(DT$Env)));rownames(E) <- colnames(E) <- unique(DT$Env)
Ei <- solve(E)
Ai <- solve(A)
EAi <- kronecker(Ei,Ai, make.dimnames = TRUE)
Ei <- as(Ei, Class="dgCMatrix")
Ai <- as(Ai, Class="dgCMatrix")
EAi <- as(EAi, Class="dgCMatrix")

ansCS <- mmec(Yield~Env, random= ~ vsc(isc(Name), Gu=Ai) + vsc(isc(Env:Name), Gu=EAi), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansCS)

#------------------------------------------------------------------------------#
# 5) Modelo de múltiplos ambientes: Modelo não estruturado (US)

# O modelo não estruturado é o mais flexível, assumindo que GxA existe e que há uma variância específica para cada ambiente, além de muitas covariâncias para cada combinação de ambientes. 
# A principal desvantagem é que é difícil fazer esses modelos convergirem devido ao grande número de componentes de variância, ao fato de que alguns desses componentes de variância ou covariância são zero, e à dificuldade na escolha de bons valores iniciais. 
# O efeito fixo para o ambiente, mais o BLUP específico do ambiente (ajustado pelas covariâncias), é usado para prever o efeito genotípico em cada local de interesse.

# ajustar o modelo
ansUS <- mmer(Yield~Env, random= ~ vsr(usr(Env),Name, Gu=A), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansUS)

# Ajustar os BLUPs de variância adicionando covariâncias
# ansUS$U[1:6] <- unsBLUP(ansUS$U[1:6])

# ou
Ai <- solve(A)
Ai <- as(Ai, Class="dgCMatrix")
ansUS <- mmec(Yield~Env, random= ~ vsc(usc(Env),isc(Name), Gu=Ai), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansUS)

#------------------------------------------------------------------------------#
# 6) Modelo de múltiplos ambientes: Modelo de regressão aleatória (RR)

# O modelo de regressão aleatória assume que o ambiente pode ser visto como uma variável contínua, portanto, um componente de variância para o intercepto e um componente de variância para o coeficiente angular (slope) podem ser ajustados. 
# O número de componentes de variância dependerá da ordem do polinômio de Legendre ajustado.

library(orthopolynom)

# dados
DT$EnvN <- as.numeric(as.factor(DT$Env))

# ajuste do modelo
ansRR <- mmer(Yield~Env, random= ~ vsr(leg(EnvN,1),Name), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansRR)

# ou
ansRR <- mmec(Yield~Env, random= ~ vsc(dsc(leg(EnvN,1)),isc(Name)), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansRR)

# Além disso, pode-se aplicar uma estrutura de variância-covariância não estruturada, diagonal ou outra sobre o modelo polinomial.
DT$EnvN <- as.numeric(as.factor(DT$Env))

# ajuste do modelo
ansRR <- mmer(Yield~Env, random= ~ vsr(usr(leg(EnvN,1)),Name), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansRR)

# ou
ansRR <- mmec(Yield~Env, random= ~ vsc(usc(leg(EnvN,1)),isc(Name)), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansRR)

#------------------------------------------------------------------------------#
# 7) Modelo de múltiplos ambientes: Outras estruturas de covariância para GxA

# Embora não seja muito comum em modelos GxE, estruturas de covariância como autoregressivas de ordem 1 (AR1) e outras podem ser utilizadas no modelamento GxE. 
# Aqui mostramos como fazê-lo (não recomendamos).

E <- AR1(DT$Env)  # pode ser AR1() ou CS(), etc.
rownames(E) <- colnames(E) <- unique(DT$Env)
EA <- kronecker(E,A, make.dimnames = TRUE)

ansCS <- mmer(Yield~Env, random= ~ vsr(Name, Gu=A) + vsr(Env:Name, Gu=EA), rcov= ~ units, data=DT, verbose = FALSE)
summary(ansCS)

#------------------------------------------------------------------------------#
# 8) Modelo de múltiplos ambientes: Regressão de Finlay-Wilkinson

# dados
data(DT_h2)
DT <- DT_h2

# construir o índice ambiental
ei <- aggregate(y~Env, data=DT,FUN=mean)
colnames(ei)[2] <- "envIndex"
ei$envIndex <- ei$envIndex - mean(ei$envIndex,na.rm=TRUE) # centralizar o envIndex para ter VC limpo
ei <- ei[with(ei, order(envIndex)), ]

# adicionar o índice ambiental ao conjunto de dados original
DT2 <- merge(DT,ei, by="Env")

# variáveis numéricas por variáveis de fator como envIndex:Name não podem ser usadas na parte aleatória assim
# elas precisam vir com a estrutura vsc()
DT2 <- DT2[with(DT2, order(Name)), ]

# modelo
mix2 <- mmec(y~ envIndex, random=~ Name + vsc(dsc(envIndex),isc(Name)), data=DT2, rcov=~vsc(dsc(Name),isc(units)), tolParConvNorm = .0001, nIters = 50, verbose = FALSE)
summary(mix2)

b=mix2$uList$`vsc(dsc(envIndex), isc(Name))`   # adaptabilidade (b) ou inclinações do genótipo
mu=mix2$uList$`vsc( isc( Name ) )`    # adaptação geral (mu) ou efeito principal
e=sqrt(summary(mix2)$varcomp[-c(1:2), 1])   # variância do erro para cada indivíduo

# adaptação geral (efeito principal) vs adaptabilidade (resposta aos melhores ambientes)
plot(mu[,1]~b[,1], ylab="general adaptation", xlab="adaptability")
text(y=mu[,1],x=b[,1], labels = rownames(mu), cex=0.5, pos = 1)

# previsão através dos ambientes
Dt <- mix2$Dtable
Dt[1,"average"]=TRUE
Dt[2,"include"]=TRUE
Dt[3,"include"]=TRUE

pp <- predict(mix2,Dtable = Dt, D="Name")
preds <- pp$pvals
# preds[with(preds, order(-predicted.value)), ]

# desempenho vs estabilidade (desvio da linha de regressão)
plot(preds[,2]~e, ylab="performance", xlab="stability")
text(y=preds[,2],x=e, labels = rownames(mu), cex=0.5, pos = 1)

#------------------------------------------------------------------------------#
# 9) Modelo de múltiplos ambientes: Modelo analítico de fatores (rank reduzido) (FA)

# Quando o número de ambientes onde os genótipos são avaliados é grande e queremos considerar a covariância genética entre os ambientes e os componentes de variância específicos de cada local, não podemos ajustar uma covariância não estruturada no modelo, pois o número de parâmetros é muito grande e a matriz pode se tornar não totalmente classificada, levando a singularidades. 
# Nesses casos, é sugerida uma técnica de redução de dimensionalidade. Entre essas técnicas, as estruturas analíticas de fatores propostas por muitos grupos de pesquisa (Piepho, Smith, Cullis, Thompson, Meyer, etc.) são as mais indicadas. 
# O Sommer possui uma implementação de estrutura analítica de fatores de rank reduzido disponível através da função rrc().

# dados
data(DT_h2)
DT <- DT_h2
DT=DT[with(DT, order(Env)), ]

# ajustar o modelo
ans1b <- mmec(y~Env,
              random=~vsc( usc( rrc(Env, Name, y, nPC = 2) ) , isc(Name)),
              rcov=~units,
              # recomendamos dar mais iterações para esses modelos
              nIters = 50,
              # recomendamos dar mais iterações EM no início para modelos usc
              emWeight = c(rep(1,10),logspace(10,1,.05), rep(.05,80)),
              verbose=FALSE, data=DT)
summary(ans1b)

Gamma=with(DT, rrc(Env, Name, y, returnGamma = TRUE, nPC = 2))$Gamma # extrair loadings
score.mat=ans1b$uList[[1]];   # extrair escores dos fatores
BLUP=score.mat %*% t(Gamma)   # BLUPs para todos os ambientes

# Como pode ser visto, os BLUPs para todos os ambientes podem ser recuperados multiplicando as cargas (Lam) pelos escores dos fatores (score.mat). Esta é uma maneira parcimoniosa de modelar uma covariância não estruturada.

#------------------------------------------------------------------------------#
# 10) Análise em duas etapas

# É comum, então, ajustar um primeiro modelo que contabiliza a variação dos elementos aleatórios do design, por exemplo, locais, anos, blocos e efeitos fixos do genótipo para obter as médias marginais estimadas (EMMs) ou os melhores estimadores lineares não tendenciosos (BLUEs) como médias ajustadas de entrada. 
# Essas médias ajustadas de entrada são então usadas como a variável fenotípica ou de resposta em estudos de GWAS e previsão genômica.

## estágio 1
## usar mmer para ensaios de campo densos

# dados
data(DT_h2)
DT <- DT_h2

envs <- unique(DT$Env)
BLUEL <- list()
XtXL <- list()
for(i in 1:length(envs)){
  ans1 <- mmer(y~Name-1, random=~Block, verbose=FALSE, data=droplevels(DT[which(DT$Env == envs[i]),]))
  
  ans1$Beta$Env <- envs[i]
  BLUEL[[i]] <- ans1$Beta
  # para ser comparável a 1/(seˆ2) = 1/PEV = 1/Ci = 1/[(X'X)inv]
  XtXL[[i]] <- solve(ans1$VarBeta)
}

DT2 <- do.call(rbind, BLUEL)
OM <- do.call(adiag1,XtXL)

#------------------------------------------------------------------------------#
## estágio 2
## use mmec para equação esparsa

m <- matrix(1/var(DT2$Estimate, na.rm = TRUE)); m

ans2 <- mmec(Estimate~Env, random=~Effect + Env:Effect, rcov=~vsc(isc(units,thetaC = matrix(3), theta = m)), W=OM, verbose=FALSE, data=DT2)
summary(ans2)

#------------------- Spatial modeling using the sommer package ----------------------#

# Estes exs se concentram em mostrar as capacidades do sommer para ajustar modelos espaciais usando modelos de splines bidimensionais.

## Modelos espaciais

# 1) Splines bidimensionais (múltiplos componentes espaciais)
# Neste exemplo, mostramos como obter os mesmos resultados do pacote SpATS. Isso é alcançado usando a função "spl2Db", que é um invólucro da função "tpsmmb".

# dados
data(DT_yatesoats)
DT <- DT_yatesoats
DT$row <- as.numeric(as.character(DT$row))
DT$col <- as.numeric(as.character(DT$col))
DT$R <- as.factor(DT$row)
DT$C <- as.factor(DT$col)

m1.sommer <- mmer(Y~1+V+spl2Db(col,row, nsegments = c(14,21), degree = c(3,3), penaltyord = c(2,2), what = "base"),
                  random = ~R+C+spl2Db(col,row, nsegments = c(14,21), degree = c(3,3), penaltyord = c(2,2), what="bits"),  data=DT, tolParConv = 1e-6, verbose = FALSE)
summary(m1.sommer)

# obter os valores ajustados para o kernel espacial e plotar
ff <- fitted.mmer(m1.sommer)
DT$fit <- as.matrix(Reduce("+", ff$Zu[-c(1:2)]))
lattice::levelplot(fit ~ row * col, data = DT)

#------------------------------------------------------------------------------#
# 2) Splines bidimensionais (único componente espacial)

# Para reduzir a carga computacional de ajustar vários kernels espaciais, Sommer fornece um único método de kernel espacial por meio da função "spl2Da". 
# Isto, como será mostrado, pode produzir resultados semelhantes aos do modelo mais flexível. Use aquele que melhor se adapta às suas necessidades.

m2.sommer <- mmer(Y~1+V, random = ~R+C+spl2Da(col,row, nsegments = c(14,21), degree = c(3,3), penaltyord = c(2,2)), data=DT, tolParConv = 1e-6, verbose = FALSE)
summary(m1.sommer)

# obter os valores ajustados para o kernel espacial e plotar
ff <- fitted.mmer(m2.sommer)
DT$fit <- as.matrix(Reduce("+",ff$Zu[-c(1:2)]))
lattice::levelplot(fit~row*col,data=DT)

#------------------------------------------------------------------------------#
# 3) Modelos espaciais em múltiplos ensaios simultaneamente

# Às vezes, queremos ajustar componentes de variância heterogêneos quando, por exemplo, temos vários ensaios ou locais diferentes. Os modelos espaciais também podem ser ajustados dessa forma usando os argumentos "at.var" e "at.levels". 
# O primeiro argumento espera uma variável que definirá os níveis nos quais os componentes de variância serão ajustados. O segundo argumento é uma forma de o usuário especificar os níveis nos quais os kernels espaciais devem ser ajustados se o usuário não quiser ajustá-los para todos os níveis (por exemplo, testes ou campos).

DT2 <- rbind(DT,DT)
DT2$Y <- DT2$Y + rnorm(length(DT2$Y))
DT2$trial <- c(rep("A",nrow(DT)),rep("B",nrow(DT)))
head(DT2)

m3.sommer <- mmer(Y~1+V,
                  random = ~vsr(dsr(trial),R)+vsr(dsr(trial),C) +
                    spl2Da(col,row, nsegments = c(14,21), degree = c(3,3), penaltyord = c(2,2), at.var = trial), 
                  rcov = ~vsr(dsr(trial),units),
                  data=DT2, tolParConv = 1e-6, verbose = FALSE)
summary(m3.sommer)

# obter os valores ajustados para o kernel espacial e plotar
ff <- fitted.mmer(m3.sommer)
DT2$fit <- as.matrix(Reduce("+",ff$Zu[-c(1:4)]))
lattice::levelplot(fit~row*col|trial,data=DT2)

