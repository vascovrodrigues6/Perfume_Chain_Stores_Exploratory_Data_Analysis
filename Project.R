
#-------------------------Leitura dos Dados dos Clientes-------------------------

current_directory <- dirname(rstudioapi::getSourceEditorContext()$path) #Leitura dinâmica com base no repositório atual do script
customers_file_to_read <- paste(current_directory,"CLIENTS.txt",sep = "/") #Leitura dinâmica com base no repositório atual do script
purchases_file_to_read <- paste(current_directory,"PURCHASES.txt",sep = "/") #Leitura dinâmica com base no repositório atual do script

DataClients <- read.table(file = customers_file_to_read, header=TRUE, sep="\t", stringsAsFactors = TRUE)
apply(apply(DataClients,2,is.na),2,sum)

dim(DataClients)
head(DataClients)
str(DataClients)
summary(DataClients)

#-------Verificação e tratamento da data de registo do cliente-------
RegistrationDateVerifier <- read.table(text = as.character(DataClients$RegistrationDt))
table(RegistrationDateVerifier[2])

DataClients$RegistrationDt <- as.Date(DataClients$RegistrationDt , format="%Y-%m-%d")
remove(RegistrationDateVerifier)

#-------Verificação e tratamento da indicação se tem filhos do dataset do cliente-------
DataClients[DataClients$HasChildren == "Y" & DataClients$NumChildren == 0,]
DataClients$HasChildren <- as.character(DataClients$HasChildren)
DataClients$HasChildren[DataClients$HasChildren == "Y" & DataClients$NumChildren == 0] <- "N"
DataClients$HasChildren <- as.factor(DataClients$HasChildren)
DataClients[DataClients$HasChildren == "Y" & DataClients$NumChildren == 0,]

#-------Colocação da tag "O"(other) nos clientes que não indicaram estado civil-------
DataClients$MaritalStatus <- as.character(DataClients$MaritalStatus)
DataClients$MaritalStatus[DataClients$MaritalStatus == ""] <- "O"
DataClients$MaritalStatus <- as.factor(DataClients$MaritalStatus)

#-------Verificação e tratamento/normalização das cidades do dataset dos clientes-------
library(stringr)

DataClients$City <- as.character(DataClients$City)
DataClients$City <- toupper(DataClients$City)
DataClients$City <- str_trim(DataClients$City)

DataClients$City[DataClients$City == "SÌO MAMEDE INFESTA"] <- "S.MAMEDE DE INFESTA"
DataClients$City[DataClients$City == "SÌO MAMEDE DE INFESTA"] <- "S.MAMEDE DE INFESTA" 
DataClients$City[DataClients$City == "SÌO MAMEDE CORONADO"] <- "S.MAMEDE CORONADO"
DataClients$City[DataClients$City == "SÌO ROMÌO CORONADO"] <- "S.MAMEDE CORONADO"
DataClients[DataClients$PostalCode == "4425-536",]
DataClients$City[DataClients$City == "4425-536"] <- "PORTO"
DataClients$City[DataClients$City == "Baguim do Monte"] <- "BAGUIM DO MONTE"
DataClients$City[DataClients$City == "BAGUIM DO MOMTE"] <- "BAGUIM DO MONTE"
DataClients$City[DataClients$City == "Bragan\u008da"] <- "BRAGAN,A"
DataClients$City[DataClients$City == "CALDAS DE S. JORGE"] <- "CALDAS DE S.JORGE"
DataClients$City[DataClients$City == "CALDAS DE SÌO JORGE"] <- "CALDAS DE S.JORGE"
DataClients$City[DataClients$City == "FANZ\u0090RES"] <- "FÅNZERES"
DataClients$City[DataClients$City == "FREIXO DE ESP. Ë CINTA"] <- "FREIXO DE ESPADA Ë CINTA"
DataClients$City[DataClients$City == "GUIMARÌES"] <- "GUIMARÅES"
DataClients$City[DataClients$City == "LE\u008dA DO BALIO"] <- "LE,A DO BALIO"
DataClients$City[DataClients$City == "MADALENA - PICO"] <- "MADALENA ( PICO )"
DataClients$City[DataClients$City == "OLIVEIRA ( S. MATEUS)"] <- "OLIVEIRA ( S. MATEUS )"
DataClients$City[DataClients$City == "PORTALEGRRE"] <- "PORTALEGRE"
DataClients$City[DataClients$City == "POVOA DO VARZIM"] <- "POVOA DE VARZIM"
DataClients$City[DataClients$City == "S.ROMÌO CORONADO"] <- "S.ROMÌO DO CORONADO"
DataClients$City[DataClients$City == "SANTA IRIA DE AZÎIA"] <- "SANTA IRIA DA AZÎIA"
DataClients$City[DataClients$City == "SANTA MARINHA DO ZÆZERE"] <- "SANTA MARINHA DO ZEZERE"
DataClients$City[DataClients$City == "SÌO FfLIX DA MARINHA"] <- "SÌO FELIX DA MARINHA"
DataClients$City[DataClients$City == "STA MARIA DA FEIRA"] <- "SANTA MARIA DA FEIRA"
DataClients$City[DataClients$City == "V. N.GAIA"] <- "V.N.GAIA"
DataClients$City[DataClients$City == "VALBOM-GDM"] <- "VALBOM GDM"
DataClients$City[DataClients$City == "VILA VI\u008dOSA"] <- "VILA VIcOSA"
DataClients$City[DataClients$City == "VILA NOVA GAIA"] <- "V.N.GAIA"
DataClients$City <- as.factor(DataClients$City)
#sort(unique(DataClients$City))


#-------------------------Leitura dos Dados das Compras-------------------------

DataPurchases <- read.table(file = purchases_file_to_read, header=TRUE, sep="\t", stringsAsFactors = TRUE)
apply(apply(DataPurchases,2,is.na),2,sum)

dim(DataPurchases)
head(DataPurchases)
str(DataPurchases)
summary(DataPurchases)

#-------Verificação e tratamento da data de compra-------
DateVerifier <- read.table(text = as.character(DataPurchases$DATE))
table(DateVerifier[2])
DataPurchases$DATE <- as.Date(DataPurchases$DATE , format="%Y-%m-%d")
remove(DateVerifier)

#-------Verificação e tratamento do fornecedor no dataset das compras-------
DataPurchases$SUPLIER <- ifelse(is.na(DataPurchases$SUPLIER),as.character("NO SUPLIER"), as.character(DataPurchases$SUPLIER))
DataPurchases$SUPLIER <- as.factor(DataPurchases$SUPLIER)

#-------Verificação e tratamento do produto no dataset das compras-------
DataPurchases$PRODUCTYPE <- ifelse(is.na(DataPurchases$PRODUCTYPE),as.character("SEM TIPO"), as.character(DataPurchases$PRODUCTYPE))
DataPurchases$PRODUCTYPE <- as.factor(DataPurchases$PRODUCTYPE)

#-------Verificação e tratamento da marca no dataset das compras-------
DataPurchases$BRAND <- ifelse(is.na(DataPurchases$BRAND),as.character("NO BRAND"), as.character(DataPurchases$BRAND))
DataPurchases$BRAND <- as.factor(DataPurchases$BRAND)

#-------Verificação e tratamento das taxas no dataset das compras-------
#sort(unique(DataPurchases$TAX))
DataPurchases$TAX <- trimws(DataPurchases$TAX, "both", whitespace = "[ \t\r\n]")

DataPurchases$TAX[DataPurchases$TAX == as.character("NORMAL") | DataPurchases$TAX == as.character("normal") ] <- "Normal"
DataPurchases$TAX[DataPurchases$TAX == as.character("INTERMEDIA") | DataPurchases$TAX == as.character("intermedia") ] <- "Intermedia"
DataPurchases$TAX[DataPurchases$TAX == as.character("REDUZIDA") | DataPurchases$TAX == as.character("reduzida") ] <- "Reduzida"
DataPurchases$TAX <- as.factor(DataPurchases$TAX)

#-------Verificação e correção do valor Total de uma compra através da multiplicação da quantidade de produtos (Qnt) com o preço de venda do produto (Tableunitprice)-------
DataPurchases$TOTAL <- ifelse(DataPurchases$QNT * DataPurchases$TABLEUNITPRICE == DataPurchases$TOTAL, DataPurchases$TOTAL, DataPurchases$TABLEUNITPRICE * DataPurchases$QNT)

#-------Remoção de linhas cuja quantidade do produto é inferior a 1-------
DataPurchases<-DataPurchases[!(DataPurchases$QNT<1),]

#-------Verificação e tratamento do ano no dataset das compras com base no Date-------
DataPurchases$YEAR[DataPurchases$YEAR!=(as.integer(str_split(DataPurchases$DATE,"-",simplify = TRUE)[,1]))] <- (as.integer(str_split(DataPurchases$DATE,"-", simplify = TRUE)[,1])) 


#-------------------------Remoção de linhas-------------------------

#-------Remoção de linhas de compras de clientes com menos de 12 compras feitas em 4 anos-------
nPurchases <- DataPurchases
nPurchases$nCompras <- 0
nPurchases <- aggregate(nCompras~ ENTITY, data = nPurchases,FUN = length )
nPurchases <- nPurchases[order(nPurchases$nCompras,decreasing = TRUE),]
DataPurchases <- merge(DataPurchases, nPurchases, by = "ENTITY")
DataPurchases <- DataPurchases[DataPurchases$nCompras >= 12,] # 4 anos x 3 por ano
DataPurchases <- DataPurchases[,-22]
DataPurchases <- droplevels(DataPurchases)
remove(nPurchases)

#-------Remoção da variação do preço dos produtos-------
products <- as.data.frame(table(DataPurchases$DESCR))
products <- products[products$Freq > 1,]
products <- droplevels(products)
products <- as.array(products$Var1)

productsPrices <- DataPurchases[DataPurchases[,8] %in% products, c("DESCR" , "TABLEUNITPRICE")]
productsPricesMin <- aggregate(TABLEUNITPRICE ~ DESCR, productsPrices, function(x) min(x))
names(productsPricesMin)[names(productsPricesMin) == "TABLEUNITPRICE"] <- "NEWTABLEUNITPRICE"

DataPurchases <- merge(DataPurchases, productsPricesMin, by.x="DESCR", by.y="DESCR", all=TRUE)
DataPurchases$TABLEUNITPRICE <- ifelse(is.na(DataPurchases$NEWTABLEUNITPRICE), DataPurchases$TABLEUNITPRICE, DataPurchases$NEWTABLEUNITPRICE)
DataPurchases$TOTAL <- DataPurchases$TABLEUNITPRICE * DataPurchases$QNT
DataPurchases <- DataPurchases[, -22]

remove(products)
remove(productsPrices)
remove(productsPricesMin)

#-------Remoção de linhas de clientes sem compras no dataset de compras-------
library("plyr")
library("dplyr")
DataClients$HasPurchases <- pull(purrr::map_df(DataClients, ~ .x %in% DataPurchases$ENTITY), Client)
DataClients <- DataClients[DataClients$HasPurchases == TRUE,]
DataClients <- DataClients[,-11]
DataClients <- droplevels(DataClients)

#-------------------------Remoção de colunas-------------------------

#-------Remoção de colunas de compras que não influenciam o dataset-------
table(DataPurchases$PACKAGE)
table(DataPurchases$PACKAGEFACT)
table(DataPurchases$DESCOUNTVAL1)
table(DataPurchases$DESCOUNTPERCENT1)
DataPurchases <- DataPurchases[,-c(17:20)]


#-------------------------Gráficos-----------------------------------

#-------Percentagem e Boxplot do número de crianças-------
library(ggplot2)
labels <- c("0","1","2","3","4","5")
pct <- round(prop.table(table(DataClients$NumChildren)) * 100, digits = 2)
labels <- paste(labels, pct)
labels <- paste(labels,"%",sep="")
pie(pct, labels = labels, main="Percentage of Number of Children")

boxplot(DataClients$NumChildren)

remove(labels)
remove(pct)

#-------Número de crianças por zona-------
barplot(table(DataClients$NumChildren,DataClients$Zone), beside = TRUE, col=rainbow(length(1:6)), ylim = c(0,2500), main="Zone vs NumChildren")
legend("topright", legend = c("0","1","2","3","4","5"), col=rainbow(length(1:6)), pch=16)

#-------Idade por género-------
barplot(table(DataClients$Gender,DataClients$Age), beside = TRUE, col = c(1:2), ylim = c(0,100), main="Age vs Gender")
legend("topright", legend = levels(DataClients$Gender), col = c(1:2), pch=16)

#-------Estado civil por género-------
barplot(table(DataClients$Gender,DataClients$MaritalStatus), beside = TRUE, col = c(1:2), ylim = c(0,1200), main="Maritial Status vs Gender")
legend("topright", legend = levels(DataClients$Gender), col = c(1:2), pch=16)

#-------Se tem crianças por género-------
barplot(table(DataClients$Gender,DataClients$HasChildren), beside = TRUE, col = c(1:2), ylim = c(0,1800), main="Has Children vs Gender")
legend("topright", legend = levels(DataClients$Gender), col = c(1:2), pch=16)

#-------Se tem crianças por estado civil-------
barplot(table(DataClients$MaritalStatus,DataClients$HasChildren), beside = TRUE, col = c(1:3), ylim = c(0,1800), main="Has Children vs Maritial Status")
legend("topright", legend = levels(DataClients$MaritalStatus), col = c(1:3), pch=16)

#-------Número de clientes por zona-------
barplot(table(DataClients$Zone), beside = TRUE, ylim = c(0,6000), main="Clients per Zone")

#-------Preço por marca-------
brandVsPrice <- aggregate(TABLEUNITPRICE ~ BRAND, data=DataPurchases, mean)
brandVsPrice <- brandVsPrice[order(brandVsPrice$TABLEUNITPRICE,decreasing = TRUE),]
ggplot(aes(x = BRAND, y = TABLEUNITPRICE), data = head(brandVsPrice,10)) + stat_summary(fun = "mean", geom = "bar",)

remove(brandVsPrice)

#-------Número de compras por ano-------
comprasAno <- DataPurchases
comprasAno$nCompras <- 0
comprasAno <- aggregate(nCompras ~ YEAR, data=comprasAno, FUN=length)
sort(unique(DataPurchases$YEAR))

comprasAno$YEAR <-as.factor(comprasAno$YEAR)
boxplot(comprasAno$nCompras, ylim = c(0, 120000), main="Nº Purchases")
barplot(comprasAno$nCompras ~ comprasAno$YEAR, beside = TRUE,ylim =c(0,120000), main ="Year vs Purchases", ylab = "Nº Purchases", xlab = "Year")

remove(comprasAno)

#-------Número de compras ao longo dos anos-------
library(plotly)
library(hrbrthemes)

comprasDate <- DataPurchases
comprasDate$nCompras <- 0
comprasDate <- aggregate(nCompras ~ DATE, data=comprasDate, FUN=length)
p <- comprasDate %>%
  ggplot( aes(x=DATE,nCompras)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("Nº Purchases") +
  theme_ipsum()

p <- ggplotly(p)
p
remove(comprasDate)
remove(p)

#-------Top 10 clientes com base no base no valor das compras-------
aggPurchases <- aggregate(TOTAL ~ ENTITY, data = DataPurchases, FUN=sum)
aggPurchases <-aggPurchases[order(aggPurchases$TOTAL,decreasing = TRUE),]
aggPurchases <- aggPurchases[1:10,]
#barplot(aggPurchases$TOTAL ~ aggPurchases$ENTITY, ylim = c(0,9000), main ="Top 10 Clients")
ggplot(aggPurchases, aes(x=ENTITY, y=TOTAL)) + geom_bar(stat = "identity", fill='#6A87AD',) + coord_flip() +  ggtitle("Top 10 Clients") + theme(plot.title = element_text(hjust = 0.5))
library(ggridges)
library(viridis)

remove(aggPurchases)

#-------Gráficos com número de compras, subdivididas por ano e por marca-------
brandQntYear <- DataPurchases[,c(5,9,14)]

brandQntYear <- brandQntYear %>%
  group_by(YEAR,BRAND) %>% 
  summarise_each(funs = sum)
brandQntYear <- brandQntYear[order(brandQntYear$QNT,decreasing = TRUE),]

brandQntYear <- brandQntYear[brandQntYear$BRAND == "Nivea"   |
                               brandQntYear$BRAND == "Colgate" |
                               brandQntYear$BRAND == "Pantene" |
                               brandQntYear$BRAND == "Elvive"  |
                               brandQntYear$BRAND == "Flormar" 
                             ,]


#-------Gráfico nº 1-------
brandQntYear %>%
  ggplot( aes(x=YEAR, y=QNT, group=BRAND, color=BRAND)) +
  geom_line() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Nº of purchases by Brand each year") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5))

#-------Gráfico nº 2-------
brandQntYear%>%
  ggplot( aes(x=YEAR, y=QNT, group=BRAND, fill=BRAND)) +
  geom_area() +
  theme(legend.position="none") +
  scale_fill_viridis(discrete = TRUE, option ="cividis")+
  ggtitle("Nº of purchases by Brand each year") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=14)
  ) +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~BRAND)
remove(brandQntYear)

#-------Compras por tipo de produto-------
qntProductType <- DataPurchases[order(DataPurchases$QNT,decreasing = TRUE),]
qntProductType <- qntProductType[,c(10,14)]
qntProductType <- aggregate(QNT ~ PRODUCTYPE, data=qntProductType, FUN = sum)
qntProductType <- qntProductType[order(qntProductType$QNT,decreasing = TRUE),]

ggplot(qntProductType[1:10,], aes(x=PRODUCTYPE, y=QNT))+
  geom_bar(stat = "identity", fill='#6A87AD')+
  geom_text(aes(label=QNT), position=position_dodge(width=0.9), vjust=-0.25)+
  ggtitle("Purchases by ProductType")+
  theme(plot.title = element_text(hjust = 0.5))
remove(qntProductType)


#-------------------------RFM-----------------------------------

mergedData <- merge(DataClients,DataPurchases, by.x = "Client", by.y ="ENTITY")
dim(mergedData)
apply(apply(mergedData, 2, is.na), 2, sum)
mergedData <- na.omit(mergedData)

library(rfm)

#-------Obtenção do RFM de cada cliente-------
analysis_date <- lubridate::as_date('2014-12-31')
rfm_order <- rfm_table_order(mergedData, Client, DATE, TOTAL, analysis_date)

rfm_heatmap(rfm_order) 
rfm_bar_chart(rfm_order)  
rfm_rm_plot(rfm_order)
rfm_rf_plot(rfm_order)
barplot(table(rfm_order$rfm$recency_score), beside = TRUE, ylim = c(0,1200), col = rainbow(length(1:6)), main="RFM Recency Score Analysis")
barplot(table(rfm_order$rfm$frequency_score), beside = TRUE, ylim = c(0,1200), col = rainbow(length(1:6)), main="RFM Frequency Score Analysis")
barplot(table(rfm_order$rfm$monetary_score), beside = TRUE, ylim = c(0,1200), col = rainbow(length(1:6)), main="RFM Monetary Score Analysis")
barplot(table(rfm_order$rfm$rfm_score), beside = TRUE, ylim = c(0,250), col = rainbow(length(unique(rfm_order$rfm$rfm_score))), main="RFM Total Score Analysis")



#-------------------------Clustering-----------------------------------

#-------Scale dos dados para usar em clustering-------
rfm_data <- rfm_order$rfm[,c(1,6,7,8,9)]

mergedData <- merge(mergedData,rfm_data, by.x = "Client", by.y ="customer_id")

DataClientsWithRFM <- DataClients
DataClientsWithRFM <- merge(DataClientsWithRFM,rfm_data, by.x = "Client", by.y ="customer_id")

ClientsRFMScaled <- DataClientsWithRFM[,-c(1,6,8,10,14)]
ClientsRFMScaled$RegistrationDt <- as.numeric(ClientsRFMScaled$RegistrationDt)
ClientsRFMScaled$Gender <- as.numeric(ClientsRFMScaled$Gender)
ClientsRFMScaled$MaritalStatus <- as.numeric(ClientsRFMScaled$MaritalStatus)
ClientsRFMScaled$Zone <- as.numeric(ClientsRFMScaled$Zone)

ClientsRFMScaled <- scale(ClientsRFMScaled)
pcenter <- attr(ClientsRFMScaled, "scaled:center")
pscale <- attr(ClientsRFMScaled, "scaled:scale")

ClientsRFMScaled <- data.frame(ClientsRFMScaled)

#-------Verificação do número de clusters-------
set.seed(123)

library(fpc)
library(cluster)

kclustersASW <- kmeansruns(ClientsRFMScaled, krange = c(2:10), criterion = "asw")
plot(c(1:10), kclustersASW$crit, type="b", xlab = "N. Clusters", ylab = "Avg. Silhouette")

kclustersCH <- kmeansruns(ClientsRFMScaled, krange = c(2:10), criterion = "ch")
plot(c(1:10), kclustersCH$crit, type="b", xlab = "N. Clusters", ylab = "Calinski-Harabasz Index")

#-------K-Means-------
set.seed(456)

dis <- dist(ClientsRFMScaled)
k3 <- kmeans(ClientsRFMScaled, centers = 3, iter.max = 100, nstart = 25)
sil3 <- silhouette(k3$cluster,dis)
plot(sil3)

ClientsRFMScaled <- cbind(ClientsRFMScaled,k3$cluster)
DataClientsWithRFM <- cbind(DataClientsWithRFM,k3$cluster)

names(ClientsRFMScaled)[names(ClientsRFMScaled) == "k3$cluster"] <- "cluster"
names(DataClientsWithRFM)[names(DataClientsWithRFM) == "k3$cluster"] <- "cluster"

clientsCluster <- DataClientsWithRFM[, c(1,15)]
mergedData <- merge(mergedData,clientsCluster, by.x = "Client", by.y ="Client")

centersUnscaled <- c()
for(i in 1:3) {
  centersUnscaled <- rbind(centersUnscaled, (k3$centers[i,c(7:9)] * pscale + pcenter))
}

centersUnscaled[,c(7:9)]

barplot(t(k3$centers), beside = TRUE, xlab = "Cluster", ylab = "Value", col= c(1:3))

library(factoextra)

fviz_cluster(k3, 
             data = ClientsRFMScaled,
             geom = "text", #point
             ellipse.type = "convex", #euclid
             ggtheme = theme_bw())

clusplot(pam(ClientsRFMScaled[,c("recency_score","frequency_score","monetary_score")],3))



#-------------------------Regras de Classificação-----------------------------------

library(arules)
library(arulesViz)

#-------Criação dos cestos-------
basket <- as(split(as.vector(DataPurchases$PRODUCT), as.vector(DataPurchases$ENTITY)), "transactions")
basket 

class(basket)
summary(basket)

dim(basket)
basket@itemInfo
inspect(basket[1:5])

itemFreq <- itemFrequency(basket)
itemFreq
summary(itemFreq)

itemFrequency(basket[,1:3])

#itemCount <- (itemFreq/sum(itemFreq)*sum(size(basket)))
#itemCount   

image(basket[1:5])
image(sample(basket,50))

basketrules <- apriori(basket, parameter = list(support=0.01, confidence=0.5))
summary(basketrules)

measures <- interestMeasure(basketrules, measure=c("coverage", "leverage", "conviction"),
                            transactions=basket)
measures

inspect(basketrules)
inspect(basketrules[1:5])
inspect(sort(basketrules, by='lift')[1:15])

#-------Visualização de Regras-------
plot(basketrules, measure=c("support", "confidence"), shading = "lift", jitter=0)

plot(basketrules, measure=c("support", "lift"), shading = "confidence", jitter=0.1)

plot(basketrules, shading = "order", control=list(main="Two-key plot"), jitter=0)

plot(basketrules, method="grouped")

library(igraph)

subrules <- head(sort(basketrules, by="support"), 20)

plot(subrules, method="graph")

plot(subrules, method="graph", control=list(type="items"))


#-------Criação de grupos de produtos-------
matrxbask <- as(basket[1:5626], "matrix")

class(matrxbask)

matrxbask <- cbind(as.character(rownames(matrxbask)), matrxbask)

matrxbask <- as.data.frame(matrxbask)
names(matrxbask)[1] <- "client"

matrxbask$client <- as.factor(matrxbask$client)

head(matrxbask,5)

matrxbask$type <- ifelse((matrxbask$`10058`==T | matrxbask$`10059`==T), "Group 1", ifelse((matrxbask$`8434`==T| matrxbask$`8484`==T), "Group 2",
                                                                                   ifelse((matrxbask$`2693`==T| matrxbask$`2703`==T), "Group 3",
                                                                                   ifelse((matrxbask$`10414`==T & (matrxbask$`3463`==T | matrxbask$`3462`==T)), "Group 4",
                                                                                   ifelse((matrxbask$`10413`==T & (matrxbask$`3463`==T | matrxbask$`3462`==T)),"Group 5",
                                                                                   ifelse((matrxbask$`3463`==T | matrxbask$`3462`==T),"Group 6", "Others"))))))


#-------------------------Previsões-----------------------------------

#-------Método de cálculo da accuracy------
acc.cross.table.measures <- function(test, pred, modelname = "") {
  if (length(unique(test)) == length(unique(pred))) {
    crosstb <- CrossTable(test, pred, prop.chisq = FALSE, prop.c = FALSE, 
                          prop.r = FALSE,  dnn = c("Actual", "Predicted"))
    
    df <- data.frame()
    
    accuracy <- sum(diag(crosstb$t)) / sum(crosstb$t)
    
  }
  return(data.frame(model = modelname, 
                    accuracy = round(accuracy, digits = 3)))
}

#-------Previsão do tipo de cliente obtido das regras de associação------
clients <- mergedData

str(matrxbask)
str(clients)

clients <- merge(clients, matrxbask[, c(1,11309)], by.x=c("Client"), by.y=c("client"),stringsAsFactors=TRUE)

clients <- clients[,-c(1,6,8,10:14,27:30)] #Remoção de colunas desnecessárias

clients$RegistrationDt <- as.factor(clients$RegistrationDt)

clients$type <- factor(clients$type, levels= c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5","Group 6","Others"),
                       labels = c("Group 1", "Group 2", "Group 3", "Group 4","Group 5","Group 6","Others"))

library(gmodels)

set.seed(123)

library(caret)
trainIndex <- createDataPartition(clients$type, p=0.7, list=F)

train.dt <- clients[trainIndex, ]
test.dt <- clients[-trainIndex, ]

str(train.dt)

library(C50)

C5Model <- C5.0(train.dt[,-c(20)], train.dt$type) #Coluna 14 equivale a PRODUCTYPE - 23 a Type

summary(C5Model)

C5pred <- predict(C5Model, test.dt)

results <- acc.cross.table.measures(test.dt$type, C5pred, "C5.0")

results


#-------Previsão do cluster do cliente------

clients <- DataClientsWithRFM

str(matrxbask)
str(clients)

clients <- clients[,-c(1,6,8,10:14)] #Remoção de colunas desnecessárias
clients$RegistrationDt <- as.factor(clients$RegistrationDt)
clients$cluster <- as.factor(clients$cluster)

set.seed(123)
library(caret)
trainIndex <- createDataPartition(clients$cluster, p=0.7, list=F)

train.dt <- clients[trainIndex, ]
test.dt <- clients[-trainIndex, ]

str(train.dt)

#---C5.0---
library(C50)

C5Model <- C5.0(train.dt[,-c(7)], train.dt$cluster) #Coluna 14 equivale a PRODUCTYPE - 23 a Type

summary(C5Model)

C5pred <- predict(C5Model, test.dt)

results <- acc.cross.table.measures(test.dt$cluster, C5pred, "C5.0")

#---SVM tanhdot---
library(kernlab)
set.seed(123)

SVM.tanhdot.model <- ksvm(cluster ~ ., data = train.dt, kernel = 'tanhdot', C = 1, prob.modle = T)
SVM.tanhdot.model

SVM.tanhdot.pred <- predict(SVM.tanhdot.model, test.dt)

results <- rbind(results, acc.cross.table.measures(test.dt$cluster, SVM.tanhdot.pred, "SVMmodel tanhdot"))

#---SVM vanilladot---
set.seed(456)
SVM.vanilladot.model <- ksvm(cluster ~ ., data = train.dt, kernel = 'vanilladot', C = 1, prob.modle = T)
SVM.vanilladot.model

SVM.vanilladot.pred <- predict(SVM.vanilladot.model, test.dt)

results <- rbind(results, acc.cross.table.measures(test.dt$cluster, SVM.vanilladot.pred, "SVMmodel vanilladot"))

#---SVM rbfdot---
set.seed(789)
SVM.rbfdot.model <- ksvm(cluster ~ ., data = train.dt, kernel = 'rbfdot', C = 1, prob.modle = T)
SVM.rbfdot.model

SVM.rbfdot.pred <- predict(SVM.rbfdot.model, test.dt)

results <- rbind(results, acc.cross.table.measures(test.dt$cluster, SVM.rbfdot.pred, "SVMmodel rbfdot"))

#---Naive Bayes---
library(e1071)

nb.model <- naiveBayes(cluster ~ ., data = train.dt, laplace = 1)
nb.pred <- predict(nb.model, newdata = test.dt)

results <- rbind(results, acc.cross.table.measures(test.dt$cluster, nb.pred, "Naive Bayes"))

results[order(results$accuracy, decreasing = TRUE),]
