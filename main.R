# Update R to R-4.1.2 #
install.packages("installr")
library(installr)
updateR()
#

# Remove packages for version conflict #
remove.packages("tidyverse")
remove.packages("plyr")
remove.packages("dplyr")
remove.packages("fmsb")
remove.packages("Hmisc")
#

# Install needed packages #
install.packages('tidyverse');
require('tidyverse');

install.packages('plyr');
require('plyr');

install.packages('fmsb');
require('fmsb');

install.packages('Hmisc')
require('Hmisc');
#


# CONFLIT ENTRE plyr ET dplyr
if("dplyr" %in% (.packages())){
  detach("package:dplyr", unload=TRUE) 
  detach("package:plyr", unload=TRUE) 
} 
library(plyr); # for ddply()
library(dplyr); # for group_by(), does not work if "plyr" is imported
library(tidyverse); # for ggplot()
library(fmsb); # for radarchart()
library(Hmisc); # for rcorr()

# FETCH ATTRIBUTE DATASET #
# /!\ Le chemin vers le dataset étant déterminé grâce à getwd(), penser à bien choisir son environnement de travail (setwd()) initial:
# contenant le dossier /celeba/
datasetPath = paste(getwd(), "/celeba/datasets/list_attr_celeba.csv", sep = "", collapse = NULL)

celebNumAttributes = read.csv(datasetPath, sep = ",")
#

# FILTER ATTRIBUTES #
NON_FACIAL_ATTRIBUTES = c("image_id", "Attractive", "Male", "Young", "Blurry", "X5_o_Clock_Shadow", "Smiling");

# Enlever les traits qui n'appartiennent pas au visage
celebFaceNumAttributes <- celebNumAttributes[, !names(celebAttributes) %in% NON_FACIAL_ATTRIBUTES]

# Enlever les traits qui n'appartiennent pas au visage mais garder le trait "Attractive"
celebFaceNumAttributesAttractive <- celebNumAttributes[, !names(celebAttributes) %in% NON_FACIAL_ATTRIBUTES[NON_FACIAL_ATTRIBUTES!="Attractive"]]
#

# RENAME VALUES BY #
falseValue = -1
trueValue = 1
newFalseValue = "non"
newTrueValue = "oui"

# /!\ Lorsqu'il faut des valeurs numériques (-1/1 et non "non"/"oui")
# Sinon problèe de pourcentages, ou alors problème de is.numeric()

# Mettre des valeurs plus explicites ("non"/"oui" au lieu de -1/1)
celebAttributes <- celebNumAttributes
celebAttributes[celebAttributes == trueValue] <- newTrueValue
celebAttributes[celebAttributes == falseValue] <- newFalseValue

# Pareil pour le dataset sans les attributs divergents du visage
celebFaceAttributes <- celebFaceNumAttributes
celebFaceAttributes[celebFaceAttributes == trueValue] <- newTrueValue
celebFaceAttributes[celebFaceAttributes == falseValue] <- newFalseValue
#

# CUSTOM LEGENDS #
LEGEND_GENDER = c("Femme", "Homme")
LEGEND_ALL_GENDER = c("Tout genre", LEGEND_GENDER)
LEGEND_ALL_GENDER_FINAL = c(LEGEND_GENDER, "Tout genre")
LEGEND_TRUE_FALSE = c("Faux", "Vrai")
LEGEND_AGE = c("Moins jeune", "Jeune")
LEGEND_ALL_AGE = c(rev(LEGEND_AGE), "Tout âge")
#

# COLORS #
COLORS_GENDER = c("#e84393", "#0984e3") # Pink, Blue
COLORS_ALL_GENDER = c("#44bd32", COLORS_GENDER) # Green + COLORS_GENDER
COLORS_ALL_GENDER_FINAL = c(COLORS_GENDER, "#44bd32") # COLORS_GENDER + Green
COLORS_FALSE_TRUE = c("#d63031", "#00b894") # Red, Green
COLORS_AGE = c("#3964C3", "#fdcb6e") # Indigo, Yellow
COLORS_ALL_AGE = c("yellow", "black", "red") # Yellow, ...
#

# FUNCTIONS #
create_beautiful_radarchart <- function(data, color="#00AFBB", 
                                        vlabels=colnames(data), vlcex=1,
                                        caxislabels=NULL, title=NULL, ...){
  radarchart(
    data, axistype=1,
    # Personnaliser le polygone
    pcol=color, pfcol=scales::alpha(color, 0.), plwd=2, plty=1,
    # Personnaliser la grille
    cglcol="grey", cglty=1, cglwd=0.8,
    # Personnaliser l'axe
    axislabcol="darkgrey", 
    # Labels
    vlcex=vlcex, vlabels=vlabels,
    caxislabels=caxislabels, title=title, ...
  )
}

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor =(cormat)[ut],
    p = pmat[ut]
  )
}
#



# ===================================== FIGURE 1 ===================================== #
# Le nombre d'entrées pour chaque genre (col)
genderCount <- celebAttributes %>%
  count(Male)

genderCount %>%
  ggplot(aes(x=Male, y=n, group=Male, fill=LEGEND_GENDER)) +
  geom_col() +
  labs(title="Figure 1: Répartition par genre", x="'Est un homme ?'", y="Total d'entrées", fill="") +
  scale_fill_manual(values=COLORS_GENDER, labels=LEGEND_GENDER) +
  geom_text(aes(label=n), vjust=1.6, color="black", size=3.5) +
  theme(plot.title = element_text(hjust = 0.5))

# En pourcentages
genderDistribution <- genderCount
genderDistribution['Distribution'] <- round(genderCount['n'] * 100 / nrow(celebAttributes), digits=2)

genderDistribution %>%
  ggplot(aes(x=Male, y=Distribution, group=Male, fill=LEGEND_GENDER)) + 
  geom_col() +
  labs(title="Figure 1: Répartition par genre", x="'Est un homme ?'", y="Répartition des célébrités (%)", fill="") +
  scale_fill_manual(values=COLORS_GENDER, labels=LEGEND_GENDER) +
  geom_text(aes(label=Distribution), vjust=1.6, color="black", size=3.5) +
  theme(plot.title = element_text(hjust = 0.5))
# ===================================== FIGURE 1 ===================================== #


# ===================================== FIGURE 2 ===================================== #
# Répartition des célébrités attirantes par genre

celebAttributes %>%
  group_by(Male, Attractive) %>%
  summarise(count=n()) %>%
  mutate(percentage=count/sum(count)*100) %>%
  ggplot(aes(x=Attractive, y=percentage, fill=Male)) +
  geom_col(position="dodge") +
  labs(title="Figure 2: Répartition des célébrités attirantes par genre", x="'Est attirant(e) ?'", y="Pourcentage de célébrités", fill="Genre") +
  scale_fill_manual(values=COLORS_GENDER, labels=LEGEND_GENDER) +
  # Afficher les pourcentages
  geom_text(aes(label=sprintf("%1.1f%%", percentage)), vjust=2, color="black", size=3.5, position=position_dodge(0.9)) +
  # Afficher le nombre d'occurrences
  geom_text(aes(label=sprintf("(%d)", count)), vjust=3.5, color="yellow", size=3.5, position=position_dodge(0.9)) +
  theme(plot.title = element_text(hjust = 0.5))
# ===================================== FIGURE 2 ===================================== #


# ===================================== FIGURE 3 ===================================== #
# Répartition des célébrités attirantes par tranche d'âge
celebAttributes %>%
  group_by(Young, Attractive) %>%
  summarise(count=n()) %>%
  mutate(percentage=count/sum(count)*100) %>%
  ggplot(aes(x=Attractive, y=percentage, fill=Young)) +
  geom_col(position="dodge") +
  labs(title="Figure 3: Répartition des célébrités attirantes par tranche d'âge", x="'Est attirant(e) ?'", y="Pourcentage de célébrités", fill="Genre") +
  scale_fill_manual(values=COLORS_AGE, labels=LEGEND_AGE) +
  # Afficher les pourcentages
  geom_text(aes(label=sprintf("%1.1f%%", percentage)), vjust=1.5, color="black", size=3.5, position=position_dodge(0.9)) +
  # Afficher le nombre d'occurrences
  geom_text(aes(label=sprintf("(%d)", count)), vjust=3, color="white", size=3.5, position=position_dodge(0.9))
# ===================================== FIGURE 3 ===================================== #


# ===================================== CALCUL 4.1 ===================================== #
# Quel est le top 5 des attributs rendant attractif, tout genre confondu ? (bar)

# Déterminer le classement des 5 traits les plus rencontrés chez les célébrités attirantes
globalAttractiveAttributesRanking <- 
  # Filtrer les célébrités attirantes
  celebFaceAttributes[celebAttributes$Attractive == newTrueValue,] %>%
  gather(cols, value) %>%
  group_by(cols, value) %>%
  summarise(count=n())  %>%
  mutate(percentage=count/sum(count)*100) %>%
  arrange(desc(count)) %>%
  head(n=5)

# Pour visualiser la répartition de ces traits dans le dataset
topAttributes <- globalAttractiveAttributesRanking$cols
# ===================================== CALCUL 4.1 ===================================== #


# ===================================== FIGURE 4bis (NEED CALCUL 4.1) (non utilisé dans le rapport) ===================================== #
# Répartition des données (bar) parmis les 5 traits les plus rencontrés chez les célébrités attirantes
celebNumAttributes %>% 
  select(all_of(topAttributes)) %>%
  gather(cols, value) %>% 
  ggplot(aes(x=value)) +
  geom_bar(aes(y=..count.., fill=factor(..x..)), stat="count") +
  facet_grid(.~cols) +
  labs(title="Répartition des traits de visage les plus attirants", x="Valeur", y="Total de célébrités", fill="Valeur") +
  scale_fill_manual(values=COLORS_FALSE_TRUE, labels=LEGEND_TRUE_FALSE) +
  # Afficher le nombre d'occurrences
  geom_text(stat='count', aes(label=..count..), vjust=1, color="white", size=3.5) +
  # Afficher les pourcentages
  geom_text(stat='count', aes(label=sprintf("(%1.1f%%)", ..prop..*100)), vjust=2, color="black", size=3.5) +
  theme(plot.title = element_text(hjust = 0.5))
# ===================================== FIGURE 4bis (NEED CALCUL 4.1) (non utilisé dans le rapport) ===================================== #


# ===================================== CALCUL 4.2 (NEED CALCUL 4.1) ===================================== #
# Compter le nombre de fois ou les hommes attirants possèdent le top 5 des traits les plus rencontrés chez les célébrités attirantes
globalAttractiveAttributesOnMale <- 
  celebAttributes %>%
  # Filtrer les célébrités homme, attirants
  filter(Male == newTrueValue, Attractive == newTrueValue) %>%
  select(globalAttractiveAttributesRanking$cols) %>%
  gather(cols, value) %>%
  group_by(cols, value) %>%
  summarise(count=n())  %>%
  mutate(percentage=count/sum(count)*100)

# Filtrer les lignes NON souhaitées (couples: attribut et !valeur)
globalAttractiveAttributesOnMaleFiltered <- globalAttractiveAttributesOnMale
for(i in 1:length(globalAttractiveAttributesRanking$cols)) {
  attractiveAttribute = globalAttractiveAttributesRanking$cols[i]
  wantedValue = globalAttractiveAttributesRanking$value[i]
  
  condition = (globalAttractiveAttributesOnMaleFiltered$cols==attractiveAttribute & globalAttractiveAttributesOnMaleFiltered$value!=wantedValue)
  
  rowsToKeep = !(condition)
  
  globalAttractiveAttributesOnMaleFiltered <- 
    globalAttractiveAttributesOnMaleFiltered[rowsToKeep,]
}
# ===================================== CALCUL 4.2 (NEED CALCUL 4.1) ===================================== #


# ===================================== CALCUL 4.3 (NEED CALCUL 4.1) ===================================== #
# Compter le nombre de fois ou les femmes attirantes possèdent ces mêmes attributs
globalAttractiveAttributesOnFemale <- 
  celebAttributes %>%
  # Filtrer les célébrités femme, attirantes
  filter(Male == newFalseValue, Attractive == newTrueValue) %>%
  select(globalAttractiveAttributesRanking$cols) %>%
  gather(cols, value) %>%
  group_by(cols, value) %>%
  summarise(count=n())  %>%
  mutate(percentage=count/sum(count)*100)

# Filtrer les lignes NON souhaitées (couples: attribut et !valeur)
globalAttractiveAttributesOnFemaleFiltered <- globalAttractiveAttributesOnFemale
for(i in 1:length(globalAttractiveAttributesRanking$cols)) {
  attractiveAttribute = globalAttractiveAttributesRanking$cols[i]
  wantedValue = globalAttractiveAttributesRanking$value[i]
  
  condition = (globalAttractiveAttributesOnFemaleFiltered$cols==attractiveAttribute & globalAttractiveAttributesOnFemaleFiltered$value!=wantedValue)
  
  rowsToKeep = !(condition)
  
  globalAttractiveAttributesOnFemaleFiltered <- 
    globalAttractiveAttributesOnFemaleFiltered[rowsToKeep,]
}
# ===================================== CALCUL 4.3 (NEED CALCUL 4.1) ===================================== #


# ===================================== FIGURE 4 (NEED CALCUL 4.1, 4.2, 4.3)  ===================================== #
# Visualiser les 5 traits les plus rencontrés chez les célébrités attirantes
# appliqués par genre (radar) pour voir si ils s'appliquent aux deux genres

# Créer un nouveau dataframe avec comme colonnes 'cols', chaque ligne (en dehors de max et min pour la construction du radar) sera affichée sur le radar.
attractiveAttributes <- data.frame(matrix(ncol = length(globalAttractiveAttributesRanking$cols), nrow=5), row.names = c("max", "min", LEGEND_ALL_GENDER)) %>%
  setNames(globalAttractiveAttributesRanking$cols)

# Remplir ce nouveau dataframe avec le max/min (obligatoire pour radarchart()), puis le pourcentage de chaque attribut
for(i in 1:ncol(attractiveAttributes)) {
  allGenderPercentage = subset(round(globalAttractiveAttributesRanking$percentage, digit=2), globalAttractiveAttributesRanking$cols == globalAttractiveAttributesRanking$cols[i])
  femalePercentage = subset(round(globalAttractiveAttributesOnFemaleFiltered$percentage, digit=2), globalAttractiveAttributesOnFemaleFiltered$cols == globalAttractiveAttributesRanking$cols[i])
  malePercentage = subset(round(globalAttractiveAttributesOnMaleFiltered$percentage, digit=2), globalAttractiveAttributesOnMaleFiltered$cols == globalAttractiveAttributesRanking$cols[i])
  attractiveAttributes[,i] <- c(100, 95, allGenderPercentage, femalePercentage, malePercentage)
}

labelAttractiveAttributes <- sprintf("%s\n(%s)", globalAttractiveAttributesRanking$cols, globalAttractiveAttributesRanking$value)

axisLabels = sprintf("%d%%", seq(96, 100, length = 5))

globalAttractiveAttributesByGender <- attractiveAttributes
create_beautiful_radarchart(
  attractiveAttributes,
  color=COLORS_ALL_GENDER,
  vlabels=labelAttractiveAttributes,
  caxislabels=axisLabels,
  title="Figure 4: Pourcentage des célébrités attirantes par genre,\n parmis le top 5 des traits attirants (Tout genre)"
)

legend(x=0.7, y=1, legend=LEGEND_ALL_GENDER, bty="n", pch=20, col=COLORS_ALL_GENDER, text.col=COLORS_ALL_GENDER, cex=1, pt.cex=2)
# ===================================== FIGURE 4 (NEED CALCUL 4.1, 4.2, 4.3)  ===================================== #


# ===================================== TABLE 1  ===================================== #
# Classement des 5 traits les plus rencontrés chez les célébrités attractives (homme)
maleAttractiveAttributesRanking <- 
  # Filtrer les célébrités attirantes
  celebFaceAttributes[celebAttributes$Attractive == newTrueValue & celebAttributes$Male == newTrueValue,] %>%
  gather(cols, value) %>%
  group_by(cols, value) %>%
  summarise(count=n())  %>%
  mutate(percentage=count/sum(count)*100) %>%
  arrange(desc(count)) %>%
  head(n=5)
# ===================================== TABLE 1  ===================================== #


# ===================================== TABLE 2  ===================================== #
# Classement des 5 traits les plus rencontrés chez les célébrités attractives (femme)
femaleAttractiveAttributesRanking <- 
  # Filtrer les célébrités femme, attirantes
  celebFaceAttributes[celebAttributes$Attractive == newTrueValue & celebAttributes$Male == newFalseValue,] %>%
  gather(cols, value) %>%
  group_by(cols, value) %>%
  summarise(count=n())  %>%
  mutate(percentage=count/sum(count)*100) %>%
  arrange(desc(count)) %>%
  head(n=5)
# ===================================== TABLE 2  ===================================== #


# ===================================== CALCUL 5.1  ===================================== #
# Corrélation entre les traits du visage et le fait d'être attirant (H/F)
attractiveCorrelationMatrix <- rcorr(as.matrix(celebFaceNumAttributesAttractive))

flatAttractiveCorrelationMatrix <- flattenCorrMatrix(attractiveCorrelationMatrix$r, attractiveCorrelationMatrix$P)

attractiveCorrelation <- flatAttractiveCorrelationMatrix[flatAttractiveCorrelationMatrix$row == "Attractive",]
head(attractiveCorrelation[order(-attractiveCorrelation$cor),])
head(attractiveCorrelation[order(attractiveCorrelation$cor),])

topCorrelatedAttractiveAttributes <- head(attractiveCorrelation[order(-attractiveCorrelation$cor),], n=5)
topCorrelatedAttractiveAttributes["gender"] = "Tout genre"
topCorrelatedAttractiveAttributes["age"] = "Tout âge"
# ===================================== CALCUL 5.1  ===================================== #


# ===================================== FIGURE 5 (NEED CALCUL 5.1)  ===================================== #
# Afficher l'impact de chaque trait sur l'attirance d'une célébrité (tout genre)
attractiveCorrelation %>%
  ggplot(aes(x=row, y=column)) +
  geom_tile(aes(fill=cor)) +
  scale_y_discrete(limits=(attractiveCorrelation$column)[order(attractiveCorrelation$cor)]) +
  labs(title="Figure 5: Impact des traits sur l'attirance\n d'une célébrité (Tout genre)", x="", y="Trait", fill="Impact") +
  scale_fill_gradient2() +
  theme(plot.title = element_text(hjust = 0.5))
# ===================================== FIGURE 5 (NEED CALCUL 5.1)  ===================================== #


# ===================================== CALCUL 5.1  ===================================== #
# Corrélation entre les traits du visage et le fait d'être attirant (homme)
celebFaceNumAttributesAttractiveMale <- celebFaceNumAttributesAttractive[celebNumAttributes$Male == trueValue,]
attractiveCorrelationMatrixMale <- rcorr(as.matrix(celebFaceNumAttributesAttractiveMale))

flatAttractiveCorrelationMatrixMale <- flattenCorrMatrix(attractiveCorrelationMatrixMale$r, attractiveCorrelationMatrixMale$P)

attractiveCorrelationMale <- flatAttractiveCorrelationMatrixMale[flatAttractiveCorrelationMatrixMale$row == "Attractive",]
head(attractiveCorrelationMale[order(-attractiveCorrelationMale$cor),])
head(attractiveCorrelationMale[order(attractiveCorrelationMale$cor),])

topCorrelatedMaleAttractiveAttributes <- head(attractiveCorrelationMale[order(-attractiveCorrelationMale$cor),], n=5)
topCorrelatedMaleAttractiveAttributes["gender"] = "Homme"
topCorrelatedMaleAttractiveAttributes["age"] = "Tout âge"
# ===================================== CALCUL 5.2  ===================================== #


# ===================================== CALCUL 5.3  ===================================== #
# Corrélation entre les traits du visage et le fait d'être attirant (femme)
celebFaceNumAttributesAttractiveFemale <- celebFaceNumAttributesAttractive[celebNumAttributes$Male == falseValue,]
attractiveCorrelationMatrixFemale <- rcorr(as.matrix(celebFaceNumAttributesAttractiveFemale))

flatAttractiveCorrelationMatrixFemale <- flattenCorrMatrix(attractiveCorrelationMatrixFemale$r, attractiveCorrelationMatrixFemale$P)

attractiveCorrelationFemale <- flatAttractiveCorrelationMatrixFemale[flatAttractiveCorrelationMatrixFemale$row == "Attractive",]
head(attractiveCorrelationFemale[order(-attractiveCorrelationFemale$cor),])
head(attractiveCorrelationFemale[order(attractiveCorrelationFemale$cor),])

topCorrelatedFemaleAttractiveAttributes <- head(attractiveCorrelationFemale[order(-attractiveCorrelationFemale$cor),], n=5)
topCorrelatedFemaleAttractiveAttributes["gender"] = "Femme"
topCorrelatedFemaleAttractiveAttributes["age"] = "Tout âge"
# ===================================== CALCUL 5.3  ===================================== #


# ===================================== CALCUL 6.1 (NEED CALCUL 5.1, 5.2, 5.3)  ===================================== #
# Créer un nouveau dataframe avec comme colonnes 'cols', chaque ligne (en dehors de max et min pour la construction du radar) sera affichée sur le radar.
attractiveCorrelatedAttributes <- data.frame(matrix(ncol = length(topCorrelatedAttractiveAttributes$column), nrow=5), row.names = c("max", "min", LEGEND_ALL_GENDER)) %>%
  setNames(topCorrelatedAttractiveAttributes$column)


# Remplir ce nouveau dataframe avec le max/min (obligatoire pour radarchart()), puis le pourcentage de chaque attribut
for(i in 1:ncol(attractiveCorrelatedAttributes)) {
  allGenderCorrelation = subset(round(attractiveCorrelation$cor, digit=2), attractiveCorrelation$column == topCorrelatedAttractiveAttributes$column[i])
  femaleCorrelation = subset(round(attractiveCorrelationFemale$cor, digit=2), attractiveCorrelationFemale$column == topCorrelatedAttractiveAttributes$column[i])
  maleCorrelation = subset(round(attractiveCorrelationMale$cor, digit=2), attractiveCorrelationMale$column == topCorrelatedAttractiveAttributes$column[i])
  attractiveCorrelatedAttributes[,i] <- c(0.5, 0, allGenderCorrelation, femaleCorrelation, maleCorrelation)
}
# ===================================== CALCUL 6.1 (NEED CALCUL 5.1, 5.2, 5.3)  ===================================== #


# ===================================== FIGURE 6.1 (NEED CALCUL 6.1 (AND 5.1, 5.2, 5.3))  ===================================== #
# Afficher la correlation des traits parmis topCorrelatedAttractiveAttributes (top des correlations tout genre), pour chaque genre (radar)
correlationAxisLabels = sprintf("%1.2f", seq(0, 0.5, length = 5))

create_beautiful_radarchart(
  attractiveCorrelatedAttributes,
  color=COLORS_ALL_GENDER,
  caxislabels=correlationAxisLabels,
  title="Impact des traits sur l'attirance d'une célébrité par genre,\n parmis les traits les plus impactants (Tout genre)"
)

legend(x=0.7, y=1, legend=LEGEND_ALL_GENDER, bty="n", pch=20, col=COLORS_ALL_GENDER, text.col=COLORS_ALL_GENDER, cex=1, pt.cex=2)
# ===================================== FIGURE 6.1 (NEED CALCUL 6.1 (AND 5.1, 5.2, 5.3))  ===================================== #


# ===================================== CALCUL 6.2 (NEED CALCUL 5.1, 5.2, 5.3)  ===================================== #
# Créer un nouveau dataframe avec comme colonnes 'cols', chaque ligne (en dehors de max et min pour la construction du radar) sera affichée sur le radar.
attractiveMaleCorrelatedAttributes <- data.frame(matrix(ncol = length(topCorrelatedMaleAttractiveAttributes$column), nrow=5), row.names = c("max", "min", LEGEND_ALL_GENDER)) %>%
  setNames(topCorrelatedMaleAttractiveAttributes$column)


# Remplir ce nouveau dataframe avec le max/min (obligatoire pour radarchart()), puis le pourcentage de chaque attribut
for(i in 1:ncol(attractiveCorrelatedAttributes)) {
  allGenderCorrelation = subset(round(attractiveCorrelation$cor, digit=2), attractiveCorrelation$column == topCorrelatedMaleAttractiveAttributes$column[i])
  femaleCorrelation = subset(round(attractiveCorrelationFemale$cor, digit=2), attractiveCorrelationFemale$column == topCorrelatedMaleAttractiveAttributes$column[i])
  maleCorrelation = subset(round(attractiveCorrelationMale$cor, digit=2), attractiveCorrelationMale$column == topCorrelatedMaleAttractiveAttributes$column[i])
  attractiveMaleCorrelatedAttributes[,i] <- c(0.5, 0, allGenderCorrelation, femaleCorrelation, maleCorrelation)
}
# ===================================== CALCUL 6.2 (NEED CALCUL 5.1, 5.2, 5.3)  ===================================== #


# ===================================== FIGURE 6.2 (NEED CALCUL 6.2 (AND 5.1, 5.2, 5.3))  ===================================== #
# Afficher la correlation des traits parmis topCorrelatedMaleAttractiveAttributes (top des correlations chez les hommes), pour chaque genre (radar)
correlationMaleAxisLabels = sprintf("%1.2f", seq(0, 0.5, length = 5))

create_beautiful_radarchart(
  attractiveMaleCorrelatedAttributes,
  color=COLORS_ALL_GENDER,
  caxislabels=correlationMaleAxisLabels,
  title="Impact des traits sur l'attirance d'une célébrité par genre,\n parmis les traits les plus impactants chez les hommes"
)

legend(x=0.7, y=1, legend=LEGEND_ALL_GENDER, bty="n", pch=20, col=COLORS_ALL_GENDER, text.col=COLORS_ALL_GENDER, cex=1, pt.cex=2)
# ===================================== FIGURE 6.2 (NEED CALCUL 6.2 (AND 5.1, 5.2, 5.3))  ===================================== #


# ===================================== CALCUL 6.3 (NEED CALCUL 5.1, 5.2, 5.3)  ===================================== #
# Créer un nouveau dataframe avec comme colonnes 'cols', chaque ligne (en dehors de max et min pour la construction du radar) sera affichée sur le radar.
attractiveFemaleCorrelatedAttributes <- data.frame(matrix(ncol = length(topCorrelatedFemaleAttractiveAttributes$column), nrow=5), row.names = c("max", "min", LEGEND_ALL_GENDER)) %>%
  setNames(topCorrelatedFemaleAttractiveAttributes$column)


# Remplir ce nouveau dataframe avec le max/min (obligatoire pour radarchart()), puis le pourcentage de chaque attribut
for(i in 1:ncol(attractiveCorrelatedAttributes)) {
  allGenderCorrelation = subset(round(attractiveCorrelation$cor, digit=2), attractiveCorrelation$column == topCorrelatedFemaleAttractiveAttributes$column[i])
  femaleCorrelation = subset(round(attractiveCorrelationFemale$cor, digit=2), attractiveCorrelationFemale$column == topCorrelatedFemaleAttractiveAttributes$column[i])
  maleCorrelation = subset(round(attractiveCorrelationMale$cor, digit=2), attractiveCorrelationMale$column == topCorrelatedFemaleAttractiveAttributes$column[i])
  attractiveFemaleCorrelatedAttributes[,i] <- c(0.5, 0, allGenderCorrelation, femaleCorrelation, maleCorrelation)
}
# ===================================== CALCUL 6.3 (NEED CALCUL 5.1, 5.2, 5.3)  ===================================== #


# ===================================== FIGURE 6.3 (NEED CALCUL 6.3 (AND 5.1, 5.2, 5.3))  ===================================== #
# Afficher la correlation des traits parmis topCorrelatedFemaleAttractiveAttributes (top des correlations chez les femmes), pour chaque genre (radar)
correlationFemaleAxisLabels = sprintf("%1.2f", seq(0, 0.5, length = 5))

create_beautiful_radarchart(
  attractiveFemaleCorrelatedAttributes,
  color=COLORS_ALL_GENDER,
  caxislabels=correlationFemaleAxisLabels,
  title="Impact des traits sur l'attirance d'une célébrité par genre,\n parmis les traits les plus impactants chez les femmes"
)

legend(x=0.7, y=1, legend=LEGEND_ALL_GENDER, bty="n", pch=20, col=COLORS_ALL_GENDER, text.col=COLORS_ALL_GENDER, cex=1, pt.cex=2)
# ===================================== FIGURE 6.3 (NEED CALCUL 6.3 (AND 5.1, 5.2, 5.3))  ===================================== #


# ===================================== CALCUL 7.1  ===================================== #
# Pour chaque tranche d'âge (Young), quel est le top 5 des attributs rendant attractif, tout genre confondu ? (geom_bar final)

# Chez les jeunes
youngAttractiveCorrelationMatrix <- rcorr(as.matrix(celebFaceNumAttributesAttractive[celebNumAttributes$Young == trueValue,]))

flatYoungAttractiveCorrelationMatrix <- flattenCorrMatrix(youngAttractiveCorrelationMatrix$r, youngAttractiveCorrelationMatrix$P)

youngAttractiveCorrelation <- flatYoungAttractiveCorrelationMatrix[flatYoungAttractiveCorrelationMatrix$row == "Attractive",]
head(youngAttractiveCorrelation[order(-youngAttractiveCorrelation$cor),])
head(youngAttractiveCorrelation[order(youngAttractiveCorrelation$cor),])

topCorrelatedYoungAttractiveAttributes <- head(youngAttractiveCorrelation[order(-youngAttractiveCorrelation$cor),], n=5)
topCorrelatedYoungAttractiveAttributes["gender"] = "Tout genre"
topCorrelatedYoungAttractiveAttributes["age"] = "Jeune"

# Chez les moins jeunes
notYoungAttractiveCorrelationMatrix <- rcorr(as.matrix(celebFaceNumAttributesAttractive[celebNumAttributes$Young == falseValue,]))

flatNotYoungAttractiveCorrelationMatrix <- flattenCorrMatrix(notYoungAttractiveCorrelationMatrix$r, notYoungAttractiveCorrelationMatrix$P)

notYoungAttractiveCorrelation <- flatNotYoungAttractiveCorrelationMatrix[flatNotYoungAttractiveCorrelationMatrix$row == "Attractive",]
head(notYoungAttractiveCorrelation[order(-notYoungAttractiveCorrelation$cor),])
head(notYoungAttractiveCorrelation[order(notYoungAttractiveCorrelation$cor),])

topCorrelatedNotYoungAttractiveAttributes <- head(notYoungAttractiveCorrelation[order(-notYoungAttractiveCorrelation$cor),], n=5)
topCorrelatedNotYoungAttractiveAttributes["gender"] = "Tout genre"
topCorrelatedNotYoungAttractiveAttributes["age"] = "Moins jeune"
# ===================================== CALCUL 7.1  ===================================== #


# ===================================== CALCUL 7.2  ===================================== #
# Pour chaque tranche d'âge (Young), quel est le top 5 des attributs rendant attractif, chez les hommes ? (geom_bar final)

# Chez les jeunes, hommes
youngAttractiveCorrelationMatrixMale <- rcorr(as.matrix(celebFaceNumAttributesAttractive[celebNumAttributes$Young == trueValue & celebNumAttributes$Male == trueValue,]))

flatYoungAttractiveCorrelationMatrixMale <- flattenCorrMatrix(youngAttractiveCorrelationMatrixMale$r, youngAttractiveCorrelationMatrixMale$P)

youngAttractiveCorrelationMale <- flatYoungAttractiveCorrelationMatrixMale[flatYoungAttractiveCorrelationMatrixMale$row == "Attractive",]
head(youngAttractiveCorrelationMale[order(-youngAttractiveCorrelationMale$cor),])
head(youngAttractiveCorrelationMale[order(youngAttractiveCorrelationMale$cor),])

topCorrelatedYoungMaleAttractiveAttributes <- head(youngAttractiveCorrelationMale[order(-youngAttractiveCorrelationMale$cor),], n=5)
topCorrelatedYoungMaleAttractiveAttributes["gender"] = "Homme"
topCorrelatedYoungMaleAttractiveAttributes["age"] = "Jeune"

# Chez les moins jeunes, hommes
notYungAttractiveCorrelationMatrixMale <- rcorr(as.matrix(celebFaceNumAttributesAttractive[celebNumAttributes$Young == falseValue & celebNumAttributes$Male == trueValue,]))

flatNotYoungAttractiveCorrelationMatrixMale <- flattenCorrMatrix(notYungAttractiveCorrelationMatrixMale$r, notYungAttractiveCorrelationMatrixMale$P)

notYoungAttractiveCorrelationMale <- flatNotYoungAttractiveCorrelationMatrixMale[flatNotYoungAttractiveCorrelationMatrixMale$row == "Attractive",]
head(notYoungAttractiveCorrelationMale[order(-notYoungAttractiveCorrelationMale$cor),])
head(notYoungAttractiveCorrelationMale[order(notYoungAttractiveCorrelationMale$cor),])

topCorrelatedNotYoungMaleAttractiveAttributes <- head(notYoungAttractiveCorrelationMale[order(-notYoungAttractiveCorrelationMale$cor),], n=5)
topCorrelatedNotYoungMaleAttractiveAttributes["gender"] = "Homme"
topCorrelatedNotYoungMaleAttractiveAttributes["age"] = "Moins jeune"
# ===================================== CALCUL 7.2  ===================================== #


# ===================================== CALCUL 7.3  ===================================== #
# Pour chaque tranche d'âge (Young), quel est le top 5 des attributs rendant attractif, chez les femmes ? (radar --> geom_bar final)

# Chez les jeunes, femmes
youngAttractiveCorrelationMatrixFemale <- rcorr(as.matrix(celebFaceNumAttributesAttractive[celebNumAttributes$Young == trueValue & celebNumAttributes$Male == falseValue,]))

flatYoungAttractiveCorrelationMatrixFemale <- flattenCorrMatrix(youngAttractiveCorrelationMatrixFemale$r, youngAttractiveCorrelationMatrixFemale$P)

youngAttractiveCorrelationFemale <- flatYoungAttractiveCorrelationMatrixFemale[flatYoungAttractiveCorrelationMatrixFemale$row == "Attractive",]
head(youngAttractiveCorrelationFemale[order(-youngAttractiveCorrelationFemale$cor),])
head(youngAttractiveCorrelationFemale[order(youngAttractiveCorrelationFemale$cor),])

topCorrelatedYoungFemaleAttractiveAttributes <- head(youngAttractiveCorrelationFemale[order(-youngAttractiveCorrelationFemale$cor),], n=5)
topCorrelatedYoungFemaleAttractiveAttributes["gender"] = "Femme"
topCorrelatedYoungFemaleAttractiveAttributes["age"] = "Jeune"

# Chez les moins jeunes, femme
notYungAttractiveCorrelationMatrixFemale <- rcorr(as.matrix(celebFaceNumAttributesAttractive[celebNumAttributes$Young == falseValue & celebNumAttributes$Male == falseValue,]))

flatNotYoungAttractiveCorrelationMatrixFemale <- flattenCorrMatrix(notYungAttractiveCorrelationMatrixFemale$r, notYungAttractiveCorrelationMatrixFemale$P)

notYoungAttractiveCorrelationFemale <- flatNotYoungAttractiveCorrelationMatrixFemale[flatNotYoungAttractiveCorrelationMatrixFemale$row == "Attractive",]
head(notYoungAttractiveCorrelationFemale[order(-notYoungAttractiveCorrelationFemale$cor),])
head(notYoungAttractiveCorrelationFemale[order(notYoungAttractiveCorrelationFemale$cor),])

topCorrelatedNotYoungFemaleAttractiveAttributes <- head(notYoungAttractiveCorrelationFemale[order(-notYoungAttractiveCorrelationFemale$cor),], n=5)
topCorrelatedNotYoungFemaleAttractiveAttributes["gender"] = "Femme"
topCorrelatedNotYoungFemaleAttractiveAttributes["age"] = "Moins jeune"
# ===================================== CALCUL 7.3  ===================================== #


# ===================================== FIGURE 7 (NEED CALCUL 6.1, 6.2, 6.3, 7.1, 7.2, 7.3)  ===================================== #
# Affichage de la correlation entre le top des attributs pour chaque genre, par age, et le fait d'être attirant (geom_bar)

# Combiner les différents dataframe de corrélations: 
# Tout âge    /   Tout genre:
#topCorrelatedAttractiveAttributes
# Tout âge    /   Homme:
#topCorrelatedMaleAttractiveAttributes
topAttractiveAttributesCorrelation <- rbind(topCorrelatedAttractiveAttributes, topCorrelatedMaleAttractiveAttributes)
# Tout âge    /   Femme:
#topCorrelatedFemaleAttractiveAttributes
topAttractiveAttributesCorrelation <- rbind(topAttractiveAttributesCorrelation, topCorrelatedFemaleAttractiveAttributes)

# Jeune       /   Tout genre:
#topCorrelatedYoungAttractiveAttributes
topAttractiveAttributesCorrelation <- rbind(topAttractiveAttributesCorrelation, topCorrelatedYoungAttractiveAttributes)
# Jeune       /   Homme:
#topCorrelatedYoungMaleAttractiveAttributes
topAttractiveAttributesCorrelation <- rbind(topAttractiveAttributesCorrelation, topCorrelatedYoungMaleAttractiveAttributes)
# Jeune       /   Femme:
#topCorrelatedYoungFemaleAttractiveAttributes
topAttractiveAttributesCorrelation <- rbind(topAttractiveAttributesCorrelation, topCorrelatedYoungFemaleAttractiveAttributes)

# Moins jeune /   Tout genre:
#topCorrelatedNotYoungAttractiveAttributes
topAttractiveAttributesCorrelation <- rbind(topAttractiveAttributesCorrelation, topCorrelatedNotYoungAttractiveAttributes)
# Moins jeune /   Homme:
#topCorrelatedNotYoungMaleAttractiveAttributes
topAttractiveAttributesCorrelation <- rbind(topAttractiveAttributesCorrelation, topCorrelatedNotYoungMaleAttractiveAttributes)
# Moins jeune /   Femme:
#topCorrelatedNotYoungFemaleAttractiveAttributes
topAttractiveAttributesCorrelation <- rbind(topAttractiveAttributesCorrelation, topCorrelatedNotYoungFemaleAttractiveAttributes)

topAttractiveAttributesCorrelation %>%
  ggplot(aes(x=cor)) +
  geom_bar(aes(y=column, fill=gender, color=age), stat="identity") +
  facet_grid(vars(factor(age, levels=c("Tout âge", "Jeune", "Moins jeune"))), vars(factor(gender, levels=rev(LEGEND_ALL_GENDER)))) +
  labs(title="Figure 7: Impact des traits sur l'attirance d'une célébrité, selon son genre et sa tranche d'âge,\n parmis les 5 traits les plus impactants de chaque catégorie", x="Correlation trait/attirance", y="Trait", fill="Genre", color="Age") +
  # Afficher la valeur de corrélation
  geom_text(stat='identity', aes(y=column, label=round(cor, digit=2)), hjust=1.1, vjust=0.4, color="black", size=3) +
  scale_fill_manual(values=COLORS_ALL_GENDER_FINAL, labels=LEGEND_ALL_GENDER_FINAL) +
  scale_color_manual(values=COLORS_ALL_AGE, labels=LEGEND_ALL_AGE) +
  theme(plot.title = element_text(hjust = 0.5))
# ===================================== FIGURE 7 (NEED CALCUL 6.1, 6.2, 6.3, 7.1, 7.2, 7.3)  ===================================== #
