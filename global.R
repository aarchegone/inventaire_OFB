# Chargement des packages
library(here) ; library(rdrop2) ; library(shiny)

path <- here() # path : donne le repertoire courant
token <- readRDS("id-mdp-dropbox.rds") ; drop_acc(dtoken = token) # acces a la Dropbox
folder_dropbox <- "Inventaire_OFB" # nom du dossier dans la dropbox

load_ress <- function() { # fonction de telechargement de l'inventaire sur la Dropbox
  ress <- drop_read_csv(file.path(folder_dropbox,"inventaire_tableur_perso.csv"), header = T, colClasses = c(rep("character",19)))
  return(ress)
}
ress <- load_ress() # telechargement de l'inventaire. "ress" pour "ressources"
nlength <- dim(ress)[1] # nombre de donnees totales
nlengthOFB <- dim(ress[which(ress$Organisme=="OFB"),])[1]  # nombre de donnees OFB

colnames(ress)[c(6:8,10,14, 18, 19)] <- c("Thème Covadis","Mots-clés","Date de mise à jour", "Données brutes", "Autre lien intéressant", 
                                          "Internet Explorer", "Hauts-de-France") # besoin de renommer, sinon on a des points a la place des espaces 
ress$`Mots-clés` <- toupper(ress$`Mots-clés`) # on passe en majuscule

table_correspondance_covadis <- read.csv("themeCovadis.csv", header = T, colClasses = c("character", "character"))
colnames(table_correspondance_covadis)[2] <- "Thème Covadis"

# fonction qui sert a fusionner des cellules a l'affichage (pour correspondance theme - theme Covadis)
dependency_covadis <- htmltools::htmlDependency(
  "RowsGroup", "2.0.0", 
  path, script = "fusion-tableau.js")


