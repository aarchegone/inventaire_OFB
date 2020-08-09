#### chargement de donnees et fonctions ####
library(here) ; library(rdrop2) # chargement des packages
path <- here() # path : donne le repertoire courant
token <- readRDS("id-mdp-dropbox.rds") ; drop_acc(dtoken = token) # acces a la Dropbox
folder_dropbox <- "Inventaire_OFB" # nom du dossier dans la dropbox

# chargement du tableau de correspondance theme - theme Covadis
table_correspondance_covadis <- read.csv("themeCovadis.csv", header = T, colClasses = c("character", "character"))
colnames(table_correspondance_covadis)[2] <- "Thème Covadis"

# fonction qui sert a fusionner des cellules (pour correspondance theme - theme Covadis)
dependency_covadis <- htmltools::htmlDependency(
  "RowsGroup", "2.0.0",
  path, script = "fusion-tableau.js")

# fonction qui cree un bouton "Lien" (pour acceder aux liens des donnees)
# prend le contenu d'une cellule, et l'insere dans une ligne de code pour renvoyer vers le lien
# s'il y a des " : " dans la cellule (c'est le cas pour les "Autres liens interessants"), ne garde que ce qu'il y a apres (le lien en soi)
createLink <- function(val) {
    val <- sapply(strsplit(as.character(val), " : "), tail, 1) 
    sprintf("<a href='%s' target='_blank' class='btn btn-primary'>Lien</a>",val)
}

#### server : regroupe toutes les fonctions utiles a l'application ####
server <- function(input, output, session) {
  load_ress <- function() { # fonction de telechargement de l'inventaire sur la Dropbox
    ress <- drop_read_csv(file.path(folder_dropbox,"inventaire_tableur_perso.csv"), header = T, colClasses = c(rep("character",17)))
    return(ress)
  }
  ress <- load_ress() # telechargement de l'inventaire. "ress" pour "ressources"
  nlength <- dim(ress)[1] # nombre de donnees totales
  nlengthOFB <- dim(ress[which(ress$Organisme=="OFB"),])[1]  # nombre de donnees OFB

  colnames(ress)[c(6:8,10,14, 18, 19)] <- c("Thème Covadis","Mots-clés","Date de mise à jour", "Données brutes", "Autre lien intéressant", 
                                            "Internet Explorer", "Hauts-de-France") # besoin de renommer, sinon on a des points a la place des espaces 
  ress$`Mots-clés` <- toupper(ress$`Mots-clés`) # on passe en majuscule
  
  output$render1 <- renderUI({ # recherche par nom / mots-cles
    fluidRow(style="padding: 10px", selectizeInput("rech_nom_motcle", label = "Nom/MOTS-CLÉS", choices = sort(c(ress$Nom,unique(unlist(strsplit(ress$`Mots-clés`, ", "))))), multiple = T, width = 550))
  })
  
  output$render2 <- renderUI({ # recherche par auteur
    fluidRow(style="padding: 10px",selectInput("rech_auteur", "Auteur", sort(unique(ress$Organisme)), multiple = T))
  })
  
  output$render3 <- renderUI({ # recherche par categorie
    fluidRow(style="padding: 10px", selectInput("rech_categorie", "Catégorie", sort(unique(ress$Catégorie)), multiple = T, width = 350))
    })
  
  output$render4 <- renderUI({ # recherche par theme / theme covadis
    fluidRow(column(style="padding: 10px",width=6,selectInput("rech_theme", "Thème", sort(unique(ress$Thème)), multiple = T, width = 350)), 
             column(style="padding: 10px",width=6,selectInput("rech_themecovadis", "Thème Covadis", sort(unique(ress$`Thème Covadis`)), multiple = T, width = 350)))
    })
  
  output$render5 <- renderUI({ # recherche plus precise par type de carte
    radioButtons("rech_carte", h4(style="color:#398035","Type de carte"), inline = F,
                 choices = c("Peu importe", "Carte interactive" = "carteinteractive", "Carte PDF" = "cartepdf", "Pas de carte" = "pascarte")) # on donne un nom a chaque option
  })
  
  output$render6 <- renderUI({# recherche plus precise par disponibilite de SIG
    checkboxInput("rech_sig", "SIG", FALSE)
  })
  
  output$render7 <- renderUI({# recherche plus precise par disponibilite de donnees brutes
    checkboxInput("rech_brutes", "Données brutes", FALSE)
  })
  
  output$render8 <- renderUI({# nom de la donnee a modifier
    selectizeInput("donnee_modif", label = "Donnée à modifier", choices = sort(c("",ress$Nom)), multiple = F, width = 550)
    })
  
  output$render9 <- renderUI({# nom de l'information a modifier (adresse, organisme, ...)
    selectizeInput("champ_modif", label = "Champ à modifier", choices = c("",colnames(ress)[-5]), multiple = F, width = 550)
  })
  
  output$render10 <- renderUI({
    selectizeInput("categorie_ajout","Choisir la catégorie : *",
                   choices = sort(unique(ress[,"Catégorie"])))
  })
  
  output$render11 <- renderUI({
    selectizeInput("covadis_ajout","Choisir le thème Covadis : *",
                   choices = sort(unique(ress[,"Thème Covadis"])))
  })
  
  output$render12 <- renderUI({
  selectizeInput("carte_ajout","Choisir le type de carte :",
                 choices = c("Aucune",sort(unique(ress[,"Cartes"]))))
  })
  
  output$render13 <- renderUI({
    selectizeInput("type_ajout","Choisir le type de la donnée :",
                   choices = sort(unique(ress[,"Type"])))
  })
  
  output$render14 <- renderUI({
    selectizeInput("donnee_suppr", label = "Donnée à supprimer", choices = sort(c("",ress$Nom)), multiple = F, width = 550)
  })
  
  aucune_recherche <- reactive({ #indique si une recherche a ete faite (1 si oui, 0 sinon)
    if(is.null(input$rech_nom_motcle) & is.null(input$rech_auteur) & is.null(input$rech_categorie)& 
       is.null(input$rech_theme)& is.null(input$rech_themecovadis)){
      return(0)}
    else{return(1)}
  })
  
  calcul_numfin <- reactive({ # renvoie le ou les indices des donnees correspondant a la/aux recherche(s)
    # on recupere le nombre de recherches de chaque champ (combien de noms, combien d'auteurs, ...)
    n_auteur <- length(input$rech_auteur) ; n_categorie <- length(input$rech_categorie)
    n_nom_motcle <- length(input$rech_nom_motcle) ; n_theme <- length(input$rech_theme) ; n_themecovadis <- length(input$rech_themecovadis)
    # exemple avec l'auteur : si on a recherche un auteur, la condition "if" suivante renvoie les indices des donnees qui ont
    # cet auteur. sinon, elle renvoie tous les indices du tableau.
    # on fait ca pour les 5 champs, puis on fait une intersection des indices obtenus : a la fin, on n'a plus que les indices
    # des donnees qui nous interessent
    if(n_auteur == 0){indice_auteur <- c(1:nlength)
    } else{indice_auteur <- which(ress$Organisme %in% input$rech_auteur)}
    if(n_nom_motcle == 0){indice_nom_motcle <- c(1:nlength)
    } else{indice_motcle <- c() ; indice_nom <- c()
    # avec les mots-cles on doit ruser. En effet, on peut avoir des problemes : si on recherche "eau", 
    # ca afficherait les donnees avec "oiseaux" dans les mots-cles, car "eau" dans "oiseaux"...
    for (i in input$rech_nom_motcle){indice_motcle <- append(indice_motcle, which(sapply(strsplit(ress$`Mots-clés`, ", "), function(y) i %in% y)))}
      indice_nom <- which(ress$Nom %in% input$rech_nom_motcle)
      indice_nom_motcle <- c(indice_motcle, indice_nom)
    }
    if(n_categorie == 0){indice_categorie <- c(1:nlength)
    } else{indice_categorie <- which(ress$Catégorie %in% input$rech_categorie)}
    if(n_theme == 0){indice_theme <- c(1:nlength)
    } else{indice_theme <- which(ress$Thème %in% input$rech_theme)}
    if(n_themecovadis == 0){indice_themecovadis <- c(1:nlength)
    } else{indice_themecovadis <- which(ress$`Thème Covadis` %in% input$rech_themecovadis)}
    # intersection des indices
    numfin <- intersect(intersect(intersect(intersect(indice_themecovadis,indice_theme),indice_categorie),indice_auteur),indice_nom_motcle)
    # precision de la recherche avec le type de carte, et les donnees disponibles
    if(input$rech_carte == "carteinteractive"){numfin <- intersect(numfin, which(ress$Cartes == "Carte interactive"))}
    if(input$rech_carte == "cartepdf"){numfin <- intersect(numfin, which(ress$Cartes == "Carte PDF"))}
    if(input$rech_carte == "pascarte"){numfin <- intersect(numfin, which(ress$Cartes == ""))}
    if(input$rech_brutes == "TRUE"){numfin <- intersect(numfin, which(ress$`Données brutes` == "TRUE"))}
    if(input$rech_sig == "TRUE"){numfin <- intersect(numfin, which(ress$SIG == "TRUE"))}
    return(numfin) # on recupere le ou les indice(s) des donnees correspondant a la recherche
  })
  
  output$texte_recherche_nulle <- renderPrint({
    # affiche du texte s'il n'y a pas de recherche faite (a l'ouverture du site) ou si rien ne correspond
    aucrech <- aucune_recherche()
    numfin <- calcul_numfin()
    if(aucrech == 0){cat("Aucune recherche saisie")}
    if(aucrech == 1 & length(numfin) == 0){cat("Aucune donnée ne satisfait tous les critères")}
  })
  
  output$message_choix_donnee <- renderUI({
    numfin <- calcul_numfin()
    auc <- aucune_recherche()
    if(auc == 1 & length(numfin) >1){ # s'il y a des resultats correspondant a la recherche
    fluidRow(length(numfin), "résultats correspondants. Choisir la donnée dans la liste ci-dessous",tags$em(style = "color: #337ab7", 
    "(les résultats sont dans l'ordre alphabétique)"), ": ")} # texte qui s'affiche
  })
  
  output$message_trop_donnee <- renderUI({
    numfin <- calcul_numfin()
    auc <- aucune_recherche()
    if(auc == 1 & length(numfin) >14 ){ # si 15 resultats ou plus : le message indique comment affiner la recherche
      if(length(unique(ress[numfin, "Catégorie"]))>1){ # affiner par categorie s'il y a plus d'une categorie
        choixtropcategorie <- paste(unique(ress[numfin, "Catégorie"]), collapse = ", ")
        fluidRow("(Pour mieux s'y retrouver, affiner la recherche en sélectionnant une des catégories suivantes :", 
                 tags$b(style = "color:#398035", choixtropcategorie), ").")
      }
      # affiner par theme covadis s'il y a plus d'un theme covadis
      else if(length(unique(ress[numfin, "Thème Covadis"]))>1){choixtropcov <- paste(unique(ress[numfin, "Thème Covadis"]), collapse = ", ")
      fluidRow("(Pour mieux s'y retrouver, affiner la recherche en sélectionnant un des thèmes Covadis suivants :", 
               tags$b(style = "color:#398035", choixtropcov), ").")}
      }
  })
  
  output$choix_donnee <- renderUI({
    numfin <- calcul_numfin()
    auc <- aucune_recherche()
    if(auc == 1 & length(numfin) >1){ # s'il y a plus d'un resultat correspondant
      radioButtons("choix", "", choices = sort(ress$Nom[numfin]), inline = T)} # resultat dans l'ordre alphabetique, et pas l'un sous l'autre (sinon trop long)
  })
  
  numfin_unique <- reactive({ # s'il y a plus d'une donnee correspondant a la recherche, la fonction renvoie la donnee choisie par
                              # l'utilisateur (par defaut la premiere dans l'ordre alphabetique)
    numfin <- c() # on initialise la valeur avec une longueur 0
    aucrech <- aucune_recherche()
    if(aucrech == 1){
      numfin <- calcul_numfin()
      if(length(numfin)>0){
        if(length(numfin)>1){numfin <- which(ress$Nom %in% input$choix)}
        return(numfin)
      }}
  })
  
  output$table_info_donnee <- renderDataTable({
    numfin <- numfin_unique()
    if(length(numfin)>0){#s'il y a une (et une seule) donnee a afficher
        table_info <- datatable(cbind(t(ress[numfin,c(1:9,12:14)]), rep("",length(c(1:9,12:14)))), # informations + colonne vide (qui sera remplie avec les liens)
                         colnames = c("Information","Lien(s)"), # nom provisoire des colonnees
                         escape = F, options = list(sDom  = "<'top'>t<'bottom'>", ordering = F,  # options pour l'affichage du tableau, notamment pour qu'il soit simple
                                                    pageLength = 15,lengthMenu = c(15, 30),columnDefs = list(list(width = "1000px", targets = 1)))) %>% 
          formatStyle(columns = 0, fontWeight = "bold", `text-align` = "left") # premiere colonne en gras
        colnames(table_info$x$data) <- c("", "Information","Lien(s)") # nom definitif des colonnees. table_info2$x$data car c'est un datatable, un gros objet complexe
        table_info$x$data$`Lien(s)` <- createLink(table_info$x$data$Information) # troisieme colonne remplie avec des liens
        table_info$x$data$`Lien(s)`[c(1,3:11)] <- "" # on ne garde que les vrais liens
        if(table_info$x$data[12,2] == ""){table_info$x$data <- table_info$x$data[-12,]} # on enleve le lien de la ligne "Autre lien interessant" s'il n'y en a pas
        return(table_info)
    }
  })
  
  output$message_donnee_agents <- renderUI({ 
    numfin <- numfin_unique()
    if(length(numfin)>0){
        if(ress[numfin, "Bancarisation"]!="" | ress[numfin, "Hauts-de-France"]!=""){ # s'il y a quelque chose dans les lignes Bancarisation et Hauts-de-France
          fluidRow(br(), br(),tags$b("Pour les agents OFB :"))
        }
      }
  })
  
  output$table_info_donnee_agents <- renderDataTable({
    numfin <- numfin_unique()
    if(length(numfin)>0){
        if(ress[numfin, "Bancarisation"]!="" | ress[numfin, "Hauts-de-France"]!=""){ # s'il y a quelque chose dans les lignes Bancarisation et Hauts-de-France
          table_info2 <- datatable(cbind(t(ress[numfin,c(15,19)]), rep("",2)), # tableau avec les 2 informations
                            colnames = c("Information","Lien(s)"),
                            escape = F, options = list(sDom  = "<'top'>t<'bottom'>", ordering = F, # options (voir precedemment)
                                                       pageLength = 12,lengthMenu = c(12,24),columnDefs = list(list(width = "1000px", targets = 1)))) %>% 
            formatStyle(columns = 0, fontWeight = "bold", `text-align` = "left")
          colnames(table_info2$x$data) <- c("", "Information","Lien(s)") # table_info2$x$data car c'est un datatable, un gros objet complexe
          table_info2$x$data$`Lien(s)` <- createLink(table_info2$x$data$Information) # on met des liens dans toute la colonne
          table_info2$x$data["Hauts-de-France", 3] <- "" # on enleve le lien car il n'y en a pas
          if(table_info2$x$data["Bancarisation",2]== ""){table_info2$x$data <- table_info2$x$data[-which(rownames(table_info2$x$data)=="Bancarisation"),]} # enleve la ligne si elle n'est pas renseignee
          if(table_info2$x$data["Hauts-de-France",2] == ""){table_info2$x$data <- table_info2$x$data[-which(rownames(table_info2$x$data)=="Hauts-de-France"),]} # idem, enleve la ligne si vide
          return(table_info2)
      }
    }
  }, escape = F, options = list(sDom  = "<'top'>t<'bottom'>", ordering = F, 
                                pageLength = 4,lengthMenu = c(4,8),columnDefs = list(list(width = "1000px", targets = 1))))
  
  output$info_moteur_recherche <- renderText({ # sites disponibles ou pas sur les moteurs de recherche
    numfin <- numfin_unique()
    if(length(numfin)>0){
        navig <- c("Mozilla", "Chrome", "Internet Explorer")[as.logical(ress[numfin, c(16:18)])] # ne garde que les moteurs qui sont indiques disponibles (TRUE)
        c("Compatible avec : ", paste(navig, collapse = ", "))
        }
  })
  
  output$info_dispo_brutes <- renderText({ # indique si donnees brutes disponibles
    numfin <- numfin_unique()
    if(length(numfin)>0){
        c("Données brutes ", ifelse(ress[numfin, 10],"\u2705", "\u274C")) # si disponibles (TRUE), affiche un V vert / sinon, un X rouge
      }
  })
  output$info_dispo_sig <- renderText({
    numfin <- numfin_unique()
    if(length(numfin)>0){
        c("SIG", ifelse(ress[numfin, 11],"\u2705", "\u274C")) # si disponible (TRUE), affiche un V vert / sinon, un X rouge
      }
  })
  
  output$bouton_telechargement_fichedonnee <- renderUI({ # bouton pour telecharger la fiche donnee
    aucrech <- aucune_recherche()
    if(aucrech == 1){
      numfin <- calcul_numfin()
      if(length(numfin)>0){
        downloadButton("download_fichedonnee", "Télécharger la fiche de la donnée")
      }}
  })
  
  output$download_fichedonnee <- downloadHandler(
          filename = function(){ # nom du ficher telecharge : contient le nom de la donnee et la date. format PDF
            numfin <- numfin_unique()
            if(length(numfin)>0){
                paste("fiche-donnée_", gsub(" ", "-", gsub("/", "", gsub("'", "-", ress$Nom[numfin]))), "_", 
                      format(Sys.Date(), format = "%d-%m-%Y"), ".pdf", sep = "")}
            },
          content = function(file) {
            chemin_fichier <- file.path(here(), "template_fichedonnee.Rmd") #
            file.copy(paste(here(),"template_fichedonnee.Rmd", sep = ""), chemin_fichier, overwrite = TRUE)
            numfin <- numfin_unique()
            if(length(numfin)>0){
                params <- list(n = numfin)} # un parametre necessaire a la compilation : numfin (indice de la donnee)
            rmarkdown::render(chemin_fichier, output_file = file,
                              params = params, envir = new.env(parent = globalenv()) # compile le document avec le bon parametre
           )
    }
  )
  
  output$titre_telechargement_dateheure <- renderUI({ # renvoie le titre du sous-onglet telechargement avec la date et l'heure actuelles
    date <- Sys.time() # recuperation date et heure
    month <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    mois <- c("janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août", "septembre", "octobre", "novembre", "décembre")
    date <- paste(format(date, format = "%d"), " ", mois[format(date, format = "%B") == month], " ", # besoin de reformater, car sinon en format entierement numerique et anglais
                  format(date, format = "%Y"), " à ", format(date, format = "%H"), "h", format(date, format = "%M"), sep = "")
    fluidRow(style="border-bottom: 2px solid ; border-top: 2px solid ; border-right: 2px solid ; border-left: 2px solid ;background-color:lightyellow",
             h4(style="color:#398035; padding: 10px", "Télécharger les données ( à jour le", date, ")"))
  })

  output$download_inventairecompletEXCEL <- downloadHandler( # telecharge inventaire complet format Excel
    filename = function() {
      paste("inventaire-complet_", format(Sys.Date(), format = "%d-%m-%Y"), ".csv", sep = "")},
    content = function(file) {
      write.table(ress, file, row.names = F, sep = ";", fileEncoding = "UTF-16LE") # write.table pour choisir encodage et sep
    })
  
  output$download_inventaireOFBEXCEL <- downloadHandler( # telecharge inventaire OFB format Excel
    filename = function() {
      paste("inventaire-OFB_", format(Sys.Date(), format = "%d-%m-%Y"), ".csv",sep = "")},
    content = function(file) {
      write.table(ress[which(ress$Organisme=="OFB"),], file, row.names = F, sep = ";", fileEncoding = "UTF-16LE")
    })
  
  output$download_inventairecomplet <- downloadHandler( # telecharge inventaire complet avec separateur classique (";")
    filename = function() {
      paste("inventaire-complet_", format(Sys.Date(), format = "%d-%m-%Y"), ".csv", sep = "")},
    content = function(file) {
      write.csv(ress, file, row.names = F) # write.csv car format classique
    })
  
  output$download_inventaireOFB <- downloadHandler( #telecharge inventaire OFB avec separateur classique (";")
    filename = function() {
      paste("inventaire-OFB_", format(Sys.Date(), format = "%d-%m-%Y"), ".csv",sep = "")},
    content = function(file) {
      write.csv(ress[which(ress$Organisme=="OFB"),], file, row.names = F)
    })
  
  output$tablethemeCovadis <- renderDataTable({ # tableau des themes / themes covadis
    # renderDataTable pour rendre un tableau propre
    tableClassement1 <- datatable(table_correspondance_covadis, rownames = FALSE, 
                        options = list(
                          rowsGroup = list(0), # fusion des cellules de 1ere colonne
                          escape = F, sDom  = "<'top'>t<'bottom'>", ordering = F,pageLength = 30,lengthMenu = c(30, 60),
                          columnDefs = list(list(width = "300px", targets = 1)) # fixation des largeurs de colonne
                        ))
    tableClassement1$dependencies <- c(tableClassement1$dependencies, list(dependency_covadis))
    tableClassement1
  })
  
  output$tablecategorie <- renderDataTable({ # tableau des categories
    tableClassement2 <- datatable(cbind(sort(unique(ress$Catégorie)),rep("",length(unique(ress$Catégorie)))), rownames = FALSE, colnames = c("Catégorie",""),
                         options = list(
                           escape = F, sDom  = "<'top'>t<'bottom'>", ordering = F,pageLength = 10,lengthMenu = c(10,20)
                         ))
    tableClassement2
  })
  
  output$tablemotsclés <- renderDataTable({ # tableau des mots-cles
    liste_motcle <- sort(unique(unlist(strsplit(ress$`Mots-clés`, ", "))))
    liste_motcle <- liste_motcle[liste_motcle %in% toupper(unique(c(ress$Catégorie, ress$Thème, ress$`Thème Covadis`))) == FALSE] # on ne garde que mots-cles qui ne sont
                                                                                                                              # ni categorie, ni theme, ni theme covadis
    tableClassement3 <- datatable(cbind(liste_motcle,
                               rep("",length(liste_motcle))), rownames = FALSE, colnames = c("Mots-clés",""),
                         options = list(
                           escape = F, sDom  = "<'top'>t<'bottom'>", ordering = F, pageLength = 200
                         ))
    tableClassement3
  })
  
  output$table_categorie_choisie <- renderDataTable({ # tableau des donnees de la categorie choisie
    if(input$liste_categorie =="Tous"){tableListe1 <- cbind(sort(ress[,c(1)])) # toutes les categories
    colnames(tableListe1) <- "Nom de la donnée"
    return(tableListe1)}
    else if(input$liste_categorie !=""){tableListe1 <- cbind(sort(ress[which(ress$Catégorie %in% input$liste_categorie),c(1)])) # seulement celles choisies
    colnames(tableListe1) <- "Nom de la donnée"
    return(tableListe1)}
  }, escape = F, options = list(sDom  = "<'top'>t<'bottom'>", ordering = F,pageLength = nlength,lengthMenu = c(nlength, 2*nlength)), rownames = F)
  
  output$table_theme_choisi <- renderDataTable({ # tableau des donnees du theme choisi
    if(input$liste_theme =="Tous"){tableListe2 <- cbind(sort(ress[,c(1)])) # tous les themes
    colnames(tableListe2) <- "Nom de la donnée"
    return(tableListe2)}
    else if(input$liste_theme !=""){tableListe2 <- cbind(sort(ress[which(ress$Thème %in% input$liste_theme),c(1)])) # seulement les themes choisis
    colnames(tableListe2) <- "Nom de la donnée"
    return(tableListe2)}
  }, escape = F, options = list(sDom  = "<'top'>t<'bottom'>", ordering = F,pageLength = nlength,lengthMenu = c(nlength, 2*nlength)), rownames = F)
  
  output$table_themecovadis_choisi <- renderDataTable({ # tableau des donnees du theme covadis choisi
    if(input$liste_themecovadis =="Tous"){tableListe3 <- cbind(sort(ress[,c(1)])) # tous les themes covadis
    colnames(tableListe3) <- "Nom de la donnée"
    return(tableListe3)}
    else if(input$liste_themecovadis !=""){tableListe3 <- cbind(sort(ress[which(ress$`Thème Covadis` %in% input$liste_themecovadis),c(1)])) # seulement les themes covadis choisis
    colnames(tableListe3) <- "Nom de la donnée"
    return(tableListe3)}
  }, escape = F, options = list(sDom  = "<'top'>t<'bottom'>", ordering = F,pageLength = nlength,lengthMenu = c(nlength, 2*nlength)), rownames = F)
  
  output$download_Ressourcesinternet <- downloadHandler( # telecharge le PDF des listes de ressources internet OFB
    filename = function() {
      "liste_ressources-internet_OFB.pdf"},
    content = function(file) {
      file.copy("liste_ressources-internet_OFB.pdf", file)
    })
  
  output$download_PAC <- downloadHandler(# telecharge le PDF des liens PAC
    filename = function() {
      "liens_PAC.pdf"},
    content = function(file) {
      file.copy("liens_PAC.pdf", file)
    })
  
  sauvegarde_inventaire <- function(data) { # enregistre l'inventaire sur la Dropbox
    file_path <- file.path(tempdir(), "inventaire_tableur.csv") # enregistrement temporaire
    write.csv(data, file_path, row.names = F) # ecriture du "nouvel" inventaire (inventaire mis a jour)
    drop_upload(file_path, path=folder_dropbox, mode = "overwrite") # chargement de cet inventaire sur la Dropbox, en ecrasant l'autre
  }
  
  output$edition_modif <- renderUI({ # modification du champ choisi
    if(input$champ_modif == "SIG"){ # si modification de SIG : radioButtons (que deux choix possibles)
      radioButtons("dispo_modif", "Choisir la nouvelle information", choices = c("Disponible","Non disponible"), 
                   selected=ifelse(ress[which(ress$Nom == input$donnee_modif),input$champ_modif]=="TRUE","Disponible", "Non disponible")) # par defaut, information dejà existante
    }
    # si modification de Donnees brutes : radioButtons (que deux choix possibles)
    else if(input$champ_modif == "Données brutes"){
      radioButtons("dispos_modif", "Choisir la nouvelle information", choices = c("Disponibles","Non disponibles"), 
                   selected=ifelse(ress[which(ress$Nom == input$donnee_modif),input$champ_modif]=="TRUE","Disponibles", "Non disponibles")) # par defaut, information dejà existante
    }
    # si modification de moteur de recherche : radioButtons (que deux choix possibles)
    else if(input$champ_modif == "Mozilla" | input$champ_modif == "Chrome" | input$champ_modif == "Internet Explorer"){
      radioButtons("truefalse_modif", "Choisir la nouvelle information", choices = c("Compatible","Non compatible"),
                   selected=ifelse(ress[which(ress$Nom == input$donnee_modif),input$champ_modif]=="TRUE","Compatible", "Non compatible")) # par defaut, information dejà existante
    }
    # si modification d'un texte : textInput (texte libre)
    else if(input$champ_modif == "Nom" | input$champ_modif == "Adresse" | input$champ_modif == "Organisme" |
       input$champ_modif == "Date de mise à jour" | input$champ_modif == "Mots-clés" | input$champ_modif == "Utilisation" | input$champ_modif == "Hauts-de-France"){
      textInput("texte_modif","Modifier l'information",
               value = ress[which(ress$Nom == input$donnee_modif),input$champ_modif], width=1000) # par defaut, information dejà existante
    }
    # si modification de Categorie/Theme Covadis/Cartes : selectizeInput (que quelques choix possibles)
    else if(input$champ_modif == "Catégorie" | input$champ_modif == "Cartes" | input$champ_modif == "Thème Covadis"| input$champ_modif == "Type"){
      selectizeInput("select_modif","Choisir la nouvelle information",
                choices = c(ifelse(input$champ_modif == "Cartes","Aucune",""),
                            sort(unique(ress[,input$champ_modif]))),
                selected = ifelse(ress[which(ress$Nom==input$donnee_modif),input$champ_modif]=="", "Aucune", # par defaut, information dejà existante
                                  ress[which(ress$Nom==input$donnee_modif),input$champ_modif]))
    }
    # si modification d'autre lien : textInput (texte libre)
    else if(input$champ_modif == "Autre lien intéressant"){
      textInput("texte_lien_modif","Information sur la cible du lien (exemple : « Notice », « Téléchargement de SIG »)", # ici, texte de precision 
                value = ifelse(ress[which(ress$Nom == input$donnee_modif),input$champ_modif]=="","",
                               sapply(strsplit(as.character(ress[which(ress$Nom == input$donnee_modif),input$champ_modif]), " : "), head, 1)), width = 700) # par defaut, information dejà existante
    }
    # si bancarisation : textInput (texte libre)
    else if(input$champ_modif == "Bancarisation"){
      textInput("bancarisation_modif","Modifier l'information",
                value = ifelse(ress[which(ress$Nom == input$donnee_modif),input$champ_modif]=="","https://",
                               sapply(strsplit(as.character(ress[which(ress$Nom == input$donnee_modif),input$champ_modif]), " : "), head, 1))) # par defaut, information dejà existante ou "https://"
    }
  })
  output$edition_modif2 <- renderUI({
    if(input$champ_modif == "Autre lien intéressant"){
      textInput("lien_modif","Lien", # ici, lien en soi
                value = ifelse(ress[which(ress$Nom == input$donnee_modif),input$champ_modif]=="","https://",
                               sapply(strsplit(as.character(ress[which(ress$Nom == input$donnee_modif),input$champ_modif]), " : "), tail, 1)), width = 1000) # par defaut, information dejà existante ou "https://"
    }
  })
  
  observeEvent(input$sauvegarde_modif,{ # modification de la cellule a l'intersection de la donnee et du champ a modifier,
    #selon ce que l'utilisateur a modifie (voir output$edition_modif)
    if(input$champ_modif == "SIG"){
      ress[which(ress$Nom == input$donnee_modif),input$champ_modif] <- ifelse(input$dispo_modif=="Disponible","TRUE","FALSE")
    }
    else if(input$champ_modif == "Données brutes"){
      ress[which(ress$Nom == input$donnee_modif),input$champ_modif] <- ifelse(input$dispos_modif=="Disponibles","TRUE","FALSE")
    }
    else if(input$champ_modif == "Mozilla" | input$champ_modif == "Chrome" | input$champ_modif == "Internet Explorer"){
      ress[which(ress$Nom == input$donnee_modif),input$champ_modif] <- ifelse(input$truefalse_modif=="Compatible","TRUE","FALSE")
    }
    else if(input$champ_modif == "Nom" | input$champ_modif == "Adresse" | input$champ_modif == "Organisme" |
            input$champ_modif == "Date de mise à jour" | input$champ_modif == "Mots-clés" | input$champ_modif == "Utilisation" | input$champ_modif == "Hauts-de-France"){
      ress[which(ress$Nom == input$donnee_modif),input$champ_modif] <- input$texte_modif
    }
    else if(input$champ_modif == "Catégorie" | input$champ_modif == "Cartes" | input$champ_modif == "Type"){
      ress[which(ress$Nom == input$donnee_modif),input$champ_modif] <- input$select_modif
    }
    else if(input$champ_modif =="Thème Covadis"){
      ress[which(ress$Nom==input$donnee_modif),input$champ_modif] <- input$select_modif
      ress[which(ress$Nom==input$donnee_modif),"Thème"] <- table_correspondance_covadis[which(table_correspondance_covadis$"Thème Covadis" == input$select_modif),1]
    }
    else if(input$champ_modif == "Autre lien intéressant"){
      # si l'utilisateur n'y touche finalement pas mais appuie sur enregistrer, input$champ_modif vaudra "https://", ce qu'on ne veut pas...
      ress[which(ress$Nom == input$donnee_modif),input$champ_modif] <- ifelse(input$texte_lien_modif !="" | nchar(input$lien_modif) <= 8, "",
                                                                          paste(input$texte_lien_modif, ":", input$lien_modif)) # texte + lien, separes par " : "
    } 
    else if(input$champ_modif == "Bancarisation"){
      ress[which(ress$Nom == input$donnee_modif),input$champ_modif] <- ifelse(nchar(input$bancarisation_modif)<=8, "", input$bancarisation_modif)
    }
    sauvegarde_inventaire(ress) # on sauvegarde ce "nouvel" inventaire sur la Dropbox
  })
  
  observeEvent(input$sauvegarde_ajout,{ # ajout d'une information
    ress <- rbind(ress,c( # creation d'une nouvelle ligne dans l'inventaire, avec remplissage de toutes les colonnes
      input$donnee_ajout, input$adresse_ajout, input$organisme_ajout, input$categorie_ajout, 
      table_correspondance_covadis[which(table_correspondance_covadis$"Thème Covadis" == input$covadis_ajout),1], input$covadis_ajout, 
      paste(input$categorie_ajout, table_correspondance_covadis[which(table_correspondance_covadis$"Thème Covadis" == input$covadis_ajout),1], input$covadis_ajout, input$motcle_ajout, sep = ", "), 
      input$maj_ajout, ifelse(input$carte_ajout=="Aucune", "", input$carte_ajout), ifelse(input$brutes_ajout=="Disponibles", "TRUE","FALSE"),
      ifelse(input$sig_ajout=="Disponible", "TRUE","FALSE"), input$utilisation_ajout, 
      ifelse((input$texte_lien_ajout==""|nchar(input$lien_ajout)<=8), "", paste(input$texte_lien_ajout, ":", input$lien_ajout)),input$type_ajout,
      ifelse(nchar(input$bancarisation_ajout)<=8, "", input$bancarisation_ajout), ifelse(input$mozilla_ajout=="Compatible", "TRUE", "FALSE"),ifelse(input$chrome_ajout=="Compatible", "TRUE", "FALSE"),
      ifelse(input$internetexp_ajout=="Compatible", "TRUE", "FALSE"), input$hdf_ajout
    ))
    sauvegarde_inventaire(ress) # sauvegarde de ce nouveau tableau, avec une ligne de plus
  })
  
  output$confirmation_suppr_text <- renderText({
    paste("Êtes-vous sûr(e) de vouloir supprimer cette donnée :",input$donnee_suppr, "? (L'action est irréversible)")
  })
  
  observeEvent(input$confirmation_suppr,{ # suppression d'une information
    ress <- ress[-which(ress$Nom==input$donnee_suppr),] # si l'utilisateur confirme la suppression, on enleve la ligne correspondante
    sauvegarde_inventaire(ress)# sauvegarde de ce nouvel inventaire
  })
  
  
}
