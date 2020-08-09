#### chargement des packages et de quelques variables ####
library(here) ; library(rdrop2) ; library(DT)
path <- here() # folder containing dataTables.rowsGroup.js
outputDir <- "Inventaire_OFB"

#### interface ####
# tout est dans une fluidPage
ui1 <- fluidPage(title="OFB – Ressources disponibles pour les diagnostics territoriaux", # nom de l'onglet du moteur de recherche
  titlePanel(fluidRow(align = 'center',column(1,), # fluidRow avec 4 colonnes : 1 vide / 1 avec le titre / une vide / une avec le logo
                      column(7,align="center",style="color:#083974",
                                              h1("Ressources OFB disponibles pour écrire et consolider les diagnostics territoriaux")), 
                      column(3,img(src="OFBlogo.jpg", width="80%")),
                      column(1,))), # une colonne pour le titre ; une colonne pour l'image
  # tant qu'on n'ecrit pas le bon mot de passe le champ pour l'ecrire s'affiche
  conditionalPanel("input.password!='stageBP'",textInput("password", "Entrer le mot de passe")), 
  # des qu'on ecrit le bon mot de passe, le champ pour l'ecrire disparait et le reste du site s'affiche
  conditionalPanel("input.password=='stageBP'",
  fluidRow(style = 'padding: 20px',align = 'left',tags$em("(Pour des explications sur le fonctionnement du site, voir l'onglet « Informations »)")),
  tabsetPanel(
    #### 1er onglet ####
    tabPanel("Recherche",style = 'padding: 30px',
             br(),
             #fluidRow (elle-meme contenant des colonnes et des fluidRow) avec toutes les barres de recherches
             # |          (largeur 5)           |             (largeur 5)               |       (largeur 2)
             # | "rechercher par nom/mot-cle"   |       "selectionner categorie"        |
             # |      barre de recherche        |          barre de recherche           | type de carte + options
             # |    "selectionner auteur"       |         "selectionner theme"          | "Donnees disponibles"
             # |      barre de recherche        | recherche theme | recherche Covadis   |       options
             # |                                |                                       |
             fluidRow(style="border-bottom: 2px solid ; border-top: 2px solid ; border-right: 2px solid ; border-left: 2px solid ;background-color:lightyellow", # definition des bordures, de l'arriere-plan
                    fluidRow(style="color:white","a"), # ligne vide qui sert a avoir un espace entre la bordure et la 1ere ligne 
                      column(5,
                             fluidRow(h4(style="color:#398035; padding: 10px", "Sélectionner par nom ou MOTS-CLÉS ... ")),
                             # on doit passer par les uiOutput, car sinon, les choix de noms, d'auteurs, etc. se figent au moment
                             # du deploiement du site, donc ne tient pas compte des modifications. En fonctionnant avec des uiOutput,
                             # les choix sont dans le SERVER, et sont mis a jour a chaque consultation du site
                             uiOutput("render1"), # recherche par nom/mot-cle
                             fluidRow(style="color:#398035 ; padding: 10px",h4("... et/ou sélectionner par auteur ...")),
                             uiOutput("render2"), # recherche par auteur
                             fluidRow(h6(" ")),
                             ),
                      column(style="border-left: 2px dashed ; border-right: 2px dashed",
                             width=5,
                             fluidRow(h4(style="color:#398035; padding: 10px", "... et/ou sélectionner par catégorie ...", tags$small(tags$em("(voir onglet « Classement »)")))),
                             uiOutput("render3"), # recherche par categorie
                             fluidRow(style="color:#398035 ; padding: 10px",h4("... et/ou sélectionner par thème", tags$small(tags$em("(voir onglet « Classement »)")))),
                             uiOutput("render4") # recherche par theme / theme covadis
                             ),
                      column(2,
                             fluidRow(h6(" ")),
                             uiOutput("render5"), # choix de disponibilite de carte
                             fluidRow(h4(style="color:#398035 ; padding: 15px","Données disponibles")),
                             uiOutput("render6"), # disponibilite de SIG
                             uiOutput("render7"), # disponibilite de donnees brutes
                             fluidRow(h6(" "))
                             )),
             br(), br(),
             # affichage des resultats de la recherche (tableaux, messages, boutons)
             textOutput("texte_recherche_nulle"),
             uiOutput("message_choix_donnee"), uiOutput("message_trop_donnee"),
             uiOutput("choix_donnee"),
             dataTableOutput("table_info_donnee"),uiOutput("message_donnee_agents"),dataTableOutput("table_info_donnee_agents"),
             hr(),
             # les infos sur les donnees brutes, le SIG et les moteurs de recherche sont sur une meme ligne
             # | (largeur 1)  |   (largeur 2)   | (largeur 2)  | (largeur 1)  |   (largeur 6)     |
             # |              | Donnees brutes  |     SIG      |              | moteur recherche  |
             fluidRow(column(1,),
                      column(2, textOutput("info_dispo_brutes")),
                      column(2, textOutput("info_dispo_sig")),
                      column(1,),
                      column(6, textOutput("info_moteur_recherche"))),
             hr(),
             # la premiere colonne vide (de largeur 9) permet de placer le bouton a droite
             fluidRow(column(9,),
                      column(3, uiOutput("bouton_telechargement_fichedonnee")))),
    #### 2e onglet ####
    tabPanel("Informations",style = "padding: 30px",
             #### sous-onglet 1 de l'onglet 2 ####
             tabsetPanel(tabPanel("Le site",style = "padding: 30px",
             br(),
             fluidRow(style="border-bottom: 2px solid ; border-top: 2px solid ; border-right: 2px solid ; border-left: 2px solid ;background-color:lightyellow",
                      h4(style="color:#398035; padding: 10px", "Informations sur le site")), 
             br(),
             fluidRow(h4(style = "color:#CB1600", "Organisation des onglets" )),
             fluidRow("Cette application, facilitant l'utilisation de l'inventaire de données OFB, comporte six onglets :"),
             
             tags$ul( # tags$ul pour une liste non-numerotee (Unordered List) ; tags$li pour un tiret (List Item)
               tags$li("un onglet",tags$b("Recherche"), "qui permet d'accéder à une base de donnée", tags$em("via"),
                       "un site internet (le bouton « Lien » permet d'y accéder directement) ;"),
               tags$li("un onglet",tags$b("Informations"), "(onglet actuel) qui permet de s'y retrouver dans l'application et explique
                        les termes utilisés ;"),
               tags$ul(
                 tags$li("un sous-onglet",tags$b("Le site"), "(sous-onglet actuel) qui indique comment se servir du site et où trouver des informations ;"),
                 tags$li("un sous-onglet",tags$b("Téléchargements"), "qui permet de télécharger l'inventaire (complet ou seulement OFB) en divers formats ;"),
                 tags$li("un sous-onglet",tags$b("Aspects techniques"), "qui donne des informations plus précises sur les codes et fonctions utilisées.")
               ),
               tags$li("un onglet",tags$b("Classement"), "qui indique à quels thèmes les sous-thèmes Covadis se rapportent ;"),
               tags$li("un onglet",tags$b("Listes"), "qui donne la liste des données correspondant à un thème, un thème Covadis ou une catégorie ;"),
               tags$li("un onglet",tags$b("Répertoires"), "qui liste les répertoires de données qui ont, entre autres, permis d'élaborer cet inventaire ;"),
               tags$li("un onglet",tags$b("Édition"), "qui permet d'éditer l'inventaire de données. On peut modifier l'information d'une donnée (adresse, date de 
                       mise à jour, etc.), ajouter une donnée, ou encore supprimer une donnée."),
               tags$ul(
                 tags$li("un sous-onglet",tags$b("Correction"), "pour modifier un(des) champ(s) d'une donnée ;"),
                 tags$li("un sous-onglet",tags$b("Ajout"), "qui permet d'ajouter une donnée ;"),
                 tags$li("un sous-onglet",tags$b("Suppression"), "pour supprimer de façon irréversible une donnée.")
               ),
             ),
             hr(),
             fluidRow(h4(style = "color:#CB1600", "Comment utiliser le résultat de la recherche" )),
             fluidRow("Le tableau sur la donnée renvoyé contient beaucoup d'informations : toutes ne sont pas utiles, mais servent à la recherche de la donnée."),
             fluidRow("On s'intéressera aux éléments suivants : "),
             tags$ul(
               tags$li(tags$b("Adresse :"), "l'adresse est accessible facilement en cliquant sur le bouton de la colonne de droite. Si on a besoin du lien, il
                       suffit de copier-coller l'adresse qui apparaît dans la colonne centrale ;"),
               tags$li(tags$b("Cartes, SIG"), "et", tags$b("données brutes :"), "disponibilité de la donnée (utile si on n'a pas fait de recherche avec ces critères) ;"),
               tags$li(tags$b("Utilisation :"), "qui donne des renseignements sur l'accès à la donnée : facilité d'usage de la visionneuse,
                       possibilité d'exporter une carte ou pas, où cliquer pour aller chercher la donnée ;"),
               tags$li(tags$b("Autre lien intéressant :"), "parfois, un autre lien est indiqué, qui renvoie sur la notice d'utilisation de la carte,
                       une explication de l'obtention de la donnée, le lien du téléchargement de la donnée, ..."),
               fluidRow(tags$span(style="color:white","w"), "Comme pour l'adresse, il suffit de cliquer sur le lien dans la colonne de droite ;"),
               tags$li(tags$b("Bancarisation :"), "parfois, pour des données venant de l'OFB, il est indiqué l'adresse où se rendre pour bancariser la donnée
                       (il est là pour s'y retrouver, constituer une forme de mémoire de service) ;"),
               tags$li(tags$b("Hauts-de-France :"), "parfois, la donnée est mise à disposition des agents des SD des Hauts-de-France (QGis Explorer, cartes
                       dans des dossiers des serveurs, ...) : si c'est le cas, c'est rappelé ici ;"),
               tags$li(tags$b("Compatibilité :"), "des sites sont parfois inaccessibles depuis certains moteurs de recherche, ou l'affichage n'y est pas excellent.
                       Trois moteurs de recherche sont indiqués pour information : Mozilla Firefox, Google Chrome et Internet Explorer.")
             ),
             hr(),
             fluidRow(h4(style = "color:#CB1600", "Classement des données" )),
             fluidRow("Les données sont classées par", tags$em("Catégorie"), "/", tags$em("Thème"), "/", tags$em("Thème Covadis"), ":"),
             tags$ul(
               tags$li("La",tags$em("catégorie"), "est un classement informel, qui reprend les grandes thématiques de l'OFB (espèces, zonages, eau, ...) ;"),
               tags$li("Le",tags$em("thème"), "est un classement formel, issu du SIdSI (Système d'Informations des Systèmes d'Informations), un outil mis au point
                         par la DREAL. Les thèmes sont très généraux (« Eau », « Risque », ...) ;"),
               tags$li("Le",tags$em("thème Covadis"), "est un modèle de standard de données, également employé dans le SIdSI, qui permet de classer des
                         données spatialisées. Les thèmes Covadis sont des sous-thèmes des", tags$em("Thèmes"), "et sont plus précis (« Masse d'eau », « Occupation du sol », ...).")
             ),
             hr(),
             fluidRow(h4(style = "color:#CB1600", "Comment rechercher" )),
             fluidRow("L'onglet",tags$b("Recherche"), "permet de naviguer dans l'inventaire de données et d'en trouver plus rapidement. On peut rechercher une donnée :"),
             tags$ul(
               tags$li("Par",tags$b("nom/mots-clés"), "en recherchant simplement le nom de la donnée (« Aire de protection de biotope », « Loup gris », ...) ou 
                         un mot-clé qui lui est rattaché (« Faune », « Protection », ...) — pour s'y retrouver plus facilement, ces mots-clés sont en MAJUSCULE ;"),
               tags$li("Par",tags$b("auteur"), "(néanmoins, parfois, le nom indiqué est le nom de la plateforme sur laquelle se trouve la donnée, 
                         comme « DREAL »). Pour des données participatives, il faut choisir « Citoyens » ;"),
               tags$li("Par",tags$b("catégorie / thème / thème Covadis"), "qui sont les classements expliqués ci-dessus ;"),
               tags$li("Par",tags$b("type de carte"), "au choix entre carte interactive / carte PDF / pas de carte (dans ce cas, uniquement le jeu de données),
                       ou n'importe (choix par défaut). Bien entendu, on ne peut choisir qu'une de ces propositions ;"),
               tags$li("Par",tags$b("données disponibles"), "(données brutes et/ou données SIG). On peut cocher les deux options, une seule, ou aucune des deux
                         (dans ce dernier cas, les données affichées peuvent ou non avoir des données brutes/du SIG.")
             ),
             fluidRow(tags$b(style="color:#009193", "Conseils :")),
             tags$ul(
               tags$li(tags$em("Si vous savez quelle donnée chercher,"), "la taper dans « Nom/MOTS-CLÉS »"),
               tags$li(tags$em("Si vous voulez comparer la même donnée présente sur plusieurs sites", tags$span(style="color:grey", "(ex : les sites Natura 2000)"), ","), " sélectionner le mot-clé correspondant, 
                       ou sur toutes les noms de données qui s'y rapportent. Ensuite, on passe de l'un à l'autre en cochant les boutons de choix"),
               tags$li(tags$em("Si vous ne savez pas le nom de la donnée que vous cherchez,"), " commencer par chercher un mot-clé. Ensuite, si besoin, affiner avec un thème,
                       un thème Covadis ou une catégorie ; ou affiner avec l'organisme ayant récolté la donnée."),
               tags$li(tags$em("Si vous cherchez de la donnée SIG, de la donnée brute ou un certain type de carte,"), " renseignez un thème, thème Covadis ou une catégorie,
                       et affiner la recherche avec la donnée disponible ou le type de carte.")
             ),
             fluidRow("On peut sélectionner plusieurs modes de recherche : par exemple, rentrer un auteur et un thème. Si une(des) donnée(s) correspond(ent) à ces deux
                        critères, elle(s) apparaîtra(ont) ; sinon, un message indiquera de modifier la recherche."),
             fluidRow("De plus, dans chaque barre de recherche, on peut sélectionner plusieurs items. Dans ce cas, la(les) donnée(s) qui satisfait(ont) 
                          l'un ou l'autre des items apparaîtra(ont). Par exemple, si l'on recherche les thèmes « Agriculture »", tags$b("et"), "« Forêt », 
                          toutes les données qui se rapportent à au moins l'un de ces thèmes apparaîtront.")),
             
             #### sous-onglet 2 de l'onglet 2 ####
             tabPanel("Téléchargement",style = "padding: 30px",
             br(),
             uiOutput("titre_telechargement_dateheure"), # titre dans le SERVER car actualise avec la date et l'heure
             br(),
             fluidRow(tags$b("Tableaux CSV"), "pour ouvrir sous Excel (avec le bon encodage et les bons séparateurs) :"), br(),
             fluidRow(column(6, downloadButton("download_inventairecompletEXCEL", paste("Télécharger l'inventaire complet (", nlength, " données)", sep=""))), 
             column(6, downloadButton("download_inventaireOFBEXCEL", paste("Télécharger l'inventaire des données OFB (", nlengthOFB, " données)", sep="")))),
             br(), br(), hr(), br(),
             fluidRow(tags$b("Tableaux CSV"), "simples (sans encodage, et avec des séparateurs « , ») :"), br(),
             fluidRow(column(6, downloadButton("download_inventairecomplet", paste("Télécharger l'inventaire complet (", nlength, " données)", sep=""))), 
             column(6, downloadButton("download_inventaireOFB", paste("Télécharger l'inventaire des données OFB (", nlengthOFB, " données)", sep=""))))
             ),
             #### sous-onglet 3 de l'onglet 2 ####
             tabPanel("Aspects techniques",style = "padding: 30px",
             fluidRow(style="border-bottom: 2px solid ; border-top: 2px solid ; border-right: 2px solid ; border-left: 2px solid ;background-color:lightyellow",
                      h4(style="color:#398035; padding: 10px", "Informations sur le code")), 
             br(),
             fluidRow(tags$b("Composition du code :")),
             fluidRow("Le site est codé sur R [1]", tags$em("(voir plus bas)"), "et est composé de 3 fichiers de code et d'un fichier Rmarkdown :" ),
             tags$ul(
               tags$li("le fichier",tags$code(style = "red", "global.R"), "permet de charger les données (notamment le tableau d'inventaire) et les", tags$em("packages"),
                       tags$code("rdrop2"), "[2],", tags$code("DT"), "[3] et", tags$code("shiny"), "[4] ;"),
               tags$li("le fichier",tags$code("ui.R"), tags$em("(user interface)"), "code l'affichage du site : les onglets, les barres de recherche,
                       l'affichage des résultats, ... ;"),
               tags$li("le fichier",tags$code(style = "green", "server.R"), "code les fonctions nécessaires pour afficher les résultats : tri des informations pour afficher les bons tableaux,
                       téléchargement des fichiers, mise à jour de l'inventaire, ... ;"),
               tags$li("le fichier",tags$code("template_fichedonnee.Rmd"), "permet de compiler un document PDF personnalisé, en l'occurence, 
                       une fiche de la donnée sélectionnée (bouton en bas de l'onglet", tags$b("Répertoires"), "), en prenant des paramètres dans le fichier",
                       tags$code("server.R"), "."),
             ),
             fluidRow("Ces codes ont été déployés localement sur un serveur ShinyApps, qui permet un accès à distance depuis n'importe quel ordinateur,
                      pourvu qu'on ait le lien et le mot de passe." ),
             fluidRow("Afin de fonctionner, le site a également besoin, localement au moment du déploiement, et continuellement à distance :"),
             tags$ul(
               tags$li("du fichier d'inventaire, stocké en ligne et qui contient toutes les données ainsi que leurs informations (nom, adresse, ...) ;"),
               tags$li("d'un fichier contenant les correspondances entre Thème et Thème Covadis, pour l'onglet", tags$b("Classement"), ";"),
               tags$li("des images affichées sur ce site (logo de l'OFB, aperçus des sites des répertoires) ;"),
               tags$li("des fichiers PDF téléchargeables dans l'onglet", tags$b("Répertoires"), "(les téléchargements de données du sous-onglet", 
                       tags$b("Téléchargement"), "se font à partir du fichier d'inventaire).")
             ),
             img(src="schema_code.jpg", width="70%", style="display: block; margin-left: auto; margin-right: auto;"),
             fluidRow(tags$b("Fonctionnement du code :")),
             tags$ul(
               tags$li("À l'ouverture de la page, le tableau d'inventaire est téléchargé depuis l'espace de stockage en ligne (Dropbox) et lu par R.
                       Il comporte de nombreuses informations pour chaque donnée (nom, adresse, organisme, ...)."),
               tags$li("Le visiteur peut rechercher des données parmi cet inventaire, selon les critères expliqués dans le sous-onglet", tags$b("Le site"), "."),
               tags$li("Il peut également s'informer sur le site (présent onglet), sur les modes de classement, sur les principaux répertoires
                       de données (OFB ou non)."),
               tags$li("Il peut enfin modifier le tableau de données (onglet", tags$b("Édition"), "), en corrigeant, ajoutant ou supprimant une donnée.
                       Une fois qu'il a cliqué sur le(s) bouton(s), le tableau est mis à jour sur l'espace de stockage, et il faut rafraîchir la page
                       pour observer l'édition.")
             ),
             fluidRow(tags$b("Fonctionnalités utilisées :")),
             tags$ul(
               tags$li("Dû à la permanente interaction avec l'utilisateur, qui effectue des recherches, modifie des champs ou clique sur les boutons,
                      plusieurs affichages sont conditionnels (usage de", tags$code("conditionalPanel"), "), et d'autres sont dans le ", 
                      tags$code("server.R"), "avec des", tags$code("renderUI"), "et sont appelés dans l'", tags$code("ui.R"), tags$em("via"), tags$code("uiOutput"), "."),
               tags$li("La diversité des champs (textes, arguments logiques, catégories) nécessite des recherches", tags$em("via"), "divers", tags$em("widgets"), ":",
                      tags$code("selectInput"), ",", tags$code("radioButtons"), ",", tags$code("checkboxInput"), " et des", tags$code("actionButton"), ". 
                      La diversité des affichages nécessite des", tags$code("dataTableOutput"), ", des ", tags$code("textOutput"), "et des", tags$code("img"), "(pour les images)."),
               tags$li("Les téléchargements de documents et de tableurs sont possibles grâce à la fonction", tags$code("downloadHandler"), "."),
               tags$li("Enfin, le mot de passe initial n'est qu'un", tags$code("conditionalPanel"), "qui s'affiche tant que l'utilisateur ne rentre pas le bon mot de passe,
                      et disparaît si le mot de passe entré est le bon ; et qui cache le reste du site (également avec un", tags$code("conditionalPanel"), ") tant que le bon mot de passe n'est pas rentré.
                      (Il a été ajouté car quelques cartes nécessitent un mot de passe.)")
               ),
             br(), br(),
             fluidRow("Ce site a été conçu en juillet 2020 par Benoît Pongérard, dans le cadre d'un 
                      stage au service Connaissances de la Direction régionale de l'OFB des Hauts-de-France."),
             hr(),
             fluidRow("[1]", tags$b("R Core Team"), "(2013).", tags$em("R: A Language and Environment for Statistical Computing,"),
                      "R Foundation for Statistical Computing, Vienna, Austria.", tags$a("https://www.R-project.org/", href="https://www.R-project.org/")),
             fluidRow("[2]", tags$b("Karthik Ram and Clayton Yochum"), "(2017).", tags$em("rdrop2: Programmatic Interface to the 'Dropbox' API,"),
                      tags$a("https://CRAN.R-project.org/package=rdrop2", href="https://CRAN.R-project.org/package=rdrop2")),
             fluidRow("[3]", tags$b("Yihui Xie, Joe Cheng and Xianying Tan"), "(2020).", tags$em("DT: A Wrapper of the JavaScript Library 'DataTables',"),
                      tags$a("https://CRAN.R-project.org/package=DT", href="https://CRAN.R-project.org/package=DT")),
             fluidRow("[4]", tags$b("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson"), "(2019).", tags$em("shiny: Web Application Framework for R,"),
                      tags$a("https://CRAN.R-project.org/package=shiny", href="https://CRAN.R-project.org/package=shiny")),
             fluidRow(tags$span(style = "color:white", "[4]"), "Pour s'initier à", tags$code("Shiny"), ", voir l'excellent site : ",
                      tags$a("https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/", href="https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/"),
                      "; et la", tags$em("cheatsheet"), ":", tags$a("https://shiny.rstudio.com/images/shiny-cheatsheet.pdf", href="https://shiny.rstudio.com/images/shiny-cheatsheet.pdf")))
    )),
    #### onglet 3 ####
    tabPanel("Classement",style = "padding: 30px",
             fluidRow(tags$em(style = "color:grey", "Les tableaux ne sont pas interactifs, mais sont là pour information. Pour accéder aux informations sur une donnée,
                              copier-coller le nom de cette donnée dans la barre « Catégorie » ou « Thème » de l'onglet", tags$b("Recherche"), ".")),
             br(),
             fluidRow(column(5,style="border-bottom: 2px solid ; border-top: 2px solid ; border-right: 2px solid ; border-left: 2px solid ;background-color:lightyellow",
                             h4(style="color:#398035; padding: 10px", align ="center", "Liste des thèmes et sous-thèmes (« thèmes Covadis »)")),
                      column(1,),
                      column(2,style="border-bottom: 2px solid ; border-top: 2px solid ; border-right: 2px solid ; border-left: 2px solid ;background-color:lightyellow",
                             h4(style="color:#398035; padding: 10px", align ="center", "Liste des catégories")),
                      column(1,),
                      column(3,style="border-bottom: 2px solid ; border-top: 2px solid ; border-right: 2px solid ; border-left: 2px solid ;background-color:lightyellow",
                             h4(style="color:#398035; padding: 10px", align ="center", "Liste des mots-clés"))), 
             br(), 
             fluidRow(column(5,DTOutput("tablethemeCovadis"),style = "height:750px; overflow-y: scroll;overflow-x: scroll;"),
                      column(1,),
                      column(2,br(),DTOutput("tablecategorie")),
                      column(1,),
                      column(3,br(),fluidRow(DTOutput("tablemotsclés"),style = "height:600px; overflow-y: scroll;overflow-x: scroll;"),
                      br(),fluidRow("(Dans l'onglet", tags$b("Recherche"), "les thème et thème Covadis,  et la catégorie d'une donnée sont également inclus dans les mots-clés, car
                               une donnée peut concerner plusieurs thèmes."),
                      fluidRow(tags$em("Par exemple, l'ICE concerne l'eau douce et les poissons."), ")")))
    ),
    #### onglet 4 ####
    tabPanel("Listes",style = "padding: 30px",
             fluidRow(tags$em(style = "color:grey", "Les tableaux ne sont pas interactifs, mais sont là pour information. Pour accéder aux informations sur une donnée,
                              copier-coller le nom de cette donnée dans la barre « Nom/MOTS-CLÉS » de l'onglet", tags$b("Recherche"), ".")),
             br(),
             fluidRow(style="border-bottom: 2px solid ; border-top: 2px solid ; border-right: 2px solid ; border-left: 2px solid ;background-color:lightyellow",
                      h4(style="color:#398035; padding: 10px", "Liste des données de chaque thème / thème Covadis")), 
             br(),
             column(4,
                    selectInput("liste_categorie", "Liste de données par catégorie", choices = c("","Tous",sort(unique(ress$Catégorie)))),
                    column(12, dataTableOutput("table_categorie_choisie"),style = "height:600px; overflow-y: scroll;overflow-x: scroll;")), # scroll dans les 2 directions
             column(4,
                    selectInput("liste_theme", "Liste des données par thèmes", choices = c("","Tous",sort(unique(ress$Thème)))),
                    column(12, DTOutput("table_theme_choisi"),style = "height:600px; overflow-y: scroll;overflow-x: scroll;")),
             column(4,
                    selectInput("liste_themecovadis", "Liste de données par thèmes Covadis", choices = c("","Tous",sort(unique(ress$`Thème Covadis`)))),
                    column(12, dataTableOutput("table_themecovadis_choisi"),style = "height:600px; overflow-y: scroll;overflow-x: scroll;"))

    ),
    #### onglet 5 ####
    tabPanel("Répertoires",style = "padding: 30px",
             br(),
             fluidRow(style="border-bottom: 2px solid ; border-top: 2px solid ; border-right: 2px solid ; border-left: 2px solid ;background-color:lightyellow",
                      h4(style="color:#398035; padding: 10px", "Sites et documents listant les jeux de données existants")), 
             br(),
             fluidRow(column(9,fluidRow(h4("Geoportail")),
                             fluidRow(h5("Visualiseur cartographique")),
                             fluidRow("Le site recèle beaucoup de données cartographiques dans divers thèmes : Agriculture, Culture, Santé, Tourisme, ..."),
                             fluidRow("Dans l'onglet « Développement durable, énergie », on trouve de nombreuses cartes sur les espaces protégés, l'occupation du sol, les risques."),
                             fluidRow("Toutes les cartes sont facilement modifiables (choix des couches, du fond de carte, de l'échelle). Elles sont aussi facilement exportables."),
                             fluidRow(tags$a("https://www.geoportail.gouv.fr/", href="https://www.geoportail.gouv.fr/"))),
                      column(3,img(src="visu_geoportail.png", width="100%"))),
             hr(),
             fluidRow(column(9,fluidRow(h4("INPN (MNHN)")),
                             fluidRow(h5("Visualiseur cartographique")),
                             fluidRow("Le site recèle beaucoup de données cartographiques dans le domaine de la biodiversité et du zonage."),
                             fluidRow("On peut retrouver notamment les parcs naturels, les réserves, les zones naturelles et protégées [premier lien ci-dessous] ; 
                                      et la répartition de chaque espèce (données expertes ou non) [second lien ci-dessous]."),
                             fluidRow("Toutes les cartes sont facilement modifiables (superposition de couches, de l'échelle). Elles sont aussi exportables."),
                             fluidRow(tags$a("https://inpn.mnhn.fr/viewer-carto/espaces", href="https://inpn.mnhn.fr/viewer-carto/espaces"), "et", 
                                      tags$a("https://inpn.mnhn.fr/viewer-carto/especes", href="https://inpn.mnhn.fr/viewer-carto/especes"))),
                      column(3,img(src="visu_inpn.png", width="100%"))),
             hr(),
             fluidRow(column(9,fluidRow(h4("Répertoires de ressources internet OFB")),
                             fluidRow(h5("Document PDF interne")),
                             fluidRow("Le document recèle 76 liens vers des sites de visualisation / contenant des jeux de données dans divers domaine de la biodiversité, 
                             dans 4 thèmes : Environnement / Espèces et espaces / Eau douce / Mer."),
                             fluidRow("On y trouve aussi bien des sites axés sur les arrêtés, des jeux de données de cours d'eau, des données d'observation de faune/flore, 
                                      le portail technique OFB, ..."),
                             fluidRow("(Quelques sites ne sont accessibles qu'avec un identifiant/mot de passe, comme ASPE.)")),
                      column(3,img(src="visu_ressources.jpg", width="40%",style="display: block; margin-left: auto; margin-right: auto;"))),
             fluidRow(h6()),
             fluidRow(downloadButton("download_Ressourcesinternet", "Télécharger le PDF")),
             hr(),
             fluidRow(column(9,fluidRow(h4("Porter à Connaissance (PAC) pour les milieux aquatiques")),
                             fluidRow(h5("Document PDF interne")),
                             fluidRow("Le document recèle plusieurs liens renvoyant vers des jeux de données auxquels contribuent les agents de l'OFB."),
                             fluidRow("On retrouve des liens vers des données piscicoles, hydromorphologiques, thermique, d'étiage, de continuité écologique."),
                             fluidRow("(Le document téléchargeable ci-dessus n'est qu'un résumé des liens : pour accéder aux PAC, s'adresser au Service régional Connaissances.)"),
                             fluidRow(h6()),
                             fluidRow(downloadButton("download_PAC", "Télécharger le PDF"))),
                      column(3,img(src="visu_PAC.jpg", width="40%", style="display: block; margin-left: auto; margin-right: auto;"))),
             hr(),
             fluidRow(column(9,fluidRow(h4("Portail cartographique de l'ONCFS")),
                             fluidRow(h5("Visualiseur cartographique")),
                             fluidRow("La page renvoie vers les cartes Carmen de répartition de nombreux mammifères et oiseaux."),
                             fluidRow("On trouve une carte par espèce majeure (daim, loup gris, grive draine, ...) et une carte par groupe 
                                      (ongulés sauvages, carnivores, migrateurs terrestres, espèces exotiques)."),
                             fluidRow("Toutes les cartes sont en accès libre, facilement manipulables, exportables, et les données brutes par espèce sont téléchargeables."),
                             fluidRow(tags$a("http://www.oncfs.gouv.fr/Cartographie-ru4/Le-portail-cartographique-de-donnees-ar291", 
                                             href="http://www.oncfs.gouv.fr/Cartographie-ru4/Le-portail-cartographique-de-donnees-ar291"))),
                      column(3,img(src="visu_oncfs.png", width="100%"))),
             hr(),
             fluidRow(column(9,fluidRow(h4("DREAL Hauts-de-France")),
                             fluidRow(h5("Recensement de données et de cartes régionales")),
                             fluidRow("La page contient trois onglets : Cartes dynamiques / Cartothèque / Catalogue"),
                             fluidRow("On trouve des données spatialisées nombreuses et variées, sur des thèmes environnementaux ou non. 
                             Souvent, les données à télécharger sont à visualiser sur un logiciel externe (type Arcgis) : 
                                      la visualisation n'est pas toujours permise. Les cartes PDF ne sont pas toujours récentes."),
                             fluidRow("Beaucoup de données sont plus manipulables sur d'autres visualisateurs, mais les cartes PDF permettent une première approche."),
                             fluidRow(tags$a("https://www.hauts-de-france.developpement-durable.gouv.fr/?Acceder-aux-donnees-SIG", 
                                             href="https://www.hauts-de-france.developpement-durable.gouv.fr/?Acceder-aux-donnees-SIG"))),
                      column(3,img(src="visu_dreal.png", width="100%"))),
             hr(),
             fluidRow(column(9,fluidRow(h4("Geo2France")),
                             fluidRow(h5("Portail de cartes")),
                             fluidRow("Le site contient trois onglets intéressants : Créer sa carte / Cartothèque / Visionneuses métiers (surtout Visionneuse naturaliste)."),
                             fluidRow("On trouve des données spatialisées nombreuses et variées, sur des thèmes environnementaux ou non. 
                                      Les données SIG ne sont pas toujours disponibles."),
                             fluidRow("La visionneuse est plus intuitive et plus pratique que la Cartothèque (d'autant plus que cette dernière contient des cartes datées
                                      et pas toujours régionales)."),
                             fluidRow(tags$a("https://www.geo2france.fr/portail/", 
                             href="https://www.geo2france.fr/portail/"))),
                      column(3,img(src="visu_geo2france.jpg", width="100%"))),
             hr(),
             fluidRow(column(9,fluidRow(h4("Cartograph' (Eaufrance)")),
                             fluidRow(h5("Visualiseur cartographique")),
                             fluidRow("Le site recèle de nombreuses données aquatiques, organisées en thème (et parfois sous-thèmes) et accessibles par année."),
                             fluidRow("Selon les données, l'information est plus ou moins précise (parfois à l'échelle de la région, parfois de la commune ou du cours d'eau)."),
                             fluidRow("La visionneuse est intuitive les cartes sont facilement modifiables et exportables. Les données brutes sont aussi accessibles."),
                             fluidRow(tags$a("https://cartograph.eaufrance.fr", 
                             href="https://cartograph.eaufrance.fr"))),
                      column(3,img(src="visu_cartograph.png", width="100%"))),
             hr(),
             fluidRow(column(9,fluidRow(h4("Atlas-catalogue du Sandre")),
                             fluidRow(h5("Recensement de données")),
                             fluidRow("Le site contient plusieurs centaines de jeux de données autour du thème de l'eau, sur divers territoires (métropoles, outre-mer, ...)."),
                             fluidRow("Pour chaque jeu de données, on a accès à plusieurs méta-données (étendue spatiale, mise à jour, auteur, etc.) et plusieurs contenus :
                                      couches à visualiser, données brutes, notice, ..."),
                             fluidRow("Les données peuvent être ouvertes dans un visualiseur (capricieux). Le site paraît compliqué au premier abord (beaucoup de données à chaque fois),
                                      mais on s'y fait après avoir recherché plusieurs données."),
                             fluidRow(tags$a("http://www.sandre.eaufrance.fr/atlas/srv/fre/catalog.search#/home", 
                                             href="http://www.sandre.eaufrance.fr/atlas/srv/fre/catalog.search#/home"))),
                      column(3,img(src="visu_sandre.png", width="100%"))),
             hr(),
             fluidRow(column(9,fluidRow(h4("SIE du bassin Seine-Normandie")),
                             fluidRow(h5("Portail de cartes")),
                             fluidRow("Le site contient une trentaine de cartes Carmen sur l'eau dans le bassin Seine-Normandie."),
                             fluidRow("Les cartes sont accessibles", tags$em("via"), "4 pages (SDAGE 2016-2021, Cartes thématiques, ...). Toutes ne sont pas recensées 
                                      dans l'inventaire car certaines datent."),
                             fluidRow("Selon les cartes, plusieurs couches sont disponibles.", tags$em(style = "color:grey", "Il n'y a pas de page équivalente pour le bassin Artois-Picardie.")),
                             fluidRow(tags$a("http://www.seine-normandie.eaufrance.fr/cartographie/", 
                                             href="http://www.seine-normandie.eaufrance.fr/cartographie/"))),
                      column(3,img(src="visu_bassinSN.png", width="100%"))),
             hr(),
             fluidRow(column(9,fluidRow(h4("Outil « Oursin » (ClicNat)")),
                             fluidRow(h5("Visualiseur cartographique")),
                             fluidRow("Le site contient de nombreuses données sur l'observation naturaliste participative en Picardie."),
                             fluidRow("3 catégories de données : observatoire de la faune ; atlas par groupe taxonomique ; zonages et outils."),
                             fluidRow("La visionneuse est intuitive, mais les données disponibles manquent de précision."),
                             fluidRow(tags$a("https://oursin.clicnat.fr/#", 
                                             href="https://oursin.clicnat.fr/#"))),
                      column(3,img(src="visu_clicnat.png", width="100%"))),
             hr(),
             fluidRow(column(9,fluidRow(h4("Cerema")),
                             fluidRow(h5("Visualiseur cartographique")),
                             fluidRow("Le site est un outil du MTES, et se concentre sur des données marines et littorales."),
                             fluidRow("Les données sont classées dans plusieurs onglets (qui se recoupent parfois) : usages de la mer, transport maritime, occupation du sol, ..."),
                             fluidRow("Plusieurs données sont intéressantes pour l'OFB, et d'autres sont là plus « pour information ». La visionneuse est intuitive."),
                             fluidRow(tags$a("https://cerema.maps.arcgis.com/apps/MapSeries/index.html?appid=354ccc3737fe4df78ed82e184713ee0c", 
                                             href="https://cerema.maps.arcgis.com/apps/MapSeries/index.html?appid=354ccc3737fe4df78ed82e184713ee0c"))),
                      column(3,img(src="visu_cerema.png", width="100%"))),
             hr()
    ),
    #### onglet 6 et 7 : pour avoir un espace entre les 5 premiers onglets et l'onglet "Informations" ####
    tabPanel(""), tabPanel(""), 
    #### onglet 8 ####
    tabPanel("Édition",style = "padding: 30px",
             #### sous-onglet 1 de l'onglet 8 ####
             tabsetPanel(tabPanel("Correction",style = "padding: 30px",
             br(),
             fluidRow(style="border-bottom: 2px solid ; border-top: 2px solid ; border-right: 2px solid ; border-left: 2px solid ;background-color:lightyellow",
                      h4(style="color:#398035; padding: 10px", "Correction ou mise à jour d'une information")), 
             br(),
             uiOutput("render8"), # choix de la donnee a modifier
             conditionalPanel(condition = "input.donnee_modif!=''", # s'affiche des qu'on a selectionne une donnee a modifier
                               uiOutput("render9")), # choix de l'attribut de la donnee a modifier
             uiOutput("edition_modif"), uiOutput("edition_modif2"),
             # des messages specifiques aux attributs peuvent s'afficher, grace aux conditionalPanels
             conditionalPanel(condition = "input.champ_modif=='Adresse'",
                              fluidRow(style="padding:10px;color:#009193",tags$em("Bien faire commencer l'adresse par «"), "http://" ,  tags$em(" » ou « "), "https://", tags$em("» (pour que le lien fonctionne)."))),
             conditionalPanel(condition = "input.champ_modif=='Thème Covadis'",
                              fluidRow(style="padding:10px;color:#009193",tags$em("Le « thème » sera modifié automatiquement, selon le « thème Covadis » (voir la correspondance à l'onglet"), tags$b("Thèmes"), tags$em(")."))),
             conditionalPanel(condition = "input.champ_modif=='Mots-clés'",
                              fluidRow(style="padding:10px;color:#009193",tags$em("Les mots-clés doivent", tags$b("impérativement"), "être séparés par une virgule, suivie d'une (et une seule) espace.")),
                              fluidRow(style="padding:10px;color:#009193",tags$em("Voir la liste des mots-clés actuels dans l'onglet"), tags$b("Classement"), tags$em("."))),
             conditionalPanel(condition = "input.champ_modif=='Autre lien intéressant'",
                              fluidRow(style="padding:10px;color:#009193",tags$em("Il ne faut", tags$b("surtout pas"), "mettre de « : » dans le nom de la cible du lien, sinon le lien ne fonctionnera pas 
                                                                    dans l'onglet"), tags$b("Recherche"), tags$em(".")),
                              fluidRow(style="padding:10px;color:#009193",tags$em("Bien faire commencer l'adresse par «"), "http://" ,  tags$em(" » ou « "), "https://", tags$em("» (pour que le lien fonctionne)."))),
             conditionalPanel(condition = "input.champ_modif!=''", # s'affiche des qu'on a selectionne un attribut a modifier
                              br(),
                              actionButton("sauvegarde_modif", "Enregistrer la modification",style="color: #fff; background-color: #337ab7"),
                              br(),
                              fluidRow(style = "padding: 10px","Pour que l'ajout soit visible, il faut rafraîchir la page", tags$b("après avoir cliqué"), "sur le bouton ci-dessus."),
                              conditionalPanel(condition = "input.sauvegarde_modif", # s'affiche si on a clique sur le bouton Enregistrer
                                               br(),
                                               fluidRow(tags$b(style="color:red","La donnée a bien été modifiée !"), "Rafraîchir la page pour voir la modification.")))
             ),
             #### sous-onglet 2 de l'onglet 8 ####
             tabPanel("Ajout",style = "padding: 30px",
                      br(),
                      fluidRow(style="border-bottom: 2px solid ; border-top: 2px solid ; border-right: 2px solid ; border-left: 2px solid ;background-color:lightyellow",
                               h4(style="color:#398035; padding: 10px", "Ajout d'une information")), 
                      br(),
                      textInput("donnee_ajout","Nom de la donnée : *", width=1165),
                      fluidRow(tags$span(style="color:white","aa"),
                               tags$em(style = "color:#009193", "Vérifier que le nom n'existe pas déjà (s'il existe, mettre l'organisme entre crochets).")), br(),
                      textInput("adresse_ajout","Adresse : *", value = "https://", width=1165),
                      fluidRow(tags$span(style="color:white","aa"),
                               style = "color:#009193", tags$em("Bien faire commencer l'adresse par «"), "http://" ,  tags$em(" » ou « "), "https://", tags$em("» (pour que le lien fonctionne).")), br(),
                      textInput("organisme_ajout","Organisme : *", width=1165),
                      fluidRow(tags$span(style="color:white","aa"),
                               style = "color:#009193", tags$em("Éviter à tout prix les doublons : vérifier dans l'onglet"), tags$b("Recherche"), tags$em("comment écrire le nom.")), 
                      fluidRow(tags$span(style="color:white","aa"),
                               style = "color:#009193", tags$em("Pour les données OFB, ex-AFB, ex-ONEMA et ex-ONCFS : impérativement indiquer «"), "OFB", tags$em("» (sans espaces et en majuscule).")),br(),
                      fluidRow(column(4,uiOutput("render10")),column(3, uiOutput("render11")), column(4, align = "left",br(),
                                                                                                      fluidRow(style = "color:#009193", tags$em("Le « thème » sera modifié automatiquement, selon le « thème Covadis » 
                                                                                                                       (voir la correspondance à l'onglet"), tags$b("Thèmes"), tags$em(")."))
                      )),br(),
                      textInput("motcle_ajout","Mots-clés :", width=1165),
                      fluidRow(tags$span(style="color:white","aa"),
                               tags$em(style = "color:#009193", "Les mots-clés doivent", tags$b("impérativement"), "être séparés par une virgule suivie d'une espace, et écrits en majuscule.")),
                      fluidRow(tags$span(style="color:white","aa"),
                               tags$em(style = "color:#009193", "Les catégories, thèmes et thèmes covadis seront ajoutés automatiquement dans la liste des mots-clés : ne pas les remettre ici.")),
                      fluidRow(tags$span(style="color:white","aa"),
                               tags$em(style = "color:#009193", "Voir la liste des mots-clés actuels dans l'onglet"), tags$b(style="color:white","Classement"), tags$em(".")), br(),
                      textInput("maj_ajout","Date de mise à jour :", width=1165),
                      fluidRow(column(2,uiOutput("render12")),
                               column(1,),
                               column(2,radioButtons("brutes_ajout", "Données brutes : *", choices = c("Non disponibles","Disponibles"))),
                               column(2,radioButtons("sig_ajout", "SIG : *", choices = c("Non disponible","Disponible"))),
                               column(3,uiOutput("render13"))),
                      textInput("utilisation_ajout","Utilisation :", width=1165),
                      fluidRow(column(5,
                                      textInput("texte_lien_ajout","S'il y en a un, nom de la cible d'un autre lien intéressant (sinon, laisser vide) :", width=1165),
                                      ),
                               column(6,
                                      textInput("lien_ajout","S'il y en a un, autre lien intéressant (sinon, laisser tel quel) :", value = "https://", width=1165),
                                      fluidRow(style="padding:10px; color:#009193", tags$em("Bien faire commencer l'adresse par «"), "http://" ,  tags$em(" » ou « "), "https://", tags$em("» (pour que le lien fonctionne).")))),
                      textInput("bancarisation_ajout","S'il y en a un, site de bancarisation (sinon, laisser tel quel) :", value = "https://", width=1165),
                      fluidRow(tags$span(style="color:white","aa"),
                               style = "color:#009193", tags$em("Bien faire commencer l'adresse par «"), "http://" ,  tags$em(" » ou « "), "https://", tags$em("» (pour que le lien fonctionne).")), br(),
                      fluidRow(column(3,radioButtons("mozilla_ajout", "Compatible avec Mozilla : *", choices = c("Compatible","Non compatible"))),
                               column(3,radioButtons("chrome_ajout", "Compatible avec Chrome : *", choices = c("Compatible","Non compatible"))),
                               column(3,radioButtons("internetexp_ajout", "Compatible avec Internet Explorer : *", choices = c("Compatible","Non compatible"))),
                               ),
                      textInput("hdf_ajout","Hauts-de-France :", width=1165),
                      br(),
                      actionButton("sauvegarde_ajout", "Enregistrer la nouvelle donnée",style="color: #fff; background-color: #337ab7"),
                      fluidRow(style = "padding: 10px","Pour que l'ajout soit visible, il faut rafraîchir la page", tags$b("après avoir cliqué"), "sur le bouton ci-dessus."),
                      conditionalPanel(condition = "input.sauvegarde_ajout", # s'affiche des qu'on a clique sur Enregistrer
                                       br(),
                                       fluidRow(tags$b(style="color:red","La donnée a bien été ajoutée !"), "Rafraîchir la page pour voir la nouvelle donnée.")),
                      hr(),
                      fluidRow(tags$b("* Champ obligatoire"))
             ),
             #### sous-onglet 3 de l'onglet 8 ####
             tabPanel("Suppression",style = "padding: 30px",
                      br(),
                      fluidRow(style="border-bottom: 2px solid ; border-top: 2px solid ; border-right: 2px solid ; border-left: 2px solid ;background-color:lightyellow",
                               h4(style="color:#398035; padding: 10px", "Suppression d'une donnée")),
                      br(),
                      uiOutput("render14"),
                      br(),
                      conditionalPanel(condition = "input.donnee_suppr!=''", # s'affiche des qu'on a selectionne une donnee a supprimer
                                       actionButton("sauvegarde_suppr", "Supprimer la donnée",style="color: #fff; background-color: #337ab7")),
                      conditionalPanel(condition = "input.donnee_suppr!=''&input.sauvegarde_suppr", # s'affiche des qu'on a clique sur Supprimer, pour valider suppression
                                       br(),br(),
                                       fluidRow(column(1,img(src="attention.png", width="50%", align = "right")), column(11,uiOutput("confirmation_suppr_text"))),
                                       br(), actionButton("confirmation_suppr", "Confirmation",style="color: #fff; background-color: #337ab7"),
                                       ),
                      conditionalPanel(condition = "input.confirmation_suppr", # s'affiche si on a confirme qu'on voulait supprimer
                                       br(),
                                       fluidRow(tags$b(style="color:red","La donnée a bien été supprimée !"), "Rafraîchir la page pour prendre en compte la suppression."))
             ))
    )),
  br(), br(), br(),
  hr(),
  fluidRow(align ="right", tags$em("Direction régionale des Hauts-de-France — Service connaissances – Été 2020"), tags$span(style="color:white","aa")),
  hr())
)
