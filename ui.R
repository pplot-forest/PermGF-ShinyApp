# token <- readRDS("droptoken.rds")
# rdrop2::drop_acc(dtoken = token)

# define UI for GF application

##### **** define dashboard **** #####
gf_gui.ui <- dashboardPage(
  skin = "green",
  title = i18n$t("Interface GF 2024"),
  
  ##### 1/ dashboard title #####
  dashboardHeader(
    title = textOutput("ui_title"),
    titleWidth = 300,
    # titleHeight = 50,
    
    # widgets in dashboardHeader
    tags$li(
      width = "100pt",
      style = "margin:5pt",
      
      # bouton choix du répertoire administrateur
      div(
        shinyDirButton(
          id = "directory",
          label = i18n$t("Choix du répertoire administrateur"),
          title = i18n$t("Choix du répertoire administrateur de la base GF")
        )
      ),
      
      # # affichage du répertoire administrateur sélectionné
      # div(
      #   style = "display:inline-block;border:1px solid;width:100%",
      #   verbatimTextOutput("directorypath")
      # ),
      
      class = "dropdown"
    )
  ),
  ##### // ####
  
  
  
  
  
  ##### 2/ dashboard sidebar - begin #####
  dashboardSidebar(
    
    # 2.1/ logo ----
    div(
      style = "text-align:center;padding-top:17px;padding-bottom:30px;",
      a(
        href = "https://prosilva.fr/association-futaie-irreguliere",
        target = "_blank",
        uiOutput("img_logo")
      )
    ),
    # // -----
    
    # 2.2/ tab content -----
    sidebarMenu(
      actionButton(
        inputId = "switch_language",
        label = "Switch language",
        icon = icon("cog", class = "fa"),
        class = "darkbutton"
      ),
      menuItem(
        i18n$t("Edit analysis report"), 
        tabName = "tab_report", 
        icon = icon("image")
      ),
      menuItem(
        i18n$t(HTML("Edition d'un livret d'inventaire<br/>V2")), 
        tabName = "tab_analysis", 
        icon = icon("clone"),
        selected = T
      )
    ),
    # // -----
    
    #   HTML("<script src=\"message-handler.js\"></script>"),
    #   shinyjs::useShinyjs(),
    #   extendShinyjs(text = jscode, functions = c("closeWindow")),
    #   shiny::tags$head(shiny::tags$style(".darkbutton{background-color:#28353b;color:#b8c7ce;width:200px;")), # background color and font color
    #   shiny::tags$head(shiny::tags$script(src = "message-handler.js")), # for actionbuttons
    #   shiny::tags$head(shiny::tags$link(rel = "icon", href = "favicon.ico")),
    # 
    
    
    
    ##### 2.3/ sidebar buttons #####
    # -- changement de langue
    div(
      style = "position:absolute;top:650px;",
      # # server-side buttons
      # p(
      #   style = "margin-top:340px;", #200px
      #   actionButton(
      #     inputId = "switch_language",
      #     label = "Switch language",
      #     icon = icon("cog", class = "fa"),
      #     class = "darkbutton"
      #   )#,
      #   # uiOutput('page_content')
      # ),
      
      # -- documentation / aide
      p(
        style = "margin-top:0px;",
        actionButton(
          "open_github_doc",
          label = i18n$t("\u2000Open documentation"),
          icon = icon("info-circle"),
          onclick = "window.open('http://pptools.fr/personnes-ressources', '_blank')",
          class = "darkbutton"
        )
      )
    ) # end of div
  ), # end of dashboardSidebar
  ##### // ####
  
  
  
  
  
  ##### 3/ dashboard body #####
  dashboardBody(
    
    tabItems(
      ##### 3.1/ 1st tab content : analyse de dispositif #####
      tabItem(
        tabName = "tab_report",
        h2(i18n$t("Traitement des données d'inventaire (livret GF)")),
        # Boxes need to be put in a row (or column)
        fluidRow(
          ##### 3.1.1/ 1st box - 'project options' #####
          box(
            title = i18n$t("Choix du répertoire administrateur dans la base GF"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            column(
              width = 12, 
              
              # -- bouton de sélection du répertoire administrateur
              div(
                style = "display:inline-block;width:100%;",
                # widget
                div(
                  style = "display:inline-block;width:200pt;", 
                  shinyDirButton(
                    id = "path_project_sel_ancien", 
                    label = i18n$t("Sélection du répertoire administrateur"), 
                    title = i18n$t("Specify directory for the project"))
                ),
                
                # -- affichage du répertoire administrateur sélectionné (possibilité de saisir le chemin du répertoire ?)
                # widget
                div(
                  style = "display:inline-block;width:calc(100% - 200pt - 10pt);",
                  textInput(
                    inputId = "path_project_textin", 
                    label = NULL, 
                    value = ""
                  )
                )
              )
            ) # end of column
          ), # end box project options
          
          
          ##### 3.1.2/ 2nd box - 'import data' #####
          box(
            style='margin-bottom:30px;',
            title = i18n$t("Import des données"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            height = "160pt",
            column(
              width = 12,
              
              # -- bouton 'import des données administrateurs'
              div(
                style = "display:inline-block;width:100%;padding-top:15pt;padding-bottom:15pt;",
                # text
                div(
                  style = "display:inline-block;width:180pt",
                  strong(i18n$t("Import des données administrateurs :"))
                ),
                # widget
                div(
                  style = "display:inline-block;width:calc(100% - 180pt - 10pt);text-align: center;",
                  shinyFilesButton(
                    id = "admin_files", 
                    label = i18n$t("Parcourir"), 
                    title = i18n$t("Sélection des fichiers administrateur"),
                    multiple = TRUE, 
                    buttonType = "default", class = NULL
                  )
                ),
                verbatimTextOutput("files_list")
                
              ),
              
              # -- bouton 'import des données d'inventaire'
              div(
                style = "display:inline-block;width:100%;padding-top:15pt;padding-bottom:15pt;",
                # text
                div(
                  style = "display:inline-block;width:180pt",
                  strong(i18n$t("Import des données d'inventaires : \u00a0"))
                ),
                # widget
                div(
                  style = "display:inline-block;width:calc(100% - 180pt - 10pt);text-align: center;",
                  shinyFilesButton(
                    id = "inventory_files", 
                    label = i18n$t("Parcourir"), 
                    title = i18n$t("Sélection des fichiers d'inventaire"),
                    multiple = TRUE, 
                    buttonType = "default", class = NULL
                  )
                )
              )
              
            ) # end of column 'project options'
          ), # end of box 'import data'
          
          
          ##### 3.1.3/ 3rd box - 'check data' #####
          box(
            title = i18n$t("Edition d'un rapport de vérification des données d'inventaire"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            # height = "140pt",
            column(
              width = 12,
              
              # -- bouton 'Vérification des données'
              # widget
              div(
                style = "display:inline-block;width:100%;text-align: center;padding:0pt",
                actionButton(
                  inputId = "check_data", 
                  label = i18n$t("Vérification des données")
                )
              )
              
            ) # end of column
          ), # end of box 'check data'
          
          
          ##### 3.1.4/ 4th box - 'edit report' #####
          box(
            title = i18n$t("Sorties"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            column(
              width = 12,
              
              # -- bouton 'Editer'
              div(
                style = "display:inline-block;width:100%;",
                # text
                div(
                  style = "display:inline-block;width:180pt", # border:1px solid;
                  strong(i18n$t("Edition d'un rapport d'analyse :"))
                ),
                # widget
                div(
                  style = "display:inline-block;margin-left:2px;width:calc(100% - 180pt - 10pt);text-align: center;", # border:1px solid;
                  actionButton(
                    inputId = "edit_report", 
                    label = i18n$t("Editer")
                  )
                )
                
              )
              
            ) # end of column
          ), # end of box 'edit report'
          
          ##### 3.1.5/ 5th box - 'remeasures tools' #####
          box(
            style='margin-bottom:30px',
            title = i18n$t("Outils de préparation à la remesure"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            column(
              width = 12,
              
              # -- bouton 'classeur de remesure'
              div(
                style = "display:inline-block;width:100%;padding-top:5pt;padding-bottom:5pt;", # border:1px solid;
                # text
                div(
                  style = "display:inline-block;width:280pt", # border:1px solid;
                  strong(i18n$t("Edition des classeurs de remesure (terrain) :"))
                ),
                # widget
                div(
                  style = "display:inline-block;margin-left:2px;width:calc(100% - 280pt - 10pt);text-align: center;", # border:1px solid;
                  actionButton(
                    inputId = "edit_remeasure_workbook", 
                    label = i18n$t("Editer")
                  )
                )
              ),
              
              # -- bouton 'fiches relevés'
              div(
                style = "display:inline-block;width:100%;padding-top:5pt;padding-bottom:5pt;", # border:1px solid;
                # text
                div(
                  style = "display:inline-block;width:280pt",
                  strong(i18n$t("Edition des fiches de relevés (.pdf) :"))
                ),
                # widget
                div(
                  style = "display:inline-block;margin-left:2px;width:calc(100% - 280pt - 10pt);text-align: center;", # border:1px solid;
                  actionButton(
                    inputId = "edit_field_sheets", 
                    label = i18n$t("Editer")
                  )
                )
              ),
              
              # -- bouton 'plans arbres'
              div(
                style = "display:inline-block;width:100%;padding-top:0pt;padding-bottom:0pt;", # border:1px solid;
                # text
                div(
                  style = "display:inline-block;width:280pt", # border:1px solid;
                  strong(i18n$t("Edition des plans d'arbres par placettes (aide à la localisation des placettes) :"))
                ),
                # widget
                div(
                  style = "display:inline-block;margin-left:2px;width:calc(100% - 280pt - 10pt);text-align: center;", # border:1px solid;
                  actionButton(
                    inputId = "edit_tree_map", 
                    label = i18n$t("Editer")
                  )
                )
              )
              
            ) # end of column 'project options'
          ) # end of box 'import data'
        ) # end of fluidRow
      ), # end of 1st tabItem
      ##### // #####
      
      ##### 3.2/ 2nd tab content : version 2 #####
      tabItem(
        tabName = "tab_analysis",
        # h6(i18n$t("Selection server")),
        # Boxes need to be put in a row (or column)
        fluidRow(
          # verbatimTextOutput("ui_title")
          ##### 3.2.1/ 1st box - 'import data' #####
          div(
            style = "display:inline-block;margin-left:0pt;width:100%;",
            uiOutput("import_data_box")
          ##### end of 3.2.1/ #####
          ), # end of div (for box 'import data')
          
          
          
          
          
          
          
          
          
          
          ##### 3.2.2/ input - 'select stand' #####
          div(
            style = "display:inline-block;margin-left:12pt;width:100%;", # border:1px solid;
            column(
              width = 12,
              style = "display:inline-block;margin-top:15pt;width:250pt;padding:5pt", # border:1px solid;
              uiOutput("load_data_btn")
              ),
            column(
              width = 12, 
              style = "display:inline-block;width:250pt;padding:0pt", # border:1px solid;
              uiOutput("stand_select_btn")
              )
          ),
          
          # div(
          #   style = "display:inline-block;border:1px solid;margin-left:12pt;width:100%;",
          #   # column(
          #   # width = 5,
          #   uiOutput("load_data_btn")
          # ),
          
          
          
          
          
          
          
          
          
          
          ##### 3.2.3/ 1st box - 'import data' #####
          div(
            style = "display:inline-block;margin-left:0pt;width:100%;",
            box(
              title = i18n$t("Calcul des résultats"),
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              
              ### -- 1st row (column()) : 'check data' -- ###
              # style = "display:inline-block;border:1px solid;width:100%;", #HERE
              column(
                width = 12,
                style = "display:inline-block;width:100%;padding:5pt;padding-top:20pt",
                
                div(
                  #   style = "display:inline-block;border:1px solid;width:100%;",
                  # -- text 'Edition du livret GF'
                  column(
                    width = 3,
                    # div(
                    style = "display:inline-block;padding:8.25pt",
                    strong(i18n$t("Edition du rapport de vérification :"))
                    # ),
                  ),
                  
                  # -- bouton 'édition du livret GF'
                  column(
                    width = 4,
                    style = "display:inline-block;padding:3pt",
                    # actionButton(
                    #   inputId = "edit_GF_report", 
                    #   label = i18n$t("Editer"),
                    #   width = "200pt"
                    # )
                    downloadButton("check_report")
                  )
                )
              ),
              
              ### -- 2nd row (column()) : 'process results' -- ###
              # style = "display:inline-block;border:1px solid;width:100%;", #HERE
              column(
                width = 12,
                style = "display:inline-block;width:100%;padding:5pt",
                
                div(
                  #   style = "display:inline-block;border:1px solid;width:100%;",
                  # -- text 'Calcul des résultats d'inventaire'
                  column(
                    width = 3,
                    # div(
                    style = "display:inline-block;padding:8.25pt",
                    strong(i18n$t("Calcul des résultats d'inventaire :"))
                    # ),
                  ),
                  
                  # -- bouton 'calcul des résultats'
                  column(
                    width = 4,
                    style = "display:inline-block;padding:3pt",
                    actionButton(
                      inputId = "process_results", 
                      label = i18n$t("Calcul des résultats"),
                      width = "200pt"
                    )
                  )
                )
              ),
              
              ### -- 3rd row (column()) : 'edit report' -- ###
              # style = "display:inline-block;border:1px solid;width:100%;", #HERE Attention bloque collasped = T dans box
              # tags$hr(style="border-color: purple;"),
              column(
                width = 12,
                style = "display:inline-block;width:100%;padding:5pt;padding-top:20pt",
                
                div(
                  #   style = "display:inline-block;border:1px solid;width:100%;",
                  # -- text 'Edition du livret GF'
                  column(
                    width = 3,
                    # div(
                    style = "display:inline-block;padding:8.25pt",
                    strong(i18n$t("Edition du livret GF :"))
                    # ),
                  ),
                  
                  # -- bouton 'édition du livret GF'
                  column(
                    width = 4,
                    style = "display:inline-block;padding:3pt",
                    # actionButton(
                    #   inputId = "edit_GF_report", 
                    #   label = i18n$t("Editer"),
                    #   width = "200pt"
                    # )
                    downloadButton("GF_report")
                  )
                )
              )
            ) # end of box 'process results'
          ), # end of div (for box 'process results')
          ##### end of 3.2.3/ #####
          
          
          dataTableOutput("data"),
          verbatimTextOutput("text")
        ) # end of fluidRow
      ) # end of 2nd tabItem
      ##### // #####
    ) # end of tabItems
  )
  
  
  
  
  
  
) # end of dashboardPage
