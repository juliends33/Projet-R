library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
# Read the TSV file correctly
file_path <- "emp_offers_fmt.tsv"
data <- read.csv(file_path, sep = ",", stringsAsFactors = FALSE)

#Partie 2.4 :

# Dans cette partie nous avons choisis de travailler étape par étape sur chaque colonne de la base de donnÃées appelée 'data_dt'
# (datatable crée à  partir de la base de donnée 'data', crée elle même à  partir du fichier emp_offers_fmt.tsv).
# Nous avons choisis de créer une nouvelle datatable appelée 'merged_data' dans lesquels nous ajoutons à chaque étape les colonne modifiée
# de data_dt plutôt que de les modifier directement au sein de 'data_dt' (afin de pouvoir vérifier si nos résultats sont corrects).
# Puis nous avons fusionner toutes les colonnes ainsi créer de 'merged_data' dans une seule datatable, créant ainsi 'base_emp'.
# Split the column names if necessary

# conversion de data en data table
data_dt <- as.data.table(data)


####
####Numerisation des salaires####
####


##creation de la colone salaire numéric en 3 etape avec 3 data base qui traite different cas puis fusion de ces 3 data base remis dans data dt 

# Lorsqu'il y avait une tranche de deux valeurs, nous avons choisis de garder la plus basse, plutôt que la moyenne des deux.
# Nous avons également convertie chaque valeur pour obtenir un salaire annuel.

# Exemple : s'il est écrit 800 - 1 796 EUR par mois  nous gardons 9600. 

# base de données 1 traitant la majorité des cas

data_dt1 <- data_dt %>%
  mutate(
    salaire_numeric = sapply(salaire, function(x) {
      x_clean <- gsub("(?i)Salaire\\s*:", "", x)  # enleve 'Salaire :' (case insensitive)
      x_clean <- gsub("(?i)jusqu’à\\s*", "", x_clean)  # enleve 'jusqu’à'
      x_clean <- gsub("de\\s*", "", x_clean)  # enleve 'de'
      x_clean <- gsub("(?i)EUR|€|\\s*par.*", "", x_clean)  # enleve certains symboles
      x_clean <- gsub(",", ".", x_clean)  # remplace les virgule par des points
      x_clean <- gsub("K", "000", x_clean, ignore.case = TRUE)  # remplace 'K' avec '000'
      x_clean <- gsub("[[:space:]]+", " ", x_clean)  # normalisation des espaces
      x_clean <- gsub("\u202f", "", x_clean)  # enleve :  non-breaking spaces
      x_clean <- trimws(x_clean)  
      
      if (grepl("à|-", x_clean)) {
        delimiter <- ifelse(grepl("à", x_clean), "à", "-") 
        vals <- suppressWarnings(as.numeric(unlist(strsplit(x_clean, delimiter))))  
        mean(vals, na.rm = TRUE)  
      } else {
        suppressWarnings(as.numeric(x_clean))  
      }
    })  
  ) %>%
  mutate(
    salaire_numeric = case_when(
      grepl("par jour", salaire, ignore.case = TRUE) ~ salaire_numeric * 365,  # convertisseur de par jours a par ans
      grepl("par mois", salaire, ignore.case = TRUE) ~ salaire_numeric * 12,   # convertisseur de par mois a par ans
      grepl("par semaine", salaire, ignore.case = TRUE) ~ salaire_numeric * 52, # convertisseur de par semaine a par ans
      TRUE ~ salaire_numeric 
    )  
  )


#base de donné 2 traitant certains cas restant

# la meme chose avec d'autre consignes pour different cas 
data_dt2 <- data_dt %>%
  mutate(
    salaire_numeric = sapply(salaire, function(x) {
      # Normalize salary format
      x_clean <- gsub("(?i)Salaire\\s*:", "", x)  
      x_clean <- gsub("(?i)jusqu’à\\s*", "", x_clean)  
      x_clean <- gsub("de\\s*", "", x_clean)  
      x_clean <- gsub("(?i)EUR|€|\\s*par.*", "", x_clean) 
      x_clean <- gsub(",", ".", x_clean)  
      x_clean <- gsub("K", "000", x_clean, ignore.case = TRUE)  
      x_clean <- gsub("[[:space:]]+", " ", x_clean) 
      x_clean <- gsub("\u202f", "", x_clean) 
      x_clean <- gsub("[^0-9.\\-à]", "", x_clean)  
      x_clean <- trimws(x_clean) 
      x_clean <- gsub("[^0-9,. -]", "", x)  
      x_clean <- gsub(",", ".", x_clean)   
      x_clean <- gsub(" +", " ", x_clean)  
      
      
      if (grepl("à|-", x_clean)) {
        delimiter <- ifelse(grepl("à", x_clean), "à", "-")  
        vals <- suppressWarnings(as.numeric(unlist(strsplit(x_clean, delimiter))))  
        mean(vals, na.rm = TRUE)  
      } else {
        suppressWarnings(as.numeric(x_clean))  
      }
      if (grepl("-", x_clean)) {
        vals <- suppressWarnings(as.numeric(unlist(strsplit(x_clean, "-"))))  
        mean(vals, na.rm = TRUE)  
      } else if (grepl("à", x_clean)) {
        vals <- suppressWarnings(as.numeric(unlist(strsplit(x_clean, "à"))))  
        mean(vals, na.rm = TRUE)  
      } else {
        suppressWarnings(as.numeric(x_clean)) 
      }
    })  
  ) %>%
  mutate(
    salaire_numeric = case_when(
      grepl("par jour", salaire, ignore.case = TRUE) ~ salaire_numeric * 365,  
      grepl("K", salaire, ignore.case = TRUE) ~ salaire_numeric * 1000,  
      grepl("par mois", salaire, ignore.case = TRUE) ~ salaire_numeric * 12,  
      grepl("par semaine", salaire, ignore.case = TRUE) ~ salaire_numeric * 52, 
      TRUE ~ salaire_numeric 
    )
  )



# base de donné 3 pour les dernires cas
data_dt3 <- data_dt %>%
  mutate(
    salaire_numeric = sapply(salaire, function(x) {
      tryCatch({
        x_clean <- gsub("[^0-9, à]", "", x)  
        x_clean <- gsub(",", ".", x_clean)  
        
        if (grepl("à", x_clean)) {
          vals <- as.numeric(unlist(strsplit(x_clean, "à")))  
          mean(vals, na.rm = TRUE) 
        } else {
          as.numeric(x_clean)  
        }
      }, error = function(e) {
        NA_real_
      })
    }),
    salaire_numeric = case_when(
      grepl("par mois", salaire, ignore.case = TRUE) ~ salaire_numeric * 12,   
      grepl("par jour", salaire, ignore.case = TRUE) ~ salaire_numeric * 365,  
      grepl("par semaine", salaire, ignore.case = TRUE) ~ salaire_numeric * 52, 
      TRUE ~ salaire_numeric 
    )
  )


# fusionne les valeurs de data_dt1 et data_dt2 et les remets dans data_dt1 pour salaire_numeric
data_dt1 <- data_dt1 %>%
  mutate(
    salaire_numeric = ifelse(
      is.na(salaire_numeric),
      data_dt2$salaire_numeric,  
      salaire_numeric            
    )
  )

# fusionne les valeurs de data_dt1 et data_dt3 et les remets dans data_dt pour salaire_numeric
data_dt <- data_dt1 %>%
  mutate(
    salaire_numeric = ifelse(
      is.na(salaire_numeric),
      data_dt3$salaire_numeric,  # Use salaire_numeric from data_dt2
      salaire_numeric            # Keep original value if not NA
    )
  )


####
###Normalisation des compétences####
####


# definition d'une table de traduction: Anglais à Francais
# Garde également un seul mot s'il est écrit de plusieurs manière différentes.
# Nous avons traité les cas restant à la main.

translation_map <- c(
  "Adaptability" = "Adaptabilité",
  "Attention to Detail" = "Attention aux Détails",
  "Autonomous" = "Autonome",
  "Communication Skills" = "Communication",
  "Creativity" = "Créativité",
  "Critical Thinking" = "Esprit Critique",
  "Curious" = "Curiosité",
  "Data Cleaning" = "Nettoyage de donnée",
  "Data Visualization" = "Visualisation de donnée",
  "Database Management" = "Base de donnée",
  "Deep Learning" = "Apprentissage profond",
  "Domain Knowledge" = "Connaissance du domaine",
  "English" = "Anglais",
  "French" = "Français",
  "Natural Language Processing" = "Traitement du Langage Naturel",
  "Optimization" = "Optimisation",
  "Presentation Skills" = "Compétences en Présentation",
  "Proactive" = "Proactif",
  "Programming" = "Programmation",
  "Project Management" = "Gestion de Projets",
  "Statistics" = "Statistique",
  "Team Collaboration" = "Collaboration en Équipe",
  "Time Management" = "Gestion du Temps",
  "Autonôme" = "Autonome",
  "Bases de donnée" = "Base de donnée",
  "Collaboration" = "Collaboration en Équipe" ,
  "Statistical Modeling" = "Statistique" 
)

# Harmoisation dans data_dt
data_dt <- data_dt %>%
  mutate(
    competences_requises = sapply(competences_requises, function(x) {
      competences <- unlist(strsplit(as.character(x), "[;,]"))
      
      harmonized <- sapply(trimws(competences), function(comp) {
        if (comp %in% names(translation_map)) {
          return(translation_map[comp])
        } else {
          return(comp)  
        }
      })
      
      paste(unique(harmonized), collapse = "; ")
    })
  )



####Normalisation des entreprises####


# Enleve les majuscule de certaines entreprises pour normaliser les noms
data_dt$entreprise <- tolower(data_dt$entreprise)

# Modification specifique vu a l'oeil
data_dt$entreprise <- ifelse(data_dt$entreprise %in% c("altelios technology group", "altelios technology"), 
                             "altelios technology", 
                             data_dt$entreprise)


#reduction de la description du poste à 100 caractères (inutile pour la data base final mais utile pour une meilleure lisibilité et manipulation durant notre travail)
data_dt$poste_desc[is.na(data_dt$poste_desc) | data_dt$poste_desc == ""] <- "No description"
data_dt$poste_desc <- substr(data_dt$poste_desc, 1, 100)  # Truncate to first 100 characters



# regroupement des informations par entreprises dans une nouvelle data_base appelé 'merged_data' permettant de regrouper toutes les offres d'emploies d'une même entreprise
merged_data <- data_dt[, .(
  intitule_poste = paste((intitule_poste), collapse = "; "),
  type_emploi = paste(unique(type_emploi), collapse = "; "),
  secteur = paste((secteur), collapse = "; "),
  experience_requise = paste((experience_requise), collapse = "; "),
  competences_requises = paste((competences_requises), collapse = "; "),
  poste_desc = paste(unique(poste_desc), collapse = " | "),
  avg_wage = paste(unique(salaire_numeric), collapse = "; "),
  departement = paste((departement), collapse = "; ")
), by = entreprise]

# Trie de merged data par nom d'entreprise
merged_data <- merged_data[order(entreprise)]

####creation de avg_exp_req ####

# Nous avons fait la moyenne des expérience.

merged_data <- merged_data %>%
  mutate(
    avg_req_exp = sapply(strsplit(experience_requise, ";"), function(x) {
      numeric_vals <- suppressWarnings(as.numeric(x))  # Convesion en nombre numéric
      if (all(is.na(numeric_vals))) NA else mean(numeric_vals, na.rm = TRUE)  # Calculate de la moyenne des experiences requises
    })  # compute average
  )


####
####Creation de top_skill_req####
####
#on a fait le choix de selectionner toutes les competences qui apparaissent plus que 1 fois

merged_data <- merged_data %>%
  # remplace les ; par , pour une uniformité et faciliter la suite
  mutate(competences_requises = str_replace_all(competences_requises, ";", ",")) %>%
  mutate(competences_requises = str_split(competences_requises, ",\\s*")) %>%
  unnest(competences_requises) %>%
  mutate(competences_requises = str_trim(competences_requises)) %>%
  # compte le nombre de fois que chaque skill apparant pour une entreprise
  group_by(entreprise, competences_requises) %>%
  summarize(skill_count = n(), .groups = "drop") %>%
  #fusionne les skills qui apparaissent plus d'une fois en un seul string
  group_by(entreprise) %>%
  filter(skill_count > 1) %>%
  summarize(top_skill_req = paste(unique(competences_requises), collapse = ", "), .groups = "drop") %>%
  # Join back to the original data
  right_join(merged_data, by = "entreprise") %>%
  # Remove duplicates caused by unnesting
  distinct(entreprise, .keep_all = TRUE)


# Création de la colonne finale avg_wage en faisant la moyenne des salaires de chaque offre d'emploie pour chaque entreprise :

merged_data <- merged_data %>%
  mutate(
    avg_wage = sapply(avg_wage, function(x) {
      # Split values by ';'
      values <- unlist(strsplit(as.character(x), ";"))
      # Convert to numeric
      numeric_values <- suppressWarnings(as.numeric(trimws(values)))
      # Calculate mean, ignoring NA
      if (all(is.na(numeric_values))) {
        NA_real_  # If all values are NA, return NA
      } else {
        mean(numeric_values, na.rm = TRUE)  # Compute mean, ignoring NA
      }
    })
  )

# Création de la colonne finale addr_dept_main en gardant le département qui apparait le plus souvent

merged_data <- merged_data %>%
  mutate(
    addr_dept_main = sapply(departement, function(x) {
      # Split values by ';'
      values <- unlist(strsplit(as.character(x), ";"))
      # Convert to numeric (remove non-numeric values and trim spaces)
      numeric_values <- suppressWarnings(as.numeric(trimws(values)))
      
      # Check if all values are NA
      if (all(is.na(numeric_values))) {
        return(NA_character_)  # Retain NA if no valid department numbers
      }
      
      # Count frequencies of numeric values
      freq_table <- table(numeric_values)
      
      # Get the most frequent department number
      main_dept <- as.character(names(which.max(freq_table)))
      return(main_dept)
    })
  )


# Création de la colonne finale sector_main en gardant le secteur qui apparait le plus souvent (en cas d'égalité en prend 1 au hasard parmis les plus nombreux)

merged_data <- merged_data %>%
  mutate(
    sector_main = sapply(secteur, function(x) {
      # Split values by both ';' and ','
      values <- unlist(strsplit(as.character(x), "[;,]"))
      # Trim spaces and filter non-empty values
      cleaned_values <- trimws(values)
      cleaned_values <- cleaned_values[cleaned_values != ""]
      
      # Check if all values are NA or empty strings
      if (length(cleaned_values) == 0 || all(is.na(cleaned_values))) {
        return(NA_character_)  # Retain NA if no valid sector values
      }
      
      # Count frequencies of sector values
      freq_table <- table(cleaned_values)
      
      # Get the most frequent sector
      main_secteur <- names(which.max(freq_table))
      return(main_secteur)
    })
  )


# Création de la colonne finale n_offres contenant la somme du nombre d'offres d'emplois de chaques entreprises.

merged_data <- merged_data %>%
  mutate(
    n_offres = sapply(intitule_poste, function(x) {
      # Split values by ';'
      values <- unlist(strsplit(as.character(x), ";"))
      # Trim spaces and remove empty strings
      cleaned_values <- trimws(values)
      cleaned_values <- cleaned_values[cleaned_values != ""]
      
      # Return the count of non-empty offers
      return(length(cleaned_values))
    })
  )

# On rename la colonne 'entreprise' en 'firm_name' pour correspondre avec l'énoncé

merged_data <- merged_data %>%
  rename(firm_name = entreprise)


# Création de la datatable final 'base_emp' en ne gardant que les colonnes concernées de merged_data.

#final data_base
base_emp <- merged_data %>%
  select(firm_name, n_offres, sector_main, avg_req_exp, top_skill_req, avg_wage, addr_dept_main)


# Suppression de toutes les data_table créé précedement
rm(data_dt1);rm(data_dt2);rm(data_dt3);rm(merged_data);rm(data);rm(data_dt)
