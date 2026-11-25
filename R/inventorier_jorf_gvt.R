# Inventaire cible des actes gouvernementaux JORF, sans import DuckDB.
# Les champs officiels sont conserves sans modification ; la selection est derivee.
#' Inventorier les actes relatifs aux gouvernements dans le JORF
#'
#' Lit les TEXTE_VERSION depuis l'archive globale ou des XML locaux. Un
#' candidat doit contenir une expression documentaire explicite relative a
#' une nomination, composition ou cessation gouvernementale. Le titre generique
#' "Decret du ..." ne suffit pas. Les champs de qualification sont derives.
#' @param archive Archive tar.gz ; facultative si fichiers est renseigne.
#' @param sortie CSV de sortie, jamais ecrase automatiquement.
#' @param motif Expression reguliere supplementaire (facultative) sur les champs
#'   documentaires. NULL conserve la selection gouvernementale par defaut.
#' @param limite Nombre maximal de TEXTE_VERSION lus.
#' @param fichiers Chemins de XML locaux pour validation, sans ouvrir l'archive.
#' @param depuis Annee minimale pour la selection, NULL pour aucune restriction.
#' @return Tableau des candidats, sans dedoublonnage des fichiers sources.
#' @export
inventorier_jorf_gvt = function(
  archive = NULL,
  sortie = "~/.cache/R/parlementR/jorf/gouvernements/inventaire_gvt.csv",
  motif = NULL,
  limite = Inf,
  fichiers = NULL,
  depuis = 1959L
) {
  if (!requireNamespace("xml2", quietly = TRUE)) stop("xml2 est requis")
  if (length(limite) != 1L || is.na(limite) || limite <= 0)
    stop("Limite invalide")
  if (!is.null(depuis) && (length(depuis) != 1L || is.na(depuis) ||
                           !is.numeric(depuis))) stop("Annee invalide")
  if (!is.null(motif) && (length(motif) != 1L || is.na(motif) ||
                          !nzchar(motif))) stop("Motif invalide")
  if (is.null(fichiers)) {
    if (is.null(archive) || length(archive) != 1L ||
        !file.exists(path.expand(archive))) stop("Archive JORF introuvable")
    archive = path.expand(archive)
  } else {
    if (!length(fichiers) || anyNA(fichiers) || any(!file.exists(fichiers)))
      stop("Fichiers XML locaux invalides")
  }
  sortie = path.expand(sortie)
  if (file.exists(sortie)) stop("Le fichier existe deja : ", sortie)
  dir.create(dirname(sortie), recursive = TRUE, showWarnings = FALSE)
  temporaire = paste0(sortie, ".part")
  if (file.exists(temporaire)) stop("Inventaire partiel existant : ", temporaire)
  champs = c("id_jorf", "nature", "date_texte", "date_publication",
             "titre", "titre_complet", "fichier_source", "critere_selection")
  resultat = list()
  compteur = 0L
  lire_texte = function(doc, xpath) {
    noeud = xml2::xml_find_first(doc, xpath)
    if (inherits(noeud, "xml_missing")) return(NA_character_)
    xml2::xml_text(noeud)
  }
  traiter = function(doc, chemin = NA_character_) {
    if (xml2::xml_name(xml2::xml_root(doc)) != "TEXTE_VERSION") return(invisible(NULL))
    compteur <<- compteur + 1L
    lire = function(xpath) lire_texte(doc, paste0("/TEXTE_VERSION/", xpath))
    nature = lire("META/META_COMMUN/NATURE")
    date_texte = lire("META/META_SPEC/META_TEXTE_CHRONICLE/DATE_TEXTE")
    date_publication = lire("META/META_SPEC/META_TEXTE_CHRONICLE/DATE_PUBLI")
    titre = lire("META/META_SPEC/META_TEXTE_VERSION/TITRE")
    titre_complet = lire("META/META_SPEC/META_TEXTE_VERSION/TITREFULL")
    if (is.na(nature) || toupper(nature) != "DECRET") return(invisible(NULL))
    # La date est utilisee uniquement pour filtrer ; sa valeur reste intacte.
    annee = suppressWarnings(as.integer(substr(date_texte, 1L, 4L)))
    if (!is.null(depuis) && (is.na(annee) || annee < depuis))
      return(invisible(NULL))
    notice = lire("NOTICE")
    principal = lire("TP")
    secondaire = lire("SM")
    mots_cles = paste(xml2::xml_text(xml2::xml_find_all(
      doc, "/TEXTE_VERSION/META/META_SPEC/META_TEXTE_VERSION/MCS_TXT/MC")),
      collapse = " ")
    parties = c(titre = titre, titre_complet = titre_complet,
                notice = notice, texte_principal = principal,
                texte_secondaire = secondaire, mots_cles = mots_cles)
    parties[is.na(parties)] = ""
    # Une expression explicite est exigee : le titre generique est insuffisant.
    gouvernement = "gouvernement|premier[[:space:]-]+ministre|ministres?|secretaires?[[:space:]-]+d.etat"
    action = "nomm[\u00e9e]s?|nomination|composition|cessation|fin[[:space:]]+des?[[:space:]]+fonctions|met[[:space:]]+fin|relatif[[:space:]]+aux?[[:space:]]+fonctions|charge[\u00e9e]s?[[:space:]]+de"
    candidats = names(parties)[vapply(parties, function(z) {
      grepl(gouvernement, z, ignore.case = TRUE, perl = TRUE) &&
        grepl(action, z, ignore.case = TRUE, perl = TRUE)
    }, logical(1))]
    # Le titre et le corps peuvent porter chacun une moitie de l'indice.
    ensemble = paste(parties, collapse = " ")
    if (!length(candidats) && grepl(gouvernement, ensemble, ignore.case = TRUE, perl = TRUE) &&
        grepl(action, ensemble, ignore.case = TRUE, perl = TRUE))
      candidats = "champs_combines"
    if (!length(candidats)) return(invisible(NULL))
    if (!is.null(motif) && !grepl(motif, ensemble, ignore.case = TRUE, perl = TRUE))
      return(invisible(NULL))
    resultat[[length(resultat) + 1L]] <<- data.frame(
      id_jorf = lire("META/META_COMMUN/ID"), nature = nature,
      date_texte = date_texte, date_publication = date_publication,
      titre = titre, titre_complet = titre_complet,
      fichier_source = chemin, critere_selection = paste(candidats, collapse = ";"),
      stringsAsFactors = FALSE
    )
    if (compteur %% 100000L == 0L)
      message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris"),
              "] [JORF] XML lus : ", compteur, "; candidats : ", length(resultat))
    invisible(NULL)
  }
  if (is.null(fichiers)) {
    commande = paste("tar -xOzf", shQuote(normalizePath(archive)),
                     "--wildcards --wildcards-match-slash",
                     shQuote("*/texte/version/*/*.xml"))
    con = pipe(commande, open = "rb")
    ouvert = TRUE
    on.exit(if (ouvert) try(close(con), silent = TRUE), add = TRUE)
    n = .jorf_flux_xml(con, traiter,
                       limite = if (is.finite(limite)) as.integer(limite) else NULL)
    statut = close(con)
    ouvert = FALSE
    if (!is.null(statut) && length(statut) && is.numeric(statut) &&
        !is.na(statut) && statut != 0L && !is.finite(limite))
      stop("tar a echoue : inventaire non enregistre")
  } else {
    n = 0L
    for (f in fichiers) {
      doc = xml2::read_xml(f, options = "NONET")
      if (xml2::xml_name(xml2::xml_root(doc)) != "TEXTE_VERSION") next
      traiter(doc, normalizePath(f))
      n = n + 1L
      if (n >= limite) break
    }
  }
  tableau = if (length(resultat)) do.call(rbind, resultat) else
    as.data.frame(stats::setNames(rep(list(character()), length(champs)), champs))
  utils::write.table(tableau, temporaire, sep = ";", row.names = FALSE,
                     col.names = TRUE, quote = TRUE, fileEncoding = "UTF-8", na = "")
  if (!file.rename(temporaire, sortie)) stop("Impossible de finaliser l'inventaire")
  message("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris"), "] [JORF] ",
          n, " XML lus ; ", nrow(tableau), " candidats ; ", sortie)
  tableau
}
