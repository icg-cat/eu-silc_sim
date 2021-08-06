# Funcions #

# veure correccions pendents amb "ATENCIO CORRECCIO"


# ------------------------------------####
# FASE 1

# 000_Data_verification: verifica que les dades estan correctes, tenen els levels correctes, no hi ha problemes de locale, etc
## Target: Tot_ok
mf_verifica_dades <- function(dades){
  # conté variables clau que utilitzarem?
  mvars <- c("anyo","trimestre",
             "edicio","U3",
             "U5","PROV",
             "FACTOREL","AOI",
             "ITBU"
  )
  clau <- sum(mvars %in% names(dades)) == length(mvars)
  # no hi ha hagut problemes amb encoding factors?
  encod <- length(levels(dades$CSE)) == 19
  # pull referència N's
  Ns <- nrow(dades)

  #retorna resultats
  return(list(
    Tot_OK = all(c(clau, encod)),
    Enes = Ns
  ))
}

# 00_Data_setup_01:  aplica filtres (un cop calculats els resultats que s'hagin d'extreure a escala espanyola)
## Target: epaT
mf_filtra_epa <- function(epafile,
                          anyos = 2020,
                          provs = NULL,
                          trimestres = NULL){
  readRDS(epafile) %>%
    {if(!is.null(anyos)) filter(., anyo %in% anyos) else .} %>%
    {if(!is.null(provs)) filter(., PROV %in% provs) else .} %>%
    {if(!is.null(trimestres)) filter(., trimestre %in% trimestres) else .}
}

# Calcula atur (mitjana sobre conjunt trimestres). No requereix filtres de dades
## target: taxa global atur segons definició
mf_calcula_Tatur <- function(epa, UN){
  # stop()
  act <- rlang::sym(paste0("actius", UN))
  atr <- rlang::sym(paste0("aturats", UN))

  epa %>%
    filter({{act}}) %>%
    group_by(edicio) %>%
    count({{atr}}, wt = FACTOREL) %>%
    mutate(tt = sum(n),
           pp = n/tt) %>%
    filter({{atr}}) %>%
    pull(pp) %>% mean(.)
}

#  00_Data_setup_03: fer taules amb probabilitats d'atur trimestrals (base espanya). Atenció, funció pensada per treballar amb trimestres!.
## semi-target: taula_probs
mf_taules_Q_atur <- function(dQx, UN){

  if(length(levels(droplevels(dQx$edicio))) >1) {
    stop("Les dades contenen més d'una edició. Cal aplicar el càlcul a cada edició per separat")
  }

  aturats <- rlang::sym(paste0("aturats", UN))
  edicio <- unique(dQx$edicio)

  dd <- dQx %>%
    select(VD = {{aturats}}, SEXO1, EDAD2,
           naix, csp, FACTOREL) %>%
    filter(complete.cases(.))

  mod <- glm(VD ~ SEXO1 + EDAD2 +
               naix + csp,
             family = binomial(link = "logit"),
             # weights = FACTOREL, # amb pesos no funciona
             data = dd)

  dd$probs <- predict(mod, type = "response")

  probs2 <- dd %>%
    group_by(SEXO1, EDAD2, naix, csp) %>%
    summarise(
      # prob = mean(probs)
      prob = Hmisc::wtd.mean(
        x = .data$probs,
        weights = .data$FACTOREL
      ) * 100,
      .groups = 'drop'# aqui
    ) %>%
    mutate(edicio = edicio) %>%
    ungroup(.)


  return(probs2)
}

# fer taules amb probabilitats de cobrar prestacions (base espanya). Atenció, funció pensada per treballar amb trimestres!
## semi-target: taula_cobra
mf_taules_Q_prestacio <- function(dQx, UN){
  if(length(levels(droplevels(dQx$edicio))) >1) {
    stop("Les dades contenen més d'una edició. Cal aplicar el càlcul a cada edició per separat")
  }

  aturats <- rlang::sym(paste0("aturats", UN))
  aoi <- rlang::sym(paste0("aoi", UN))
  edicio <- unique(dQx$edicio)
  treatment <- "Inscrita i prest."

  tt <- dQx %>%
    select(atr = {{aturats}},
           aoi = {{aoi}},
           OFEMP, FACTOREL,
           SEXO1, EDAD2,
           naix, csp) %>%
    filter(complete.cases(.)) %>%
    filter(aoi == "parados") %>%
    mutate(VD = factor(ifelse(OFEMP == treatment, 1, 0)))

  mod <- glm(VD ~ SEXO1 + EDAD2 +
               naix + csp,
             family = binomial(link = "logit"),
             # weights = FACTOREL, # amb pesos no funciona
             data = tt)

  tt$probs <- predict(mod, type = "response")

  serp2 <- tt %>%
    group_by(SEXO1, EDAD2, naix, csp) %>%
    summarise(
      prob = Hmisc::wtd.mean(
        x = .data$probs,
        weights = .data$FACTOREL
      ) * 100,
      .groups = 'drop' # aqui
    ) %>%
    mutate(edicio = edicio) %>%
    ungroup(.)


  return(serp2)
}

# Combina taules de probabilitats atur i taula de probabilitats cobertura prestacions
## semi-target: taula_única
mf_fusio_taules <- function(prob, serp){
  # funció que agrupa les taules de prob atur i prob cobrar (UNA EDICIÓ), en el mateix format que ja les tinc dissenyades per utilitzar dins del loop de la maquineta

  prob$KEY1 <- interaction(prob$SEXO1, prob$EDAD2, prob$naix, prob$csp)
  serp$KEY1  <- interaction(serp$SEXO1,  serp$EDAD2,  serp$naix, prob$csp)

  prob$prob_cobra_atur <- serp$prob[match(prob$KEY1,
                                          serp$KEY1)]
  # Adapta noms a EMCV
  prob <- dplyr::select(prob,
                        edicio,
                        RB090 = SEXO1,
                        RB080_COV = EDAD2,
                        PB210 = naix,
                        ESEC4 = csp,
                        prob, KEY1,
                        prob,
                        prob_cobra_atur2 =
                          prob_cobra_atur)

  return(prob)

}

# Combina càlcul de funcions trimestrals en una sola crida
## target: taula_anual
mf_taula_anual <- function(epa, UN){
  reslist1 <- epa %>%
    droplevels() %>%
    mutate(edicio2 = edicio) %>%
    group_by(edicio2) %>%
    nest(.) %>%
    mutate(taula = map(data, mf_taules_Q_atur, UN)) %>%
    pull(taula)

  reslist2 <- epa %>%
    droplevels() %>%
    mutate(edicio2 = edicio) %>%
    group_by(edicio2) %>%
    nest(.) %>%
    mutate(taula = map(data, mf_taules_Q_prestacio, UN))%>%
    pull(taula)

  reslist3 <- map2(.x = reslist1,
                   .y = reslist2,
                   .f = function(.x, .y){
                     mf_fusio_taules(prob = .x,
                                     serp = .y)
                   })

  #calcula els valors anuals a partir dels 4 trimestres
  (names(reslist3) <- paste0("T", c(1:4)))

  resAnual <- do.call("rbind", reslist3) %>%
    as_tibble(.) %>%
    mutate(KEY = interaction(RB090, RB080_COV,
                             PB210, ESEC4,
                             sep = "-")) %>%
    dplyr::select(edicio,
                  KEY,
                  prob,
                  prob_cobra_atur2) %>%
    gather("probabilitat", "valor", -edicio, -KEY) %>%
    spread(edicio, valor) %>%
    mutate(Anual = apply(.[,3:6], 1, mean)) %>%
    select(KEY, probabilitat, Anual) %>%
    spread(probabilitat, Anual)

  # reestructura resultats anuals en una taula com les trimestrals. Agafo T1 per referència
  taula_anual <- reslist3[[1]]

  pvars <- c(names(taula_anual)[grep("prob",
                                     names(taula_anual))])
  taula_anual[,pvars] <- NA

  taula_anual$KEY <- interaction(
    taula_anual$RB090,
    taula_anual$RB080_COV,
    taula_anual$PB210,
    taula_anual$ESEC4, sep = "-")

  taula_anual[,c("prob", "prob_cobra_atur2")] <-
    resAnual[match(taula_anual$KEY, resAnual$KEY),
             c("prob", "prob_cobra_atur2")]

  taula_anual <- taula_anual %>%
    select(-c(KEY, edicio))

    return(taula_anual)

}

# 00_Data_setup_04: Calcula les probabilitats globals de cobrar prestació d'atur tant per U3 com per U5, tenint en compte un conjunt de trimestres. Territory agnostic. Mitjana anual a partir dels trimestres seleccionats, o trimestre seleccionat.
## Target: TGcobertura
mf_calcula_Tcobertura <- function(epa, UN){
  aoi <- rlang::sym(paste0("aoi", UN))

  epa %>%
    filter(!is.na(AOI)) %>%
    filter({{aoi}} == "parados") %>%
    group_by(edicio) %>%
    count(OFEMP, wt = FACTOREL) %>%
    mutate(tasa = n/sum(n)) %>%
    select(-n) %>%
    spread(OFEMP, tasa) %>%
    ungroup(.) %>%
    summarise(
      across(
        where(is.numeric),
        mean)
    ) %>%
    pull(1)
}

# 00_Data_setup_05: Calcula prob. estimada de la durada de l'atur per mesos. Dades agregades de tots els trimestres. Territory agnostic. Entre la població aturada (definició U3/U5) que ha treballat per última vegada durant els darrers 12 mesos, quant temps fa des de la última vegada que va treballar?
mf_prob_durada_atur <- function(epa, UN){
  # epa <- get(nom_epa)
  aoi <- rlang::sym(paste0("aoi", UN))

  xx <- epa %>%
    filter({{aoi}} == "parados") %>%
    filter(DTANT < 13) %>%
    # pq són tots aturats, si DTANT < 1 -> 1
    mutate(dtant = ifelse(DTANT < 1,
                          1, DTANT)) %>%
    count(dtant, wt = FACTOREL) %>%
    mutate(pp = n/sum(n))

  return(xx)

}

# probs durada ERTO per sectors
## Target: probs_ERTO_sectors
mf_prob_erto_sectors <- function(epa){

  # read.csv2("Data/F1/ERTO_sector_sint.csv", fileEncoding = "UTF-8-BOM")
  z <- levels(epa$RZNOTB)[grep("expediente",
                               levels(epa$RZNOTB))]

  tab <- epa %>%
    filter(trimestre == "T2") %>%
    filter(RZNOTB == z) %>%
    count(ACT1,
          wt = FACTOREL,
          .drop = FALSE) %>%
    mutate(pp = n/sum(n)*100,
           codi = LETTERS[1:11]) %>%
    rename(n = n,
           Val = pp)

  return(tab)

}


# ------------------------------------####
# FASE 2

# Pendents:
## reponderació població (> mortalitat, < natalitat, > migració)


# 1. carrega referències fixes per la simulació
## target: myrefs
COV_defineix_refs_sim <- function(F1_taula_probs_grups_ESP,
                                  F1_ERTO_sect_ESP,
                                  F1_dur_atur){
  myrefs <- list()

  myrefs$mesos_atur <- "quasi-random"
  myrefs$seed <- 4321 # aquí per l'assignació aleatòria de mesos

  # Carrega referències fixes
  ## categories de referència
  noms <- list.files("Data/F2/Refs/",
                     pattern = ".txt",
                     recursive = F,
                     full.names = F) %>%
    stringr::str_remove(., ".txt")

  arxius <- list.files("Data/F2/Refs",
                       pattern = ".txt",
                       recursive = F,
                       full.names = T)

  lst <- purrr::map(
    .x =  arxius,
    .f = function(.x){
      readLines(.x, encoding = "UTF-8")
    })

  names(lst) <- noms
  myrefs <- append(myrefs, lst)

  myrefs$vector_refs <- c("ID", "IDllar",
                          "COV_RendInd_reestimat_atSI",
                          "COV_RendInd_reestimat_atNO",
                          "COV_RendInd_reestimat_erto",
                          "COV_RendInd_original",
                          "COV_hy020_0",
                          "COV_rendes_HH_brut",
                          "COV_correccio_irpf1",
                          "COV_correccio_irpf2", "HX240",
                          "menors18", "COV_llindar_IVU")

  ## cal aplicar correcció per a que a tot arreu la referència sigui el títol de l'arxiu i no això altre
  myrefs$cap <- myrefs$Cats_sense_menors
  myrefs$dos <- myrefs$Cats_dos_menors
  myrefs$parcial <- myrefs$Cats_Tparcial
  myrefs$complet <- myrefs$Cats_Tcomplet

  # Carrega referències relatives (EPA)
  ## Faig la versió que em permet modificar la menor quantitat de codi, tot i no ser la més eficient.

  ## Probabilitats individuals atur
  myrefs$COV_prob_atur_se <- F1_taula_probs_grups_ESP

  ## Probs ERTO per sectors
  myrefs$probs_ERTO_sector <- F1_ERTO_sect_ESP

  ##Probabilitats durada atur
  myrefs$probs_mensuals_atur <- F1_dur_atur %>%
    pull(pp)

  ## Probs durada ertos
  myrefs$probs_mensuals_erto <- readRDS(
    "_targets/objects/F1_dur_erto") %>%
    pull(probs)

  # ## Claus primàries
  myrefs$probs_ERTO_sector$key <-
    paste0(myrefs$probs_ERTO_sector$codi) # modificat 210420

  ## Variables de referència
  myrefs$Vars_PL211    <- paste0("PL211",
                                 LETTERS[1:12])

  # ## Zones de referència
  # myrefs$BCN <- c("Barcelona")
  # myrefs$AMB <- c("Barcelona", "Resta AMB")

  ## IPC
  # myrefs$fct_inf <- (COV_pull_IPC(territori = "ESP")) + 100)/100

  return(myrefs)
}

# COV_skim_dataset <- function(nom_dades, vars_conserva){
#
# }

# 2. deflactació de les variables de rendes
## semi-target: ecv
COV_deflacta <- function(ecv, fct_inf, mvars){
  # ATENCIÓ. aquesta funció sobre-escriu els noms de les variables originals
  # filtra vars que no hi siguin a les dades
  mvars <- intersect(names(ecv), mvars)

  if(length(mvars) < 1){
    stop("Cap variable d'origen coincideix amb les variables de destí. \nCal revisar el vector de variables que es volen deflactar.")
  }

  infl <- (100 + fct_inf)/100 # variació % IPC al territori entre T0 i T1

  ecv[,mvars] <- sapply(ecv[,mvars], `*`, infl)

  return(ecv)
}

# 3. automatitzar la creació de les variables necessàries durant la simulació
## semi-target: ecv
COV_computa_vars_ECV <- function(dades, Myrefs){
  # stop()
  ##  Socio-demogràfiques
  dades <- dades %>%
    mutate(prim = stringr::str_sub(ID, start = -1))

  dades$RB080_COV <- factor(cut(
    dades$RB080G,
    breaks = c(-1, 17, 30, 45, 110),
    include.lowest = T,
    right = F,
    labels = c("menors 16", "16:29",
               "30:44", "45+")
  ))

  dades <- IERMB::fes_IE(dades)
  dades <- IERMB::fes_IE_gini(dades)
  dades <- IERMB::fes_ESECs(dades)
  dades <- IERMB::fes_RTLs(dades)
  dades <- IERMB::fes_HCB(dades)
  dades <- IERMB::fes_HX060s(dades)

   # dades$RB090 <- car::recode(dades$RB090,
   #                            '"Mujer" = "D";
   #                            else = "H"')

  dades$RB090 <- forcats::fct_collapse(dades$RB090,
                                    "D" = c("Mujer"),
                                    other_level = "H")


  levels(dades$PB210) <- c("Espanya", rep("Resto del mundo", 3))
  # si no està informat, assigno default:
  dades$PB210 <-
    forcats::fct_explicit_na(dades$PB210, na_level = "Espanya")

  dades$menors <- dplyr::case_when(
    dades$HX060 %in% Myrefs$Cats_sense_menors ~ 0,
    dades$HX060 %in% Myrefs$Cats_un_menor  ~ 1,
    dades$HX060 %in% Myrefs$Cats_dos_menors ~ 2,
    is.na(dades$HX060)   ~ 0
  )

  dades <- dades %>%
    group_by(IDllar) %>%
    mutate(menors18 = ifelse(any(RB080G < 18), T, F)) %>%
    ungroup(.)

  dades$COV_suma_rendes_treb <- (rowSums(
    x = dades[, c("PY010G",
                 "PY021G",
                 "PY050G")],
             na.rm = T,
             dims = 1))


  dades$pl111a <- dades$PL111A
  # agrupa en els mateixos levels que la EPA
  lv1 <- levels(dades$PL111A)
  dades$pl111a <- case_when(
    dades$PL111A %in% lv1[grep("Agricultura", lv1)] ~ "A",
    dades$PL111A %in% lv1[grep("extractivas", lv1)] ~ "C",
    dades$PL111A %in% lv1[grep("manufacturera", lv1)] ~ "B",
    dades$PL111A %in% lv1[grep("Suministro", lv1)] ~ "C",
    dades$PL111A %in% lv1[grep("Construcci", lv1)] ~ "E",
    dades$PL111A %in% lv1[grep("Comercio", lv1)] ~ "F",
    dades$PL111A %in% lv1[grep("Transporte", lv1)] ~ "G",
    dades$PL111A %in% lv1[grep("Hosteler", lv1)] ~ "F",
    dades$PL111A %in% lv1[grep("comunicaciones", lv1)] ~ "G",
    dades$PL111A %in% lv1[grep("financieras", lv1)] ~ "H",
    dades$PL111A %in% lv1[grep("inmobiliarias", lv1)] ~ "H",
    dades$PL111A %in% lv1[grep("profesionales", lv1)] ~ "H",
    dades$PL111A %in% lv1[grep("administrativas", lv1)] ~ "H",
    dades$PL111A %in% lv1[grep("Administraci", lv1)] ~ "I",
    dades$PL111A %in% lv1[grep("Educaci", lv1)] ~ "I",
    dades$PL111A %in% lv1[grep("sanitarias", lv1)] ~ "I",
    dades$PL111A %in% lv1[grep("recreativas", lv1)] ~ "J",
    dades$PL111A %in% lv1[grep("Otros", lv1)] ~ "J",
    dades$PL111A %in% lv1[grep("Hogares", lv1)] ~ "J",
    dades$PL111A %in% lv1[grep("Organismos", lv1)] ~ "I",
    (dades$PL111A == "" | is.na(dades$PL111A)) ~ "K",
  )

  levels(dades$ESEC4) <- c("Dir. i prof.",
                           "Ocup. interm",
                           "Classe treb",
                           "No classif")

  dades$ESEC4 <- forcats::fct_explicit_na(dades$ESEC4,
                                          na_level = "No classif")

  dades <- dades %>%
    mutate(
      PL031 = as.factor(PL031),
      ACTIV = case_when(
        PL031 %in% Myrefs$Cats_poblacio_ocupada ~ "Ocupats",
        PL031 %in% Myrefs$Cats_poblacio_aturada ~ "Aturats",
        PL031 %in% c(setdiff(
          levels(dades$PL031),
          Myrefs$Cats_poblacio_activa
        )) ~ "Inactius"
      )
    )

  dades$ACTIV <- forcats::fct_explicit_na(
    dades$ACTIV,
    na_level = "Altres inactiv.")

  ## Nº mesos atur
  dades$COV_MESOS_ATUR <- apply(dades[, Myrefs$Vars_PL211],
                                1, function(fila) {
                                  sum(fila %in% Myrefs$Cats_poblacio_aturada)
                                })
  ## Nº mesos ocupats
  dades$COV_MESOS_OCUP <- apply(dades[, Myrefs$Vars_PL211],
                                1, function(fila) {
                                  sum(fila %in% Myrefs$Cats_poblacio_ocupada)
                                })
  ## Nº mesos actius
  dades$COV_MESOS_ACTIV <- apply(dades[, Myrefs$Vars_PL211], 1,
                                 function(fila) {
                                   sum(fila %in% Myrefs$Cats_poblacio_activa)
                                 })
  ## Any mesos parcial
  dades$COV_MESOS_PARCIAL <- apply(dades[Myrefs$Vars_PL211], 1,
                                   function(fila) {
                                     sum(fila %in% Myrefs$Cats_Tparcial)
                                   })
  ## Any mesos complet
  dades$COV_MESOS_COMPLET <- apply(dades[Myrefs$Vars_PL211], 1,
                                   function(fila) {
                                     sum(fila %in% Myrefs$Cats_Tcomplet)
                                   })
  ## suma mesos ocupats parcial/complet
  dades$COV_suma <- rowSums(dades[, c("COV_MESOS_COMPLET",
                                      "COV_MESOS_PARCIAL")],
                            na.rm = T)

  ### Caveats
  ### Si es tenen ingressos del treball però no s'ha informat que l'activitat laboral sigui la situació principal en cap mes, asigna-li 12 a mesos ocupat/actius:
  dades$COV_MESOS_OCUP <- ifelse(dades$COV_suma_rendes_treb > 0 &
                                   dades$COV_MESOS_OCUP == 0,
                                 12,
                                 dades$COV_MESOS_OCUP)

  dades$COV_MESOS_ACTIV <- ifelse(dades$COV_MESOS_OCUP == 12,
                                  12,
                                  dades$COV_MESOS_ACTIV)

  ##  Exclusió social

  dades$COV_exclusio <- ifelse(dades$COV_MESOS_ATUR == 12 &
                                 dades$ACTIV == "Aturats" &
                                 dades$PY090G < 100,
                               T,
                               F)

  ## sobre tot complet o parcial?

  dades$ParCompl <- dplyr::case_when(
    dades$COV_MESOS_COMPLET > dades$COV_MESOS_PARCIAL  ~ "complet",
    dades$COV_MESOS_COMPLET < dades$COV_MESOS_PARCIAL  ~ "parcial",
    dades$COV_suma == 0                                ~ "inactiu",
    dades$COV_MESOS_COMPLET == dades$COV_MESOS_PARCIAL ~ "complet"
  )


  ## Llindar de referència IMV

  monop <- "Un adulto con al menos un niño dependiente"

  dades <-
    dades %>%
    group_by(IDllar) %>%
    #base llar unipersonal + 138.5 mensual per persona addicional
    mutate(NPER = length(ID),
           COV_llindar_IVU =
             (461.5 * 12) + (138.5 * 12) * (NPER - 1)) %>%
    # aplica correcció llars monoparentals i màxims
    mutate(
      COV_llindar_IVU = ifelse(HX060 == monop,
                               COV_llindar_IVU + 1200, COV_llindar_IVU),
      COV_llindar_IVU = ifelse(COV_llindar_IVU > 1015.3 * 12,
                               1015.3 * 12, COV_llindar_IVU)
    ) %>%
    ungroup(.)

  # Noms vars
  dades$PY140G <- dades$PY140N

  #   Elegibles

  dades$COV_Pactiva <- ifelse(dades$COV_MESOS_ACTIV > 5, T, F)
  dades$COV_Ocupats <- ifelse(dades$COV_MESOS_OCUP > 5, T, F)

  # correcció sobre pl111a que no es pot fer abans
  dades$pl111a[dades$COV_Pactiva == F] <- NA

  # IMV

  dades <- dades %>%
    group_by(IDllar) %>%
    mutate(nper = length(ID),
           COV_llindar_IVU = ifelse(
             is.na(COV_llindar_IVU),
             (461.5 * 12) + (138.5 * 12) * (.data$nper-1),
             COV_llindar_IVU
           ))%>%
    ungroup(.)

  mediana <- Hmisc::wtd.quantile(dades$IE,
                                 weights = dades$RB050,
                                 probs = c(.5))
  dades$TRP60 <- factor(
    ifelse(dades$IE < mediana * 0.6, "Pobre", "No pobre")
    )
  dades$TRP40 <- factor(
    ifelse(dades$IE < mediana * 0.4, "Pobre", "No pobre")
    )
  dades$TRP30 <- factor(
    ifelse(dades$IE < mediana * 0.3, "Pobre", "No pobre")
    )

  return(dades)

}

# 4. funcions per crear les variables inicials de la simulació, i.e., probabilitats atur, mesos atur, etc
## semi-target: ecv
COV_computa_vars_probs_sim <- function(dades, Myrefs){
  # stop()
  ## probabilitats individuals d'anar a l'atur
  # Crea referència per traslladar probs per sexe * edat * naix *csp a les dades
  Myrefs$COV_prob_atur_se$KEY1 <- interaction(
    Myrefs$COV_prob_atur_se$RB090,
    Myrefs$COV_prob_atur_se$RB080_COV,
    Myrefs$COV_prob_atur_se$PB210,
    Myrefs$COV_prob_atur_se$ESEC4,
    drop = T)

  dades$KEY1 <- interaction(
    dades$RB090,
    dades$RB080_COV,
    dades$PB210,
    dades$ESEC4,
    drop = T)
  # checks:
  # setdiff(levels(Myrefs$COV_prob_atur_se$KEY1), levels(dades$KEY1))
  # setdiff(levels(dades$KEY1), levels(Myrefs$COV_prob_atur_se$KEY1))

  # Trasllada valors de taula de probs a les dades
  dades$COV_pa_se <- Myrefs$COV_prob_atur_se$prob[match(dades$KEY1,
                                                        Myrefs$COV_prob_atur_se$KEY1)]
  # les probabilitas s'han d'expressar en base 1
  dades$COV_probs_atur <- dades$COV_pa_se/100

  # Correcció per aturats actuals
  ### sobre la població activa (Pactiva <- dades$ID[dades$COV_MESOS_ACTIV > 6]),
  #quina proporció ha passat 12 mesos a l'atur? 9.5% -> és un % molt pròxim a la taxa projectada. dona molt poc joc.
  dades$COV_probs_atur[dades$COV_MESOS_ATUR == 12] <- 1
  dades$COV_probs_atur[dades$COV_MESOS_ATUR  %in% c(9:11)] <-
    dades$COV_probs_atur[dades$COV_MESOS_ATUR  %in% c(9:11)] * 1.2

  # Correcció per menors i jubilats
  dades$COV_probs_atur[dades$RB080G < 16] <- 0
  dades$COV_probs_atur[dades$RB080G > 65] <- 0

  # No pot haver-hi NA's, per tant, si NA, assigna 0
  # No pot haver-hi probs > 1
  dades$COV_probs_atur[is.na(dades$COV_probs_atur)] <- 0
  dades$COV_probs_atur[dades$COV_probs_atur > 1] <- 1

  ## probabilitats individuals de cobrar l'atur

  dades$KEY2 <- interaction(dades$RB080_COV,
                            dades$ESEC4)
  Myrefs$COV_prob_atur_se$KEY2 <- interaction(
    Myrefs$COV_prob_atur_se$RB080_COV,
    Myrefs$COV_prob_atur_se$ESEC4
  )
  # trasllada variable prob_cobra_atur2
  dades$COV_prob_cobrar_atur <-
    Myrefs$COV_prob_atur_se$prob_cobra_atur2[
      match(dades$KEY2,
            Myrefs$COV_prob_atur_se$KEY2)
    ]
  dades$COV_prob_cobrar_atur <- dades$COV_prob_cobrar_atur / 100
  # exclou de cobrar atur als perfils d'exclusió social
  dades$COV_prob_cobrar_atur[dades$COV_exclusio == T] <- 0

  # correccions per menors i jubilats
  dades$COV_prob_cobrar_atur[dades$RB080G < 16] <- 0
  dades$COV_prob_cobrar_atur[dades$RB080G > 65] <- 0

  # No pot haver-hi NA per a que sample funcioni
  dades$COV_prob_cobrar_atur[is.na(dades$COV_prob_cobrar_atur)] <- 0

  ## probabilitats ERTO per sector activitat
  # dades$key <- dades$pl111a # modificat 210420
  dades$COV_probs_sector <-
    Myrefs$probs_ERTO_sector$Val[
      match(dades$pl111a,
            Myrefs$probs_ERTO_sector$key)]

  # als NA els imputo prob 0 pq són menors de 16 anys
  dades$COV_probs_sector[is.na(dades$COV_probs_sector)] <- 0


  # summary(dades$COV_probs_sector) # cal expressar amb base 1
  dades$COV_probs_sector <- dades$COV_probs_sector/100

  # correccions per menors i jubilats
  dades$COV_probs_sector[dades$RB080G < 16] <- 0
  dades$COV_probs_sector[dades$RB080G > 65] <- 0

  # Ajust per autònoms (el doble de probabilitats)
  auto <- levels(dades$PL031)[grep("cuenta propia", levels(dades$PL031))]
  dades$COV_probs_sector[dades$PL031 %in% auto] <- dades$COV_probs_sector[dades$PL031 %in% auto] * 2
  dades$COV_probs_sector[dades$COV_probs_sector > 1] <- 1

  # No pot haver-hi NA's, per tant, si NA, assigna 0
  # No pot haver-hi probs > 1
  dades$COV_probs_sector[is.na(dades$COV_probs_sector)] <- 0
  dades$COV_probs_sector[dades$COV_probs_sector > 1] <- 1

  # en algun lloc apareix la variable amb un altre nom. caldria revisar i uniformitzar. De momento deixo això aquí:
  dades$COV_probs_erto <- dades$COV_probs_sector

  ## nombre de mesos a l'atur
  set.seed(Myrefs$seed)
  dades$COV_estim_mesos_atur <- sample(
    c(1:12),
    prob = Myrefs$probs_mensuals_atur,
    length(dades$ID),
    replace = T
  )

  ## nombre de mesos en erto
  set.seed(Myrefs$seed)
  dades$COV_estim_mesos_erto <- sample(
    c(1:12),
    prob = Myrefs$probs_mensuals_erto, # AQUI
    length(dades$ID),
    replace = T
  )
  # Introduir correcció per autònoms: màxim 4.5 mesos:
  dades$COV_estim_mesos_erto[dades$PL031 %in% Myrefs$Cats_autonoms] <- 4.5


  dades <- within(dades, rm("KEY1", "KEY2"))

  return(dades)

}

# 5. funcions pel re-càlcul de les rendes individuals
## semi-target: ecv
COV_calcula_BC <- function(dades, Myrefs){
  ##  Bases de cotització per recàlcul rendes
  ## Estimació imports base cotització. L'objectiu és crear una base mensual que després    es multiplicarà pel nombre de mesos passats a l'atur, NOMÉS per aquelles persones a qui    els toca anar a l'atur/ERTO. No es pot fer igual que ho fa la SS (en base als darrers 6    mesos), però és la millor aproximació possible

  # aquesta verisió, sense cotxe, es fa servir pel càlcul import prestacions
  dades$COV_suma_treball_anual <- (rowSums(x = dades[,c("PY010G",
                                                        "PY050G")],
                                           na.rm = T,
                                           dims = 1))

  dades$COV_Base_Mens_cotitza  <- dades$COV_suma_treball_anual / dades$COV_MESOS_OCUP

  # Rectifica errors, negatius i autònoms
  dades$COV_Base_Mens_cotitza[is.infinite(dades$COV_Base_Mens_cotitza)] <- 0
  dades$COV_Base_Mens_cotitza[is.na(dades$COV_Base_Mens_cotitza)] <- 0
  dades$COV_Base_Mens_cotitza[dades$COV_Base_Mens_cotitza < 0] <- 0

  dades$COV_Base_Mens_cotitza[dades$PL031 %in% Myrefs$Cats_autonoms] <- 723.59 * 100/70

  ## Cal aplicar mínims i màxims sobre les quantitats mensuals
  ### El mínim vol dir que si malgrat les teves circumstàncies l'import queda per sota,    se't puja fins al mínim. Ara mateix, si les rendes del treball informades són 0,    s'imputa la prestació mínima d'atur. exemple 1: no s'ha cobrat salari pq ja s'estava a    l'atur. s'imputa el valor mínim de l'atur. exemple 2: no s'ha cobrat salari pq s'és    aturat de llarga durada, o s'ha desistit de buscar feina: s'imputa el valor mínim de    l'atur, que és una mica superior al subsidi (430€).
  # Referència per definir llindars mínims i màxims: https://www.sepe.es/HomeSepe/Personas   /distributiva-prestaciones/Cuantias-anuales.html -> la definició d'aquests llindars    passa a 00_02.R

  # # # # MÍNIMS # # # #

  # Temps complet, sense fills, mínim 502 €
  dades$COV_Base_Mens_cotitza <- ifelse(dades$COV_Base_Mens_cotitza * 0.7 < 502 &
                                          dades$ParCompl == "complet" &
                                          dades$menors == 0,
                                        502 * 100 / 70,
                                        dades$COV_Base_Mens_cotitza)

  # Temps parcial, sense fills, mínim 248 €
  dades$COV_Base_Mens_cotitza <- ifelse(dades$COV_Base_Mens_cotitza * 0.7 < 248.5 &
                                          dades$ParCompl == "parcial" &
                                          dades$menors == 0,
                                        248.5 * 100 / 70,
                                        dades$COV_Base_Mens_cotitza)

  # Temps complet, amb fills, mínim 671 €
  dades$COV_Base_Mens_cotitza <- ifelse(dades$COV_Base_Mens_cotitza * 0.7 < 671 &
                                          dades$ParCompl == "complet" &
                                          dades$menors > 0,
                                        671 * 100 / 70,
                                        dades$COV_Base_Mens_cotitza)

  # Temps parcial, amb fills, mínim 335 €
  dades$COV_Base_Mens_cotitza <- ifelse(dades$COV_Base_Mens_cotitza * 0.7 < 335 &
                                          dades$ParCompl == "parcial" &
                                          dades$menors > 0,
                                        335 * 100 / 70,
                                        dades$COV_Base_Mens_cotitza)

  # # # # MÀXIMS # # # #

  # Temps complet o parcial, sense fills, màxim 1098 €
  dades$COV_Base_Mens_cotitza <- ifelse(dades$menors == 0 &
                                          dades$COV_Base_Mens_cotitza * 0.7 > 1098,
                                        1567,
                                        dades$COV_Base_Mens_cotitza)

  # Temps complet o parcial, un fill, màxim 1255 €
  dades$COV_Base_Mens_cotitza <- ifelse(dades$menors == 1 &
                                          dades$COV_Base_Mens_cotitza * 0.7 > 1255,
                                        1793,
                                        dades$COV_Base_Mens_cotitza)

  # Temps complet o parcial, dos+ fills, màxim 1412 €
  dades$COV_Base_Mens_cotitza <- ifelse(dades$menors > 1 &
                                          dades$COV_Base_Mens_cotitza * 0.7 > 1412,
                                        2017,
                                        dades$COV_Base_Mens_cotitza)


  return(dades)
}

COV_calcula_atur <- function(dades, Myrefs){

  dades$COV_py090 <- case_when(
    dades$COV_estim_mesos_atur <= 6 ~  dades$COV_Base_Mens_cotitza * 0.7 *    dades$COV_estim_mesos_atur,
    dades$COV_estim_mesos_atur >  6 ~ (dades$COV_Base_Mens_cotitza * 0.7 *    6) +
      (dades$COV_Base_Mens_cotitza * 0.5 * (dades$COV_estim_mesos_atur - 6))
  )

  return(dades)

}

COV_calcula_RendTreb <- function(dades, Myrefs){
  # 1. defineix el que serien els ingressos mensuals per treball
  # diferent al càlcul de la base pq inclou cotxe d'empresa
  dades$COV_import_mensual_treball <-
    dades$COV_suma_rendes_treb /
    dades$COV_MESOS_OCUP

  dades$COV_import_mensual_treball[is.nan(dades$COV_import_mensual_treball)] <- 0

  # 2.1 defineix nombre mesos que es treballa post atur
  dades$COV_dif_mesos_atr <-
    dades$COV_MESOS_OCUP -
    dades$COV_estim_mesos_atur

  dades$COV_dif_mesos_atr[dades$COV_dif_mesos_atr < 0] <- 0

  # 2.2 defineix nombre mesos que es treballa post erto
  dades$COV_dif_mesos_ert <- dades$COV_MESOS_OCUP -
    dades$COV_estim_mesos_erto

  dades$COV_dif_mesos_ert[dades$COV_dif_mesos_ert < 0] <- 0

  # 3. calcula imports erto

  dades$COV_pyERTO <- dades$COV_Base_Mens_cotitza * dades$COV_estim_mesos_erto * 0.7

  # hist(dades$COV_pyERTO)

  # 4.1 calcula rendes treball si es passés a l'atur: mesos treballats * import mensual
  dades$COV_py010_atr <- dades$COV_import_mensual_treball * dades$COV_dif_mesos_atr

  # 4.2 calcula rendes treball si es passés a erto
  # mantinc rendes del treball com a treball + erto, però prèviament he creat una variable amb l'import dels ERTO per a càlculs pre/post

  dades$COV_py010_ert <-
    (dades$COV_import_mensual_treball * dades$COV_dif_mesos_ert) +
    (dades$COV_Base_Mens_cotitza * 0.7 * dades$COV_estim_mesos_erto)


  return(dades)

}

COV_agrega_RIndiv <- function(dades, Myrefs) {

  dades$COV_RendInd_original  <-
    apply(dades[, Myrefs$Vars_rendIndiv_original],
          1, sum, na.rm = T)

  dades$COV_RendInd_reestimat_atSI <-
    rowSums(x = dades[, c(Myrefs$Vars_rendIndiv_estimat_base,
                          "COV_py090",
                          "COV_py010_atr")],
            dims = 1,
            na.rm = T)

  dades$COV_RendInd_reestimat_atNO <-
    rowSums(x = dades[, c(Myrefs$Vars_rendIndiv_estimat_base,
                          "COV_py010_atr")],
            dims = 1,
            na.rm = T)

  dades$COV_RendInd_reestimat_erto <-
    rowSums(x = dades[, c(Myrefs$Vars_rendIndiv_estimat_base,
                          "COV_py010_ert")],
            dims = 1,
            na.rm = T)

  return(dades)
}

is.crap <- function(vect, value){
  #@vect: vector de valors a revisar
  #@value: valor a imputar als perduts, infinits, nuls...
  vect[is.na(vect)] <- value
  vect[is.infinite(vect)] <- value
  vect[is.null(vect)] <- value
  vect
}

COV_agrega_RLlars <- function(dades, Myrefs){

  # 1. Fes vectors per rendes brutes i impostos a treure
  dades$COV_rendes_HH_brut <- apply(dades[,Myrefs$Vars_rendes_llar_suma],
                                    1, sum, na.rm = T)
  dades$COV_impostos_HH <- apply(dades[,Myrefs$Vars_rendes_llar_resta],
                                 1, sum, na.rm = T)

  # 2. Correcció dels impostos per no gravar sobre IRPF que es deixa d'ingressar. Només pels casos seleccionats (atur & ERTO).
  ### Si es cobra atur / erto (ho faig sobre la base del erto per facilitar). la diferència seria només 6 mesos al 70% per atur
  #### calcula diferència import observat i estimat
  dades$COV_dif_rendes1 <- dades$COV_suma_treball_anual - dades$COV_py010_ert
  #### calcula perc cotització inicial
  dades$COV_perc_IRPF1 <- (dades$PY010G - dades$PY010N) / dades$PY010G
  dades$COV_perc_IRPF1 <- is.crap(dades$COV_perc_IRPF1, .19) # poso el valor d'autònoms
  #### calcula import de HY140G que no s'hauria de pagar
  dades$COV_correccio_irpf1 <- dades$COV_dif_rendes1 * dades$COV_perc_IRPF1 # aquesta variable s'ha de sumar a COV_rendes_HH_net_0 per les persones que vagin a l'atur/erto i cobren prestació
  dades$COV_correccio_irpf1[is.nan(dades$COV_correccio_irpf1)] <- 0

  ### Si es va a l'atur i no es cobra cap prestació
  #### calcula diferència import observat i estimat
  dades$COV_dif_rendes2 <- dades$COV_suma_treball_anual - dades$COV_py010_atr
  #### calcula perc cotització inicial
  dades$COV_perc_IRPF2 <- (dades$PY010G - dades$PY010N) / dades$PY010G
  dades$COV_perc_IRPF2 <- is.crap(dades$COV_perc_IRPF2, .19) # poso el valor d'autònoms
  #### calcula import de HY140G que no s'hauria de pagar
  dades$COV_correccio_irpf2 <- dades$COV_dif_rendes2 * dades$COV_perc_IRPF2 # aquesta variable
  dades$COV_correccio_irpf2[is.nan(dades$COV_correccio_irpf2)] <- 0

  # 3. Rendes component llar si no hi ha ningú a la llar atur/erto
  dades$COV_rendes_HH_net_0 <- dades$COV_rendes_HH_brut - dades$COV_impostos_HH

  # 3.1. Rendes total llar si ningú va a l'atur/erto
  dades <- dades %>%
    group_by(IDllar) %>%
    mutate(
      COV_suma_rendes_indiv = sum(COV_RendInd_original, na.rm = T)
    ) %>%
    ungroup(.) %>%
    mutate(COV_hy020_0 = COV_rendes_HH_net_0 +
             COV_suma_rendes_indiv)

  # les rendes de la llar si algú va a l'atur es calculen dinàmicament, pq pot haver-hi més d'una persona a l'atur/erto

  return(dades)

}


# 6. unifica funcions amb el mateix target
## target: ecv
COV_fes_target_ecv <- function(dades,
                               Myrefs,
                               fct_inf){
  # stop()
  fct_inf <- as.numeric(as.character(fct_inf))
  # skim dataset
  dades <- dades %>%
    select(intersect(Myrefs$Vars_inicials, names(.)))

  # aplica deflactacions segons territori
  dades <- COV_deflacta(ecv     = dades,
                        fct_inf = fct_inf,
                        mvars   = Myrefs$Vars_deflactar
  )

  # estandarditza variables
  dades <- COV_computa_vars_ECV(dades, Myrefs)

  # imputa probabilitats i durades segons territori
  dades <- COV_computa_vars_probs_sim(dades,
                                      Myrefs = Myrefs)

  # # re-calcula rendes individuals en escenari atur/erto
  dades <- COV_calcula_BC(dades, Myrefs = Myrefs)
  dades <- COV_calcula_atur(dades, Myrefs = Myrefs)
  dades <- COV_calcula_RendTreb(dades, Myrefs = Myrefs)
  dades <- COV_agrega_RIndiv(dades, Myrefs = Myrefs)
  dades <- COV_agrega_RLlars(dades, Myrefs = Myrefs)

  # retorna
  return(dades)
}




# ------------------------------------####
# FASE 3

# funcions setup

  ## Definició: Extreu conjunt de seeds aleatòriament amb llargada == iteracions
  ## @inputs: simrefs$iteracions, simrefs$seed
  ## @outputs: llista amb [[1]] seeds
  ## @arguments: simrefs
  fes_COV_reslist_seeds <- function(simrefs){
    # 1. crea la llista
    llista <- list()
    # 2. crea vector seeds
    llista$seeds <-
      if(simrefs$iteracions > 1){
        set.seed(simrefs$seed_inicial)
        sample(x = 9999999,
               size = simrefs$iteracions,
               replace = F)
      } else {
        simrefs$seed
      }
    checks$ch3 <- length(simrefs$seed) ==
      simrefs$iteracions
    print(head(llista$seeds))

    return(llista)
  }

  ## Definició: split llista de seeds per paral·lelitzar
  ## @inputs: mreslist$seeds, simrefs$chunk_length
  ## @outputs: split list de seeds
  ## @arguments: mreslist, simrefs
  fes_COV_split_seeds <- function(mreslist,
                                  simrefs){
    split(
      x = mreslist$seeds,
      f = ceiling(seq_along(mreslist$seeds) /
                    simrefs$chunk_length)) %>%
      lapply(.,
             function(l) list(seeds = l))

  }

# Funcions selecció casos

  ## Definició: extreu aleatòriament persones afectades
  ## @inputs: rsl, dades, simrefs$idx_Pactiva, simrefs$Taxa_atur_proj, simrefs$Prop_cobra_atur
  ## @outputs: rsl$aturats, rsl$seeds
  ## @arguments: rsl, dades, simrefs
  fes_COV_sample_afectats <- function(rsl,
                                      dades = dades,
                                      simrefs,
                                      Taxa_atur_proj,
                                      Prop_cobra_atur
                                      ){
    # 1. crea llista ID's aturats
    rsl$aturats <- lapply(rsl$seeds, function(s){
      set.seed(s)
      sample(
        x       = dades$ID[simrefs$idx_Pactiva],
        size    = round(length(simrefs$idx_Pactiva) *
                          Taxa_atur_proj, 0),
        replace = F,
        prob    = dades$COV_probs_atur[simrefs$idx_Pactiva]
      )
    })

    # 2.  tria casos que cobren prestació atur, entre els aturats

    rsl$cobren <- lapply(seq_len(length(rsl$seeds)), function(it){
      pool <- dades[match(rsl$aturats[[it]], dades$ID),
                    c("ID", "COV_prob_cobrar_atur")]
      set.seed(rsl$seeds[it])
      sample(x = pool$ID,
             size = round(nrow(pool) * Prop_cobra_atur, 0),
             replace = F,
             prob = pool$COV_prob_cobrar_atur
      )

    })

    checks$ch6 <- all(rsl$cobren %in% rsl$aturats)

    # 3. crea llista ID's ERTO
    rsl$ertos <- purrr::map2(
      .x = rsl$seeds,
      .y = rsl$aturats,
      .f = function(.x,.y){
        # stop()
        set.seed(.x)
        pool <- setdiff(
          dades$ID[simrefs$idx_Pocupada], .y # ha de ser població
        ) %>%
          setdiff(., dades$ID[dades$COV_exclusio])
        # pool <- COV_fes_pool_erto(.y, dades = dades)
        # check: l'extensió del pool de ERTOS ha de ser INFERIOR a la llargada de la població ocupada i de la població activa
        checks$ch1 <- length(pool) < length(simrefs$idx_Pactiva)
        checks$ch2 <- length(pool) < length(simrefs$idx_Pocupada)

        sample(
          x       = pool,
          size    = ceiling(length(simrefs$idx_Pocupada) *
                              simrefs$Taxa_erto_proj),
          replace = F,
          prob    = dades$COV_probs_erto[dades$ID %in% pool]
        )
      }
    )

    return(rsl)

  }

# Funcions re-càlcul de les rendes

  ## Definició: A partir de la selecció de casos, recalcula les rendes individuals
  ## @inputs: seeds, aturats, cobren, ertos, rendes, dades
  ## @outputs: subconjunt dades amb vars. clau (dad)
  ## @arguments: seeds, aturats, cobren, ertos, rendes, dades
  fes_COV_calcula_rendes_llarg <- function(seeds, aturats, cobren, ertos, dades, simrefs){
    # stop()
    # cal que aquesta passa sigui aquí:
    dad <- dades[,simrefs$vector_refs]
    dad$IDllar <- stringr::str_sub(dad$ID,  end = -3)

    llars_cobren <- stringr::str_sub(cobren,  end = -3)
    llars_ertos  <- stringr::str_sub(ertos,   end = -3)
    llars_atur   <- stringr::str_sub(aturats, end = -3)

    # 1. assigna rendes individuals
    dad$COV_RI <- dplyr::case_when(
      dad$ID %in% cobren  ~ dad$COV_RendInd_reestimat_atSI,
      (dad$ID %in% aturats &
         (!dad$ID %in% cobren)) ~ dad$COV_RendInd_reestimat_atNO,
      dad$ID %in% ertos ~ dad$COV_RendInd_reestimat_erto,
      !(dad$ID %in% c(aturats, ertos))  ~ dad$COV_RendInd_original
    )

    # 2. Defineix component impostos individual
    dad$COV_irpf <- dplyr::case_when(
      (dad$ID %in% c(cobren, ertos))  ~ dad$COV_correccio_irpf1,
      (dad$ID %in% aturats & !(dad$ID %in% cobren)) ~ dad$COV_correccio_irpf2,
      !(dad$ID %in% c(aturats, ertos)) ~ 0
    )

    dad <- dad %>%
      group_by(IDllar) %>%
      mutate(COV_irpf_HH = sum(COV_irpf, na.rm = T)) %>%
      ungroup(.)

    dad$COV_impostos_HH <- dades$COV_impostos_HH - dad$COV_irpf_HH # segueix individual

    # 3. Suma rendes individuals per llars
    dad <- dad %>%
      group_by(IDllar) %>%
      mutate(COV_RI_HH = sum(COV_RI, na.rm = T)) %>%
      ungroup(.)

    # 4. Calcula rendes llar
    dad$COV_hy020_1 <-  (dad$COV_rendes_HH_brut - dad$COV_impostos_HH) + dad$COV_RI_HH

    # 5. Assigna segons selecció
    dad$COV_hy020 <- ifelse(dad$IDllar %in% c(llars_atur, llars_ertos),
                            dad$COV_hy020_1,
                            dad$COV_hy020_0)


    # 6. Determina elegibilitat IMV
    dad$COV_elegible_IMV <- ifelse(dad$menors18 &
                                     dad$COV_hy020 <
                                     dad$COV_llindar_IVU,
                                   T, F)

    dad$COV_elegible_IMV2 <- ifelse((dad$COV_hy020 <
                                       dad$COV_llindar_IVU) &
                                      dad$COV_elegible_IMV == F,
                                    T, F)

    # 7. Calcula IE pre IMV
    dad$COV_IE <- dad$COV_hy020 / dad$HX240

    # 8. calcula vars clau pre IMV
    ## Calcula TRPs
    mediana <- Hmisc::wtd.quantile(x = dad$COV_IE,
                                   weights = dades$RB050,
                                   probs = c(.5))

    dad[,paste0("COV_TRP", c(60, 40, 30))] <-
      lapply(X = c(.6, .4, .3), function(perc){
        fes_trp(ie = dad$COV_IE,
                med = mediana,
                perc = perc)
      })


    ## Calcula TRPs ancorades
    medianA <- Hmisc::wtd.quantile(x = dades$IE, # amb dades inicials
                                   weights = dades$RB050,
                                   probs = c(.5))

    dad[,paste0("COV_TRP_ANC", c(60, 40, 30))] <-
      lapply(X = c(.6, .4, .3), function(perc){
        fes_trp(ie = dad$COV_IE,
                med = medianA,
                perc = perc)
      })


    ## Calcula Gini pre IMV
    dad$COV_IE_Gini <- ifelse(dad$COV_IE <= 0, 1, dad$COV_IE)

    # Assigna IMV # # # # # # # # # # # # # # # # # # # # # # # #
    pool1 <- dad$IDllar[dad$COV_elegible_IMV == T]
    pool2 <- dad$IDllar[dad$COV_elegible_IMV2 == T]
    pool <- unique(c(pool1, pool2))

    set.seed(seeds)
    benef_IMV1 <- sample(x = pool1,
                         size = floor(length(pool) * simrefs$Prop_ofici_IMV),
                         replace = FALSE)

    set.seed(seeds)
    benef_IMV2 <- sample(x = pool2,
                         size = ceiling(length(pool) * simrefs$Prop_cobra_IMV),
                         replace = FALSE)

    dad$COV_hy020_IMV <- ifelse(
      dad$IDllar %in% c(benef_IMV1, benef_IMV2),
      dad$COV_llindar_IVU,
      dad$COV_hy020)

    checks$ch7 <-
      (round(
        (length(c(benef_IMV1, benef_IMV2)) / length(pool)), 4)) %in% seq(
       ((simrefs$Prop_cobra_IMV + simrefs$Prop_ofici_IMV) - 0.001),
       ((simrefs$Prop_cobra_IMV + simrefs$Prop_ofici_IMV) + 0.001),
         0.0001)

    # Recalcula indicadors clau després de IMV
    dad$COV_IE_IMV <- dad$COV_hy020_IMV / dad$HX240


    ## Calcula TRPs
    mediana <- Hmisc::wtd.quantile(x = dad$COV_IE_IMV,
                                   weights = dades$RB050,
                                   probs = c(.5))

    dad[,paste0("COV_TRP_IMV", c(60, 40, 30))] <-
      lapply(X = c(.6, .4, .3), function(perc){
        fes_trp(ie = dad$COV_IE_IMV,
                med = mediana,
                perc = perc)
      })


    ## Calcula TRPs ancorades
    medianA <- Hmisc::wtd.quantile(x = dades$IE, # amb dades inicials
                                   weights = dades$RB050,
                                   probs = c(.5))

    dad[,paste0("COV_TRP_ANC_IMV", c(60, 40, 30))] <-
      lapply(X = c(.6, .4, .3), function(perc){
        fes_trp(ie = dad$COV_IE_IMV,
                med = medianA,
                perc = perc)
      })


    ## Calcula Gini post IMV
    dad$COV_IE_Gini_IMV <- ifelse(dad$COV_IE_IMV <= 0, 1, dad$COV_IE_IMV)

    # . Retorna
    return(dad)

  }

  ## Definició: A partir de la selecció de casos, recalcula les rendes individuals
  ## @inputs: seeds, aturats, cobren, ertos, dades, simrefs
  ## @outputs: subconjunt dades amb vars. clau (dad)
  ## @arguments: seeds, aturats, cobren, ertos, dades, simrefs
  ## ATT:
  #cal revisar que l'ordre dels arguments sigui correcte
  #cal veure què aporta aquesta funció respecte fes_COV_calcula_rendes_llarg o viceversa. per què calen 2?
  fes_COV_calcula_rendes_curt  <- function(seeds, aturats, cobren, ertos, dades, simrefs){
    # stop()
    # note to self: cal que aquesta passa sigui aquí, ja ho has intentat canviar i no funciona pq necessites tant dad com dades!:
    dad <- dades[,simrefs$vector_refs]
    dad$IDllar <- stringr::str_sub(dad$ID,  end = -3)

    llars_cobren <- stringr::str_sub(cobren,  end = -3)
    llars_ertos  <- stringr::str_sub(ertos,   end = -3)
    llars_atur   <- stringr::str_sub(aturats, end = -3)

    # 1. assigna rendes individuals
    dad$COV_RI <- dplyr::case_when(
      dad$ID %in% cobren  ~ dad$COV_RendInd_reestimat_atSI,
      (dad$ID %in% aturats &
         (!dad$ID %in% cobren)) ~ dad$COV_RendInd_reestimat_atNO,
      dad$ID %in% ertos ~ dad$COV_RendInd_reestimat_erto,
      !(dad$ID %in% c(aturats, ertos))  ~ dad$COV_RendInd_original
    )

    # 2. Defineix component impostos individual
    dad$COV_irpf <- dplyr::case_when(
      (dad$ID %in% c(cobren, ertos))  ~ dad$COV_correccio_irpf1,
      (dad$ID %in% aturats & !(dad$ID %in% cobren)) ~ dad$COV_correccio_irpf2,
      !(dad$ID %in% c(aturats, ertos)) ~ 0
    )

    dad <- dad %>%
      group_by(IDllar) %>%
      mutate(COV_irpf_HH = sum(COV_irpf,
                               na.rm = T)) %>%
      ungroup(.)

    dad$COV_impostos_HH <- dades$COV_impostos_HH - dad$COV_irpf_HH # segueix individual

    # 3. Suma rendes individuals per llars
    dad <- dad %>%
      group_by(IDllar) %>%
      mutate(COV_RI_HH = sum(COV_RI, na.rm = T)) %>%
      ungroup(.)

    # 4. Calcula rendes llar
    dad$COV_hy020_1 <-  (dad$COV_rendes_HH_brut - dad$COV_impostos_HH) + dad$COV_RI_HH

    # 5. Assigna segons selecció
    dad$COV_hy020 <-
      ifelse(
        as.character(dad$IDllar) %in%
          c(llars_atur, llars_ertos),
        dad$COV_hy020_1,
        dad$COV_hy020_0)

    # 6. Determina elegibilitat IMV
    dad$COV_elegible_IMV <-
      ifelse(
        dad$menors18 &
          dad$COV_hy020 < dad$COV_llindar_IVU,
        T, F)

    dad$COV_elegible_IMV2 <-
      ifelse(
        (dad$COV_hy020 < dad$COV_llindar_IVU) &
          dad$COV_elegible_IMV == F,
        T, F)

    # 7. Calcula IE pre IMV
    dad$COV_IE <- dad$COV_hy020 / dad$HX240
    # Assigna IMV # # # # # # # # # # # # # # # # # # # # # # # #
    pool1 <- dad$IDllar[dad$COV_elegible_IMV == T]
    pool2 <- dad$IDllar[dad$COV_elegible_IMV2 == T]
    pool <- unique(c(pool1, pool2))

    set.seed(seeds)
    benef_IMV1 <-
      sample(
        # segons dades amb 2018. veure 00_Pieces_setup_00.r
        x = pool1,
        size = floor(
          length(pool) *
            simrefs$Prop_ofici_IMV
        ), #0.04929351
        replace = FALSE)

    set.seed(seeds)
    benef_IMV2 <-
      sample(
        x = pool2,
        size = ceiling(length(pool) *
                       simrefs$Prop_cobra_IMV
        ),
        replace = FALSE)

    dad$COV_hy020_IMV <-
      ifelse(
        dad$IDllar %in%
          c(benef_IMV1, benef_IMV2),
        dad$COV_llindar_IVU,
        dad$COV_hy020)

    # Recalcula indicadors clau després de IMV
    dad$COV_IE_IMV <- dad$COV_hy020_IMV / dad$HX240

    # . Retorna vectors

    return(dplyr::select(dad, ID,
                         COV_hy020_IMV,
                         COV_IE,
                         COV_IE_IMV)
    )

  }

# Funcions pel càlcul d'indicadors clau

  ## Definició: calcula TRP
  ## @inputs: ie, med, perc
  ## @outputs: variable TRP
  fes_trp <- function(ie, med, perc){
    factor(ifelse(ie < (med * perc), "Pobre", "No pobre"))
  }

  ## Definició: calcula indicadors clau pobresa & desigualtat
  ## @inputs: rendes (llista amb subconjunts vars rendes simulades)
  ## @outputs: resmat (matriu que agrupa conjunt resultats per simulacions)
  ## ATT:
  # revisar/re-valorar si és imprescindible tenir 2 versions
  fes_COV_calcula_indicadors_llarg <- function(rendes){
    # stop()
    # crea matriu resultats
    resvars <- c(names(
      rendes[[1]])[grep("TRP", names(rendes[[1]]))],
      "Gini", "Renda_mitjana_llars", "Mediana_IE",
      "Gini_IMV", "Renda_mitjana_llars_IMV", "Mediana_IE_IMV",
      "Mitjana_IE"
    )

    dimnames <-
      resmat <- matrix(data = 0,
                       nrow = simrefs$iteracions,
                       ncol = length(resvars),
                       dimnames = list(
                         c(),
                         c(resvars)
                       )) %>%
      as_tibble(.)

    denom <- sum(dades$RB050)

    # Calcula TRPs
    resmat[,resvars[grep("TRP", resvars)]] <-
      lapply(rendes, function(dd){
        # stop()
        dd %>%
          summarise(across(contains("TRP"),
                           ~sum(dades$RB050[.x == "Pobre"], na.rm = T))) %>%
          mutate_all(.funs = function(x){
            x / denom * 100
          })
      }) %>% do.call("rbind", .)

    # Calcula Gini
    resmat$Gini <- sapply(rendes, function(dd){
      laeken::gini(dd$COV_IE_Gini,weights = dades$RB050, na.rm = T)$value
    })

    resmat$Gini_IMV <- sapply(rendes, function(dd){
      laeken::gini(dd$COV_IE_Gini_IMV, weights = dades$RB050, na.rm = T)$value
    })

    # Calcula renda mitnaja

    d2 <- dades[dades$prim == "1", c("ID", "RB050")]

    resmat$Renda_mitjana_llars <- sapply(rendes, function(dd){
      d2$COV_hy020 <- dd$COV_hy020[match(d2$ID, dd$ID)]

      Hmisc::wtd.mean(d2$COV_hy020,
                      weights = d2$RB050,
                      na.rm = T)
    })

    resmat$Renda_mitjana_llars_IMV <- sapply(rendes, function(dd){
      d2$COV_hy020_IMV <- dd$COV_hy020_IMV[match(d2$ID, dd$ID)]

      Hmisc::wtd.mean(d2$COV_hy020_IMV,
                      weights = d2$RB050,
                      na.rm = T)
    })

    # Calcula mediana IE
    resmat$Mediana_IE <- sapply(rendes, function(dd){
      Hmisc::wtd.quantile(dd$COV_IE,
                          weights = dades$RB050,
                          probs = c(.5),
                          na.rm = T)
    })

    resmat$Mediana_IE_IMV <- sapply(rendes, function(dd){
      Hmisc::wtd.quantile(dd$COV_IE_IMV,
                          weights = dades$RB050,
                          probs = c(.5),
                          na.rm = T)
    })

    resmat$Mitjana_IE <- sapply(rendes, function(dd){
      Hmisc::wtd.mean(dd$COV_IE,
                      weights = dades$RB050,
                      na.rm = T)
    })



    # return
    return(resmat)

  }

  ## Definició: calcula indicadors clau pobresa & desigualtat
  ## @inputs: rendes (llista amb subconjunts vars rendes simulades)
  ## @outputs: resmat (matriu que agrupa conjunt resultats per simulacions)
  ## ATT:
  # revisar/re-valorar si és imprescindible tenir 2 versions
  fes_COV_calcula_indicadors_curt  <- function(rendes, dades, simrefs){
    # stop()

    # calcula variables clau per fer indicadors
    ## Calcula variable TRPs ancorades

    medianA <- Hmisc::wtd.quantile(
      x       = dades$IE, # amb dades inicials
      weights = dades$RB050,
      probs   = c(.5)
    )

    rendes <-
      lapply(rendes, function(set){
        set[,paste0("COV_TRP_ANC_IMV", c(60, 30))] <-
          lapply(X = c(.6, .3), function(perc){
            fes_trp(ie = set$COV_IE_IMV,
                    med = medianA,
                    perc = perc)
          })
        return(set)
      })


    ## Calcula variable Gini post IMV

    rendes <-
      lapply(rendes, function(set){
        set$COV_IE_Gini_IMV <-
          ifelse(set$COV_IE_IMV <= 0,
                 1,
                 set$COV_IE_IMV)
        return(set)
      })


    # crea matriu resultats
    resvars <- c("COV_TRP_ANC_IMV60", "COV_TRP_ANC_IMV30",
                 "Gini_IMV", "Renda_mitjana_llars_IMV",
                 "Mediana_IE"
    )

    resmat <- matrix(data = 0,
                     nrow = simrefs$chunk_length,
                     ncol = length(resvars),
                     dimnames = list(
                       c(),
                       c(resvars)
                     )) %>%
      as_tibble(.)

    denom <- sum(dades$RB050)

    # Calcula TRPs
    resmat[,resvars[grep("TRP", resvars)]] <-
      lapply(rendes, function(dd){
        # stop()
        dd %>%
          summarise(across(resvars[grep("TRP", resvars)],
                           ~sum(dades$RB050[.x == "Pobre"], na.rm = T))) %>%
          mutate_all(.funs = function(x){
            x / denom * 100
          })
      }) %>% do.call("rbind", .)

    # Calcula Gini
    resmat$Gini_IMV <- sapply(rendes, function(dd){
      laeken::gini(dd$COV_IE_Gini_IMV, weights = dades$RB050, na.rm = T)$value
    })

    # Calcula renda mitjana
    d2 <- dades[dades$prim == "1", c("ID", "RB050")]

    resmat$Renda_mitjana_llars_IMV <- sapply(rendes, function(dd){
      d2$COV_hy020_IMV <- dd$COV_hy020_IMV[match(d2$ID, dd$ID)]

      Hmisc::wtd.mean(d2$COV_hy020_IMV,
                      weights = d2$RB050,
                      na.rm = T)
    })

    # Calcula mediana IE
    resmat$Mediana_IE <- sapply(rendes, function(dd){
      Hmisc::wtd.quantile(dd$COV_IE,
                          weights = dades$RB050,
                          probs = c(.5),
                          na.rm = T)
    })


    # return
    return(resmat)

  }


# Extreu escenari més plausible

  ## Definició: identifica i replica escenari més similar a la mitjana
  ## @inputs: resmat, mreslist, dades, simrefs
  ## @outputs: replica (rèplica del escenari triat)
  fes_COV_most_likely_curt <- function(resmat,
                                       # mreslist,
                                       dades, simrefs,
                                       Taxa_atur_proj,
                                       Prop_cobra_atur){
    # stop()

    rl_a <- mean(resmat$COV_TRP_ANC_IMV60)
    rl_b <- mean(resmat$COV_TRP_ANC_IMV30)
    rl_c <- mean(resmat$Gini_IMV)
    rl_d <- mean(resmat$Renda_mitjana_llars_IMV)
    rl_e <- mean(resmat$Mediana_IE)

    # calcula la distància total respecte conjunt indicadors clau
    sumdifs <- resmat %>%
      mutate(
        dla = abs(COV_TRP_ANC_IMV60       - rl_a),
        dlb = abs(COV_TRP_ANC_IMV30       - rl_b),
        dlc = abs(Gini_IMV                - rl_c),
        dld = abs(Renda_mitjana_llars_IMV - rl_d),
        dle = abs(Mediana_IE              - rl_e)
      ) %>%
      select(contains("dl")) %>%
      # en fer simulació per bcn obtinc resultats raros i no diferències consistents entre u3 i u5. torno a donar major rellevància a rendes
      # mutate(across(.cols = everything(.),
      #               .fns = scales::rescale)) %>%
      rowSums(.)

    # Selecciona most likely
    idx <- which(sumdifs == min(sumdifs))

    # Replica most likely (amb funcions long) o escenari desitjat
    # if(length(resmat$seeds) > 1){
    simrefs$seed            <- resmat$seeds[idx]
    simrefs$iteracions      <- 1
    # }

    lkl_reslist <- fes_COV_reslist_seeds(
      simrefs
    )
    lkl_reslist <- fes_COV_sample_afectats(
      rsl = lkl_reslist,
      dades = dades,
      simrefs = simrefs,
      Taxa_atur_proj,
      Prop_cobra_atur
    )

    lkl_reslist$rendes <- pmap(
      lkl_reslist,
      fes_COV_calcula_rendes_llarg,
      dades = dades,
      simrefs)

    replica <- lkl_reslist$rendes[[1]]
    replica$COV_select_atur <-
      ifelse(replica$ID %in% lkl_reslist$aturats[[1]],
             T, F)
    replica$COV_select_erto <-
      ifelse(replica$ID %in% lkl_reslist$ertos[[1]],
             T, F)
    replica$COV_select_cobra <-
      ifelse(replica$ID %in% lkl_reslist$cobren[[1]],
             T, F)
    replica$COV_seed <- simrefs$seed

    checks$ch9 <- Hmisc::wtd.quantile(
      replica$COV_IE_IMV,
      weights = replica$RB050,
      probs = c(.5))   ==   resmat$Mediana_IE[idx]


    return(replica)
  }

# Compilacions de funcions

  ## Definició: agrupa l'execució de funcions simulació
  ## @inputs: dades, reslist, simrefs
  ## @outputs: rsl (llista de resultats)
  fes_COV_aplica_simulacio <- function(dades,
                                       reslist,
                                       simrefs,
                                       Taxa_atur_proj,
                                       Prop_cobra_atur){
    # stop()
    rsl        <- purrr::flatten(reslist) # cal que sigui llista, d'un sol nivell
    rsl        <- fes_COV_sample_afectats(rsl, dades, simrefs,
                                          Taxa_atur_proj,
                                          Prop_cobra_atur)

    # aplica checks
    checks$ch4 <- purrr::map2(
      rsl$aturats, rsl$ertos,
      function(.x, .y){
        any(.x %in% .y)
      }) %>%
      unlist(.) %>% sum(.) == 0
    checks$ch5 <- !sum(rsl$aturats[[1]] %in%
                         rsl$aturats[[2]]) ==
      length(rsl$aturats[[1]])

    rsl$rendes <- pmap(rsl,
                       fes_COV_calcula_rendes_curt,
                       dades = dades,
                       simrefs = simrefs)

    rsl$resmat <- fes_COV_calcula_indicadors_curt(
      rendes = rsl$rendes,
      dades = dades,
      simrefs = simrefs)
    return(rsl)
  }

  ## Definició: Pren els sub-targets de sims i agrupa'ls en un conjunt únic
  ## @inputs: ruta
  ## @outputs: matriu de resultats
  ## ATT: cal definir ordre d'execució pq no té dependència lògica de target sims
  ## 210609: he modificat funció, incloent seed, ja que la parelelització em feia un scramble amb seeds. order unreliable.Aquest canvi afecta també a la funció fes_COV_most_likely_curt
  fes_COV_res_mat <- function(ruta){
  list.files(path = ruta,
             pattern = "^sims_",
             full.names = T) %>%
    lapply(., function(x){
      xx <- readRDS(x)
      mat   <- pluck(xx, "resmat")
      mat$seeds <- pluck(xx, "seeds")
      return(mat)
    }) %>%
    do.call(args = .,
            what = "bind_rows")
}

#   fes_COV_res_mat <- function(ruta){
#   list.files(path = ruta,
#              pattern = "^sims_",
#              full.names = T) %>%
#     lapply(., function(x){
#       readRDS(x) %>%
#         pluck(., "resmat")
#     }) %>%
#     do.call(args = .,
#             what = "bind_rows")
# }
#

# Update simrefs amb paràmetres mòbils a partir de l'excel
  upd_smrf <- function(smrf, params, dd){
    # stop()
  idxs <- grep("smrf", params$cntx)
  uplst <- params$param[idxs]
  smrf <- purrr::map(.x = params[idxs, "val"],
                     .f = function(x){
                       as.numeric(x)
                     }
  ) %>%
    purrr::set_names(uplst) %>%
    append(smrf, .)

  ## Refs estàtiques
  smrf$idx_Pocupada    <- which(dd$COV_Ocupats)
  smrf$idx_Pactiva     <- which(dd$COV_Pactiva)

  return(smrf)
}


# ------------------------------------####
# FASE 4
  # Doc 1: Dashboard avaluació metadades simulació ####
  #@md = metadades (F3_resmat)
  #@ds = dades simulació (escenari most likely) (F3_escenari)
  #@dp = dades inicials (F2_dades)
  #@do = dades originals (F2_bsl_dt)
  fes_setup_dfs <- function(ds, dp, do){
    # agrupa les variables de les diferents fases
    # identifica l'àmbit d'aplicació de la simulació

    # passa les variables de la simulació a les dades inicials
    newvars_escenari <- setdiff(names(ds), "ID")
    dp[,newvars_escenari] <-
      ds[match(dp$ID, ds$ID),newvars_escenari]

    # passa qualsevol variable original no present a les dades de treball
    newvars_originals <- setdiff(names(do), names(dp))
    dp[,newvars_originals] <- do[match(dp$ID, do$ID), newvars_originals]

    # dades treball
    dt <- dp %>%
      filter(ID %in% ds$ID)

    ambit <- case_when(
      sum(unique(dt$CAMB) ==
            c("Barcelona",
              "Resta AMB",
              "Resta ATM")) == 1 ~ "BCN",
      sum(unique(dt$CAMB) ==
            c("Barcelona",
              "Resta AMB",
              "Resta ATM")) == 2 ~ "AMB",
      sum(unique(dt$CAMB) ==
            c("Barcelona",
              "Resta AMB",
              "Resta ATM")) == 3 ~ "ATM",
      length(unique(dt$DB040)) == 1 ~ "CAT",
      length(unique(dt$DB040)) > 10 ~ "ESP"
    )

    dt$ambit_simulacio <- ambit

    # crea identificadors clau:
    dt$ID_PIL <- dt$RB010 * 10^8 + dt$HB080
    dt$PIL <- ifelse(dt$ID %in% dt$ID_PIL, T, F)

    # Altres variables
    dt$COV_HCB <- (((dt$HH070 * 12) - dt$HY070N)/(dt$COV_hy020_IMV - dt$HY070N)) * 100

    # Correccions necessàries
    dt$COV_HCB[((dt$HH070 * 12) - dt$HY070N) < 0] <- 0 # Si les despeses llar menys les transferències és menor a 0 (les transferències cobreixen més que les despeses), la sobrecàrrega és 0
    dt$COV_HCB[(dt$COV_hy020_IMV - dt$HY070N) <= 0] <- 100 #Si els ingressos menys les transferències és menor que 0 (tots els ingressos són les transferències per llar), la sobrecàrrega és 100
    dt$COV_HCB[(dt$COV_hy020_IMV - dt$HY070N) < ((dt$HH070*12) - dt$HY070N)] <- 100 #si ingressos menys transferències és menor a despeses menys transferències, la sobrecàrrega és 100

    dt$COV_Taxa_HCB <- as.factor(
      cut(dt$COV_HCB,
          breaks = c(0, 40, 100),
          include.lowest = T,
          right = T,
          labels = c("Sense sobrecàrrega",
                     "Amb sobrecàrrega")))

    dt$COV_Taxa_HCB60 <- as.factor(
      cut(dt$COV_HCB,
          breaks = c(0, 60, 100),
          include.lowest = T,
          right = T,
          labels = c("Sense sobrecàrrega",
                     "Amb sobrecàrrega")))


    return(dt)

  }

  fes_replica <- function(myseed = F3_myseed,
                          dades = F4_dd_dashb,
                          Taxa_atur_proj = F1_TGatur1,
                          Prop_cobra_atur = F1_TGcob){

  replica <- dades
  rm("dades")

    # Calcula rendes individuals si tot l'erto passa per circuit atur (CF1) ####
    ### assigna aleatòriament quins ertos cobren atur i transforma a atur
    pool <- replica[replica$COV_select_erto, c("ID", "COV_prob_cobrar_atur")]
    set.seed(myseed)
    CF1_nousCobren <-  sample(x = pool$ID,
                              size = round(nrow(pool) * Prop_cobra_atur, 0),
                              replace = F,
                              prob = pool$COV_prob_cobrar_atur
    )

    replica$CF1_nousCobren <- ifelse(
      replica$COV_select_cobra | replica$ID %in% CF1_nousCobren,
      T, F
    )
    replica$CF1_selectAtur <- ifelse(
      replica$COV_select_atur | replica$COV_select_erto,
      T, F
    )
    rm("pool", "CF1_nousCobren")

    ### recalcula rendes individuals (atur i erto passa per atur)
    replica$CF1_RI <- dplyr::case_when(
      # si cobra atur
      replica$CF1_nousCobren  ~ replica$COV_RendInd_reestimat_atSI,
      # si no cobra atur
      replica$CF1_selectAtur & replica$CF1_nousCobren == F ~
        replica$COV_RendInd_reestimat_atNO,
      # si no pateix atur (ni erto)
      replica$CF1_selectAtur == F  ~ replica$COV_RendInd_original
    )
    ### Defineix component impostos individual/llar
    replica$CF1_irpf <- dplyr::case_when(
      replica$CF1_nousCobren  ~ replica$COV_correccio_irpf1,
      replica$CF1_selectAtur & !replica$CF1_nousCobren ~ replica$COV_correccio_irpf2,
      replica$CF1_selectAtur == F ~ 0
    )

    replica <- replica %>%
      group_by(IDllar) %>%
      mutate(CF1_irpf_HH = sum(CF1_irpf, na.rm = T)) %>%
      ungroup(.)

    replica$CF1_impostos_HH <- replica$COV_impostos_HH - replica$CF1_irpf_HH # segueix individual


    ### Suma rendes individuals per llars
    replica <- replica %>%
      group_by(IDllar) %>%
      mutate(CF1_RI_HH = sum(CF1_RI, na.rm = T)) %>%
      ungroup(.)

    ### Calcula rendes llar
    replica$CF1_hy020_1 <-  (replica$COV_rendes_HH_brut - replica$CF1_impostos_HH) + replica$CF1_RI_HH

    ### Assigna segons selecció
    llars_recalcul <- replica$IDllar[replica$CF1_selectAtur]

    replica$CF1_hy020 <-
      ifelse(
        replica$IDllar %in% llars_recalcul,
        replica$CF1_hy020_1,
        replica$COV_hy020_0)

    replica$CF1_IE <- replica$CF1_hy020 / replica$HX240


    # Calcula rendes individuals si no es cobra cap prestació (ni atur, ni erto, ni IMV) (CF0)

    ### recalcula rendes individuals (sense atur ni erto)
    replica$CF0_RI <- dplyr::case_when(
      # tothom que ha passat a l'atur a CF1
      replica$CF1_selectAtur  ~  replica$COV_RendInd_reestimat_atNO,
      # si no pateix atur
      replica$CF1_selectAtur == F  ~ replica$COV_RendInd_original
    )
    ### Defineix component impostos individual/llar
    replica$CF0_irpf <- dplyr::case_when(
      replica$CF1_selectAtur  ~ replica$COV_correccio_irpf2,
      replica$CF1_selectAtur == F ~ 0
    )

    replica <- replica %>%
      group_by(IDllar) %>%
      mutate(CF0_irpf_HH = sum(CF0_irpf, na.rm = T)) %>%
      ungroup(.)

    replica$CF0_impostos_HH <- replica$COV_impostos_HH - replica$CF0_irpf_HH # segueix individual


    ### Suma rendes individuals per llars
    replica <- replica %>%
      group_by(IDllar) %>%
      mutate(CF0_RI_HH = sum(CF0_RI, na.rm = T)) %>%
      ungroup(.)

    ### Calcula rendes llar
    replica$CF0_hy020_1 <-  (replica$COV_rendes_HH_brut - replica$CF0_impostos_HH) + replica$CF0_RI_HH

    ### Assigna segons selecció

    replica$CF0_hy020 <-
      ifelse(
        replica$IDllar %in% llars_recalcul,
        replica$CF0_hy020_1,
        replica$COV_hy020_0)

    replica$CF0_IE <- replica$CF0_hy020 / replica$HX240


    return(replica)
  }

  #######################################################
  #######################################################
  #######################################################
  #######################################################

