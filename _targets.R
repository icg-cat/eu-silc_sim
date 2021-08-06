# Canvis: 210602-0900: L196 - tar_render: he canviat la referència a document unitari per explotació resultats
defaultW <- getOption("warn")
options(warn = -1)
options(tidyverse.quiet = TRUE)
options(warn = defaultW); rm("defaultW")
# options(dplyr.summarise.inform = FALSE)

library(targets)
library(tarchetypes)
library(tidyverse)
library(future)
library(future.callr)
# library(clustermq)
# options(clustermq.scheduler = "multicore")

source("R/functions.R")

# Set target-specific options such as packages ####
tar_option_set(packages = "tidyverse")

# debug options ####
tar_option_set(debug = "F2_dades")


# carrega paràmetres ####
full <- "U3-BCN"
param <- openxlsx::read.xlsx("Data/sim_params.xlsx",
                             sheet = full)

checks <- new.env(parent = .GlobalEnv)

# parallelization
future::plan(future::multisession)
# options(clustermq.scheduler = "multicore")

# target objects ####
list(
## Fase 1 ####
  # carrega dades
  tar_target(
    epaESP,
    mf_filtra_epa(
      epafile = param$val[param$param == "epafile"]))
  ,
  tar_target(
    epa,
    mf_filtra_epa(
      epafile = param$val[param$param == "epafile"],
      provs   = eval(parse(text = param$val[param$param == "provs"]))))
  ,
  tar_target(
    parametres,
    get("param")
  )
  ,
  # calcula targets amb base Espanya
    ## Taula probs individuals (només espanya)
  tar_target(
    F1_taula_probs_grups_ESP,
    mf_taula_anual(epa = epaESP,
                   UN = param$val[param$param == "UN"]))
  ,
   ## Taula probs ERTO per sectors (només espanya)
  tar_target(
    F1_ERTO_sect_ESP,
    mf_prob_erto_sectors(epaESP))
  ,
  # Calcula paràmetres per territori
   ## Taxa d'atur
  tar_target(
    F1_TGatur0,
    mf_calcula_Tatur(epa = epa,
                     UN  = param$val[param$param == "UN"]))
  ,
  tar_target(
    F1_TGatur1,
    tar_read(F1_TGatur0) +
      as.numeric(param$val[param$param == "Taxa_erto_proj"])
    )
  ,

   ## Taxes de cobertura prestacions atur
  tar_target(
    F1_TGcob,
    mf_calcula_Tcobertura(epa = epa,
                          UN  = param$val[param$param == "UN"]))
  ,
   ## Durada estimada atur
  tar_target(
    F1_dur_atur,
    mf_prob_durada_atur(epa = epa,
                        UN  = param$val[param$param == "UN"]))
  ,
  # Carrega i filtra taula durada ERTO
  tar_target(
    F1_dur_erto,
    read.csv2("Data/F1/210303_probs_durada_ERTOs.csv") %>%
      filter(Territori == param$val[param$param == "ambit2"])
  )
  ,

## Fase 2 ####
  tar_target(
    F2_bsl_dt,
    readRDS(
      param$val[param$param == "bsl_data"]) %>%
      filter(
        !!sym(param$val[param$param == "ambit_var"]) %in%
          eval(parse(text = param$val[param$param == "ambit"])))
  )
  ,
  tar_target(
    F2_myrefs,
    COV_defineix_refs_sim(F1_taula_probs_grups_ESP,
                          F1_ERTO_sect_ESP,
                          F1_dur_atur)
  )
  ,
  tar_target(
    F2_dades,
    COV_fes_target_ecv(
      dades   = F2_bsl_dt,
      Myrefs  = F2_myrefs,
      fct_inf = param$val[param$param == "fct_inf"])
  )
  ,

## Fase 3 ####
  tar_target(
    F3_simrefs,
    upd_smrf(F2_myrefs, param, F2_dades)
  )
  ,
 tar_target(
   F3_mreslist,
   fes_COV_reslist_seeds(F3_simrefs))
 ,
 tar_target(
   F3_sublists,
   fes_COV_split_seeds(mreslist = F3_mreslist,
                       simrefs = F3_simrefs)
   )
  ,
  tar_target(
   sims,
   fes_COV_aplica_simulacio(
     dades   = F2_dades,
     reslist = F3_sublists,
     simrefs = F3_simrefs,
     Taxa_atur_proj  = F1_TGatur0,
     Prop_cobra_atur = F1_TGcob),
   pattern = map(F3_sublists),
   memory = "transient",
   garbage_collection = T,
   storage = "worker"
   )
  ,
 tar_target(
   F3_resmat, # cal executar en blocs, no té dep directa
   fes_COV_res_mat(ruta = "_targets/objects"))
 ,
 tar_target(
   F3_escenari,
   fes_COV_most_likely_curt(resmat   = F3_resmat,
                            # mreslist = F3_mreslist,
                            dades    = F2_dades,
                            simrefs  = F3_simrefs,
                            Taxa_atur_proj  = F1_TGatur0,
                            Prop_cobra_atur = F1_TGcob
                            ))
  ,
 tar_target(
   F3_myseed,
   tar_read(F3_escenari) %>%
     pull(COV_seed) %>% unique(.)
 )
,
 tar_target(
   tot_be,
   sum(unlist(as.list(checks))) == length(as.list(checks))
 )
 ,
 tar_target(
   titol,
   paste(
     format(Sys.Date(), "%y%m%d"),
     full,
     "_",
     param$val[param$param == "iteracions"],
     sep = "_"
   )
 )
 ,
## Fase 4 ####
  tar_target(
    F4_dd_dashb,
    fes_setup_dfs(F3_escenari,
                  F2_dades,
                  F2_bsl_dt)
  )
,
tar_target(
  F5_contrafactic,
  fes_replica(myseed = F3_myseed,
              dades = F4_dd_dashb,
              Taxa_atur_proj = F1_TGatur1,
              Prop_cobra_atur = F1_TGcob
  )
)
,
  tar_render(
    dashboard1,
    "Docs/10_Dash_avalua_metadades.Rmd"
 )
,
  tar_render(
    pla_explotacio,
    paste0("Docs/",
           "11_Pla_explotacio_simulacions_ALL.Rmd"),
    params = list(territori = param$val[param$param == "ambit3"])
 )
,
  tar_render(
    tules_contrafactic,
    "Docs/12_Analisi_cobertura_contrafactic.RMD",
    params = list(territori = param$val[param$param == "ambit3"])
 )
,
tar_target(
  tupper,
  zip::zip(
    zipfile = paste0(
      here::here("Outputs"),
      "/",
      titol, "_",
      tot_be,
      ".zip"),
    files = c(
      list.files( #1
       path = "_targets/objects",
       full.names = T),
      list.files( #2
       path = "R",
       pattern = "^functions\\.R$",
       full.names = T
      ),
      list.files( #3
        # només els actualitzats darrers 10 min.
        path = "Docs",
        pattern = "html",
        full.names = T)[file.mtime(list.files(
          path = "Docs/",
          pattern = "html",
          full.names = T)) > (Sys.time() - 3000)]
      )
    )
 )

)



# library(targets, quietly = T, warn.conflicts = F, verbose = F)
# options(tidyverse.quiet = TRUE)
# library(tidyverse, quietly = T, warn.conflicts = F, verbose = F)
# options(dplyr.summarise.inform = FALSE)
# library(dplyr, warn.conflicts = FALSE)
# # options(clustermq.scheduler = "multicore")
# # library(clustermq)
# library(future, quietly = T, warn.conflicts = F, verbose = F)
# plan(cluster)
# library(tarchetypes, quietly = T, warn.conflicts = F, verbose = F)
# source("R/functions.R")
