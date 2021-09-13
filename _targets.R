# Setup ####
defaultW <- getOption("warn")
options(warn = -1)
options(tidyverse.quiet = TRUE)
options(warn = defaultW); rm("defaultW")

library(targets)
library(tarchetypes)
library(tidyverse)
library(future)
library(future.callr)

source("R/functions.R")

# Set target-specific options such as packages ####
tar_option_set(packages = "tidyverse")

# debug options ####
tar_option_set(debug = "F2_dades")

# load parameters ####
full <- "U3-ESP"
param <- openxlsx::read.xlsx("Data/sim_params.xlsx",
                             sheet = full)

checks <- new.env(parent = .GlobalEnv)

# parallelization ####
future::plan(future::multisession)

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
  # compute targets -  base Spain
    ## Individual probs table (only spain)
  tar_target(
    F1_taula_probs_grups_ESP,
    mf_taula_anual(epa = epaESP,
                   UN = param$val[param$param == "UN"]))
  ,
   ## Sector probs for ERTO 
  tar_target(
    F1_ERTO_sect_ESP,
    mf_prob_erto_sectors(epaESP))
  ,
  # Compute parameters by territory
   ## unemployment rate
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

   ## Uenmployment benefits reach
  tar_target(
    F1_TGcob,
    mf_calcula_Tcobertura(epa = epa,
                          UN  = param$val[param$param == "UN"]))
  ,
   ## unemployment length
  tar_target(
    F1_dur_atur,
    mf_prob_durada_atur(epa = epa,
                        UN  = param$val[param$param == "UN"]))
  ,
  # ERTO tables
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
   F3_resmat, # needs to be executed in blocks
   fes_COV_res_mat(ruta = "_targets/objects"))
 ,
 tar_target(
   F3_escenari,
   fes_COV_most_likely_curt(resmat   = F3_resmat,
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
)
