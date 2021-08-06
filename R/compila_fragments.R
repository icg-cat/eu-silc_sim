agrupa_parts_sim <- function(territori, UN){
    require(tidyverse)
    require(targets)
    source("R/functions.R")
    # declara identificadors
    # territori <- "ESP"
    # UN <- "U3"
    checks <- new.env(parent = .GlobalEnv)

    carpeta <- paste0("Outputs/",
                      "resmats",
                      "_",
                      territori,
                      "_",
                      UN)
    # fes llista d'objectes zip
    (myzips <- list.files("Outputs/",
                          pattern = paste0(territori, "_", UN, ".*", "\\.zip"),
                          full.names = T))
    myzips <- myzips[!grepl("COMP", myzips)]

    # extreu nom dels arxius resmat
    resmat_names <- lapply(myzips, function(ruta){
      unzip(ruta, list = T)
    })

    resmat_name <- lapply(resmat_names, function(llista){
      llista$Name[grep("_targets/objects/F3_resmat",llista$Name)]
    }) %>%
      unlist(.) %>%
      unique(.)

    # extreu els resmat, en carpetes úniques que els identifiqui
    IDs <- lapply(myzips, function(cadena){
      stringr::str_sub(cadena, start = 9, end = -5)
    })

    purrr::map2(.x = myzips, .y = IDs, .f = function(.x, .y){
      zip::unzip(.x,
                 files = resmat_name,
                 overwrite = T,
                 junkpaths = T,
                 exdir = paste0(carpeta,
                                "/",
                                .y))
    })
    # genera missatge d'avís
    cat(paste0("S'han extret els arius anomenats '",
          resmat_name,
          "' de les següents ubicacions: "),
          myzips)

    # llegeix i agrupa les resmat. elimina duplicats
    BRM <- lapply(list.files(carpeta, pattern = "resmat", recursive = T, full.names = T),
           readRDS) %>% do.call(what = "bind_rows", args = .)

    # check for duplicates: si més d'un 5% són duplicats, atura i llança missatge d'error

    seeds <- stringr::str_sub(myzips, start = -9, end = -5) %>%
      as.numeric(as.character(.))

    all_seeds <- lapply(seeds, function(ss){
      set.seed(ss)
      sample(x = 9999999,
             size = 25000,
             replace = F)
    }) %>%
    unlist(.)

    prop <- all_seeds %>%
      data.frame(.) %>%
      mutate(dups = duplicated(.)) %>%
      count(dups) %>%
        mutate(pp = n/sum(n)*100) %>%
        filter(dups == TRUE) %>%
        pull(pp)

    if(prop > 5) {
      stop("Més d'un 5% dels casos són duplicats. Revisar les fonts")
    }

    objrt <- paste0(carpeta,
                    "/",
                    "objects")
    saveRDS(BRM, paste0(objrt,"/F3_resmat"))

    # porta la resta d'objectes d'una de les simulacions
    objnames <- unzip(myzips[1], list = T)
    myobjs <- objnames$Name[grep("F3_mreslist|F2_dades|F3_simrefs|F1_TGatur|F1_TGcob|F2_bsl_dt|parametres|F1_ERTO_sect_ESP",
                                 objnames$Name)]
    purrr::map(.x = myobjs, .f = function(.x){
                zip::unzip(myzips[1],
                 files = .x,
                 overwrite = T,
                 junkpaths = T,
                 exdir = objrt)
    })


    F3_mreslist <- readRDS(paste0(objrt, "/F3_mreslist"))
    F3_mreslist$seeds <- all_seeds
    F2_dades <- readRDS(paste0(objrt,"/F2_dades"))
    F3_simrefs <- readRDS(paste0(objrt,"/F3_simrefs"))
    F1_TGatur <- readRDS(paste0(objrt,"/F1_TGatur"))
    F1_TGcob <- readRDS(paste0(objrt,"/F1_TGcob"))
    F2_bsl_dt <- readRDS(paste0(objrt,"/F2_bsl_dt"))
    # parametres <- readRDS(paste0(objrt,"/parametres"))



    F3_escenari <- fes_COV_most_likely_curt(resmat   = BRM,
                             mreslist = F3_mreslist,
                             dades    = F2_dades,
                             simrefs  = F3_simrefs,
                             Taxa_atur_proj  = F1_TGatur,
                             Prop_cobra_atur = F1_TGcob)
    saveRDS(F3_escenari, paste0(objrt,"/F3_escenari"))


    tot_be <- sum(unlist(as.list(checks))) == length(as.list(checks))

    titol <-
    paste(
      format(Sys.Date(), "%y%m%d"),
      territori,
      UN,
      nrow(BRM),
      sep = "_"
    )

    F4_dd_dashb <- fes_setup_dfs(F3_escenari,
                                 F2_dades,
                                 F2_bsl_dt)

    saveRDS(F4_dd_dashb, paste0(objrt,"/F4_dd_dashb"))

    # Make RMDs
    alldocs <- list.files("Docs", pattern = "rmd", ignore.case = T)
    mydoc <- alldocs[grep(paste0(territori, "_segments"), alldocs)]
    rmarkdown::render(paste0("Docs/", mydoc),
                      params = list(carpeta = carpeta,
                                    iter = nrow(BRM)),
                      output_file = paste0(here::here(),
                                           "/", carpeta,
                                           "/",
                                           mydoc,
                                           ".html"))
    rmarkdown::render("Docs/10_Dash_avalua_metadades_segments.Rmd",
                      params = list(carpeta = carpeta),
                      output_file = paste0(here::here(),
                                           "/", carpeta,
                                           "/", "10_Dash_avalua_metadades_segments.html"))



    zip::zip(
      zipfile = paste0(
        here::here("Outputs"),
        "/",
        titol, "COMP", "_",
        tot_be,
        ".zip"),
      files = c(
        list.files( #1
          path = carpeta,
          full.names = T, recursive = T)))



}



