#################################################
# Genera explotacions 11 i 12 des del directori #
#################################################

# 1. Document 11-Pla explotació ----

##Extreu una llista de totes les carpetes. Després filtra x criteri
carpetes <- list.dirs(here::here("Outputs"),
                      full.names = T,
                      recursive = F)
(carpetes <- carpetes[grep("CAT", carpetes)]) #BCN|AMB

## Verifica que les carpetes tinguin els arxius necessaris
"F4_dd_dashb" %in% list.files(paste0(carpetes[[2]], "/_targets/objects/"))

lapply(carpetes, function(carpeta){
  bcn <- grepl("BCN", carpeta)
  amb <- grepl("AMB", carpeta)
  cat <- grepl("CAT", carpeta)
  esp <- grepl("ESP", carpeta)
  terri <- c("BCN", "AMB", "CAT", "ESP")[which(c(bcn, amb, cat, esp) == TRUE)]
  U <- ifelse(grepl("_U3_", carpeta), "U3", "U5")
  rmarkdown::render(
    input = here::here("Docs", "11_Pla_explotacio_simulacions_ALL_inSitu.Rmd"),
    params = list(
      territori = terri,
      wd = carpeta
    ),
    output_dir = carpeta,
    output_file = paste0("11_Pla_explotacio_simulacions_",
                         terri,
                         U,
                         ".html")
  )
})



# 2. Document 12-Anàlisi contrafàctics ----

##Extreu una llista de totes les carpetes. Després filtra x criteri
carpetes <- list.dirs(here::here("Outputs"),
                      full.names = T,
                      recursive = F)
(carpetes <- carpetes[grep("U5-ESP", carpetes)]) #BCN|AMB

## Verifica que les carpetes tinguin els arxius necessaris
"F5_contrafactic" %in% list.files(paste0(carpetes[[1]], "/_targets/objects/"))

lapply(carpetes, function(carpeta){
  bcn <- grepl("BCN", carpeta)
  amb <- grepl("AMB", carpeta)
  cat <- grepl("CAT", carpeta)
  esp <- grepl("ESP", carpeta)
  terri <- c("BCN", "AMB", "CAT", "ESP")[which(c(bcn, amb, cat, esp) == TRUE)]
  U <- "U5"
  rmarkdown::render(
    input = here::here("Docs/12_Analisi_cobertura_contrafactic_inSitu.Rmd"),
    params = list(
      territori = terri,
      wd = carpeta
    ),
    output_dir = carpeta,
    output_file = paste0("12_Analisi_cobertura_contrafactic_",
                         terri,
                         U,
                         ".html")
  )
})

