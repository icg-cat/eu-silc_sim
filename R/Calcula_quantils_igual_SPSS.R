# # #   # # #   # # #   # # #   # # #   # # #   # # #   # # #
# Tipus: source
# Descripció: Computa quantils ponderats, arrodonint prèviament a 2 dígits
# Data: 2019/01/07
# # #   # # #   # # #   # # #   # # #   # # #   # # #   # # #


# Arrodonint els IE a dos dígits surten els resultats iguals que a SPSS



fes_WQnt <- function(ie, pes, ps){

  factor(
    cut(
      round(ie,2),
      breaks =
        Hmisc::wtd.quantile(x = round(ie,2), weights = pes, probs = ps),
      include.lowest = T,
      right = T
    )
  )
}

# # Proves:
#
# EM16 <- readRDS("N:/RECERCA/SOCIETAT/BASES DE DADES/0201_Estadistiques_Metropolitanes/0_Dades/2017/V4/1_Versions_treball/2019-06-21_EM17_pesMET_Laeken_Vreal_V4.RDS")
#
#
# fes_WQntR <- function(ie, pes, ps){
#
#   factor(
#     cut(
#       round(ie,2),
#       breaks =
#         reldist::wtd.quantile(x = round(ie,2), q = ps, weight = pes),
#       include.lowest = T,
#       right = T
#     )
#   )
# }
#
#
# fes_WQntH <- function(ie, pes, ps){
#
#   factor(
#     cut(
#       round(ie,2),
#       breaks =
#         Hmisc::wtd.quantile(x = round(ie,2),
#                             weights = pes,
#                             probs = ps),
#       include.lowest = T,
#       right = T
#     )
#   )
# }
#
#
# xx <- round(runif(100, min = 0.01, max = 0.99), 2)
#
# table(
#   unlist(lapply(xx, function(pb){
#     all.equal(fes_WQntR(EM16$IE, EM16$RB050, pb),
#               fes_WQntH(EM16$IE, EM16$RB050, pb))
#   }))
# )

# fes_Wquantils <- function(IE, quants, pes){
#   # Arrodoneix a 2 decimals
#   IE2 <- round(IE, 2)
#   # Determina quantils
#   p  <- case_when(
#                quants == 2 ~ 0.5,
#                quants == 4 ~ 0.25,
#                quants == 5 ~ 0.2,
#                quants == 10 ~ 0.1,
#                quants == 100 ~ 0.01)
#
#   # Calcula punts de tall
#   qq <- reldist::wtd.quantile(x = IE2, q = seq(0,1,p), weight = pes)
#   # qq <- Hmisc::wtd.quantile(x = IE2, probs = seq(0,1,p))
#
#   # expandeix els extrems
#   # br <- c(min(IE2)-1,
#   #         qq[2:quants],
#   #         max(IE2)+1)
#   # # NOTA: en treballar amb la variable d'IE arrodonida a 2 dígits no cal expandir els extrems, però no fa mal, així que ho deixo.
#
#   # genera nova variable
#   cut(IE2,
#       # breaks = br,
#       breaks = qq,
#       dig.lab = 6,
#       right = TRUE)
#
# }
#
