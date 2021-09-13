# # #   # # #   # # #   # # #   # # #   # # #   # # #   # # #
# Descripció: Computa quantils ponderats, arrodonint prèviament a 2 dígits
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

