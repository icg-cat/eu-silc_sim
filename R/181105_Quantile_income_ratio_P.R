# # #   # # #   # # #   # # #   # # #   # # #   # # #   # # # 
# Descripció: Computa qualsevol combinació de ràtios de renda tipus P
# # #   # # #   # # #   # # #   # # #   # # #   # # #   # # # 


cal_QuantRat <- function(Qnume, Qdenom, nom_dades, IV = "IE", PES = "RB050"){
  
  dades <- if(is.character(nom_dades)){
    get(nom_dades)
  } else {
    nom_dades
  }
  
   qants <- Hmisc::wtd.quantile(x      = dades[[IV]], 
                               weights = dades[[PES]], 
                               probs   = c(Qnume,Qdenom)
                               )

  
  x <- unname(qants[1]/qants[2])
  names(x) <- paste0("p", Qnume * 100, "/p", Qdenom * 100)
  return(x)
  
}

