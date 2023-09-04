skapa_kortnamn_lan_alternativ <- function(lansnamn, byt_ut_riket_mot_sverige = FALSE){
  # Skript som skapar kortnamn av län. Alternativ till Peters skript (som inte fungerar i de fall där län saknas i namnen redan från början)
  nyttnamn <- NA
  for (elem in 1:length(lansnamn)){
    if (substr(lansnamn[elem], nchar(lansnamn[elem])-3, nchar(lansnamn[elem]))==" län") nyttnamn[elem] <- substr(lansnamn[elem],1, nchar(lansnamn[elem])-4) else nyttnamn[elem] <- lansnamn[elem]
    if (substr(nyttnamn[elem], nchar(nyttnamn[elem]),nchar(nyttnamn[elem]))=="s")
    if (byt_ut_riket_mot_sverige == TRUE) if (lansnamn[elem] == "Riket") nyttnamn[elem] <- "Sverige"
  }
  return(nyttnamn)
}