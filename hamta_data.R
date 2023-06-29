

hamta_data_FK <- function(webbadresser = "https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/tfp-barn/TFPVabBarnAlder.xlsx)",
                          dataset_namn = c("Vab_antal_barn")){
  
  # Funktion som tar hem data från Försäkringskassans öppna data. Kräver dels webbadresser, dels de namn man vill att datasetten skall ha 
  # Webbadresser finns här: https://www.dataportal.se, välj organisation Försäkringskassan.
  # När du har hittat rätt data, klicka på länk och högerklicka sedan på ladda ned data och välj kopiera länkadress.
  # För Antal barn vars föräldrar har vabbat ser länken ut som följer: https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/tfp-barn/TFPVabBarnAlder.xlsx
  
  # Funktionen returnerar en lista med dataset som sedan med fördel kan sparas till Excel. Notera att antalet webbadresser och antalet namn på dataset måste var a lika många, annars avslutas funktion
                            
  if(length(webbadresser) != length(dataset_namn)){
    print("Felaktigt val av namn! Måste vara lika många som webbadresser")
    break
  }
  
  # Adresser till data
  path = webbadresser
  
  flik_lista = lst()
  i=1
  
  # Uttag av data. Eftersom det är en väldigt stor datamängd delas den upp i två flikar (av Försäkringskassan), varför lapply används,
  while(i <= length(path)){
    
    td = tempdir()              # skapa temporär mapp
    varsel_fil <- tempfile(tmpdir=td, fileext = ".xlsx")
    download.file(path[[i]], destfile = varsel_fil, mode = "wb")       # ladda hem hela filen, mode = "wb" viktigt, annars blir det fel
    
    lista = lapply(getSheetNames(varsel_fil), function(x) import(file=path[[i]],which = x) %>% 
                     filter(!row_number() %in% c(0, 1)) %>% 
                     row_to_names(1) %>% 
                     filter(substr(Län,1,2) == region_vekt))
    
    # Binder ihop data från de olika flikarna i Excelfilen
    j=1
    df=c()
    while(j<=length(lista)){
      df <- rbind(df,lista[[j]])
      j=j+1
    }
    
    flik_lista[[i]] <- df
    
    print(paste0("Dataset ",i, " klart"))
    i=i+1
  }
  
  names(flik_lista) <- dataset_namn
  
  return(flik_lista)
  
}
                          
                          