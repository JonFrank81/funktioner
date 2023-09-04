skapa_linjediagram_ny(df <- KPI_df %>% filter(ar>"2021"),
                      linje_typ = "dashed",
                      x_axel_namn = "")


skapa_linjediagram_ny <- function(df = data.frame(),
                                  x_variabel = "Period",
                                  y_variabel = "KPIF,.månadsförändring,.1987=100",
                                  linje_typ = "solid", # Som standard. Övriga val: blank,dashed,dotted,dotdash,longdash,twodash,
                                  farg = c("red"),
                                  vertikal_justering = 0.5,
                                  horisontell_justering = 0.5,
                                  rotera_text = 0, # I grader, 0 är standard,
                                  x_axel_namn = NA, # Sätt namn på x-axel. "" om man vill ha blankt
                                  y_axel_namn = NA  # Sätt namn på y-axel. "" om man vill ha blankt
){
  
  # Testar om det finns ett dataset
  if(is_empty(df)){
    break
    print("Dataset saknas")
  }
  
  # Sätter namn på y-axel
  if(is.na(y_axel_namn) == TRUE){
    y_axel_namn = y_variabel
  }else y_axel_namn = y_axel_namn
  
  # Sätter namn på x-axel
  if(is.na(x_axel_namn) == TRUE){
    x_axel_namn = x_variabel
  }else x_axel_namn = x_axel_namn
  
  
  
  figur <- df %>%
    ggplot(aes(get(x_variabel),get(y_variabel)))+
    geom_line(group=1,linetype=linetype,color = farg)+
    theme(axis.text.x=element_text(angle = 90, vjust = vertikal_justering,hjust = horisontell_justering))+
    labs(y = y_axel_namn, x = x_axel_namn) 
  
  return(figur)
}
