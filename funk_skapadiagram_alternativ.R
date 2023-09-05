
skapa_linjediagram_ny(df <- KPI_df %>% filter(ar>"2021"),
                      linje_typ = "solid",
                      rotera_text =45,
                      x_axel_namn = "",
                      valt_tema = "minimal")


skapa_linjediagram_ny <- function(df = data.frame(),
                                  x_variabel = "Period",
                                  y_variabel = "KPIF,.månadsförändring,.1987=100",
                                  linje_typ = "solid", # Som standard. Övriga val: blank,dashed,dotted,dotdash,longdash,twodash,
                                  farg = c("red"),
                                  vertikal_justering = 0.5,
                                  horisontell_justering = 0.5,
                                  rotera_text = 90, # I grader, 0 är standard,
                                  x_axel_namn = NA, # Sätt namn på x-axel. "" om man vill ha blankt
                                  y_axel_namn = NA,  # Sätt namn på y-axel. "" om man vill ha blankt
                                  valt_tema = "classic" # Andra val, bw,gray, dark, light, iinedraw, minimal, void
){
  
  # Testar om det finns ett dataset
  if(is_empty(df)){
    break
    print("Dataset saknas")
  }
  
  # Sätter ett tema baserat på användarens val
  # Standardval
  tema <- theme_set(theme_classic())
  
  if(valt_tema == "bw"){
    tema <- theme_set(theme_bw())
  }
  if(valt_tema == "gray"){
    tema <- theme_set(theme_gray())
  }
  if(valt_tema == "dark"){
    tema <- theme_set(theme_dark())
  }
  if(valt_tema == "light"){
    tema <- theme_set(theme_light())
  }
  if(valt_tema == "linedraw"){
    tema <- theme_set(theme_linedraw())
  }
  if(valt_tema == "minimal"){
    tema <- theme_set(theme_minimal())
  }
  if(valt_tema == "void"){
    tema <- theme_set(theme_void())
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
    geom_line(group=1,linetype=linje_typ,color = farg)+
    theme_set(tema)+
    theme(axis.text.x=element_text(angle = rotera_text, vjust = vertikal_justering,hjust = horisontell_justering))+
    labs(y = y_axel_namn, x = x_axel_namn)
  
  return(figur)
}

