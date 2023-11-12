# Instalar y cargar los paquetes necesarios
library(FactoMineR)
library(factoextra)

visualizacion_vars <- function(base_ACP){
  
  base_ACP$anno <- as.numeric(rownames(base_ACP))
  
  # Reshape the data to long format
  base_ACP_long <- base_ACP %>%
    pivot_longer(cols = -anno, names_to = "Variable", values_to = "Valor")
  
  (ggplot(base_ACP_long, aes(x = anno, y = Valor)) +
    geom_line() +
    facet_wrap(~ Variable, scales = "free_y")  +
      geom_smooth(method = "lm", se = FALSE, lwd = 0.5, color = "red") +
    labs(x = "Año", y = "Valor") +
      theme(strip.text = element_text(vjust = 1.1, size = 14, margin = margin()), 
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20))+
    ggtitle(""))  %>% print()
  
}

imprime_resultados_pca <- function(res.pca){
 
  # Importancia de las componentes 
  # Valores propios y varianza explicada
  # Importancia de las componentes 
  cat("## Importancia de las Componentes Principales\n")
  importancia <- res.pca$eig %>% as.data.frame()
  kbl(importancia)
  
  # Visualización de los valores propios (varianza explicada)
  cat2("Gráficamente:")
  print(fviz_eig(res.pca))
 
  # Seleccion de componentes (usando un umbral valor propio > 1)
  nComp <- max(which( importancia$eigenvalue > 1))
  cat2(paste("Usando valor propio > 1, se selecciona el subespacio con", nComp, "dimensiones."))
  
  # Representacion grafica de individuos y variables 
  for(nC in 0:floor((nComp-1)/2)){
    
    # Dimensiones 
    cat2(paste('## Dimensiones: ',nC*2+1, "y",2*(nC+1)))
    
    # Grafico de variables 
    cat2('Gráfico de Variables:')
    factoextra::fviz_pca_var(res.pca,
                             axes = c(nC*2+1,2*(nC+1)), 
                             repel = TRUE,
                             select.var = list(cos2 = 0.5)) %>% print()
    
    # Grafico de individuos 
    cat2('Gráfico de Individuos:')
    factoextra::fviz_pca_ind(res.pca, 
                             repel = TRUE, 
                             axes = c(nC*2+1,2*(nC+1))) %>% print()
    
    # Biplot de individuos y variables
    cat2('Gráfico de Individuos y Variables:')
    fviz_pca_biplot(res.pca, 
                    repel = TRUE, 
                    axes = c(nC*2+1,2*(nC+1))) %>% print()
    
  }
  
  # Calidad de representacion y contribuciones por componente
  cat2('## Índices de Calidad y Contribución por Componente')
  for(nC in 1:nComp){
    
    # Componente 
    cat2(paste('## Componente Número: ',nC))
    
    cat2('### Calidad de representación')
    # Gráfico de la calidad de representación (cos2)
    # de las variables en las componentes principales
    fviz_cos2(res.pca, choice = "var", axes = nC) %>% print()
    # de los individuos en las componentes principales
    fviz_cos2(res.pca, choice = "ind", axes = nC) %>% print()
    
    cat2('### Contribución')
    # Gráfico de contribuciones
    # de las variables en las componentes principales
    fviz_contrib(res.pca, choice = "var", axes = nC) %>% print()
    # de los individuos en las componentes principales
    fviz_contrib(res.pca, choice = "ind", axes = nC) %>% print()
    
  }
  
  cat2('## Índices de Calidad y Contribución sobre todo el subespacio')

  cat2('### Calidad de representación')
  # Gráfico de la calidad de representación (cos2)
  # de las variables en las componentes principales
  fviz_cos2(res.pca, choice = "var", axes = 1:nC) %>% print()
  # de los individuos en las componentes principales
  fviz_cos2(res.pca, choice = "ind", axes = 1:nC)  %>% print()
  
  cat2('### Contribución')
  # Gráfico de contribuciones
  # de las variables en las componentes principales
  fviz_contrib(res.pca, choice = "var", axes = 1:nC) %>% print()
  # de los individuos en las componentes principales
  fviz_contrib(res.pca, choice = "ind", axes = 1:nC) %>% print()
  
}

cat2 <- function(x){
  cat("\n\n")
  cat(x)
  cat("\n\n")
}


kbl <- function (df) {
  cat("\n\n")
  
  df <- df %>%    
    mutate(across(where(is.numeric), ~ formattable::comma(.x,  big.mark = ",", format = "f", digits = 4)))
  df2<-kable(df, 
             format.args = list(big.mark = ",")) %>% 
    kable_styling(bootstrap_options = c("striped", "condensed"))
  print(df2)
  cat("\n\n")
}