# Function to make tables ----
hacer_tabla_1 <- function(x,y,z = NA){
  comentario <- list()
  comentario$pos <- list()
  comentario$pos[[1]] <- c(nrow(x$est.att)+1)
  comentario$command <- c(paste0("\\hline \n",  # we`ll replace all default hlines with this and the ones below
                                 "\\multicolumn{7}{c}{*** p$<$0.01, ** p$<$0.05, * p$<$0.1} \\\\ \n",
                                 paste0("\\multicolumn{7}{c}{Total municipalities = ", x$N, ", Always-control municipalities = ",x$Nco,"}\\\\")))
  
  
  tab <- x$est.avg %>%
    as_tibble(rownames = NA) %>%
    rownames_to_column("Year") %>%
    mutate(Year = "Cummulative") %>%
    rename(ATT = Estimate) %>%
    bind_cols(tibble("n.Treated" = x$Ntr)) %>%
    bind_rows(x$est.att %>% as_tibble(rownames = NA) %>% rownames_to_column("Year")) %>%
    mutate(ATT = round(ATT,2)) %>%
    mutate(ATT = case_when(p.value < 0.1 &  p.value >= 0.05 ~ paste0(ATT,"$^{*}$"),
                           p.value < 0.05 & p.value >= 0.01 ~ paste0(ATT,"$^{**}$"),
                           p.value < 0.01 ~ paste0(ATT,"$^{***}$"),
                           TRUE ~ as.character(ATT)))
  names(tab) <- c("Year",
                  "ATT", 
                  "SE",
                  "CI (lower)",
                  "CI (upper)",
                  "p-value",
                  "n (treated)")
  
  if(is.na(z)) name <- paste0("plots/att_",y,".tex") else name <- z
    
  xtable(tab,
         type = "latex",
           caption = paste0("Estimated ATT by year relative to implementation"),
           label = paste0("tab:att_", y),
         digits = c(0,0,2,2,2,2,2,0)) %>%
    print(file = name,
          caption.placement = "top",
          sanitize.text.function = identity,
          include.rownames = FALSE,
          add.to.row = comentario)
}

hacer_tabla_2 <- function(x,i,y,z = NA){
  delitos<- c("total","homicidio","secuestro","robo","violacion")
  comentario <- list()
  comentario$pos <- list()
  comentario$pos[[1]] <- c(nrow(x[[i]]$est.att)+1)
  comentario$command <- c(paste0("\\hline \n",  # we`ll replace all default hlines with this and the ones below
                                 "\\multicolumn{7}{c}{*** p$<$0.01, ** p$<$0.05, * p$<$0.1} \\\\ \n",
                                 paste0("\\multicolumn{7}{c}{Total municipalities = ", x[[i]]$N, ", Always control-municipalities = ",x[[i]]$Nco,"}\\\\")))
  
  
  tab <- x[[i]]$est.avg %>%
    as_tibble(rownames = NA) %>%
    rownames_to_column("Year") %>%
    mutate(Year = "Cummulative") %>%
    rename(ATT = Estimate) %>%
    bind_cols(tibble("n.Treated" = x[[i]]$Ntr)) %>%
    bind_rows(x[[i]]$est.att %>% as_tibble(rownames = NA) %>% rownames_to_column("Year")) %>%
    mutate(ATT = round(ATT,2)) %>%
    mutate(ATT = case_when(p.value < 0.1 &  p.value >= 0.05 ~ paste0(ATT,"$^{*}$"),
                           p.value < 0.05 & p.value >= 0.01 ~ paste0(ATT,"$^{**}$"),
                           p.value < 0.01 ~ paste0(ATT,"$^{***}$"),
                           TRUE ~ as.character(ATT)))
  
  names(tab) <- c("Year",
                  "ATT", 
                  "SE",
                  "CI (lower)",
                  "CI (upper)",
                  "p-value",
                  "n (treated)")
  if(is.na(z)) name <- paste0("plots/att_",y,".tex") else name <- z
  
  xtable(tab,
         type = "latex",
         caption = paste0("Estimated ATT by year relative to implementation"),
         label = paste0("tab:att_", y ,"_",delitos[i]),
         digits = c(0,0,2,2,2,2,2,0)) %>%
    print(file = name,
          caption.placement = "top",
          sanitize.text.function = identity,
          include.rownames = FALSE,
          add.to.row = comentario)
}

tabla_gsynth <- function(x,
                         dynamic = FALSE,
                         pre.periods = FALSE,
                         header = NULL,
                         r = 2,
                         col.names = NULL){
  require(tidyverse)
  require(gsynth)
  require(xtable)
  
  #Probamos que los objetos sean outputs de gsynth
  if(!is.logical(dynamic)) stop("dynamic debe ser de tipo lógico")
  if(!is.logical(pre.periods)) stop("pre.periods debe ser de tipo lógico")
  #if(!is.character(header) | !is.null(header)) stop("header debe ser de tipo character")
  
  if(class(x) == "list" & if(class(x) == "list") !all(map_chr(x, class) == "fect" | map_chr(x, class) == "gsynth") else F) stop("No todos los elementos de la lista son outputs de gsynth")
  if(class(x) == "list" & is.null(names(x))) stop("La lista de objetos no tiene atributo nombres definido")
  if(!class(x) == "list" & !(class(x) == "fect" | class(x) == "gsynth")) stop("El objeto no es un output de gsynth")
  if(!class(x) == "list" & (class(x) == "fect" | class(x) == "gsynth") & is.null(header)) stop("Falta especificar header")
  
  if(!dynamic){
    #tabla en caso que sea lista
    if(class(x) == "list" & if(class(x) == "list") all(map_chr(x, class) == "fect" | map_chr(x, class) == "gsynth") else F){
      description <- c("Cumulative ATT", "SE", "95\\% CI", "P-value","Municipality fixed-effect", "Year fixed-effect", "No. of factors", "Years", "Treatment municipalities", "Always-control municipalities", "Total municipalities")
      
      for(i in 1:length(x)){
        columna <- c(x[[i]][["est.avg"]][1,1] %>% round(2),
                     x[[i]][["est.avg"]][1,2] %>% round(2),
                     paste0(x[[i]][["est.avg"]][1,3] %>% round(r),", ", x[[i]][["est.avg"]][1,4] %>% round(r)),
                     case_when(x[[i]][["est.avg"]][1,5] >= 0.1 ~ x[[i]][["est.avg"]][1,5] %>% round(3) %>% as.character(),
                               x[[i]][["est.avg"]][1,5] < 0.1 & x[[i]][["est.avg"]][1,5] >= 0.05 ~ x[[i]][["est.avg"]][1,5] %>% round(3) %>% paste0("$^{*}$"),
                               x[[i]][["est.avg"]][1,5] < 0.05 & x[[i]][["est.avg"]][1,5] >= 0.01 ~ x[[i]][["est.avg"]][1,5] %>% round(3) %>% paste0("$^{**}$"),
                               x[[i]][["est.avg"]][1,5] < 0.01 ~ x[[i]][["est.avg"]][1,5] %>% round(3) %>% paste0("$^{***}$")),
                     if(x[[i]][["call"]][["force"]] == "two-way" | x[[i]][["call"]][["force"]] == "unit") "Yes" else "No",
                     if(x[[i]][["call"]][["force"]] == "two-way" | x[[i]][["call"]][["force"]] == "time") "Yes" else "No",
                     if(is.null(x[[i]][["r.cv"]])) 0 else x[[i]][["r.cv"]],
                     nrow(x[[i]][["Y.dat"]]),
                     if(!is.null(x[[i]][["D.dat"]])) sum(x[[i]][["D.dat"]][16,]) else x[[i]][["Ntr"]],
                     if(!is.null(x[[i]][["D.dat"]])) ncol(x[[i]][["D.dat"]]) - sum(x[[i]][["D.dat"]][16,]) else x[[i]][["Nco"]],
                     if(!is.null(x[[i]][["D.dat"]])) ncol(x[[i]][["D.dat"]]) else x[[i]][["N"]])
        
        names(columna) <- NULL
        if(i == 1) tabla <- tibble(description, columna) else tabla <- bind_cols(tabla, columna)
      }
      
      if(is.null(col.names)) names(tabla) <-  c(" ",names(x)) else names(tabla) <- c(" ", col.names)
      return(tabla)
    }
    #tabla en caso que sea un solo objeto
    if(class(x) == "fect" | class(x) == "gsynth"){
      
      description <- c("Cumulative ATT", "SE", "95\\% CI", "P-value","Municipality fixed-effect", "Year fixed-effect", "No. of factors", "Years", "Treatment municipalities", "Always-control municipalities", "Total municipalities")
      columna <- c(x[["est.avg"]][1,1] %>% round(2),
                   x[["est.avg"]][1,2] %>% round(2),
                   paste0(x[["est.avg"]][1,3] %>% round(2),", ", x[["est.avg"]][1,4] %>% round(2)),
                   case_when(x[["est.avg"]][1,5] >= 0.1 ~ x[["est.avg"]][1,5] %>% round(3) %>% as.character(),
                             x[["est.avg"]][1,5] < 0.1 & x[["est.avg"]][1,5] >= 0.05 ~ x[["est.avg"]][1,5] %>% round(3) %>% paste0("$^{*}$"),
                             x[["est.avg"]][1,5] < 0.05 & x[["est.avg"]][1,5] >= 0.01 ~ x[["est.avg"]][1,5] %>% round(3) %>% paste0("$^{**}$"),
                             x[["est.avg"]][1,5] < 0.01 ~ x[["est.avg"]][1,5] %>% round(3) %>% paste0("$^{***}$")),
                   if(x[["call"]][["force"]] == "two-way" | x[["call"]][["force"]] == "unit") "Yes" else "No",
                   if(x[["call"]][["force"]] == "two-way" | x[["call"]][["force"]] == "time") "Yes" else "No",
                   if(is.null(x[["r.cv"]])) 0 else x[["r.cv"]],
                   nrow(x[["Y.dat"]]),
                   if(!is.null(x[["D.dat"]])) sum(x[["D.dat"]][16,]) else x[["Ntr"]],
                   if(!is.null(x[["D.dat"]])) ncol(x[["D.dat"]]) - sum(x[["D.dat"]][16,]) else x[["Nco"]],
                   if(!is.null(x[["D.dat"]])) ncol(x[["D.dat"]]) else x[["N"]])
      
      names(columna) <- NULL
      tabla <- tibble(description, columna)
      
      names(tabla) <- c(" ", header)
      return(tabla %>% xtable)
      
    }
  }
  
  if(dynamic){
    if(pre.periods){
      #tabla en caso que sea un solo objeto
      if(class(x) == "fect" | class(x) == "gsynth"){
        x[["est.att"]] %>%
          as_tibble(rownames = NA) %>%
          rownames_to_column("Year") %>%
          mutate(ATT = round(ATT,2)) %>%
          mutate(ATT = case_when(p.value < 0.1 &  p.value >= 0.05 ~ paste0(ATT,"$^{*}$"),
                                 p.value < 0.05 & p.value >= 0.01 ~ paste0(ATT,"$^{**}$"),
                                 p.value < 0.01 ~ paste0(ATT,"$^{***}$"),
                                 TRUE ~ as.character(ATT))) %>%
          select(Year, ATT, S.E., n.Treated) %>%
          xtable(type = "latex",
                 caption = paste0("Estimated ATT by year relative to implementation"),
                 label = paste0("tab:att_", header))
      } else{
        
        for(i in seq_along(x)){
          columna<- x[[i]][["est.att"]] %>%
            as_tibble(rownames = NA) %>%
            rename(n.Treated = count) %>%
            rownames_to_column("Year") %>%
            mutate(ATT = round(ATT,2)) %>%
            mutate(ATT = case_when(p.value < 0.1 &  p.value >= 0.05 ~ paste0(ATT,"$^{*}$"),
                                   p.value < 0.05 & p.value >= 0.01 ~ paste0(ATT,"$^{**}$"),
                                   p.value < 0.01 ~ paste0(ATT,"$^{***}$"),
                                   TRUE ~ as.character(ATT))) %>%
            select(Year, ATT, n.Treated)
          if(i == 1) tabla <- columna else tabla <- bind_cols(tabla,columna %>% select(-Year))
        }
        
        tabla %>%
          xtable(type = "latex",
                 caption = paste0("Estimated ATT by year relative to implementation"),
                 label = paste0("tab:att_", header))
          
      }
        
      
    } else{
      if(pre.periods){
        #tabla en caso que sea un solo objeto
        if(class(x) == "fect" | class(x) == "gsynth"){
          tabla <- x[["est.att"]] %>%
            as_tibble(rownames = NA) %>%
            rownames_to_column("Year") %>%
            mutate(ATT = round(ATT,2)) %>%
            filter(Year>0) %>%
            mutate(ATT = case_when(p.value < 0.1 &  p.value >= 0.05 ~ paste0(ATT,"$^{*}$"),
                                   p.value < 0.05 & p.value >= 0.01 ~ paste0(ATT,"$^{**}$"),
                                   p.value < 0.01 ~ paste0(ATT,"$^{***}$"),
                                   TRUE ~ as.character(ATT))) %>%
            select(Year, ATT, S.E., n.Treated)
          
          return(tabla)
          
          
        } else{
          
          for(i in seq_along(x)){
            columna<- x[[i]][["est.att"]] %>%
              as_tibble(rownames = NA) %>%
              rename(n.Treated = count) %>%
              rownames_to_column("Year") %>%
              mutate(ATT = round(ATT,2)) %>%
              filter(Year>0) %>%
              mutate(ATT = case_when(p.value < 0.1 &  p.value >= 0.05 ~ paste0(ATT,"$^{*}$"),
                                     p.value < 0.05 & p.value >= 0.01 ~ paste0(ATT,"$^{**}$"),
                                     p.value < 0.01 ~ paste0(ATT,"$^{***}$"),
                                     TRUE ~ as.character(ATT))) %>%
              select(Year, ATT, n.Treated)
            
            if(i == 1) tabla <- columna else tabla <- bind_cols(tabla,columna %>% select(-Year))
          }
          
        }
        
        
      }
    }
    }
    
  }
  