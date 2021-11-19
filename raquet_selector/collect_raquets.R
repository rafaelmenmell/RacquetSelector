#harvest_completo
library(rvest)
library(purrr)
library(dplyr)

get_fabricantes <- function(){
  fabricantes <- c("Adidas","Asics","Avery","Babolat","Boris Becker","Cayman","Donnay","Dunlop","Estusa","Fischer","Fox","G-Star","Gamma","Gosen","Head","Kneissl","Lacoste","Le Coq Sportif","Mantis","Mitt","One Strings","Pacific","PowerAngle","Prince","ProKennex","Rossignol","Slazenger","Snauwaert","Solinco","Spalding","Tecnifibre","Toalson","Volkl","Wavex","Weed","Wilson","Wonderwand","Yonex")
  fabricantes <- gsub(" ","+",fabricantes)
  return(fabricantes)
}

rvest_raquetas <- function(fabricante){
  print(fabricante)
  intervalos <- seq(80,135,5)
  raquetas_intervalo <- vector("list",length(intervalos))
  n <- 1
  for (intervalo in intervalos){
    url <- sprintf("http://www.racquetfinder.com/?name=&manufacturer=%s&hsMin=%s&hsMax=%s&lMin=&lMax=&wMin=&wMax=&swMin=&swMax=&fMin=&fMax=&bpMin=&bpMax=&bwMin=&bwMax=&mains=&crosses=&currentcheckbox=ASICS&current=N",fabricante,intervalo,intervalo+4)
    url_current <- sprintf("http://www.racquetfinder.com/?name=&manufacturer=%s&hsMin=%s&hsMax=%s&lMin=&lMax=&wMin=&wMax=&swMin=&swMax=&fMin=&fMax=&bpMin=&bpMax=&bwMin=&bwMax=&mains=&crosses=&currentcheckbox=ASICS&current=Y",fabricante,intervalo,intervalo+4)
    basichtml_current <- read_html(x = url_current)
    raquetas_name_current <- basichtml_current %>% html_elements("div.rac_name") %>% html_text2()
    basichtml <- read_html(x = url) 
    imagenes <- basichtml %>% html_elements("img.rac_img") %>% html_attr("src")
    raquetas_name <- basichtml %>% html_elements("div.rac_name") %>% html_text2()
    raquetas <- basichtml %>% html_elements("div.rac_info") %>% html_table()
    raquetas <- raquetas %>% lapply( function(x) x %>% tidyr::pivot_wider(names_from = "X1",values_from="X2"))
    raquetas <- raquetas %>% lapply( function(x) {colnames(x) <- gsub(":","",colnames(x));x})
    raquetas <- dplyr::bind_rows(raquetas)
    raquetas$Name <- raquetas_name
    
    #Head Size, solo cm
    raquetas$`Head Size` <- gsub(".* sq. in. / ","",raquetas$`Head Size`)
    raquetas$`Head Size` <- gsub(" sq. cm.","",raquetas$`Head Size`)
    raquetas$`Head Size` <- as.numeric(raquetas$`Head Size`)
    #Length en pulgadas
    raquetas$Length <- as.numeric(gsub(" inches / .* cm","",raquetas$Length))
    #Strung Weight en g
    raquetas$`Strung Weight` <- gsub(".* oz / ","",raquetas$`Strung Weight`)
    raquetas$`Strung Weight` <- gsub(" g","",raquetas$`Strung Weight`)
    raquetas$`Strung Weight` <- as.numeric(raquetas$`Strung Weight`)
    #Balance en puntos
    raquetas$Balance <- as.numeric(gsub("pts .*","",raquetas$Balance))
    
    raquetas$`Swing Weight` <- as.numeric(raquetas$`Swing Weight`)
    raquetas$`Beam Width` <- as.numeric(gsub("mm","",raquetas$`Beam Width`))
    raquetas$Stiffness <- as.numeric(raquetas$Stiffness)
    
    raquetas$`String Pattern` <- gsub(" Mains/","x",raquetas$`String Pattern`)
    raquetas$`String Pattern` <- gsub(" Crosses","",raquetas$`String Pattern`)
    
    colnames(raquetas) <- gsub(" ","",colnames(raquetas))
    
    raquetas$Marca <- sapply(strsplit(raquetas$Name," "), function(x) x[1])
    raquetas$imagen <- imagenes
    raquetas$PowerLevel <- factor(x = raquetas$PowerLevel,levels = c("Low","Low-Medium","Medium","Medium-High","High",""))
    
    raquetas <- raquetas %>% dplyr::filter(!grepl(pattern = "test",x = tolower(Name)))
    raquetas$current <- FALSE
    raquetas[raquetas$Name %in% raquetas_name_current,]$current <- TRUE
    raquetas_intervalo[[n]] <- raquetas
    n <- n + 1
  }
  raquetas_intervalo <- discard(raquetas_intervalo, function(z) nrow(z) == 0) %>% bind_rows()
  return(raquetas_intervalo)
}

collect_all_raquets <- function(){
  fabricantes <- get_fabricantes()
  raquetas <- fabricantes %>% map(rvest_raquetas)
  raquetas <- discard(raquetas, function(z) nrow(z) == 0) %>% bind_rows()
  raquetas[raquetas$Marca=="adidas",]$Marca <- "Adidas"
  raquetas[raquetas$Marca=="HEAD",]$Marca <- "Head"
  raquetas[raquetas$Marca=="2012/ProKennex",]$Marca <- "ProKennex"
  raquetas[raquetas$Marca=="2013/Tecnifibre",]$Marca <- "Tecnifibre"
  raquetas[raquetas$Marca=="WIlson",]$Marca <- "Wilson"
  saveRDS(raquetas,file="raquetas.rds")
  saveRDS(raquetas,file="raquet_selector/raquetas.rds")
}

get_similar_raquet <- function(dfraquetas,marca,modelo,curr,uHS=20,uL=1,uSW=6,uS=2){
  raquetas <- readRDS("raquetas.rds")
  raqueta <- raquetas %>% dplyr::filter(Marca==marca,Name==modelo)
  raqueta <- raqueta[1,]
  minHS <- raqueta$HeadSize - uHS
  maxHS <- raqueta$HeadSize + uHS
  minL <- raqueta$Length - uL
  maxL <- raqueta$Length + uL
  minSW <- raqueta$SwingWeight - uL
  maxSW <- raqueta$SwingWeight + uL
  minS <- raqueta$Stiffness - uS
  maxS <- raqueta$Stiffness + uS
  raquetas_similares <- dfraquetas %>% dplyr::filter(HeadSize>=minHS,HeadSize<=maxHS) %>% dplyr::filter(Length>=minL,Length<=maxL) %>% dplyr::filter(SwingWeight>=minSW,SwingWeight<=maxSW) %>% dplyr::filter(Stiffness>=minS,Stiffness<=maxS) %>% dplyr::filter(current==curr)
  raquetas_similares <- raquetas_similares[raquetas_similares$Name!=modelo,]
  return(raquetas_similares)
}