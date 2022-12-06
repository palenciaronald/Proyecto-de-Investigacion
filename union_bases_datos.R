load("pro_inv1.RData")

library(tidyverse)
library(magrittr)
library(janitor)


## para url1
names(url1) <- as.matrix(url1[9, ])
url1 = url1[10:nrow(url1), ]


## para url2

names(url2) <- as.matrix(url2[9, ])
url2 = url2[10:nrow(url2), ]


## para url3

names(url3) <- as.matrix(url3[9, ])
url3 = url3[10:nrow(url3), ]

## para url4

names(url4) <- as.matrix(url4[9, ])
url4 = url4[10:nrow(url4), ]


## para url5

names(url5) <- as.matrix(url5[8, ])
url5 = url5[9:nrow(url5), ]


## para url6

names(url6) <- as.matrix(url6[8, ])
url6 = url6[9:nrow(url6), ]



## para url7

names(url7) <- as.matrix(url7[8, ])
url7 = url7[9:nrow(url7), ]



## para url8

names(url8) <- as.matrix(url8[8, ])
url8 = url8[9:nrow(url8), ]


## para url9

names(url9) <- as.matrix(url9[8, ])
url9 <- url9[9:nrow(url9), ]



## para url10

names(url10) <- as.matrix(url10[8, ])
url10 <- url10[9:nrow(url10), ]


## para url11

names(url11) <- as.matrix(url11[6, ])
url11 <- url11[7:nrow(url11), ]


## para url12

names(url12) <- as.matrix(url12[7, ])
url12 <- url12[8:nrow(url12), ]



## para url13

names(url13) <- as.matrix(url13[7, ])
url13 <- url13[8:nrow(url13), ]


## para url14

names(url14) <- as.matrix(url14[8, ])
url14 <- url14[9:nrow(url14), ]


## para url15

names(url15) <- as.matrix(url15[7, ])
url15 <- url15[8:nrow(url15), ]


## para url16

names(url16) <- as.matrix(url16[7, ])
url16 <- url16[8:nrow(url16), ]




## para url17

# names(url17) <- as.matrix(url17[7, ])
# url17 <- url17[8:nrow(url17), ]
# 


## para url18

#names(url18) <- as.matrix(url16[7, ])
#url16 <- url16[8:nrow(url16), ]



## para url19

names(url19) <- as.matrix(url19[7, ])
url19 <- url19[8:nrow(url19), ]



## para url20

names(url20) <- as.matrix(url20[6, ])
url20 <- url20[7:nrow(url20), ]



## para url21

names(url21) <- as.matrix(url21[6, ])
url21 <- url21[7:nrow(url21), ]




## para url22

names(url22) <- as.matrix(url22[6, ])
url22 <- url22[7:nrow(url22), ]


## para url23

names(url23) <- as.matrix(url23[6, ])
url23 <- url23[7:nrow(url23), ]



## para url25

names(url25) <- as.matrix(url25[6, ])
url25 <- url25[7:nrow(url25), ]


## para url26

names(url26) <- as.matrix(url26[6, ])
url26 <- url26[7:nrow(url26), ]


## para url27

names(url27) <- as.matrix(url27[6, ])
url27 <- url27[7:nrow(url27), ]


## para url28

names(url28) <- as.matrix(url28[6, ])
url28 <- url28[7:nrow(url28), ]



## para url29

names(url29) <- as.matrix(url29[8, ])
url29 <- url29[9:nrow(url29), ]


## para url31

names(url31) <- as.matrix(url31[6, ])
url31 <- url31[7:nrow(url31), ]



## para url32

names(url32) <- as.matrix(url32[6, ])
url32 <- url32[7:nrow(url32), ]



## para url33

names(url33) <- as.matrix(url33[6, ])
url33 <- url33[7:nrow(url33), ]



## para url34

names(url34) <- as.matrix(url34[6, ])
url34 <- url34[7:nrow(url34), ]



## para url35

names(url35) <- as.matrix(url35[6, ])
url35 <- url35[7:nrow(url35), ]



## para url37

names(url37) <- as.matrix(url37[6, ])
url37 <- url37[7:nrow(url37), ]


## para url38

names(url38) <- as.matrix(url38[6, ])
url38 <- url38[7:nrow(url38), ]


## para url39

names(url39) <- as.matrix(url39[6, ])
url39 <- url39[7:nrow(url39), ]


## para url40

names(url40) <- as.matrix(url40[6, ])
url40 <- url40[7:nrow(url40), ]



## para url41

names(url41) <- as.matrix(url41[7, ])
url41 <- url41[8:nrow(url41), ]


#### UNION PARA LAS BASES DE DATOS CON 33 COLUMNAS 

nombres_bases <- list(url1, url2, url3, url4, url5, url6, url7,
                   url8, url9, url10, url11, url12, url13, url14,
                   url15, url16, url17, url18, url19, url20, url21,
                   url22, url23, url24, url25, url26, url27, url28,
                   url29, url30, url31, url32, url33, url34, url35,
                   url36, url37, url38, url39, url40, url41)

lapply(nombres_bases, clean_names)



difere_cantida_vari <- c(33, 41, 39, 34, 104, 62, 106, 41)


 
for (i in nombres_bases){
  i <- clean_names(i)
 }


# merge_aux <- function(...) merge (..., all=TRUE)
# datos_4<-Reduce(merge_aux, list(url11, url19, url20, url21, url22, url23, url25, url26,
#                                 url27, url28, url29, url31, url32, url33, url34, url35,
#                                 url37, url38, url39))

url1 %<>% clean_names()
url2 %<>% clean_names()
url3 %<>% clean_names()
url4 %<>% clean_names()
url5 %<>% clean_names()
url6 %<>% clean_names()
url7 %<>% clean_names()
url8 %<>% clean_names()
url9 %<>% clean_names()
url10 %<>% clean_names()
url11 %<>% clean_names()
url12 %<>% clean_names()
url13 %<>% clean_names()
url14 %<>% clean_names()
url15 %<>% clean_names()
url16 %<>% clean_names()
url17 %<>% clean_names()
url18 %<>% clean_names()
url19 %<>% clean_names()
url20 %<>% clean_names()
url21 %<>% clean_names()
url22 %<>% clean_names()
url23 %<>% clean_names()
url24 %<>% clean_names()
url25 %<>% clean_names()
url26 %<>% clean_names()
url27 %<>% clean_names()
url28 %<>% clean_names()
url29 %<>% clean_names()
url30 %<>% clean_names()
url31 %<>% clean_names()
url32 %<>% clean_names()
url33 %<>% clean_names()
url34 %<>% clean_names()
url35 %<>% clean_names()
url36 %<>% clean_names()
url37 %<>% clean_names()
url38 %<>% clean_names()
url39 %<>% clean_names()
url40 %<>% clean_names()
url41 %<>% clean_names()


# base_33col <- rbind(url11, url19, url20, url21, url22, url23, url25, url26,
#                     url27, url28, url29, url31, url32, url33, url34, url35,
#                     url37, url38, url39, url40)


################# notas

# url17 posible base de graduados menor a 2013
# url18 Identificar a que corresponde esa base datos (encabezado) # matriculados hasta el 2013
# url19 matriculados 2014 
# url20 matriculados 2015
# url30 identificar de que trata (problemas de encabezado)
# url36 identificar de que trata (problemas de encabezado)
# url41 revisar si faltan datos, asociada al año 2018 de los inscritos (no se sabe si realmente es)
# url24 indentificar de que trata.



#################

b2021 <- c("url3", "url5", "url1", "url2", "url4")

b2020 <- c("url6", "url7", "url8", "url9", "url10")

b2019 <- c("url14", "url16", "url13", "url12", "url15") ##  posibles problemas url13 url14

b2018 <- c("url41", "url35", "url23", "url29", "url11") # problemas url41

b2017 <- c("url40", "url34", "url22", "url28", "Falta_graduados_2017, pero esta en url17")

b2016 <- c("url33", "url39", "url21", "url27", "Falta_graduados_2016, pero esta en url17")

b2015 <- c("url38", "url32", "url20", "url26", "Falta_graduados_2015, pero esta en url17")

b2014 <- c("url37", "url31", "url19", "url25", "Falta_graduados_2014, pero esta en url17")


# Organizar base de datos:

# Lectura del total de dimensiones:

columnas <- c()

for (i in 1:41) {
  t <- paste0("dim(url",i,")[2]")
  columnas[i] <- eval(parse(text=t))
}

dic0 <- data.frame(titulo,urls)

dic0 <- cbind(dic0,columnas)

dic0[grepl("inscritos", tolower(titulo)),]

titulo

url9 %>% names()

dim(url9)

i.ins <- dic0[grepl("inscritos", tolower(titulo)),]$url %>% parse_number()

ins <- c(2,3,6,7,8,9,10,13,14,16,18,20,21,23,24,35,37,38,39)

nom.ins <- names(url9[,ins]) %>% tolower()
nom.ins


iv <- paste0("i",i.ins)
c.ins <- paste0("c.url",i.ins)

#### Con omisión ####

c = 1
for (x in i.ins) {
  # Creación de las variables encontradas
  t1 <- paste0("grep(paste(nom.ins, collapse = '|'), tolower(names(url",
               x,")), value = T)") %>% parse(text = .) %>% eval()
  assign(iv[c],t1)
  # Creación de nuevos data.frames:
  t2 <- paste0("url",x,"[,",iv[c],"[(names(url",
               x,"[,",iv[c],"]) %in% nom.ins)]]") %>% parse(text = .) %>% eval()

  assign(c.ins[c], t2)
  c = c+1
}

#### Sin Omisión ####
# 
# c = 1
# for (x in i.ins) {
#   # Creación de las variables encontradas
#   t1 <- paste0("grep(paste(nom.ins, collapse = '|'), tolower(names(url",
#                x,")), value = T)") %>% parse(text = .) %>% eval()
#   assign(iv[c],t1)
#   # Creación de nuevos data.frames:
#   t2 <- paste0("url",x,"[,",iv[c],"]") %>% parse(text = .) %>% eval()
#   
#   assign(c.ins[c], t2)
#   c = c+1
# }


u <- dic0[grepl("inscritos", tolower(titulo)),]$url

lc.ins <- list()
c = 1
for (i in u) {
  lc.ins[[i]] <- c.ins[c] %>% parse(text = .) %>% eval()
  c = c+1
}


lapply(lapply(lc.ins, names), length)

# Para el caso de la omisión

omit_lc.ins <- lc.ins
c = 1
for (x in i.ins) {
  t <- paste0(iv[c],"[!(names(url",x,
              "[,",iv[c],"]) %in% nom.ins)]") %>% parse(text = .) %>% eval()
  omit_lc.ins[[u[c]]] <- t
  c = c +1
}

omit_lc.ins



url14[,i14[(names(url14[,i14]) %in% nom.ins)]] %>% names()

cbind(names(url14[,i14[(names(url14[,i14]) %in% nom.ins)]]),nom.ins[1:18])

url37[,i37[(names(url37[,i37]) %in% nom.ins)]] %>% names()

inscritos <- nombres_bases[c(9,14,36,37,38,39,40,41)]

save(nombres_bases,
     file= "BD_lista.RData")
