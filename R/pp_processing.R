process_pp <- function(path) {
  pp_g <- readxl::read_excel(path)
  pp_g <- na.omit(pp_g)
  length <- ncol(pp_g)
  min = 5
  if("DPMP" %in% colnames(pp_g)){
    min = 7
  }

  pp_g_f <- tidyr::pivot_longer(pp_g, cols =  seq(min, length), 
                                names_to = "SEXO_EDAD", 
                                values_to = "CONTEO")
  pp_g_f <- dplyr::mutate(pp_g_f,
    SEXO = unlist(stringr::str_split(SEXO_EDAD, "_", simplify = T))[, 1],
    EDAD = unlist(stringr::str_split(SEXO_EDAD, "_", simplify = T))[, 2]
  )
  if("DPMP" %in% colnames(pp_g_f)){
    pp_g_out <- dplyr::select(pp_g_f, c("DP", "DPNOM", "MPIO", "DPMP", "AÑO", 
                                          "ÁREA GEOGRÁFICA","SEXO", "EDAD", 
                                          "CONTEO"))
  } else{
  pp_g_out <- dplyr::select(pp_g_f, c("DP", "DPNOM", "AÑO", "ÁREA GEOGRÁFICA",
                                        "SEXO", "EDAD", "CONTEO"))
  }
  return(pp_g_out)
}

setwd("C:\\Users\\Julia\\OneDrive - Universidad de los Andes\\Data ColOpenData\\PP\\preprocessed")
files <- list.files()

dpto_2020_2050 <- process_pp(files[1])
dpto_2005_2019 <- process_pp(files[2])
mun_2005_2019 <- process_pp(files[3])
mun_2020_2035 <- process_pp(files[4])
nac_1950_2019 <- process_pp(files[5])
nac_2020_2020 <- process_pp(files[6])

write.csv(dpto_2020_2050, file = "DCD-area-sexo-edad-proyepoblacion-dep-2020-2050-ActPostCOVID-19.csv")
write.csv(dpto_2005_2019, file = "DCD-area-sexo-edad-proypoblacion-dep-2005-2019.csv")
write.csv(mun_2005_2019, file = "DCD-area-sexo-edad-proypoblacion-Mun-2005-2019.csv")
write.csv(mun_2020_2035, file = "DCD-area-sexo-edad-proypoblacion-Mun-2020-2035-ActPostCOVID-19.csv")
write.csv(nac_1950_2019, file = "DCD-area-sexo-edad-proypoblacion-Nac-1950-2019.csv")
write.csv(nac_2020_2020, file = "DCD-area-sexo-edad-proypoblacion-Nac-2020-2070.csv")
