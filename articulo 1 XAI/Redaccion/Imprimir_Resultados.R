# Cargar el paquete
library(writexl)
load("C:/Users/Ricardo/OneDrive - UMH/Documentos/Cafee/articulo 1 XAI/code paper/resultados_art_XAI_NN_CV_1_3.RData")

write_xlsx(as.data.frame(list_method$nnet$peer_list$"0.95", "peer095.xlsx"))
