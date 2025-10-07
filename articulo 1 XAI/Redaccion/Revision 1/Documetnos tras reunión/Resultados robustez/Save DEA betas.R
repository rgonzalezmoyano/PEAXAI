betas_g1 <- read_excel(
  "articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/02_betas_DEA.xlsx",
  sheet = "g_1")
betas_g_mean <- read_excel(
  "articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/02_betas_DEA.xlsx",
  sheet = "g_mean")
betas_g_x0_y0 <- read_excel(
  "articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/02_betas_DEA.xlsx",
  sheet = "g_X0_Y0")

save(betas_g1, file = "articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/result_DEA_g1.Rdata")
save(betas_g_mean, file = "articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/result_DEA_betas_g_mean.Rdata")
save(betas_g_x0_y0, file = "articulo 1 XAI/Redaccion/Revision 1/Documetnos tras reunión/Resultados robustez/result_DEA_betas_g_x0_y0.Rdata")
