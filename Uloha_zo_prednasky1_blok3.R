predaj <- c(120, 135, 128, 150, 142, 160, 155)   # skutočné denné predaje (7 dní)
pred_a <- c(118, 132, 130, 148, 145, 158, 157)   # predikcia analytika A
pred_b <- c(120, 134, 128, 151, 141, 160, 167)   # predikcia analytika B

str(predaj)
head(predaj)
summary(predaj)

help(plot)

plot(predaj, type='o', pch=16, xlab = "deň", ylab = "predaj (kusy)", ylim=range(c(predaj, pred_a, pred_b)) )
lines(pred_a, type = "o", pch = 17, col = "red")
lines(pred_b, type = "o", pch = 15, col = "blue")
legend("topleft", legend = c("skutočnosť", "model A", "model B"),
       col = c("black", "red", "blue"), pch = c(16, 17, 15), lty = 1, bty = "n")



chyba_a <- predaj-pred_a
chyba_b <- predaj-pred_b

mae_a <- mean(abs(chyba_a))
rmse_a <- sqrt(mean(chyba_a^2))
mae_b <- mean(abs(chyba_b))
rmse_b <- sqrt(mean(chyba_b^2))

sae_a <- sum(abs(chyba_a))        # celková absolútna chyba za týždeň
sae_b <- sum(abs(chyba_b))

print(mae_a)
print(mae_b)
print(rmse_a)
print(rmse_b)
print(sae_a)
print(sae_b)
#За MAE трохи краща B, але різниця мізерна (2.14 проти 2.29). Натомість за RMSE A вдвічі краща, бо B має один великий промах. Для планування кур'єрів зазвичай обирають модель A: вона передбачувана, її похибка завжди мала, і на неї можна спертися. Модель B щодня «майже ідеальна», але ти ніколи не знаєш, коли вона схибить на 12.

kapacita <- 30                                                  # objednávok na kuriéra za deň

priemer_predaja <- mean(predaj)
kurieri_priemer <- ceiling(priemer_predaja/kapacita)

spicka_predaja <- max(predaj)                          
kurieri_spicka <- ceiling(spicka_predaja / kapacita)


g_denne     <- log(tail(predaj, 1) / predaj[1]) / (length(predaj) - 1)
rast_denne  <- exp(g_denne) - 1            # denný rast ako podiel (0,04 = 4 %)
faktor_7dni <- exp(7 * g_denne)            # koľkokrát vyšší dopyt o týždeň


report <- list(
  mae_a  = round(mae_a, 2),  rmse_a = round(rmse_a, 2),  sae_a = sae_a,
  mae_b  = round(mae_b, 2),  rmse_b = round(rmse_b, 2),  sae_b = sae_b,
  priemer_predaja  = round(priemer_predaja, 2),
  kurieri_priemer  = kurieri_priemer,
  spicka_predaja   = spicka_predaja,
  kurieri_spicka   = kurieri_spicka,
  denny_rast_pct   = round(100 * rast_denne, 2),
  faktor_7dni      = round(faktor_7dni, 2)
)
report
