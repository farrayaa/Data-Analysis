library(ggplot2)
library(survival)
library(readxl)
library(tidyverse)
library(survminer)
library(dplyr)

# Definisi Data
datafp <- read.csv("C:/Users/ThinkPad/Downloads/FP ANSUR/data_covid.csv")
View(datafp)

### PREPROCESSING ###
# Jumlah Data Kosong
sum(is.na(datafp))

## Statistika Deskriptif ##
summary(datafp)

# 1. Variabel Time
summary(datafp$Time)
stats <- datafp %>%
  group_by(status) %>%
  summarise(
    Min = min(Time, na.rm = TRUE),
    Max = max(Time, na.rm = TRUE),
    Mean = mean(Time, na.rm = TRUE),
    Count = n()
  )
print(stats)
ggplot(datafp, aes(x = Time, fill = as.factor(status))) + 
  geom_histogram(binwidth = 1, position = "stack") +
  scale_fill_manual(values = c("#81D4FA", "#FFB74D"), labels = c("Hidup/Tersensor", "Meninggal Dunia")) +
  labs(title = "Distribusi Waktu (Time) Berdasarkan Status",
       x = "Waktu (hari)",
       y = "Frekuensi",
       fill = "Status") +
  # Tambahkan garis min, max, dan mean untuk Status = 0 (Hidup/Tersensor)
  geom_vline(data = stats %>% filter(status == 0), 
             aes(xintercept = Min), color = "#0288D1", linetype = "dashed", size = 0.8, show.legend = FALSE) +
  geom_vline(data = stats %>% filter(status == 0), 
             aes(xintercept = Max), color = "#0288D1", linetype = "dashed", size = 0.8, show.legend = FALSE) +
  geom_vline(data = stats %>% filter(status == 0), 
             aes(xintercept = Mean), color = "#0288D1", linetype = "solid", size = 1, show.legend = FALSE) +
  # Tambahkan garis min, max, dan mean untuk Status = 1 (Meninggal Dunia)
  geom_vline(data = stats %>% filter(status == 1), 
             aes(xintercept = Min), color = "#F57C00", linetype = "dashed", size = 0.8, show.legend = FALSE) +
  geom_vline(data = stats %>% filter(status == 1), 
             aes(xintercept = Max), color = "#F57C00", linetype = "dashed", size = 0.8, show.legend = FALSE) +
  geom_vline(data = stats %>% filter(status == 1), 
             aes(xintercept = Mean), color = "#F57C00", linetype = "solid", size = 1, show.legend = FALSE) +
  theme_minimal()

# 2. Variabel status
ggplot(datafp, aes(x = as.factor(status))) +
  geom_bar(fill = c("#1A3D6D", "#FF9F00")) +
  labs(title = "Status Pasien",
       x = "Status Pasien",
       y = "Frekuensi") +
  scale_x_discrete(labels = c("Hidup/Tersensor", "Meninggal Dunia")) +  
  theme_minimal()

# 3. Variabel Diabetic_mellitus
ggplot(datafp, aes(x = as.factor(Diabetic_mellitus))) +
  geom_bar(fill = c("#1A3D6D", "#FF9F00")) +
  labs(title = "Diabetes Mellitus",
       x = "Riwayat Penyakit Diabetes",
       y = "Frekuensi") +
  scale_x_discrete(labels = c("Tidak Diabetes", "Diabetes")) +  
  theme_minimal()

# 4. Variabel ICU_admission
ggplot(datafp, aes(x = as.factor(ICU_admission))) +
  geom_bar(fill = c("#1A3D6D", "#FF9F00")) +
  labs(title = "ICU Admission",
       x = "Tempat Perawatan Pasien",
       y = "Frekuensi") +
  scale_x_discrete(labels = c("Bukan ICU", "ICU")) +  
  theme_minimal()

# 5. Variabel Chronic_kidney_disease
ggplot(datafp, aes(x = as.factor(Chronic_kidney_disease))) +
  geom_bar(fill = c("#1A3D6D", "#FF9F00")) +
  labs(title = "Gagal Ginjal",
       x = "Riwayat Penyakit Gagal ginjal",
       y = "Frekuensi") +
  scale_x_discrete(labels = c("Tidak Gagal Ginjal", "Gagal Ginjal")) +  
  theme_minimal()

# 6. Variabel Chronic_cardiac
ggplot(datafp, aes(x = as.factor(Chronic_cardiac))) +
  geom_bar(fill = c("#1A3D6D", "#FF9F00")) +
  labs(title = "Jantung Kronis",
       x = "Riwayat Penyakit Jantung Kronis",
       y = "Frekuensi") +
  scale_x_discrete(labels = c("Tidak Memiliki Penyakit Jantung", "Memiliki Penyakit Jantung")) +  
  theme_minimal()

## KAPLAN MEIER ##
surv_obj <- Surv(time = datafp$Time, event = datafp$status)

# 1. Kaplan-Meier berdasarkan Diabetic_mellitus
fit_diabetic <- survfit(Surv(Time, status) ~ Diabetic_mellitus, data = datafp)
ggsurvplot(fit_diabetic, data = datafp, conf.int = TRUE, xlab = "Time (days)", ylab = "Survival Probability", ggtheme = theme_minimal(), legend.labs = c("No Diabetes", "Has Diabetes"), 
           legend.title = "Diabetic Mellitus", legend.position = "bottom", title = "Kaplan-Meier Curve for Diabetic Mellitus")

# 2. Kaplan-Meier berdasarkan ICU_admission
fit_icu <- survfit(Surv(Time, status) ~ ICU_admission, data = datafp)
ggsurvplot(fit_icu, data = datafp, conf.int = TRUE, xlab = "Time (days)", ylab = "Survival Probability", ggtheme = theme_minimal(), legend.labs = c("No ICU Admission", "ICU Admission"), 
           legend.title = "ICU Admission", legend.position = "bottom", title = "Kaplan-Meier Curve for ICU Admission")

# 3. Kaplan-Meier berdasarkan Chronic_kidney_disease
fit_kidney <- survfit(Surv(Time, status) ~ Chronic_kidney_disease, data = datafp)
ggsurvplot(fit_kidney, data = datafp, conf.int = TRUE, xlab = "Time (days)", ylab = "Survival Probability", ggtheme = theme_minimal(), legend.labs = c("No Chronic Kidney Disease", "Has Chronic Kidney Disease"), 
           legend.title = "Chronic Kidney Disease", legend.position = "bottom", title = "Kaplan-Meier Curve for Chronic Kidney Disease")

# 4. Kaplan-Meier berdasarkan Chronic_cardiac
fit_cardiac <- survfit(Surv(Time, status) ~ Chronic_cardiac, data = datafp)
ggsurvplot(fit_cardiac, data = datafp, conf.int = TRUE, xlab = "Time (days)", ylab = "Survival Probability", ggtheme = theme_minimal(), legend.labs = c("No Chronic Cardiac Disease", "Has Chronic Cardiac Disease"), 
           legend.title = "Chronic Cardiac Disease", legend.position = "bottom", title = "Kaplan-Meier Curve for Chronic Cardiac Disease")

## UJI LOG-RANK ## (PR)
Y=Surv(datafp$Time,datafp$status==1)
survdiff(Y~Diabetic_mellitus, data=datafp) # Tidak terdapat perbedaan waktu survival
survdiff(Y~ICU_admission, data=datafp) # Terdapat perbedaan waktu survival
survdiff(Y~Chronic_kidney_disease, data=datafp) # Terdapat perbedaan waktu survival
survdiff(Y~Chronic_cardiac, data=datafp) # Terdapat perbedaan waktu survival

## MODEL COX PH ##
model1 <- coxph(Y~Diabetic_mellitus+ICU_admission+Chronic_kidney_disease+Chronic_cardiac, data=datafp)
model1 # Diabetes tidak signifikan

## Grafik -ln(-ln S(t)) ##
# 1. Diabetic_mellitus
log_diabetic_mellitus <- survfit(surv_obj ~ Diabetic_mellitus, data=datafp)
plot(log_diabetic_mellitus, fun="cloglog", col=c("#FFB74D", "#81D4FA"),
     xlab = "Time", ylab = "-log(log(S(t)))",
     main = "-log(log(S(t)) Plot Diabetic Mellitus")
legend("bottomright", legend = levels(as.factor(datafp$Diabetic_mellitus)),
       col=c("#FFB74D", "#81D4FA"), lty=1, title="Diabetes Mellitus")

# 2. ICU_admission
log_icu_admission <- survfit(surv_obj ~ ICU_admission, data=datafp)
plot(log_icu_admission, fun="event", col=c("#FFB74D", "#81D4FA"),
     xlab = "Time", ylab = "-log(log(S(t)))",
     main = "-log(log(S(t)) Plot ICU Admission")
legend("topright", legend = levels(as.factor(datafp$ICU_admission)),
       col=c("#FFB74D", "#81D4FA"), lty=1, title="ICU Admission")

# 3. Chronic_kidney_disease
log_chronic_kidney_disease <- survfit(surv_obj ~ Chronic_kidney_disease, data=datafp)
plot(log_chronic_kidney_disease, fun="cloglog", col=c("#FFB74D", "#81D4FA"),
     xlab = "Time", ylab = "-log(log(S(t)))",
     main = "-log(log(S(t)) Plot Chronic Kidney Disease")
legend("bottomright", legend = levels(as.factor(datafp$Chronic_kidney_disease)),
       col=c("#FFB74D", "#81D4FA"), lty=1, title="Penyakit Gagal Ginjal")

# 4. Chronic_cardiac
log_chronic_cardiac <- survfit(surv_obj ~ Chronic_cardiac, data=datafp)
plot(log_chronic_cardiac, fun="cloglog", col=c("#FFB74D", "#81D4FA"),
     xlab = "Time", ylab = "-log(log(S(t)))",
     main = "-log(log(S(t)) Plot Chronic Cardiac")
legend("bottomright", legend = levels(as.factor(datafp$Chronic_cardiac)),
       col=c("#FFB74D", "#81D4FA"), lty=1, title="Penyakit Jantung Kronis")

## Grafik Observed vs Expected ##
# 1. Diabetic_mellitus: Observed vs Expected
y = Surv(datafp$Time,datafp$status==1)
log_km_fit_diabetic <- survfit(y ~ Diabetic_mellitus, data = datafp)
plot(log_km_fit_diabetic,
     main = "Observed vs Expected Survival Probability for Diabetic Mellitus",
     xlab = "Time (day)", ylab = "Survival Probability",
     lty = 1, col = c("#FFB74D", "#81D4FA"), lwd = 2)
log_km_fit_diabetic2 <- coxph(formula = y ~ Diabetic_mellitus, data = datafp)
log_km_fit_diabetic2_new <- data.frame(Diabetic_mellitus = 1:2)
lines(survfit(log_km_fit_diabetic2, log_km_fit_diabetic2_new), 
      col = c("#FFB74D", "#81D4FA"), lty = 2, lwd = 2)
legend("bottomright", legend = c("No Diabetes (Obs)", "No Diabetes (Exp)", "Has Diabetes (Obs)", "Has Diabetes (Exp)"), 
       col = c("#FFB74D", "#FFB74D", "#81D4FA", "#81D4FA"), lty = c(1, 2, 1, 2), lwd = 2, title = "Clinic")


# 2. ICU_admission: Observed vs Expected
log_km_fit_icu <- survfit(y ~ ICU_admission, data = datafp)
plot(log_km_fit_icu,
     main = "Observed vs Expected Survival Probability for ICU Admission",
     xlab = "Time (day)", ylab = "Survival Probability",
     lty = 1, col = c("#FFB74D", "#81D4FA"), lwd = 2)
log_km_fit_icu2 <- coxph(formula = y ~ ICU_admission, data = datafp)
log_km_fit_icu2_new <- data.frame(ICU_admission = 0:1)
lines(survfit(log_km_fit_icu2, newdata = log_km_fit_icu2_new), 
      col = c("#FFB74D", "#81D4FA"), lty = 2, lwd = 2)
legend("bottomleft", 
       legend = c("No ICU Admission (Obs)", "No ICU Admission (Exp)", 
                  "ICU Admission (Obs)", "ICU Admission (Exp)"), 
       col = c("#FFB74D", "#FFB74D", "#81D4FA", "#81D4FA"), 
       lty = c(1, 2, 1, 2), lwd = 2, title = "ICU Admission")

# 3. Chronic_kidney_disease: Observed vs Expected
log_km_fit_kidney <- survfit(y ~ Chronic_kidney_disease, data = datafp)
plot(log_km_fit_kidney,
     main = "Observed vs Expected Survival Probability for Chronic Kidney Disease",
     xlab = "Time (day)", ylab = "Survival Probability",
     lty = 1, col = c("#FFB74D", "#81D4FA"), lwd = 2)
log_km_fit_kidney2 <- coxph(formula = y ~ Chronic_kidney_disease, data = datafp)
log_km_fit_kidney2_new <- data.frame(Chronic_kidney_disease = 0:1)
lines(survfit(log_km_fit_kidney2, newdata = log_km_fit_kidney2_new), 
      col = c("#FFB74D", "#81D4FA"), lty = 2, lwd = 2)
legend("bottomright", 
       legend = c("No Kidney Disease (Obs)", "No Kidney Disease (Exp)", 
                  "Has Kidney Disease (Obs)", "Has Kidney Disease (Exp)"), 
       col = c("#FFB74D", "#FFB74D", "#81D4FA", "#81D4FA"), 
       lty = c(1, 2, 1, 2), lwd = 2, title = "Chronic Kidney Disease")

# 4. Chronic_cardiac: Observed vs Expected
log_km_fit_cardiac <- survfit(y ~ Chronic_cardiac, data = datafp)
plot(log_km_fit_cardiac,
     main = "Observed vs Expected Survival Probability for Chronic Cardiac Disease",
     xlab = "Time (day)", ylab = "Survival Probability",
     lty = 1, col = c("#FFB74D", "#81D4FA"), lwd = 2)
log_km_fit_cardiac2 <- coxph(formula = y ~ Chronic_cardiac, data = datafp)
log_km_fit_cardiac2_new <- data.frame(Chronic_cardiac = 0:1)
lines(survfit(log_km_fit_cardiac2, newdata = log_km_fit_cardiac2_new), 
      col = c("#FFB74D", "#81D4FA"), lty = 2, lwd = 2)
legend("bottomright", 
       legend = c("No Cardiac Disease (Obs)", "No Cardiac Disease (Exp)", 
                  "Has Cardiac Disease (Obs)", "Has Cardiac Disease (Exp)"), 
       col = c("#FFB74D", "#FFB74D", "#81D4FA", "#81D4FA"), 
       lty = c(1, 2, 1, 2), lwd = 2, title = "Chronic Cardiac Disease")


# METODE GOF #
Y=Surv(datafp$Time,datafp$status==1)
model=coxph(Y~Diabetic_mellitus + ICU_admission + Chronic_kidney_disease + 
              Chronic_cardiac, data=datafp)
cox.zph(model,transform=rank)

# MODEL TANPA INTERAKSI 
stra01 <- coxph(Y~Diabetic_mellitus + Chronic_kidney_disease +
                  Chronic_cardiac + strata(ICU_admission), data=datafp)
summary(stra01)
AIC(stra01)

stra1 <- coxph(Y~Chronic_kidney_disease +
                 Chronic_cardiac + strata(ICU_admission), data=datafp)
summary(stra1)
AIC(stra1)

# MODEL DENGAN INTERAKSI
stra2 <- coxph(Y~Chronic_kidney_disease +
                 Chronic_cardiac + Chronic_kidney_disease:ICU_admission+Chronic_cardiac:ICU_admission+strata(ICU_admission), data=datafp)
summary(stra2)
AIC(stra2)

stra21 <- coxph(Y~Chronic_kidney_disease +
                  Chronic_cardiac + Chronic_kidney_disease:ICU_admission+strata(ICU_admission), data=datafp)
summary(stra21)
AIC(stra21)

# Likelihood Ratio Test
LL.noint <- stra1$loglik[2]
LL.int <- stra21$loglik[2]
chisq_stat <- -2*(LL.noint - LL.int)
ncoef.dif <- length(stra21$coefficients) - length(stra1$coefficients)
pvalue <- 1 - pchisq(chisq_stat, df = ncoef.dif)
pvalue
