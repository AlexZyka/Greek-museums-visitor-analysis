options(scipen = 999)

library(tidyverse)
library(readxl)
library(scales) 
library(lubridate)


cat("Visitors:\n")
df_raw_vis <- read_excel(file.choose(), skip = 4)
cat("Revenues:\n")
df_raw_rev <- read_excel(file.choose(), skip = 4)

clean_structural <- function(df) {
  colnames(df)[1] <- "Xwros"
  df %>%
    filter(!is.na(Xwros), Xwros != "ΣΥΝΟΛΟ", !str_detect(Xwros, "Νομός")) %>%
    mutate(Xwros = toupper(str_trim(Xwros)))
}

vis_pre <- clean_structural(df_raw_vis)
rev_pre <- clean_structural(df_raw_rev)

common_sites <- intersect(vis_pre$Xwros, rev_pre$Xwros)
vis_pre <- vis_pre %>% filter(Xwros %in% common_sites) %>% arrange(Xwros)
rev_pre <- rev_pre %>% filter(Xwros %in% common_sites) %>% arrange(Xwros)

vis_long <- vis_pre %>%
  mutate(across(2:13, as.character)) %>% 
  pivot_longer(cols = 2:13, names_to = "Month_Raw", values_to = "Visitors") %>%
  mutate(Visitors = as.numeric(Visitors))

rev_long <- rev_pre %>%
  mutate(across(2:13, as.character)) %>% 
  pivot_longer(cols = 2:13, names_to = "Month_Raw", values_to = "Revenue") %>%
  mutate(Revenue = as.numeric(Revenue))

vis_long$id <- 1:nrow(vis_long)
rev_long$id <- 1:nrow(rev_long)

df_merged <- vis_long %>%
  inner_join(rev_long %>% select(id, Revenue), by = "id") %>%
  select(Xwros, Month = Month_Raw, Visitors, Revenue) %>%
  drop_na(Visitors, Revenue)

all_museums <- unique(df_merged$Xwros)

cat("\n--- ΔΙΑΘΕΣΙΜΑ ΜΟΥΣΕΙΑ ΣΤΟ ΑΡΧΕΙΟ ΣΟΥ ---\n")
print(data.frame(Αριθμός = 1:length(all_museums), Μουσείο = all_museums), max = 100)

target_numbers <- c(9, 11, 17, 32, 23, 46, 27, 21, 20, 29)
target_sites <- all_museums[target_numbers]
df_final <- df_merged %>%
  filter(Xwros %in% target_sites) %>%
  mutate(Month = as.factor(Month))

cat("\n--- ΤΑ ΜΟΥΣΕΙΑ ΠΟΥ ΕΝΤΟΠΙΣΤΗΚΑΝ ΚΑΙ ΑΝΑΛΥΟΝΤΑΙ ---\n")
print(unique(df_final$Xwros))

 Xwros                 Total_Visitors Total_Revenue Mean_Visitors Mean_Revenue
   <chr>                          <dbl>         <dbl>         <dbl>        <dbl>
 1 ΑΡΧΑΊΑΣ ΟΛΥΜΠΊΑΣ              237783     277657200        19815.    23138100 
 2 ΑΡΧΑΙΟΛΟΓΙΚΌ ΘΕΣ/ΝΊΚ…          67073      96235900         5589.     8019658.
 3 ΒΥΖΑΝΤΙΝΌ ΘΕΣ/ΝΊΚΗΣ            69581     115465500         5798.     9622125 
 4 ΔΊΟΥ                           20362      14654800         1697.     1221233.
 5 ΔΕΛΦΏΝ                        300200     353100000        25017.    29425000 
 6 ΕΘΝΙΚΌ ΑΡΧΑΙΟΛΟΓΙΚΌ …         325002     627429000        27084.    52285750 
 7 ΗΡΑΚΛΕΊΟΥ                     341300     495150000        28442.    41262500 
 8 ΘΉΡΑΣ                          15402      11950000         1284.      995833.
 9 ΙΣΤΟΡΙΚΌ ΝΕΩΤΈΡΑΣ ΕΛ…          14334      11279150         1194.      939929.
10 ΜΠΕΝΆΚΗ                            0             0            0            0 


dev.new(width = 8, height = 6)
p1 <- ggplot(df_final, aes(x = Month, y = Visitors, group = Xwros, color = Xwros)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) + # Κανονική μορφή αριθμών στον άξονα Y
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 7)) +
  labs(title = "Περιγραφική Στατιστική: Μηνιαία Διακύμανση Επισκεπτών ανά Μουσείο",
       x = "Μήνας", y = "Αριθμός Επισκεπτών", color = "Μουσείο")
print(p1)


correlation <- cor.test(df_final$Visitors, df_final$Revenue)
cat("\n--- ΣΥΝΤΕΛΕΣΤΗΣ ΣΥΣΧΕΤΙΣΗΣ (PEARSON) ---\n")
print(correlation)

        Pearson's product-moment correlation

data:  df_final$Visitors and df_final$Revenue
t = 44.285, df = 118, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.9588917 0.9798729
sample estimates:
      cor 
0.9712085 


regression_model <- lm(Revenue ~ Visitors + Xwros, data = df_final)
cat("\n--- ΑΠΟΤΕΛΕΣΜΑΤΑ ΜΟΝΤΕΛΟΥ ΠΑΛΙΝΔΡΟΜΗΣΗΣ ---\n")
summary(regression_model)


Call:
lm(formula = Revenue ~ Visitors + Xwros, data = df_final)

Residuals:
      Min        1Q    Median        3Q       Max 
-10710207  -1082132         0    677072  11411252 

Coefficients:
                                   Estimate  Std. Error t value
(Intercept)                     -6046091.93  1159888.33  -5.213
Visitors                            1472.81       27.35  53.860
XwrosΑΡΧΑΙΟΛΟΓΙΚΌ ΘΕΣ/ΝΊΚΗΣ      5833575.10  1501601.16   3.885
XwrosΒΥΖΑΝΤΙΝΌ ΘΕΣ/ΝΊΚΗΣ         7128223.49  1500130.75   4.752
XwrosΔΕΛΦΏΝ                     -1373823.04  1457295.52  -0.943
XwrosΔΊΟΥ                        4768204.15  1532628.53   3.111
XwrosΕΘΝΙΚΌ ΑΡΧΑΙΟΛΟΓΙΚΌ ΑΘΗΝΏΝ 18442864.39  1463892.71  12.599
XwrosΗΡΑΚΛΕΊΟΥ                   5419286.53  1469395.82   3.688
XwrosΘΉΡΑΣ                       5151567.56  1536319.54   3.353
XwrosΙΣΤΟΡΙΚΌ ΝΕΩΤΈΡΑΣ ΕΛΛΆΔΟΣ   5226743.91  1537124.01   3.400
XwrosΜΠΕΝΆΚΗ                     6046091.93  1548250.93   3.905
                                            Pr(>|t|)    
(Intercept)                              0.000000891 ***
Visitors                        < 0.0000000000000002 ***
XwrosΑΡΧΑΙΟΛΟΓΙΚΌ ΘΕΣ/ΝΊΚΗΣ                 0.000176 ***
XwrosΒΥΖΑΝΤΙΝΌ ΘΕΣ/ΝΊΚΗΣ                 0.000006192 ***
XwrosΔΕΛΦΏΝ                                 0.347908    
XwrosΔΊΟΥ                                   0.002379 ** 
XwrosΕΘΝΙΚΌ ΑΡΧΑΙΟΛΟΓΙΚΌ ΑΘΗΝΏΝ < 0.0000000000000002 ***
XwrosΗΡΑΚΛΕΊΟΥ                              0.000354 ***
XwrosΘΉΡΑΣ                                  0.001099 ** 
XwrosΙΣΤΟΡΙΚΌ ΝΕΩΤΈΡΑΣ ΕΛΛΆΔΟΣ              0.000941 ***
XwrosΜΠΕΝΆΚΗ                                0.000164 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3553000 on 109 degrees of freedom
Multiple R-squared:  0.982,     Adjusted R-squared:  0.9804 
F-statistic: 595.5 on 10 and 109 DF,  p-value: < 0.00000000000000022





dev.new(width = 8, height = 6)
p2 <- ggplot(df_final, aes(x = Visitors, y = Revenue, color = Xwros)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  scale_x_continuous(labels = comma) + # Κανονικά νούμερα
  scale_y_continuous(labels = comma) + # Κανονικά νούμερα
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7)) +
  labs(title = "Μοντέλο Παλινδρόμησης: Σχέση Επισκεπτών και Εισπράξεων",
       x = "Αριθμός Επισκεπτών", y = "Εισπράξεις (€)", color = "Μουσείο")
print(p2)



cluster_data <- df_final %>% select(Visitors, Revenue)
cluster_data_scaled <- scale(cluster_data)

set.seed(123)
kmeans_output <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)
df_final$Cluster <- as.factor(kmeans_output$cluster)

cat("\n--- ΚΕΝΤΡΑ ΤΩΝ CLUSTERS (STANDARDIZED) ---\n")
print(kmeans_output$centers)


    Visitors    Revenue
1 -0.5315554 -0.5061108
2  2.2767606  2.6412776
3  1.3940348  1.1142294


dev.new(width = 8, height = 6)
p3 <- ggplot(df_final, aes(x = Visitors, y = Revenue, color = Cluster)) +
  geom_point(size = 3, alpha = 0.4) +
  geom_text(aes(label = Xwros), size = 2.3, vjust = -1, check_overlap = TRUE, show.legend = FALSE) + 
  scale_x_continuous(labels = comma) + # Διόρθωση άξονα Χ (Επισκέπτες)
  scale_y_continuous(labels = comma) + # Διόρθωση άξονα Υ (Εισπράξεις - Αντίο e+07)
  theme_minimal() +
  labs(title = "Τεχνική Clustering: Ομαδοποίηση Μουσείων (K-Means)",
       subtitle = "Διαχωρισμός των 10 μουσείων σε 3 διακριτές ομάδες συμπεριφοράς",
       x = "Αριθμός Επισκεπτών", y = "Εισπράξεις (€)", color = "Ομάδα (Cluster)")
print(p3)
