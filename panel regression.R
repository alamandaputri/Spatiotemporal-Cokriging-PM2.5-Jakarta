library(openxlsx)

# data berupa tabel yang terdiri atas kolom "Sensor", "Longitude", dan "Latitude"
# kolom "Sensor" berisi nama lokasi sensor
data = read.xlsx("D:/S2 STATISTIKA/TESIS/DATA/DataRequest_UNPAD_FarizaAP_Jakarta2023.xlsx", sheet = "Koordinat")
data

# Load the necessary package
library(sf)

# Konversi data frame menjadi objek spatial
data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

# Ubah ke UTM, misalnya UTM zona 48S (sesuai dengan Jakarta)
data_utm <- st_transform(data_sf, crs = 32748)

# Tampilkan hasilnya
head(data_utm)

# Jika Anda ingin menambah kolom UTM ke dalam dataframe awal
data$UTM_Easting <- st_coordinates(data_utm)[,1]
data$UTM_Northing <- st_coordinates(data_utm)[,2]

# Asumsikan bahwa kolom longitude dan latitude di file lokasi bernama "longitude" dan "latitude"
coords <- as.matrix(data[, c("UTM_Easting", "UTM_Northing")])

# Hitung jarak dalam satuan kilometer
library(geosphere)
distance_matrix <- as.matrix(dist(coords, method = "euclidean") / 1000)
rownames(distance_matrix) = data$Sensor; colnames(distance_matrix) = data$Sensor
View(distance_matrix[c(1:29),c(30:31)])

#================================================================== CURAH HUJAN
# data regresi
library(openxlsx)
data = read.xlsx("D:/S2 STATISTIKA/TESIS/DATA/database panel.xlsx", sheet = "DATABASE PANEL CURAH HUJAN")
data

# mengubah variabel Bulan menjadi kategorik
data$Bulan <- as.factor(data$Bulan)

# Membuat dummy variables untuk Bulan
dummy_bulan <- model.matrix(~Bulan, data)[,-1]  # -1 untuk menghapus intercept/dummy referensi

# Gabungkan dummy ke data asli
data_dummy <- cbind(data, dummy_bulan)

# Cek struktur data untuk memastikan dummy sudah ditambahkan
str(data_dummy)

# Model panel menggunakan variabel dummy Bulan
library(plm)
model_precip = plm(sqrt(Curah.Hujan) ~ PM2.5 + dummy_bulan, data = data_dummy, 
                   effect = 'individual', model = "random", index = c("Lokasi"))

# Ringkasan hasil model
summary(model_precip)
res = fitted(model_precip)^2 - data$Curah.Hujan
min(res);max(res)

# menyiapkan data yang ingin diprediksi
data_baru = read.xlsx("D:/S2 STATISTIKA/TESIS/DATA/new data to predict.xlsx", sheet = "curah hujan")
data_baru$Bulan <- as.factor(data_baru$Bulan)

# Membuat dummy variables untuk Bulan
dummy_bulan_baru <- model.matrix(~Bulan, data_baru)[,-1]  # -1 untuk menghapus intercept/dummy referensi

# Gabungkan dummy ke data asli
data_dummy_baru <- cbind(data_baru, dummy_bulan_baru)

pdata_baru = pdata.frame(data_dummy_baru, index = c("Lokasi"))

# prediksi
prediksi_precip <- c()

num_rows <- nrow(pdata_baru)
step_size <- nrow(data)

for (i in seq(1, num_rows, by = step_size)) {
  # Tentukan indeks akhir untuk setiap iterasi
  end_index <- min(i + step_size - 1, num_rows)
  
  # Lakukan prediksi untuk setiap 36 baris
  prediksi <- round(predict(model_precip, newdata = pdata_baru[i:end_index,])^2, 2)
  
  # Gabungkan hasil prediksi ke dalam vektor hasil
  prediksi_precip <- c(prediksi_precip, prediksi)
}

# Hasil prediksi disimpan dalam prediksi_semua
prediksi_precip

hasil_precip = cbind(data_baru, Curah_Hujan = prediksi_precip)
hasil_precip

library(tidyverse)
# Mengubah format data agar setiap lokasi menjadi kolom
precip_wide <- hasil_precip[1:num_rows,-3] %>%
  arrange(Lokasi) %>%
  pivot_wider(names_from = Lokasi, values_from = Curah_Hujan)

# Menampilkan data dalam format yang diubah
precip_wide <- precip_wide %>%
  select(-contains("HAPUS"))

write.xlsx(precip_wide, "D:/S2 STATISTIKA/TESIS/DATA/hasil prediksi curah hujan.xlsx")

#=================================================================== KELEMBABAN
# data regresi
library(openxlsx)
data = read.xlsx("D:/S2 STATISTIKA/TESIS/DATA/database panel.xlsx", sheet = "DATABASE PANEL KELEMBABAN")
data

# mengubah variabel Bulan menjadi kategorik
data$Bulan <- as.factor(data$Bulan)

# Membuat dummy variables untuk Bulan
dummy_bulan <- model.matrix(~Bulan, data)[,-1]  # -1 untuk menghapus intercept/dummy referensi

# Gabungkan dummy ke data asli
data_dummy <- cbind(data, dummy_bulan)

# Cek struktur data untuk memastikan dummy sudah ditambahkan
str(data_dummy)

# Model panel menggunakan variabel dummy Bulan
library(plm)
model_humid = plm(Kelembaban ~ PM2.5 + dummy_bulan, data = data_dummy, 
                  effect = 'individual', model = "random", index = c("Lokasi"))

# Ringkasan hasil model
summary(model_humid)
res = resid(model_humid)
min(res);max(res)

# menyiapkan data yang ingin diprediksi
data_baru = read.xlsx("D:/S2 STATISTIKA/TESIS/DATA/new data to predict.xlsx", sheet = "kelembaban")
data_baru$Bulan <- as.factor(data_baru$Bulan)

# Membuat dummy variables untuk Bulan
dummy_bulan_baru <- model.matrix(~Bulan, data_baru)[,-1]  # -1 untuk menghapus intercept/dummy referensi

# Gabungkan dummy ke data asli
data_dummy_baru <- cbind(data_baru, dummy_bulan_baru)

pdata_baru = pdata.frame(data_dummy_baru, index = c("Lokasi"))

# prediksi
prediksi_humid <- c()

num_rows <- nrow(pdata_baru)
step_size <- nrow(data)

for (i in seq(1, num_rows, by = step_size)) {
  # Tentukan indeks akhir untuk setiap iterasi
  end_index <- min(i + step_size - 1, num_rows)
  
  # Lakukan prediksi untuk setiap 36 baris
  prediksi36 <- round(predict(model_humid, newdata = pdata_baru[i:end_index,]), 2)
  
  # Gabungkan hasil prediksi ke dalam vektor hasil
  prediksi_humid <- c(prediksi_humid, prediksi36)
}

# Hasil prediksi disimpan dalam prediksi_semua
prediksi_humid

hasil_humid = cbind(data_baru, Kelembaban = prediksi_humid)
hasil_humid

library(tidyverse)
# Mengubah format data agar setiap lokasi menjadi kolom
humid_wide <- hasil_humid[1:num_rows,-3] %>%
  arrange(Lokasi) %>%
  pivot_wider(names_from = Lokasi, values_from = Kelembaban)

# Menampilkan data dalam format yang diubah
humid_wide <- humid_wide %>%
  select(-contains("HAPUS"))

write.xlsx(humid_wide, "D:/S2 STATISTIKA/TESIS/DATA/hasil prediksi kelembaban.xlsx")

#============================================================== KECEPATAN ANGIN
# data regresi
library(openxlsx)
data = read.xlsx("D:/S2 STATISTIKA/TESIS/DATA/database panel.xlsx", sheet = "DATABASE PANEL KECEPATAN ANGIN")
data

# mengubah variabel Bulan menjadi kategorik
data$Bulan <- as.factor(data$Bulan)

# Membuat dummy variables untuk Bulan
dummy_bulan <- model.matrix(~Bulan, data)[,-1]  # -1 untuk menghapus intercept/dummy referensi

# Gabungkan dummy ke data asli
data_dummy <- cbind(data, dummy_bulan)

# Cek struktur data untuk memastikan dummy sudah ditambahkan
str(data_dummy)

# Model panel menggunakan variabel dummy Bulan
library(plm)
model_ws = plm(Kecepatan.Angin ~ PM2.5 + dummy_bulan, data = data_dummy, 
                effect = 'individual', model = "random", index = c("Lokasi"))

# Ringkasan hasil model
summary(model_ws)
res = resid(model_ws)
min(res);max(res)

# menyiapkan data yang ingin diprediksi
data_baru = read.xlsx("D:/S2 STATISTIKA/TESIS/DATA/new data to predict.xlsx", sheet = "kecepatan")
data_baru$Bulan <- as.factor(data_baru$Bulan)

# Membuat dummy variables untuk Bulan
dummy_bulan_baru <- model.matrix(~Bulan, data_baru)[,-1]  # -1 untuk menghapus intercept/dummy referensi

# Gabungkan dummy ke data asli
data_dummy_baru <- cbind(data_baru, dummy_bulan_baru)

pdata_baru = pdata.frame(data_dummy_baru, index = c("Lokasi"))

# prediksi
prediksi_ws <- c()

num_rows <- nrow(pdata_baru)
step_size <- nrow(data)

for (i in seq(1, num_rows, by = step_size)) {
  # Tentukan indeks akhir untuk setiap iterasi
  end_index <- min(i + step_size - 1, num_rows)
  
  # Lakukan prediksi untuk setiap 36 baris
  prediksi36 <- round(predict(model_ws, newdata = pdata_baru[i:end_index,]), 2)
  
  # Gabungkan hasil prediksi ke dalam vektor hasil
  prediksi_ws <- c(prediksi_ws, prediksi36)
}

# Hasil prediksi disimpan dalam prediksi_semua
prediksi_ws

hasil_ws = cbind(data_baru, Wind_Speed = prediksi_ws)
hasil_ws

library(tidyverse)
# Mengubah format data agar setiap lokasi menjadi kolom
ws_wide <- hasil_ws[1:num_rows,-3] %>%
  arrange(Lokasi) %>%
  pivot_wider(names_from = Lokasi, values_from = Wind_Speed)

# Menampilkan data dalam format yang diubah
ws_wide <- ws_wide %>%
  select(-contains(c("HAPUS", "Tanjung Priok")))

write.xlsx(ws_wide, "D:/S2 STATISTIKA/TESIS/DATA/hasil prediksi kecepatan angin.xlsx")

#========================================================================== NO2
# data regresi
data_NO2 = read.xlsx("D:/S2 STATISTIKA/TESIS/DATA/database panel.xlsx", sheet = "DATABASE PANEL NO2")
data_NO2

# mengubah variabel Bulan menjadi kategorik
data_NO2$Bulan <- as.factor(data_NO2$Bulan)

# Membuat dummy variables untuk Bulan
dummy_bulan <- model.matrix(~Bulan, data_NO2)[,-1]  # -1 untuk menghapus intercept/dummy referensi

# Gabungkan dummy ke data asli
data_dummy <- cbind(data_NO2, dummy_bulan)

# Cek struktur data untuk memastikan dummy sudah ditambahkan
str(data_dummy)

# Model panel menggunakan variabel dummy Bulan
library(plm)
model_no2 = plm(NO2 ~ PM2.5 + dummy_bulan, data = data_dummy, 
                effect = 'individual', model = "random", index = c("Lokasi"))

# Ringkasan hasil model
summary(model_no2)
res = resid(model_no2)
max(res);min(res)

# menyiapkan data yang ingin diprediksi
data_baru = read.xlsx("D:/S2 STATISTIKA/TESIS/DATA/new data to predict.xlsx", sheet = "kecepatan")
data_baru$Bulan <- as.factor(data_baru$Bulan)

# Membuat dummy variables untuk Bulan
dummy_bulan_baru <- model.matrix(~Bulan, data_baru)[,-1]  # -1 untuk menghapus intercept/dummy referensi

# Gabungkan dummy ke data asli
data_dummy_baru <- cbind(data_baru, dummy_bulan_baru)

pdata_baru = pdata.frame(data_dummy_baru, index = c("Lokasi"))

# prediksi
prediksi_no2 <- c()

num_rows <- nrow(pdata_baru)
step_size <- nrow(data_NO2)

for (i in seq(1, num_rows, by = step_size)) {
  # Tentukan indeks akhir untuk setiap iterasi
  end_index <- min(i + step_size - 1, num_rows)
  
  # Lakukan prediksi untuk setiap 36 baris
  prediksi36 <- round(predict(model_no2, newdata = pdata_baru[i:end_index,]), 2)
  
  # Gabungkan hasil prediksi ke dalam vektor hasil
  prediksi_no2 <- c(prediksi_no2, prediksi36)
}

# Hasil prediksi disimpan dalam prediksi_semua
prediksi_no2

hasil_no2 = cbind(data_baru, NO2 = prediksi_no2)

library(tidyverse)
# Mengubah format data agar setiap lokasi menjadi kolom
no2_wide <- hasil_no2[1:num_rows,-3] %>%
  arrange(Lokasi) %>%
  pivot_wider(names_from = Lokasi, values_from = NO2)

# Menampilkan data dalam format yang diubah
no2_wide_clean <- no2_wide %>%
  select(-contains("HAPUS"))

write.xlsx(no2_wide, "D:/S2 STATISTIKA/TESIS/DATA/hasil prediksi NO2.xlsx")

df1 = as.data.frame(prediksi_precip)
df2 = as.data.frame(prediksi_humid)
df3 = as.data.frame(prediksi_ws)
df4 = as.data.frame(prediksi_no2)

# Langkah 1: Set rownames sebagai kolom baru (misalnya, 'id') di setiap dataframe
df1$id <- rownames(df1)
df2$id <- rownames(df2)
df3$id <- rownames(df3)
df4$id <- rownames(df4)

# Langkah 2: Gabungkan dataframe berdasarkan kolom 'id' yang berisi rownames
combined_df <- Reduce(function(x, y) merge(x, y, by = "id"), list(df1, df2, df3, df4))

combined_df_clean <- combined_df %>%
  filter(!grepl("HAPUS", id)) %>%
  separate(id, into = c("id_clean", "after_dash"), sep = "-", remove = FALSE) %>%
  arrange(id_clean, as.numeric(after_dash))  # Mengurutkan kolom secara numerik

combined_df_clean
write.xlsx(combined_df_clean, "D:/S2 STATISTIKA/TESIS/DATA/hasil prediksi all.xlsx")
