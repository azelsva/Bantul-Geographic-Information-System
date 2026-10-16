# ANALISIS INDEKS SPEKTRAL & KLASIFIKASI TUTUPAN LAHAN

# ETS - PEMROGRAMAN SPASIAL (CM234529) -  KELAS A

# Nama :
# Nisrin Finira Safina    (5016241051)
# Avisa Azalia            (5016241098)

# Dosen Pengampu :
# Mohammad Rohmaneo Darminto, S.T., M.Sc.
# NIP. 1990201911078

# Wilayah Studi:
# Kabupaten Bantul, Daerah Istimewa Yogyakarta



# ==============================================================
# PERSIAPAN
# ==============================================================

Sys.unsetenv(c("PROJ_LIB", "PROJ_DATA", "GDAL_DATA"))

library(terra)
library(sf)
library(dplyr)

# FOLDER & DATA ================================================

setwd("C:/Users/nisri/OneDrive/Documents/Uni/Semester 5/Pemrograman Spasial/ETS/Folder")
getwd()
gdb_path <- "02_Administration_Data/Kab Bantul RBI BIG/2022_RBI25K_KAB_BANTUL_KUGI50_20221231.gdb"

dir_crop <- "03_Cropped"
dir_index <- "04_Index"
dir_klasifikasi <- "05_Classification"
dir_web <- "06_WebGIS"

dir.create(dir_crop, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_index, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_klasifikasi, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_web, showWarnings = FALSE, recursive = TRUE)


# ==============================================================
# FILTERING KABUPATEN & KECAMATAN
# ==============================================================

list_layer <- st_layers(gdb_path)
print(list_layer)


# Kabupaten ====================================================

kabupaten <- st_read(
  dsn = gdb_path,
  layer = "ADMINISTRASI_AR_KABKOTA") |>
  filter(WADMKK == "Bantul")

plot(st_geometry(kabupaten))


# Kecamatan ====================================================

kecamatan <- st_read(
  dsn = gdb_path,
  layer = "ADMINISTRASI_AR_KECAMATAN") |>
  filter(WADMKK == "Bantul")

plot(st_geometry(kecamatan))


kecamatan <- kecamatan |>
  st_zm() |>
  st_make_valid() |>
  st_transform(4326) |>
  mutate(kec_nama = tools::toTitleCase(tolower(trimws(as.character(WADMKC))))) |>
  group_by(kec_nama) |>
  summarise()

nrow(kecamatan)      # Bantul mempunyai 17 kecamatan
kecamatan$kec_nama


# =============================================================
# MEMBACA BAND SENTINEL
# =============================================================

dir_sentinel <- ("01_Satellite_Data/20261004/S2B_MSIL2A_20261004T023529_N0513_R089_T49MDM_20261004T075248.SAFE")

# Folder hasil ================================================

output_files <- file.path(
  dir_crop,
  c(
    "B02_Cropped.tif",
    "B03_Cropped.tif",
    "B04_Cropped.tif",
    "B05_Cropped.tif",
    "B08_Cropped.tif",
    "B11_Cropped.tif"
  )
)

# FALSE = Pakai hasil crop yang sudah ada
# TRUE  = Proses ulang semua band

force_reprocess <- TRUE

file_metadata <- list.files(dir_sentinel, pattern = "^MTD_MSIL2A\\.xml$", full.names = TRUE)
if (length(file_metadata) > 0) {
  baris_xml <- readLines(file_metadata[1], warn = FALSE)
  baris_offset <- grep("BOA_ADD_OFFSET", baris_xml, value = TRUE)
  if (length(baris_offset) > 0) {
    offset_boa <- suppressWarnings(as.numeric(sub(".*>\\s*(-?[0-9.]+)\\s*<.*", "\\1", baris_offset[1])))
    if (is.na(offset_boa)) offset_boa <- 0
  } else {
    offset_boa <- 0
  }
} else {
  offset_boa <- -1000
}

if (all(file.exists(output_files)) && !force_reprocess) {
  
  message("File crop sudah tersedia. Membaca hasil crop...")
  
  b2  <- rast(output_files[1])
  b3  <- rast(output_files[2])
  b4  <- rast(output_files[3])
  b5  <- rast(output_files[4])
  b8  <- rast(output_files[5])
  b11 <- rast(output_files[6])
  
} else {
  
  message("Memproses band Sentinel-2...")
  
  # Mencari Band ===============================================
  
  band_search <- function(pola) {
    hasil <- list.files(
      path = dir_sentinel,
      pattern = pola,
      recursive = TRUE,
      full.names = TRUE
    )
    hasil <- hasil[grepl("IMG_DATA", hasil)]
    
    if (length(hasil) == 0) {
      stop(paste("Band tidak ditemukan:", pola))
    }
    
    if (length(hasil) > 1) {
      stop(paste("File ditemukan lebih dari satu, pilih produk yang benar!"))
    }
    
    hasil
  }
  
  file_b2  <- band_search("_B02_10m\\.jp2$")
  file_b3  <- band_search("_B03_10m\\.jp2$")
  file_b4  <- band_search("_B04_10m\\.jp2$")
  file_b5  <- band_search("_B05_20m\\.jp2$")
  file_b8  <- band_search("_B08_10m\\.jp2$")
  file_b11 <- band_search("_B11_20m\\.jp2$")
  
  b8_full      <- rast(file_b8)
  
  # Perbaikan CRS ==============================================
  
  crs_b8 <- crs(b8_full)
  if (is.na(crs_b8) || !nzchar(crs_b8)) {
    message("CRS kosong, mengatur secara manual ke EPSG:32749...")
    crs(b8_full) <- "EPSG:32749"
  }
  
  bantul_terra <- project(vect(kabupaten), crs(b8_full))
  
  
  # Fungsi Untuk Band ==========================================
  
  siapkan_band <- function(file, acuan = NULL) {
    r <- crop(rast(file), bantul_terra, snap = "out")
    
    if (!is.null(acuan)) {
      r <- resample(r, acuan, method = "bilinear")
    }
    
    r <- classify(r, cbind(-Inf, 0, NA))
    r <- (r + offset_boa) / 10000
    r <- clamp(r, lower = 0, upper = 2, values = TRUE)
    
    mask(r, bantul_terra)
  }
  
  b8  <- siapkan_band(file_b8)
  b2  <- siapkan_band(file_b2)
  b3  <- siapkan_band(file_b3)
  b4  <- siapkan_band(file_b4)
  b5  <- siapkan_band(file_b5,  acuan = b8)
  b11 <- siapkan_band(file_b11, acuan = b8)
  
  writeRaster(b2,  output_files[1], overwrite = TRUE)
  writeRaster(b3,  output_files[2], overwrite = TRUE)
  writeRaster(b4,  output_files[3], overwrite = TRUE)
  writeRaster(b5,  output_files[4], overwrite = TRUE)
  writeRaster(b8,  output_files[5], overwrite = TRUE)
  writeRaster(b11, output_files[6], overwrite = TRUE)
}

# Pemeriksaan Singkat ==========================================

res(b8)
crs(b8)
global(b4, c("min", "max", "mean"), na.rm = TRUE)   # harus 0-1, bukan ribuan

# Cek visual ===================================================
plotRGB(c(b4, b3, b2), r = 1, g = 2, b = 3, stretch = "lin",
        main = "Citra Warna Asli Sentinel-2")


# ==============================================================
# MENGHITUNG INDEKS
# ==============================================================

# Buat Fungsi Indeks ===========================================

nd_index <- function(a, b){
  penyebut <- a + b
  
  ifel(penyebut == 0,
       NA,
       (a - b) / penyebut
  )
}


# Hitung Indeks ================================================

ndvi <- nd_index(b8,  b4)  # Vegetasi
ndbi <- nd_index(b11, b8)  # Area terbangun
ndwi <- nd_index(b3,  b8)  # Perairan
ndci <- nd_index(b4,  b8)  # Awan: (Red - NIR) / (Red + NIR)

names(ndvi) <- "NDVI"
names(ndbi) <- "NDBI"
names(ndwi) <- "NDWI"
names(ndci) <- "NDCI"

# Simpan raster indeks di satu lokasi saja
writeRaster(ndvi, file.path(dir_index, "NDVI.tif"), overwrite = TRUE)
writeRaster(ndbi, file.path(dir_index, "NDBI.tif"), overwrite = TRUE)
writeRaster(ndwi, file.path(dir_index, "NDWI.tif"), overwrite = TRUE)
writeRaster(ndci, file.path(dir_index, "NDCI.tif"), overwrite = TRUE)


# Check Hasilnya ===============================================

plot(ndvi, main = "NDVI Kabupaten Bantul")
plot(ndbi, main = "NDBI Kabupaten Bantul")
plot(ndwi, main = "NDWI Kabupaten Bantul")
plot(ndci, main = "NDCI Kabupaten Bantul")

global(ndvi, c("min", "max", "mean"), na.rm = TRUE)
global(ndbi, c("min", "max", "mean"), na.rm = TRUE)
global(ndwi, c("min", "max", "mean"), na.rm = TRUE)
global(ndci, c("min", "max", "mean"), na.rm = TRUE)


# ==============================================================
# MENENTUKAN THRESHOLD KLASIFIKASI
# ==============================================================

#  Threshold Awal ==============================================

thr_ndvi <- 0.3
thr_ndbi <- 0.0
thr_ndwi <- 0.05
thr_ndci <- 0.05
thr_biru <- 0.2

# Cek sebaran nilai + posisi threshold (garis merah) ===========

par(mfrow = c(2, 2))
hist(ndvi, breaks = 50, main = "NDVI"); abline(v = thr_ndvi, col = "red", lwd = 2)
hist(ndbi, breaks = 50, main = "NDBI"); abline(v = thr_ndbi, col = "red", lwd = 2)
hist(ndwi, breaks = 50, main = "NDWI"); abline(v = thr_ndwi, col = "red", lwd = 2)
hist(ndci, breaks = 50, main = "NDCI"); abline(v = c(-thr_ndci, thr_ndci), col = "red", lwd = 2)
par(mfrow = c(1, 1))


# Aturan Indikasi ==============================================

awan <- (abs(ndci) <= thr_ndci) & (b2 > thr_biru)

plot(awan, main = "Piksel Awan (cek dengan citra RGB)")

sisa_jadi_bangunan <- TRUE
sisa <- if (sisa_jadi_bangunan) 2 else NA_real_


# Prioritas: Awan (4) > Air (3) > Vegetasi (1) > Bangunan (2) ==

kelas <- ifel(awan, 4,
              ifel(ndwi > thr_ndwi, 3,
                   ifel(ndvi > thr_ndvi, 1,
                        ifel(ndbi > thr_ndbi, 2, sisa))))

names(kelas) <- "tutupan_lahan"

writeRaster(
  kelas,
  file.path(dir_klasifikasi, "klasifikasi_tutupan_lahan_bantul.tif"),
  overwrite = TRUE,
  datatype = "INT1U",
  NAflag = 0
)


# Tabel kelas ==================================================

LC <- data.frame(
  kode  = 1:4,
  kelas = c("Vegetasi", "Bangunan", "Perairan", "Awan"),
  warna = c("#358221", "#ED022A", "#1A5BAB", "#C8C8C8"),
  stringsAsFactors = FALSE
)

# Tabel jumlah piksel + persentase per kelas ===================

hasil_kelas <- freq(kelas)
hasil_kelas$kelas  <- LC$kelas[hasil_kelas$value]
hasil_kelas$persen <- round(100 * hasil_kelas$count / sum(hasil_kelas$count), 2)
print(hasil_kelas)

plot(
  kelas,
  col  = LC$warna,
  main = "Klasifikasi Tutupan Lahan Kab. Bantul"
)


# ==============================================================
# ZONAL STATISTICS
# ==============================================================

# Modus ========================================================

modus <- function(x) {
  tb <- table(round(x, 2))
  as.numeric(names(tb)[which.max(tb)])
}


# Fungsi zonal statistics ======================================

zonal_stats <- function(r, poligon) {
  
  nilai <- terra::extract(r, poligon)
  names(nilai) <- c("ID", "nilai")
  
  hasil <- nilai |>
    filter(is.finite(nilai)) |>
    group_by(ID) |>
    summarise(
      Mean   = mean(nilai),
      Min    = min(nilai),
      Max    = max(nilai),
      Median = median(nilai),
      SD     = sd(nilai),
      Modus  = modus(nilai)
    )
  
  data.frame(ID = seq_len(nrow(poligon))) |>
    left_join(hasil, by = "ID") |>
    select(-ID) |>
    as.data.frame()
}

kecamatan_terra <- project(vect(kecamatan), crs(ndvi))

indeks_list <- list(NDVI = ndvi, NDBI = ndbi, NDWI = ndwi, NDCI = ndci)
IDX_AVAIL   <- names(indeks_list)

# Tabel ========================================================

STATS_TABLE <- data.frame(Kecamatan = kecamatan$kec_nama)

for (k in IDX_AVAIL) {
  df <- zonal_stats(indeks_list[[k]], kecamatan_terra)
  names(df) <- paste0(k, "_", names(df))
  STATS_TABLE <- cbind(STATS_TABLE, round(df, 4))
}

print(STATS_TABLE)

# Statistik seluruh kabupaten ==================================

kabupaten_sf    <- st_sf(geometry = st_geometry(st_union(st_make_valid(st_zm(kabupaten)))))
kabupaten_terra <- project(vect(kabupaten_sf), crs(ndvi))

STATS_KAB <- NULL

for (k in IDX_AVAIL) {
  df <- zonal_stats(indeks_list[[k]], kabupaten_terra)
  STATS_KAB <- rbind(STATS_KAB, cbind(Indeks = k, round(df, 4)))
}

print(STATS_KAB)


# Luas tiap kelas tutupan lahan per kecamatan (hektar) =========

nilai_kelas <- terra::extract(kelas, kecamatan_terra)

luas_piksel_ha <- prod(res(kelas)) / 10000

tabel_kelas <- table(
  factor(nilai_kelas$ID, levels = seq_len(nrow(kecamatan))),
  factor(nilai_kelas[[2]], levels = LC$kode)
)

luas <- as.data.frame.matrix(tabel_kelas) * luas_piksel_ha
names(luas) <- LC$kelas
total_ha <- rowSums(luas)

dominan <- ifelse(
  total_ha > 0,
  LC$kelas[max.col(as.matrix(luas), ties.method = "first")],
  NA_character_
)

LC_AREA <- cbind(
  Kecamatan = kecamatan$kec_nama,
  round(luas, 2),
  Total_ha = round(total_ha, 2),
  Kelas_Dominan = dominan
)

LC_TOTAL <- colSums(luas)

print(LC_AREA)


# ==============================================================
# EKSPOR HASIL UNTUK WEB
# ==============================================================


# 1) Threshold =================================================

THRESH <- list(
  NDVI_VEGETASI = thr_ndvi,
  NDWI_PERAIRAN = thr_ndwi,
  NDBI_BANGUNAN = thr_ndbi,
  NDCI_AWAN_ABS = thr_ndci,
  BLUE_AWAN     = thr_biru
)

METODE_AWAN <- "ndci"
SCL_AWAN    <- c(8, 9, 10)     # hanya dicatat, tidak dipakai dalam proses

write.csv(
  data.frame(
    parameter = c(names(THRESH), "METODE_AWAN", "SISA_JADI_BANGUNAN", "BOA_ADD_OFFSET"),
    nilai     = c(unlist(THRESH), METODE_AWAN, sisa_jadi_bangunan, offset_boa)
  ),
  file.path(dir_web, "threshold_klasifikasi.csv"),
  row.names = FALSE
)


# 2) Tabel CSV =================================================

write.csv(STATS_TABLE, file.path(dir_web, "zonal_statistics_kecamatan.csv"),   row.names = FALSE)
write.csv(STATS_KAB,   file.path(dir_web, "statistik_kabupaten.csv"),          row.names = FALSE)
write.csv(LC_AREA,     file.path(dir_web, "luas_tutupan_lahan_kecamatan.csv"), row.names = FALSE)


# 3) GeoPackage (EPSG:4326) ====================================

kec_out <- st_sf(
  kec_nama = kecamatan$kec_nama,
  geometry = st_geometry(kecamatan)
)

for (kolom in setdiff(names(STATS_TABLE), "Kecamatan")) {
  kec_out[[kolom]] <- STATS_TABLE[[kolom]]
}

for (kl in LC$kelas) {
  kec_out[[paste0("PCT_", kl)]] <- round(100 * luas[[kl]] / total_ha, 2)
}

kec_out$Kelas_Dominan <- dominan

# Batas kabupaten
kab_out <- st_sf(
  nama     = "Kabupaten Bantul",
  geometry = st_geometry(st_transform(kabupaten_sf, 4326))
)

kab_ctx <- kab_out

file_gpkg <- file.path(dir_web, "hasil_analisis.gpkg")
unlink(file_gpkg)

st_write(kab_out, file_gpkg, layer = "batas_kabupaten", quiet = TRUE)
st_write(kec_out, file_gpkg, layer = "batas_kecamatan", quiet = TRUE)
st_write(kab_ctx, file_gpkg, layer = "konteks_wilayah", quiet = TRUE)


# 4) Metadata (.rds) ===========================================

# Tanggal citra diambil dari nama folder .SAFE (8 digit pertama = YYYYMMDD)
bulan_id <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli",
              "Agustus", "September", "Oktober", "November", "Desember")

tgl_txt <- regmatches(basename(dir_sentinel), regexpr("[0-9]{8}", basename(dir_sentinel)))
tgl     <- as.Date(tgl_txt, format = "%Y%m%d")

S2_TGL_TXT <- paste(
  as.integer(format(tgl, "%d")),
  bulan_id[as.integer(format(tgl, "%m"))],
  format(tgl, "%Y")
)

saveRDS(
  list(
    LC = LC, THRESH = THRESH, METODE_AWAN = METODE_AWAN, SCL_AWAN = SCL_AWAN,
    SISA_JADI_BANGUNAN = sisa_jadi_bangunan, S2_TGL_TXT = S2_TGL_TXT,
    UTM_EPSG_USE = 32749, NAMA_KAB = "Bantul", IDX_AVAIL = IDX_AVAIL,
    STATS_TABLE = STATS_TABLE, STATS_KAB = STATS_KAB,
    LC_AREA = LC_AREA, LC_TOTAL = LC_TOTAL,
    waktu = Sys.time()
  ),
  file.path(dir_web, "hasil_analisis.rds")
)

cat("\nSelesai.\n")
list.files(dir_web)