# =============================================================================
#  deploy.R  -  Publikasi WebGIS Bantul ke shinyapps.io  (link HTTPS)
# =============================================================================
#  URUTAN KERJA:
#   1. Jalankan 01_ANALISIS_RASTER.R  (menghasilkan folder hasil_olah)
#   2. Jalankan 02_WEBGIS_BANTUL.R secara lokal sampai tampil normal
#   3. Akun shinyapps.io didaftarkan otomatis oleh blok "AKUN" di bawah
#      (token: shinyapps.io > Account > Tokens > Show > Show Secret).
#   4. Sesuaikan 3 path di bawah, lalu jalankan skrip ini (Source).
#  Hasil: https://<USERNAME>.shinyapps.io/tutupan-lahan-bantul/
# =============================================================================

APP_FILE   <- "D:/TUBES PEMSPA/Data/02_WEBGIS_BANTUL.R"   # skrip WebGIS
DIR_OUTPUT <- "D:/TUBES PEMSPA/Data/hasil_olah"           # hasil 01_ANALISIS_RASTER.R
DIR_LOGO   <- "D:/TUBES PEMSPA/Data"                      # folder berisi logo_its.png & logo_geomatika.jpeg
APP_NAME   <- "tutupan-lahan-bantul"
AGG_FACT   <- 1   # 1 = resolusi asli 10 m. Jika memori server (1 GB) tidak cukup, pakai 2 (20 m).
                  # Statistik zonal tetap berasal dari analisis resolusi penuh.

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
library(terra)

# --- AKUN shinyapps.io ---------------------------------------------------------
# PERINGATAN: token & secret = kata sandi akun. Jangan upload skrip ini ke GitHub/LMS
# dengan isi ini; setelah tugas selesai, reset token di shinyapps.io (Account > Tokens).
ACCOUNT <- "azelsva"
rsconnect::setAccountInfo(name   = ACCOUNT,
                          token  = "E7EFDB6641E7964C8D79D94AC666B7CC",
                          secret = "orQvCZaNThJekwFQBNP4Aqe0xHcTxB1VIex+E1S6")

# --- cek skrip WebGIS (penyebab error "static content" = app.R tidak ikut ter-copy) ---
if (!file.exists(APP_FILE)) {
  cand <- list.files(dirname(APP_FILE), pattern = "WEBGIS.*\\.R$", full.names = TRUE,
                     ignore.case = TRUE)
  if (length(cand)) {
    message("[INFO] APP_FILE tidak ada, memakai: ", cand[1]); APP_FILE <- cand[1]
  } else stop("File WebGIS tidak ditemukan: ", APP_FILE,
              "\nPerbaiki APP_FILE di bagian atas deploy.R (nama & lokasi file harus persis).")
}

STAGE <- file.path(dirname(APP_FILE), "deploy_bantul")
unlink(STAGE, recursive = TRUE)                      # buang sisa staging lama
dd <- file.path(STAGE, "data_deploy")
dir.create(dd, recursive = TRUE, showWarnings = FALSE)

# --- raster hasil analisis (terkompresi) ---------------------------------------
rasters <- c(lc = "klasifikasi_tutupan_lahan_bantul.tif", NDVI = "NDVI_bantul.tif",
             NDBI = "NDBI_bantul.tif", NDWI = "NDWI_bantul.tif", NDCI = "NDCI_bantul.tif")
for (k in names(rasters)) {
  f <- file.path(DIR_OUTPUT, rasters[[k]])
  if (!file.exists(f)) { message("[LEWAT] tidak ada: ", f); next }
  r <- terra::rast(f)[[1]]
  if (AGG_FACT > 1) r <- terra::aggregate(r, AGG_FACT, fun = if (k == "lc") "modal" else "mean", na.rm = TRUE)
  out <- file.path(dd, rasters[[k]])
  if (k == "lc") {
    terra::writeRaster(r, out, overwrite = TRUE, datatype = "INT1U", NAflag = 0, gdal = "COMPRESS=DEFLATE")
  } else {
    terra::writeRaster(r, out, overwrite = TRUE, datatype = "FLT4S", gdal = "COMPRESS=DEFLATE")
  }
  message("[OK] ", rasters[[k]])
}

# --- GeoPackage hasil, metadata, logo, aplikasi -----------------------------------
for (f in c("hasil_analisis.gpkg", "hasil_analisis.rds")) {
  if (!file.copy(file.path(DIR_OUTPUT, f), file.path(dd, f), overwrite = TRUE))
    stop("Tidak ada ", f, " di ", DIR_OUTPUT, " - jalankan 01_ANALISIS_RASTER.R dulu.")
}
file.copy(file.path(DIR_LOGO, "logo_its.png"),       file.path(dd, "logo_its.png"),       overwrite = TRUE)
file.copy(file.path(DIR_LOGO, "logo_geomatika.jpeg"), file.path(dd, "logo_geomatika.jpeg"), overwrite = TRUE)
# shinyapps.io mewajibkan nama app.R
if (!file.copy(APP_FILE, file.path(STAGE, "app.R"), overwrite = TRUE) ||
    !file.exists(file.path(STAGE, "app.R")))
  stop("Gagal menyalin ", APP_FILE, " menjadi app.R di ", STAGE)
if (!any(grepl("shinyApp\\(", readLines(file.path(STAGE, "app.R"), warn = FALSE))))
  stop("app.R tidak berisi shinyApp(...). Pastikan APP_FILE = 02_WEBGIS_BANTUL.R, bukan 01_ANALISIS_RASTER.R.")

sz <- sum(file.size(list.files(STAGE, recursive = TRUE, full.names = TRUE))) / 1024^2
message(sprintf("[INFO] Ukuran paket: %.1f MB (%s)", sz, STAGE))
app_files <- list.files(STAGE, recursive = TRUE, all.files = FALSE)   # path relatif thd STAGE
message("[INFO] Isi paket:\n  ", paste(app_files, collapse = "\n  "))

# --- upload ----------------------------------------------------------------------
rsconnect::deployApp(appDir = STAGE, appFiles = app_files, appPrimaryDoc = "app.R",
                     appName = APP_NAME, account = ACCOUNT, server = "shinyapps.io",
                     appTitle = "Tutupan Lahan Interaktif Kab. Bantul", forceUpdate = TRUE)
message("[SELESAI] https://", ACCOUNT, ".shinyapps.io/", APP_NAME, "/")
