# =============================================================================
#  01_ANALISIS_RASTER.R  -  ANALISIS RASTER KAB. BANTUL 
#  Pemrograman Spasial A
# =============================================================================

Sys.unsetenv(c("PROJ_LIB", "PROJ_DATA", "GDAL_DATA"))

suppressPackageStartupMessages({
  pkgs <- c("sf", "terra")
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) > 0)
    stop("Package belum terpasang: ", paste(miss, collapse = ", "),
         "\nJalankan: install.packages(c(", paste0('"', miss, '"', collapse = ", "), "))", call. = FALSE)
  invisible(lapply(pkgs, library, character.only = TRUE))
})

# =============================================================================
# 1. KONFIGURASI
# =============================================================================
DIR_DATA   <- "D:/TUBES PEMSPA/Data"
DIR_OUTPUT <- file.path(DIR_DATA, "OUTPUT")

# --- Citra Sentinel-2 Level-2A ---
SAFE_PATH  <- "D:/TUBES PEMSPA/Data/S2B_MSIL2A_20261004T023529_N0513_R089_T49MDM_20261004T075248.SAFE"
FORCE_REPROCESS   <- TRUE    
                             
SIMPAN_CITRA_CROP <- TRUE    
S2_TANGGAL <- NA             

# --- GeoPackage batas administrasi ---------------------------
FILE_GPKG  <- file.path(DIR_DATA, "ADMINISTRASI_AR.gpkg")
LAYER_KAB  <- NA   
LAYER_KEC  <- NA   
NAMA_KAB   <- "Bantul"
KOLOM_KEC  <- NA   

# --- THRESHOLD KLASIFIKASI ---------------------------------------------------
THRESH <- list(
  NDVI_VEGETASI  = 0.30,   # NDVI  >  Vegetasi
  NDWI_PERAIRAN  = 0.05,   # NDWI  >  Perairan
  NDBI_BANGUNAN  = 0.00,   # NDBI  >  Bangunan
  NDCI_AWAN_ABS  = 0.05,   # |NDCI| <=   Awan
  BLUE_AWAN      = 0.20    
)
METODE_AWAN <- "ndci"       
SCL_AWAN    <- c(8, 9, 10)  
SISA_JADI_BANGUNAN <- TRUE   

# --- Berkas keluaran raster ---------------------------------------------------
FILE_RASTER <- list(
  lc   = file.path(DIR_OUTPUT, "klasifikasi_tutupan_lahan_bantul.tif"),
  NDVI = file.path(DIR_OUTPUT, "NDVI_bantul.tif"),
  NDBI = file.path(DIR_OUTPUT, "NDBI_bantul.tif"),
  NDWI = file.path(DIR_OUTPUT, "NDWI_bantul.tif"),
  NDCI = file.path(DIR_OUTPUT, "NDCI_bantul.tif")
)
FILE_CROP        <- file.path(DIR_OUTPUT, "sentinel2_crop_bantul.tif")
FILE_HASIL_GPKG  <- file.path(DIR_OUTPUT, "hasil_analisis.gpkg")
FILE_META        <- file.path(DIR_OUTPUT, "hasil_analisis.rds")

# --- Kelas tutupan lahan  ---------------------------------
LC <- data.frame(
  kode  = 1:4,
  kelas = c("Vegetasi", "Bangunan", "Perairan", "Awan"),
  warna = c("#2E9E44", "#E53935", "#1E88E5", "#CFCFCF"),
  stringsAsFactors = FALSE
)

UTM_EPSG <- NA               
MASK_RASTER_KE_KAB <- TRUE   
DEPLOY_MODE <- FALSE         

# =============================================================================
# 2. FUNGSI BANTU
# =============================================================================
pick_layer <- function(gpkg, pattern, idx) {
  ly <- sf::st_layers(gpkg)$name
  hit <- grep(pattern, ly, ignore.case = TRUE, value = TRUE)
  if (length(hit)) hit[1] else ly[min(idx, length(ly))]
}

read_boundary <- function(gpkg, layer, filter_cols, nama_kab) {
  x <- read_layer(gpkg, layer)
  if (is.na(sf::st_crs(x))) sf::st_crs(x) <- 4326
  x <- sf::st_zm(x)
  x <- sf::st_make_valid(x)
  x <- sf::st_transform(x, 4326)
  for (cl in filter_cols) {
    if (cl %in% names(x)) {
      idx <- grepl(nama_kab, as.character(x[[cl]]), ignore.case = TRUE)
      if (any(idx)) { x <- x[idx, ]; break }
    }
  }
  x
}

fmt_ha <- function(x) ifelse(is.na(x), "-",
                             format(round(x), big.mark = ".", decimal.mark = ","))

zonal_stats <- function(r, polys_v) {
  ex <- terra::extract(r, polys_v, ID = TRUE)
  v  <- ex[[2]]
  id <- factor(ex$ID, levels = seq_len(nrow(polys_v)))
  safe <- function(fn) function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) NA_real_ else fn(x)
  }
  g <- function(fn) as.numeric(tapply(v, id, safe(fn)))
  mode_fn <- function(x) {                     # modus (dibulatkan 2 desimal)
    tb <- table(round(x, 2))
    as.numeric(names(tb)[which.max(tb)])
  }
  data.frame(Mean = g(mean), Min = g(min), Max = g(max),
             Median = g(stats::median), SD = g(stats::sd), Modus = g(mode_fn))
}

WKT_BBOX <- "POLYGON((109.4 -8.9, 111.2 -8.9, 111.2 -7.0, 109.4 -7.0, 109.4 -8.9))"
WKT_BBOX_CTX <- "POLYGON((108.0 -9.6, 113.2 -9.6, 113.2 -5.9, 108.0 -5.9, 108.0 -9.6))"  
read_layer <- function(gpkg, layer, wkt = WKT_BBOX) {
  x <- tryCatch(sf::st_read(gpkg, layer = layer, quiet = TRUE, wkt_filter = wkt),
                error = function(e) NULL)
  if (is.null(x) || nrow(x) == 0) x <- sf::st_read(gpkg, layer = layer, quiet = TRUE)
  x
}

# ---- Sistem koordinat UTM -----------------------------------------------------
utm_epsg_auto <- function(x) {
  ctr <- suppressWarnings(sf::st_coordinates(
    sf::st_centroid(sf::st_union(sf::st_geometry(sf::st_transform(x, 4326))))))
  zone <- floor((ctr[1, 1] + 180) / 6) + 1
  as.integer(if (ctr[1, 2] >= 0) 32600 + zone else 32700 + zone)
}
utm_label <- function(epsg) sprintf("UTM Zona %d%s (EPSG:%d)", as.integer(epsg %% 100),
                                    if (epsg >= 32700) "S" else "N", as.integer(epsg))
utm_params_txt <- function(epsg) {
  z <- epsg %% 100
  sprintf("Meridian tengah %d derajat BT, faktor skala 0,9996, false easting 500.000 m, false northing %s",
          as.integer(6 * z - 183),
          if (epsg >= 32700) "10.000.000 m (belahan selatan)" else "0 m (belahan utara)")
}

MONTHS_ID <- c("Januari", "Februari", "Maret", "April", "Mei", "Juni", "Juli",
               "Agustus", "September", "Oktober", "November", "Desember")
tgl_indo <- function(d) {
  d <- as.Date(d)
  sprintf("%d %s %s", as.integer(format(d, "%d")),
          MONTHS_ID[as.integer(format(d, "%m"))], format(d, "%Y"))
}

# ---- Log  -------------------------------------------
LOG <- character(0)
lg <- function(...) {
  m <- paste0(...)
  message(m)
  LOG <<- c(LOG, paste(format(Sys.time(), "%H:%M:%S"), m))
  invisible(NULL)
}

to_vect <- function(x, r) {
  g <- sf::st_sf(geometry = sf::st_geometry(x))
  crs_r <- terra::crs(r)
  if (is.na(crs_r) || !nzchar(crs_r)) stop("Raster tidak punya CRS")
  terra::vect(sf::st_transform(g, sf::st_crs(crs_r)))
}

# baca file raster dengan pesan error yang jelas
rd <- function(f) {
  tryCatch(terra::rast(f), error = function(e)
    stop("Gagal membaca ", basename(f), ": ", conditionMessage(e),
         " (jika berkas .jp2: driver JP2/OpenJPEG pada GDAL mungkin tidak tersedia; ",
         "coba update package terra atau jalankan R dari instalasi yang punya OpenJPEG)",
         call. = FALSE))
}

# cari raster cadangan bila path di FILE_RASTER salah 
RASTER_PATTERN <- list(lc = "klasifikasi|tutupan|landcover|land_cover|lulc",
                       NDVI = "ndvi", NDBI = "ndbi", NDWI = "ndwi", NDCI = "ndci")
cari_raster <- function(k, dirs) {
  for (d in unique(dirs[!is.na(dirs) & dir.exists(dirs)])) {
    ff <- list.files(d, pattern = "\\.tiff?$", recursive = TRUE, full.names = TRUE,
                     ignore.case = TRUE)
    ff <- ff[grepl(RASTER_PATTERN[[k]], basename(ff), ignore.case = TRUE)]
    ff <- ff[!grepl("crop", basename(ff), ignore.case = TRUE)]
    if (length(ff)) return(ff[1])
  }
  NA_character_
}

# ---- Pemrosesan Sentinel-2 L2A (.SAFE) --------------------------------------
find_safe_root <- function(p, tmp_dir) {
  if (is.na(p) || !nzchar(p) || !file.exists(p)) return(NULL)
  if (grepl("\\.zip$", p, ignore.case = TRUE)) {
    lg("[INFO] Mengekstrak band yang diperlukan dari zip (mohon tunggu)...")
    lst  <- utils::unzip(p, list = TRUE)$Name
    keep <- lst[grepl(paste0("IMG_DATA/R10m/.*_(B02|B03|B04|B08)_10m\\.jp2$|",
                             "IMG_DATA/R20m/.*_(B11|SCL)_20m\\.jp2$|",
                             "MTD_MSIL2A\\.xml$"), lst)]
    if (!length(keep)) stop("Zip tidak berisi band L2A (R10m/R20m). Pastikan produk Level-2A.")
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    utils::unzip(p, files = keep, exdir = tmp_dir)
    p <- tmp_dir
  }
  if (dir.exists(file.path(p, "GRANULE"))) return(p)
  cand <- list.dirs(p, recursive = FALSE, full.names = TRUE)
  cand <- cand[grepl("\\.SAFE$", cand, ignore.case = TRUE)]
  if (length(cand)) return(cand[1])
  NULL
}

# Offset reflektansi (BOA_ADD_OFFSET) dari metadata; produk baru = -1000
read_boa_offset <- function(safe) {
  x <- list.files(safe, pattern = "^MTD_MSIL2A\\.xml$", full.names = TRUE)
  if (!length(x)) return(0)
  ln <- readLines(x[1], warn = FALSE)
  h  <- grep("BOA_ADD_OFFSET", ln, value = TRUE)
  if (!length(h)) return(0)
  v <- suppressWarnings(as.numeric(sub(".*>\\s*(-?[0-9.]+)\\s*<.*", "\\1", h[1])))
  if (is.na(v)) 0 else v
}

process_s2 <- function(safe, kab_sf, out, thr, metode_awan, scl_awan,
                       sisa_bangunan, file_crop, simpan_crop) {
  dir.create(dirname(out$lc), showWarnings = FALSE, recursive = TRUE)

  bf <- function(b, res) {
    f <- list.files(safe, pattern = sprintf("_%s_%sm\\.jp2$", b, res),
                    recursive = TRUE, full.names = TRUE)
    f <- f[grepl("IMG_DATA", f)]
    if (!length(f)) stop("Band ", b, " (", res, "m) tidak ditemukan di ", safe,
                         ". Pastikan produk Level-2A (MSIL2A), bukan Level-1C.")
    f[1]
  }

  lg("[PROSES] Membaca & memotong band (crop batas kabupaten)...")
  ref0 <- rd(bf("B08", "10"))

  crs_ref0 <- terra::crs(ref0)
  if (is.na(crs_ref0) || !nzchar(crs_ref0)) {
    lg("[PERINGATAN] CRS B08 tidak terbaca dari JP2; memakai EPSG:32749 berdasarkan tile T49.")
    terra::crs(ref0) <- "EPSG:32749"
    crs_ref0 <- terra::crs(ref0)
  }
  if (is.na(crs_ref0) || !nzchar(crs_ref0)) {
    stop("CRS raster B08 tetap kosong. Periksa instalasi GDAL/PROJ dan metadata Sentinel-2.")
  }
  kv <- to_vect(sf::st_union(kab_sf), ref0)
  eb <- as.vector(terra::ext(ref0)); ek <- as.vector(terra::ext(kv))
  ix <- max(0, min(eb[2], ek[2]) - max(eb[1], ek[1]))
  iy <- max(0, min(eb[4], ek[4]) - max(eb[3], ek[3]))
  cover <- (ix * iy) / ((ek[2] - ek[1]) * (ek[4] - ek[3]))
  if (cover <= 0)
    stop("Tile Sentinel-2 ini tidak menutupi Kab. Bantul. Unduh tile yang sesuai ",
         "(Bantul berada di sekitar 110.3 BT, 7.9 LS).")
  if (cover < 0.98)
    lg("[PERINGATAN] Tile hanya menutupi ", round(100 * cover),
       "% dari bounding box Kab. Bantul - hasil akan terpotong. Pertimbangkan tile lain/mosaik.")
  ref  <- terra::crop(ref0, kv, snap = "out")
  offs <- read_boa_offset(safe)
  lg("[INFO] BOA_ADD_OFFSET = ", offs)

  ld <- function(b, res) {
    r <- terra::crop(rd(bf(b, res)), kv, snap = "out")
    if (res != "10" || !terra::compareGeom(r, ref, stopOnError = FALSE))
      r <- terra::resample(r, ref, method = "bilinear")
    r <- terra::ifel(r == 0, NA, r)              
    r <- (r + offs) / 10000                      
    terra::clamp(r, lower = 0, upper = 2, values = TRUE)
  }
  b02 <- ld("B02", "10"); b03 <- ld("B03", "10"); b04 <- ld("B04", "10")
  b08 <- ld("B08", "10"); b11 <- ld("B11", "20")
  msk <- function(r) terra::mask(r, kv)

  lg("[PROSES] Menghitung indeks spektral...")
  ndvi <- (b08 - b04) / (b08 + b04)   # vegetasi
  ndbi <- (b11 - b08) / (b11 + b08)   # area terbangun
  ndwi <- (b03 - b08) / (b03 + b08)   # perairan 
  ndci <- (b04 - b08) / (b04 + b08)   # NDCI = (Red - NIR) / (Red + NIR)
  names(ndvi) <- "NDVI"; names(ndbi) <- "NDBI"
  names(ndwi) <- "NDWI"; names(ndci) <- "NDCI"

  lg("[PROSES] Klasifikasi tutupan lahan (threshold)...")
  cl_ndci <- (abs(ndci) <= thr$NDCI_AWAN_ABS) & (b02 > thr$BLUE_AWAN)
  cl_scl  <- NULL
  if (metode_awan %in% c("scl", "keduanya")) {
    scl <- terra::resample(terra::crop(rd(bf("SCL", "20")), kv, snap = "out"),
                           ref, method = "near")
    cl_scl <- scl %in% scl_awan
  }
  cloud <- switch(metode_awan,
                  ndci = cl_ndci, scl = cl_scl, keduanya = cl_ndci | cl_scl,
                  stop("METODE_AWAN harus 'ndci', 'scl', atau 'keduanya'"))

  #  Awan > Perairan > Vegetasi > Bangunan
  c_awan <- terra::ifel(cloud, 4, NA)
  c_air  <- terra::ifel(ndwi > thr$NDWI_PERAIRAN, 3, NA)
  c_veg  <- terra::ifel(ndvi > thr$NDVI_VEGETASI, 1, NA)
  c_bgn  <- terra::ifel(ndbi > thr$NDBI_BANGUNAN, 2, NA)
  lc <- terra::cover(terra::cover(terra::cover(c_awan, c_air), c_veg), c_bgn)
  if (isTRUE(sisa_bangunan)) lc <- terra::cover(lc, terra::ifel(!is.na(ndvi), 2, NA))
  names(lc) <- "tutupan_lahan"

  lg("[PROSES] Menyimpan hasil ke: ", dirname(out$lc))
  wr <- function(r, f, dt, ...) terra::writeRaster(msk(r), f, overwrite = TRUE,
                                                  datatype = dt, ...)
  wr(ndvi, out$NDVI, "FLT4S"); wr(ndbi, out$NDBI, "FLT4S")
  wr(ndwi, out$NDWI, "FLT4S"); wr(ndci, out$NDCI, "FLT4S")
  wr(lc, out$lc, "INT1U", NAflag = 0)

  if (isTRUE(simpan_crop)) {
    st <- c(b02, b03, b04, b08, b11)
    names(st) <- c("B02", "B03", "B04", "B08", "B11")
    terra::writeRaster(msk(st), file_crop, overwrite = TRUE, datatype = "FLT4S",
                       gdal = "COMPRESS=DEFLATE")
  }

  # dokumentasi threshold
  utils::write.csv(
    data.frame(parameter = c(names(thr), "METODE_AWAN", "SISA_JADI_BANGUNAN", "BOA_ADD_OFFSET"),
               nilai = c(unlist(thr), metode_awan, sisa_bangunan, offs)),
    file.path(dirname(out$lc), "threshold_klasifikasi.csv"), row.names = FALSE)
  lg("[OK] Pemrosesan Sentinel-2 selesai.")
  invisible(TRUE)
}

# =============================================================================
# 3. MUAT DATA 
# =============================================================================
kab <- NULL; kec <- NULL; kab_line <- NULL; kec_line <- NULL; kab_ctx <- NULL

if (file.exists(FILE_GPKG)) {
  tryCatch({
    layer_names <- sf::st_layers(FILE_GPKG)$name
    lg("[INFO] Layer di GeoPackage: ", paste(layer_names, collapse = ", "))

    lk_hit <- if (is.na(LAYER_KAB)) grep("kab|kabupaten|kabkota", layer_names, ignore.case = TRUE, value = TRUE) else LAYER_KAB
    lc_hit <- if (is.na(LAYER_KEC)) grep("kec|kecamatan", layer_names, ignore.case = TRUE, value = TRUE) else LAYER_KEC

    if (length(lk_hit) > 0 && length(lc_hit) > 0) {
      lk <- lk_hit[1]; lc_ <- lc_hit[1]
      lg("[INFO] Layer dipakai -> kabupaten: ", lk, " | kecamatan: ", lc_)
      kab_raw <- read_boundary(FILE_GPKG, lk,
        c("WADMKK", "KABKOT", "KAB_KOTA", "NAME_2", "NAMOBJ"), NAMA_KAB)
      kec_raw <- read_boundary(FILE_GPKG, lc_,
        c("WADMKK", "KABKOT", "KAB_KOTA", "NAME_2"), NAMA_KAB)
      kab <- sf::st_sf(nama = paste("Kabupaten", NAMA_KAB),
                       geometry = sf::st_union(sf::st_geometry(kab_raw)))
      kcol <- if (!is.na(KOLOM_KEC)) KOLOM_KEC else {
        h <- intersect(c("WADMKC", "KECAMATAN", "NAMOBJ", "NAME_3", "KEC"), names(kec_raw))
        if (length(h)) h[1] else NA_character_
      }
      if (is.na(kcol)) stop("Kolom nama kecamatan tidak ditemukan pada layer ", lc_, ".")
      kec_raw$kec_nama <- tools::toTitleCase(tolower(trimws(as.character(kec_raw[[kcol]]))))
      kec_nama_unik <- sort(unique(kec_raw$kec_nama[!is.na(kec_raw$kec_nama) & nzchar(kec_raw$kec_nama)]))
      kec_geom <- lapply(kec_nama_unik, function(nm) {
        sf::st_union(sf::st_geometry(kec_raw[kec_raw$kec_nama == nm, ]))
      })
      kec <- sf::st_sf(kec_nama = kec_nama_unik, geometry = do.call(c, kec_geom))
    } else {
      lk <- if (!is.na(LAYER_KAB)) LAYER_KAB else layer_names[1]
      lg("[INFO] Tidak ditemukan layer khusus kabupaten/kecamatan; memeriksa layer ", lk)
      x <- read_layer(FILE_GPKG, lk)
      if (is.na(sf::st_crs(x))) stop("CRS GeoPackage tidak diketahui: ", lk)
      x <- sf::st_transform(sf::st_make_valid(sf::st_zm(x)), 4326)
      kabcol <- intersect(c("WADMKK", "KABKOT", "KAB_KOTA", "NAME_2"), names(x))
      kec_candidates <- intersect(c("WADMKC", "KECAMATAN", "NAME_3", "KEC"), names(x))
      if (!length(kabcol)) stop("Kolom kabupaten tidak ditemukan pada layer ", lk, ".")
      is_bantul <- grepl(NAMA_KAB, as.character(x[[kabcol[1]]]), ignore.case = TRUE)
      x <- x[!is.na(is_bantul) & is_bantul, ]
      if (!nrow(x)) stop("Tidak ditemukan fitur Kabupaten ", NAMA_KAB, " pada kolom ", kabcol[1], ".")
      kab <- sf::st_sf(nama = paste("Kabupaten", NAMA_KAB),
                       geometry = sf::st_union(sf::st_geometry(x)))
      if (length(kec_candidates)) {
        x$kec_nama <- tools::toTitleCase(tolower(trimws(as.character(x[[kec_candidates[1]]]))))
        kec_nama_unik <- sort(unique(x$kec_nama[!is.na(x$kec_nama) & nzchar(x$kec_nama)]))
        kec_geom <- lapply(kec_nama_unik, function(nm) {
          sf::st_union(sf::st_geometry(x[x$kec_nama == nm, ]))
        })
        kec <- sf::st_sf(kec_nama = kec_nama_unik, geometry = do.call(c, kec_geom))
      } else stop("Kolom nama kecamatan tidak ditemukan pada layer ", lk,
                  ". Periksa atribut GeoPackage dan isi KOLOM_KEC.")
    }

    kec$kec_nama <- trimws(as.character(kec$kec_nama))
    kec <- kec[!is.na(kec$kec_nama) & nzchar(kec$kec_nama), ]
    kec <- kec[order(kec$kec_nama), ]
    kab_line <- sf::st_sf(geometry = sf::st_boundary(sf::st_union(kab)))
    kec_line <- suppressWarnings(sf::st_boundary(kec))

    # Layer konteks inset 
    kab_ctx <- tryCatch({
      lcx <- grep("konteks|kabupaten|kabkota|administrasi", layer_names,
                  ignore.case = TRUE, value = TRUE)
      if (!length(lcx)) lcx <- layer_names[1]
      xctx <- read_layer(FILE_GPKG, lcx[1], WKT_BBOX_CTX)
      if (is.na(sf::st_crs(xctx))) {
        NULL
      } else {
        xctx <- sf::st_transform(sf::st_make_valid(sf::st_zm(xctx)), 4326)
        win <- sf::st_as_sfc(sf::st_bbox(c(xmin = 108.0, ymin = -9.6, xmax = 113.2, ymax = -5.9),
                                         crs = sf::st_crs(4326)))
        xctx <- xctx[lengths(sf::st_intersects(xctx, win)) > 0, ]
        nc <- intersect(c("WADMKK", "NAMOBJ", "NAME_2", "KABKOT", "nama"), names(xctx))
        xctx$nama <- if (length(nc)) as.character(xctx[[nc[1]]]) else NA_character_
        xctx[, "nama", drop = FALSE]
      }
    }, error = function(e) { lg("[PERINGATAN] Layer konteks inset: ", conditionMessage(e)); NULL })
    lg("[OK] Batas berhasil disiapkan: ", nrow(kab), " poligon kabupaten dan ",
       nrow(kec), " kecamatan.")
  }, error = function(e) {
    kab <<- NULL; kec <<- NULL
    lg("[GAGAL] Membaca GeoPackage: ", conditionMessage(e))
  })
} else {
  lg("[GAGAL] GeoPackage tidak ditemukan: ", FILE_GPKG)
}

# --- Sistem koordinat (UTM) untuk peta cetak -----------------------------------
UTM_EPSG_USE <- if (!is.na(UTM_EPSG)) as.integer(UTM_EPSG) else
  if (!is.null(kab)) utm_epsg_auto(kab) else 32749L
lg("[INFO] Sistem koordinat peta cetak: ", utm_label(UTM_EPSG_USE))

# --- Tanggal perekaman citra  ---------------------------
S2_TGL_TXT <- S2_TANGGAL
if (is.na(S2_TGL_TXT) && !is.na(SAFE_PATH)) {
  mm_ <- regmatches(basename(SAFE_PATH), regexpr("[0-9]{8}T[0-9]{6}", basename(SAFE_PATH)))
  if (length(mm_) == 1) S2_TGL_TXT <- tgl_indo(as.Date(substr(mm_, 1, 8), format = "%Y%m%d"))
}
if (is.na(S2_TGL_TXT) && file.exists("data_deploy/s2_tanggal.txt"))
  S2_TGL_TXT <- readLines("data_deploy/s2_tanggal.txt", warn = FALSE)[1]

# --- Proses Sentinel-2 .SAFE -> indeks + klasifikasi --------------------------
if (!is.na(SAFE_PATH)) {
  butuh <- FORCE_REPROCESS || !all(file.exists(unlist(FILE_RASTER)))
  if (butuh) {
    if (is.null(kab)) {
      lg("[GAGAL] Pemrosesan .SAFE butuh batas kabupaten (cek FILE_GPKG).")
    } else {
      tryCatch({
        safe <- find_safe_root(SAFE_PATH, file.path(tempdir(), "s2safe"))
        if (is.null(safe)) stop("Folder .SAFE / zip tidak ditemukan di: ", SAFE_PATH)
        lg("[INFO] Memproses: ", basename(safe))
        process_s2(safe, kab, FILE_RASTER, THRESH, METODE_AWAN, SCL_AWAN,
                   SISA_JADI_BANGUNAN, FILE_CROP, SIMPAN_CITRA_CROP)
      }, error = function(e) lg("[GAGAL] Proses Sentinel-2: ", conditionMessage(e)))
    }
  } else {
    lg("[INFO] Hasil olahan sudah ada di ", DIR_OUTPUT,
            " (set FORCE_REPROCESS <- TRUE untuk hitung ulang).")
  }
}

# --- Raster ------------------------------------------------------------------
R_FULL <- list()
RASTER_STATUS <- data.frame(raster = names(FILE_RASTER), file = NA_character_,
                            ada = FALSE, dimuat = FALSE, catatan = "", stringsAsFactors = FALSE)
for (i in seq_along(FILE_RASTER)) {
  k <- names(FILE_RASTER)[i]
  f <- FILE_RASTER[[i]]
  if (is.na(f) || !file.exists(f)) {
    alt <- cari_raster(k, c(DIR_OUTPUT, DIR_DATA, getwd(), "data_deploy"))
    if (!is.na(alt)) { lg("[INFO] Raster ", k, ": path di FILE_RASTER tidak ada, memakai ", alt); f <- alt }
  }
  RASTER_STATUS$file[i] <- if (is.na(f)) "(tidak ada)" else f
  if (is.na(f) || !file.exists(f)) {
    lg("[INFO] Raster ", k, " belum ada: ", ifelse(is.na(f), "-", f))
    RASTER_STATUS$catatan[i] <- "file tidak ditemukan"
    next
  }
  RASTER_STATUS$ada[i] <- TRUE
  tryCatch({
    r <- terra::rast(f)[[1]]
    if (is.na(terra::crs(r)) || !nzchar(terra::crs(r))) {
      lg("[PERINGATAN] Raster ", k, " tanpa CRS -> diasumsikan ", utm_label(UTM_EPSG_USE))
      terra::crs(r) <- paste0("EPSG:", UTM_EPSG_USE)
    }
    if (MASK_RASTER_KE_KAB && !DEPLOY_MODE && !is.null(kab)) {
      kv <- to_vect(sf::st_union(kab), r)
      r2 <- tryCatch(terra::mask(terra::crop(r, kv), kv), error = function(e) NULL)
      if (is.null(r2)) {
        lg("[PERINGATAN] Raster ", k, " tidak beririsan dengan batas kabupaten ",
           "(pastikan tile Sentinel-2 menutupi Bantul & batas benar). Ditampilkan tanpa crop.")
        RASTER_STATUS$catatan[i] <- "tidak beririsan dgn batas kabupaten"
      } else r <- r2
    }
    if (k == "lc") {
      mm <- terra::minmax(terra::setMinMax(r))
      if (any(!is.finite(mm)) || mm[1] < min(LC$kode) || mm[2] > max(LC$kode))
        lg("[PERINGATAN] Raster klasifikasi bernilai ", mm[1], "..", mm[2],
           " padahal kode kelas yang diharapkan ", min(LC$kode), "-", max(LC$kode),
           " (cek tabel LC).")
    }
    R_FULL[[k]] <- r
    RASTER_STATUS$dimuat[i] <- TRUE
    lg("[OK] Raster ", k, " dimuat (", terra::ncell(r), " piksel).")
  }, error = function(e) {
    RASTER_STATUS$catatan[i] <<- conditionMessage(e)
    lg("[GAGAL] Raster ", k, ": ", conditionMessage(e))
  })
}
if (length(R_FULL) == 0) {
  stop("Tidak ada raster yang berhasil dibuat/dimuat. Periksa pesan [GAGAL] di atas; " ,
       "jangan jalankan 02_WEBGIS_BANTUL.R sebelum kelima raster berhasil dibuat.", call. = FALSE)
}


IDX_AVAIL <- setdiff(names(R_FULL), "lc")

# sampel nilai indeks untuk histogram threshold
SAMPLES <- lapply(IDX_AVAIL, function(k) {
  s <- tryCatch(as.numeric(terra::spatSample(R_FULL[[k]], 1e5, method = "regular",
                                             as.df = FALSE)),
                error = function(e) numeric(0))
  s[is.finite(s)]
})
names(SAMPLES) <- IDX_AVAIL

# --- Statistik zonal per kecamatan -------------------------------------------
STATS_TABLE <- NULL
if (!is.null(kec) && length(IDX_AVAIL) > 0) {
  tryCatch({
    st <- data.frame(Kecamatan = kec$kec_nama, stringsAsFactors = FALSE)
    for (k in IDX_AVAIL) {
      r  <- R_FULL[[k]]
      pv <- to_vect(kec, r)
      df <- zonal_stats(r, pv)
      names(df) <- paste0(k, "_", names(df))
      st <- cbind(st, round(df, 4))
    }
    STATS_TABLE <- st
    for (cn in setdiff(names(st), "Kecamatan")) kec[[cn]] <- st[[cn]]
    lg("[OK] Zonal statistics selesai.")
  }, error = function(e) lg("[GAGAL] Zonal statistics: ", conditionMessage(e)))
}

# --- Statistik zonal seluruh kabupaten  --
STATS_KAB <- NULL
if (!is.null(kab) && length(IDX_AVAIL) > 0) {
  tryCatch({
    STATS_KAB <- do.call(rbind, lapply(IDX_AVAIL, function(k) {
      r  <- R_FULL[[k]]
      df <- zonal_stats(r, to_vect(sf::st_union(sf::st_geometry(kab)), r))
      cbind(Indeks = k, round(df, 4))
    }))
    lg("[OK] Statistik seluruh kabupaten dihitung.")
  }, error = function(e) lg("[GAGAL] Statistik kabupaten: ", conditionMessage(e)))
}

# --- Luas tiap kelas tutupan lahan per kecamatan (hektar) --------------------
LC_AREA <- NULL; LC_TOTAL <- setNames(rep(NA_real_, nrow(LC)), LC$kelas)
if (!is.null(kec) && "lc" %in% names(R_FULL)) {
  tryCatch({
    r  <- R_FULL$lc
    pv <- to_vect(kec, r)
    ex <- terra::extract(r, pv, ID = TRUE)
    cell_ha <- if (terra::is.lonlat(r)) mean(terra::values(terra::cellSize(r, unit = "ha")), na.rm = TRUE)
               else prod(terra::res(r)) / 10000
    tab <- table(factor(ex$ID, levels = seq_len(nrow(kec))),
                 factor(ex[[2]], levels = LC$kode))
    ar <- as.data.frame.matrix(tab) * cell_ha
    names(ar) <- LC$kelas
    tot_ <- rowSums(ar)
    dom <- ifelse(tot_ > 0, LC$kelas[max.col(as.matrix(ar), ties.method = "first")], NA_character_)
    LC_AREA <- cbind(Kecamatan = kec$kec_nama, round(ar, 2), Total_ha = round(tot_, 2),
                     Kelas_Dominan = dom)
    LC_TOTAL <- colSums(ar)
    for (kl in LC$kelas) kec[[paste0("PCT_", kl)]] <- round(100 * ar[[kl]] / tot_, 2)
    kec$Kelas_Dominan <- dom
    lg("[OK] Luas kelas tutupan lahan dihitung.")
  }, error = function(e) lg("[GAGAL] Luas kelas: ", conditionMessage(e)))
}


# =============================================================================
# 4. EKSPOR HASIL ANALISIS 
# =============================================================================
dir.create(DIR_OUTPUT, showWarnings = FALSE, recursive = TRUE)

# --- 4a. Tabel CSV ------------------------------------------------------------
if (!is.null(STATS_TABLE)) {
  utils::write.csv(STATS_TABLE, file.path(DIR_OUTPUT, "zonal_statistics_kecamatan.csv"), row.names = FALSE)
  lg("[OK] zonal_statistics_kecamatan.csv  (satu baris per kecamatan: Mean, Min, Max, Median, SD, Modus)")
}
if (!is.null(STATS_KAB))
  utils::write.csv(STATS_KAB, file.path(DIR_OUTPUT, "statistik_kabupaten.csv"), row.names = FALSE)
if (!is.null(LC_AREA))
  utils::write.csv(LC_AREA, file.path(DIR_OUTPUT, "luas_tutupan_lahan_kecamatan.csv"), row.names = FALSE)

# --- 4b. GeoPackage hasil (EPSG:4326; dipakai WebGIS) ---------------------------
if (!is.null(kab) && !is.null(kec)) {
  keep <- c("kec_nama",
            if (!is.null(STATS_TABLE)) setdiff(names(STATS_TABLE), "Kecamatan"),
            grep("^PCT_", names(kec), value = TRUE), "Kelas_Dominan")
  kec_out <- kec[, intersect(keep, names(kec))]
  kab_out <- sf::st_sf(nama = paste("Kabupaten", NAMA_KAB),
                       geometry = sf::st_union(sf::st_geometry(kab)))
  unlink(FILE_HASIL_GPKG)
  sf::st_write(kab_out, FILE_HASIL_GPKG, layer = "batas_kabupaten", quiet = TRUE)
  sf::st_write(kec_out, FILE_HASIL_GPKG, layer = "batas_kecamatan", quiet = TRUE)
  if (!is.null(kab_ctx))
    sf::st_write(kab_ctx, FILE_HASIL_GPKG, layer = "konteks_wilayah", quiet = TRUE)
  lg("[OK] ", basename(FILE_HASIL_GPKG), " ditulis.")
} else lg("[GAGAL] Batas administrasi belum terbaca - GeoPackage hasil tidak dibuat.")

# --- 4c. Metadata analisis (RDS) -----------------------------------------------
saveRDS(list(
  LC = LC, THRESH = THRESH, METODE_AWAN = METODE_AWAN, SCL_AWAN = SCL_AWAN,
  SISA_JADI_BANGUNAN = SISA_JADI_BANGUNAN, S2_TGL_TXT = S2_TGL_TXT,
  UTM_EPSG_USE = UTM_EPSG_USE, NAMA_KAB = NAMA_KAB, IDX_AVAIL = IDX_AVAIL,
  STATS_TABLE = STATS_TABLE, STATS_KAB = STATS_KAB, LC_AREA = LC_AREA, LC_TOTAL = LC_TOTAL,
  SAMPLES = SAMPLES, waktu = Sys.time()
), FILE_META)
lg("[OK] ", basename(FILE_META), " ditulis.")

# --- 4d. Ringkasan di Console --------------------------------------------------
if (!is.null(STATS_TABLE)) { cat("\n=== STATISTIK ZONAL PER KECAMATAN ===\n"); print(STATS_TABLE, row.names = FALSE) }
if (!is.null(STATS_KAB))   { cat("\n=== STATISTIK SELURUH KABUPATEN ===\n");  print(STATS_KAB, row.names = FALSE) }
if (!is.null(LC_AREA))     { cat("\n=== LUAS TUTUPAN LAHAN (ha) PER KECAMATAN ===\n"); print(LC_AREA, row.names = FALSE) }
cat("\nSelesai. Hasil tersimpan di: ", DIR_OUTPUT, "\nLanjut: jalankan 02_WEBGIS.R\n", sep = "")
