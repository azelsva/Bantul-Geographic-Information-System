# WEBGIS SHINY DAN LEAFLET

# ETS - PEMROGRAMAN SPASIAL (CM234529) -  KELAS A

# Nama :
# Nisrin Finira Safina    (5016241051)
# Avisa Azalia            (5016241098)

# Dosen Pengampu :
# Mohammad Rohmaneo Darminto, S.T., M.Sc.
# NIP. 1990201911078

# Wilayah Studi:
# Kabupaten Bantul, Daerah Istimewa Yogyakarta



# PACKAGES

Sys.unsetenv(c("PROJ_LIB", "PROJ_DATA", "GDAL_DATA"))

suppressPackageStartupMessages({
  pkgs <- c("shiny", "shinydashboard", "shinyWidgets", "leaflet",
            "leaflet.extras", "sf", "terra", "DT", "plotly",
            "png", "jpeg", "maptiles", "htmlwidgets")
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) > 0)
    stop("Package belum terpasang: ", paste(miss, collapse = ", "),
         "\nJalankan: install.packages(c(",
         paste0('"', miss, '"', collapse = ", "), "))", call. = FALSE)
  if (utils::packageVersion("leaflet") < "2.2.0")
    stop("Versi leaflet terlalu lama. Jalankan: install.packages('leaflet')", call. = FALSE)
  invisible(lapply(pkgs, library, character.only = TRUE))
})

# =============================================================================
# 1. FILE & DATA
# =============================================================================

# Cari folder proyek supaya script bisa dijalankan di komputer lain.
get_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) return(sub("^--file=", "", file_arg[1]))
  
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    path <- tryCatch(rstudioapi::getActiveDocumentContext()$path,
                     error = function(e) "")
    if (length(path) && nzchar(path)) return(path)
  }
  ""
}

script_path <- get_script_path()
folder_awal <- unique(c(
  getwd(),
  if (nzchar(script_path)) dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)),
  if (nzchar(script_path)) dirname(dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)))
))

cari_folder_proyek <- function(folders) {
  for (folder in folders) {
    folder <- normalizePath(folder, winslash = "/", mustWork = FALSE)
    calon <- folder
    for (i in 1:5) {
      if (dir.exists(file.path(calon, "04_Index")) ||
          dir.exists(file.path(calon, "05_Classification")) ||
          dir.exists(file.path(calon, "06_WebGIS")) ||
          file.exists(file.path(calon, "Bantul_WebGIS.Rproj"))) {
        return(calon)
      }
      induk <- dirname(calon)
      if (identical(induk, calon)) break
      calon <- induk
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

DIR_PROYEK <- cari_folder_proyek(folder_awal)
DIR_INDEX <- file.path(DIR_PROYEK, "04_Index")
DIR_KLASIFIKASI <- file.path(DIR_PROYEK, "05_Classification")
DIR_OUTPUT <- file.path(DIR_PROYEK, "06_WebGIS")
DIR_DEPLOY <- file.path(DIR_PROYEK, "data_deploy")
DIR_ASSETS <- file.path(DIR_PROYEK, "assets")

# Untuk aplikasi online, semua file yang dibutuhkan diletakkan di data_deploy.
file_deploy_lengkap <- file.path(DIR_DEPLOY, c(
  "klasifikasi_tutupan_lahan_bantul.tif", "NDVI.tif", "NDBI.tif",
  "NDWI.tif", "NDCI.tif", "hasil_analisis.gpkg", "hasil_analisis.rds"
))
DEPLOY_MODE <- all(file.exists(file_deploy_lengkap))

if (DEPLOY_MODE) {
  DIR_OUTPUT <- DIR_DEPLOY
  DIR_INDEX <- DIR_DEPLOY
  DIR_KLASIFIKASI <- DIR_DEPLOY
}

# Logo disimpan di folder assets agar path-nya tidak bergantung pada komputer tertentu.
LOGO_ITS <- file.path(DIR_ASSETS, "logo_its.png")
LOGO_GEO <- file.path(DIR_ASSETS, "logo_geomatika.png")
if (DEPLOY_MODE) {
  if (file.exists(file.path(DIR_DEPLOY, "logo_its.png")))
    LOGO_ITS <- file.path(DIR_DEPLOY, "logo_its.png")
  if (file.exists(file.path(DIR_DEPLOY, "logo_geomatika.png")))
    LOGO_GEO <- file.path(DIR_DEPLOY, "logo_geomatika.png")
  if (file.exists(file.path(DIR_DEPLOY, "logo_geomatika.jpeg")))
    LOGO_GEO <- file.path(DIR_DEPLOY, "logo_geomatika.jpeg")
}

# Identitas pada layout peta cetak
PETA_INFO <- list(
  instansi = c("INSTITUT TEKNOLOGI SEPULUH NOPEMBER",
               "FAKULTAS TEKNIK SIPIL, PERENCANAAN, DAN KEBUMIAN",
               "SURABAYA 2026"),
  matkul = "Pemrograman Spasial A",
  dosen = "Mohammad Rohmaneo Darminto, S.T, M.Sc.",
  nip = "1990201911078",
  anggota = c("Nisrin Finira Safina (5016241051)",
              "Avisa Azalia (5016241098)")
)

FILE_RASTER <- list(
  lc = file.path(DIR_KLASIFIKASI, "klasifikasi_tutupan_lahan_bantul.tif"),
  NDVI = file.path(DIR_INDEX, "NDVI.tif"),
  NDBI = file.path(DIR_INDEX, "NDBI.tif"),
  NDWI = file.path(DIR_INDEX, "NDWI.tif"),
  NDCI = file.path(DIR_INDEX, "NDCI.tif")
)
FILE_HASIL_GPKG <- file.path(DIR_OUTPUT, "hasil_analisis.gpkg")
FILE_META <- file.path(DIR_OUTPUT, "hasil_analisis.rds")

# --- Tampilan ------------------------------------------------------------------
IDX_RANGE <- list(NDVI = c(-0.2, 0.9), NDBI = c(-0.6, 0.6),
                  NDWI = c(-0.6, 0.6), NDCI = c(-0.9, 0.5))
IDX_PAL   <- list(NDVI = "RdYlGn", NDBI = "YlOrRd", NDWI = "Blues", NDCI = "BuPu")
UTM_EPSG   <- NA                                          
PETA_FONT  <- if (DEPLOY_MODE) "serif" else "Times New Roman"   # font peta (server Linux tak punya TNR)
SKALA_BAKU <- c(25000, 50000, 100000, 150000, 200000)     # skala 
MAX_CELL_TAMPIL <- 1.2e6   

# =============================================================================
# 2. FUNGSI BANTU (ringkas)
# =============================================================================
fmt_ha <- function(x) ifelse(is.na(x), "-",
                             format(round(x), big.mark = ".", decimal.mark = ","))


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

# ---- Log Rstudio -------------------------------------------
LOG <- character(0)
lg <- function(...) {
  m <- paste0(...)
  message(m)
  LOG <<- c(LOG, paste(format(Sys.time(), "%H:%M:%S"), m))
  invisible(NULL)
}

# sf -> SpatVector pada CRS raster (hanya geometri; urutan baris dipertahankan)
to_vect <- function(x, r) {
  g <- sf::st_sf(geometry = sf::st_geometry(x))
  crs_r <- terra::crs(r)
  if (is.na(crs_r) || !nzchar(crs_r)) stop("Raster tidak punya CRS")
  terra::vect(sf::st_transform(g, sf::st_crs(crs_r)))
}

# =============================================================================
# 3. INPUT HASIL ANALISIS RASTER
# =============================================================================

lg("Folder proyek: ", DIR_PROYEK)
META <- if (file.exists(FILE_META)) readRDS(FILE_META) else NULL
if (is.null(META)) {
  lg("[GAGAL] ", FILE_META, " tidak ditemukan. Jalankan 01_ANALISIS_RASTER.R terlebih dahulu.")
}
meta_get <- function(n, default = NULL) if (!is.null(META[[n]])) META[[n]] else default

LC  <- meta_get("LC", data.frame(kode = 1:4, kelas = c("Vegetasi", "Bangunan", "Perairan", "Awan"),
                                 warna = c("#358221", "#ED022A", "#1A5BAB", "#C8C8C8"),
                                 stringsAsFactors = FALSE))
THRESH             <- meta_get("THRESH", list(NDVI_VEGETASI = NA, NDWI_PERAIRAN = NA, NDBI_BANGUNAN = NA,
                                              NDCI_AWAN_ABS = NA, BLUE_AWAN = NA))
METODE_AWAN        <- meta_get("METODE_AWAN", "ndci")
SCL_AWAN           <- meta_get("SCL_AWAN", c(8, 9, 10))
SISA_JADI_BANGUNAN <- meta_get("SISA_JADI_BANGUNAN", TRUE)
S2_TGL_TXT         <- meta_get("S2_TGL_TXT", NA_character_)
STATS_TABLE        <- meta_get("STATS_TABLE")
STATS_KAB          <- meta_get("STATS_KAB")
LC_AREA            <- meta_get("LC_AREA")
LC_TOTAL           <- meta_get("LC_TOTAL", setNames(rep(NA_real_, nrow(LC)), LC$kelas))
SAMPLES            <- meta_get("SAMPLES", list())

# --- Batas administrasi + atribut statistik (dari GeoPackage hasil) -------------
kab <- NULL; kec <- NULL; kab_line <- NULL; kec_line <- NULL; kab_ctx <- NULL
if (file.exists(FILE_HASIL_GPKG)) {
  tryCatch({
    ly  <- sf::st_layers(FILE_HASIL_GPKG)$name
    kab <- sf::st_read(FILE_HASIL_GPKG, layer = "batas_kabupaten", quiet = TRUE)
    kec <- sf::st_read(FILE_HASIL_GPKG, layer = "batas_kecamatan", quiet = TRUE)
    kec <- kec[order(kec$kec_nama), ]
    kab_line <- sf::st_sf(geometry = sf::st_boundary(sf::st_union(kab)))
    kec_line <- suppressWarnings(sf::st_boundary(kec))
    if ("konteks_wilayah" %in% ly)
      kab_ctx <- sf::st_read(FILE_HASIL_GPKG, layer = "konteks_wilayah", quiet = TRUE)
    lg("[OK] Batas: ", nrow(kec), " kecamatan dimuat dari ", basename(FILE_HASIL_GPKG))
  }, error = function(e) lg("[GAGAL] Membaca ", FILE_HASIL_GPKG, ": ", conditionMessage(e)))
} else {
  lg("[GAGAL] ", FILE_HASIL_GPKG, " tidak ditemukan. Jalankan 01_ANALISIS_RASTER.R terlebih dahulu.")
}

UTM_EPSG_USE <- if (!is.na(UTM_EPSG)) as.integer(UTM_EPSG) else
  as.integer(meta_get("UTM_EPSG_USE", if (!is.null(kab)) utm_epsg_auto(kab) else 32749L))

# --- Raster hasil analisis -----------------------------------------------------
R_FULL <- list()
for (k in names(FILE_RASTER)) {
  f <- FILE_RASTER[[k]]
  if (!file.exists(f)) { lg("[INFO] Raster ", k, " belum ada: ", f); next }
  tryCatch({
    r <- terra::rast(f)[[1]]
    if (is.na(terra::crs(r)) || !nzchar(terra::crs(r))) terra::crs(r) <- paste0("EPSG:", UTM_EPSG_USE)
    R_FULL[[k]] <- r
    lg("[OK] Raster ", k, " dimuat (", terra::ncell(r), " piksel).")
  }, error = function(e) lg("[GAGAL] Raster ", k, ": ", conditionMessage(e)))
}
if (length(R_FULL) == 0) lg("[GAGAL] Tidak ada raster yang dimuat. Jalankan 01_ANALISIS_RASTER.R terlebih dahulu.")

# versi ringan untuk ditampilkan di peta
R_DISP <- lapply(names(R_FULL), function(k) {
  r <- R_FULL[[k]]
  if (terra::ncell(r) > MAX_CELL_TAMPIL) {
    f <- ceiling(sqrt(terra::ncell(r) / MAX_CELL_TAMPIL))
    r <- terra::aggregate(r, fact = f, fun = if (k == "lc") "modal" else "mean", na.rm = TRUE)
  }
  if (k != "lc") r <- terra::clamp(r, IDX_RANGE[[k]][1], IDX_RANGE[[k]][2], values = TRUE)
  r
})
names(R_DISP) <- names(R_FULL)
IDX_AVAIL <- setdiff(names(R_FULL), "lc")

# --- Tabel aturan klasifikasi  ---------------------------
awan_rule <- switch(METODE_AWAN,
                    ndci     = sprintf("|NDCI| &le; %.2f dan Blue (B02) &gt; %.2f", THRESH$NDCI_AWAN_ABS, THRESH$BLUE_AWAN),
                    scl      = sprintf("SCL kelas %s", paste(SCL_AWAN, collapse = ", ")),
                    keduanya = sprintf("(|NDCI| &le; %.2f dan B02 &gt; %.2f) ATAU SCL kelas %s",
                                       THRESH$NDCI_AWAN_ABS, THRESH$BLUE_AWAN, paste(SCL_AWAN, collapse = ", ")),
                    "-")
RULES <- data.frame(
  Prioritas = 1:4,
  Kelas = sprintf("<span style='color:%s;font-size:18px'>&#9632;</span> <b>%s</b>",
                  LC$warna[c(4, 3, 1, 2)], LC$kelas[c(4, 3, 1, 2)]),
  Indeks = c("NDCI (+ B02)", "NDWI", "NDVI", "NDBI"),
  Aturan = c(awan_rule,
             sprintf("NDWI &gt; %.2f", THRESH$NDWI_PERAIRAN),
             sprintf("NDVI &gt; %.2f", THRESH$NDVI_VEGETASI),
             paste0(sprintf("NDBI &gt; %.2f", THRESH$NDBI_BANGUNAN),
                    if (SISA_JADI_BANGUNAN) " (+ semua piksel sisa)" else "")),
  stringsAsFactors = FALSE
)

# --- Palet -------------------------------------------------------------------
PAL_LC  <- colorFactor(LC$warna, domain = LC$kode, na.color = "transparent")
PAL_IDX <- lapply(names(IDX_RANGE), function(k)
  colorNumeric(IDX_PAL[[k]], domain = IDX_RANGE[[k]], na.color = "transparent"))
names(PAL_IDX) <- names(IDX_RANGE)

# --- Basemap pada muka peta  --------------
BASEMAP_WEB <- c("OpenStreetMap", "Esri World Imagery", "Google Satelit")
# padanan nama basemap web -> nama provider untuk peta cetak
BASEMAP_CETAK <- c("OpenStreetMap" = "OpenStreetMap", "Esri World Imagery" = "Esri.WorldImagery",
                   "Google Satelit" = "Google.Satellite")

# --- Daftar layer  -----------------------------------
RASTER_LABEL <- c(lc = "Tutupan Lahan", NDVI = "NDVI", NDBI = "NDBI", NDWI = "NDWI", NDCI = "NDCI")
CH_RASTER <- setNames(intersect(names(RASTER_LABEL), names(R_DISP)),
                      RASTER_LABEL[intersect(names(RASTER_LABEL), names(R_DISP))])
CH_BATAS  <- c("Garis batas kabupaten" = "kab_line", "Garis batas kecamatan" = "kec_line",
               "Poligon kabupaten" = "kab_poly", "Label nama kecamatan" = "kec_label")
CHORO_CHOICES <- c()
for (k in IDX_AVAIL) CHORO_CHOICES[k] <- k
if (!is.null(LC_AREA)) for (cl in LC$kelas) CHORO_CHOICES[paste("Persentase", cl)] <- paste0("PCT_", cl)
CHORO_DEFAULT <- if (length(CHORO_CHOICES)) unname(CHORO_CHOICES[1]) else "NDVI"
CHORO_STATS <- c("Mean", "Median", "Modus", "Min", "Max", "SD")
LAYER_ALL     <- c(unname(CH_RASTER), "choro", unname(CH_BATAS))   # kec_poly selalu aktif
LAYER_DEFAULT <- intersect(c("lc", "kab_line", "kec_line"), LAYER_ALL)

PCT_PAL <- c(Vegetasi = "Greens", Bangunan = "Reds", Perairan = "Blues", Awan = "Greys")
choro_pal_name <- function(idx) {
  if (idx %in% names(IDX_PAL)) return(IDX_PAL[[idx]])
  kls <- sub("^PCT_", "", idx)
  if (kls %in% names(PCT_PAL)) PCT_PAL[[kls]] else "YlGnBu"
}

# --- Helper layer peta -------------------------------------------------------
add_raster_layer <- function(m, key, opac = 0.85) {
  r <- R_DISP[[key]]
  if (is.null(r)) return(m)
  m %>% clearGroup(key) %>%
    addRasterImage(r, colors = if (key == "lc") PAL_LC else PAL_IDX[[key]],
                   opacity = opac, group = key, project = TRUE,
                   method = if (key == "lc") "ngb" else "bilinear",
                   maxBytes = 30 * 1024 * 1024)
}

choro_info <- function(idx, stat) {
  if (is.null(kec) || is.null(idx) || length(idx) == 0 || !nzchar(idx)) return(NULL)
  pct <- startsWith(idx, "PCT_")
  col <- if (pct) idx else paste0(idx, "_", if (is.null(stat)) "Mean" else stat)
  if (!col %in% names(kec)) return(NULL)
  v <- kec[[col]]
  if (all(is.na(v))) return(NULL)
  rg <- range(v, na.rm = TRUE)
  if (diff(rg) == 0) rg <- rg + c(-0.5, 0.5)
  pal <- colorNumeric(choro_pal_name(idx), domain = rg, na.color = "#cccccc")
  ttl <- if (pct) paste("Persentase", sub("^PCT_", "", idx), "(%)") else paste(idx, stat)
  list(col = col, v = v, pal = pal, title = ttl, pct = pct, rng = rg)
}

draw_choro <- function(m, idx, stat) {
  m <- m %>% clearGroup("choro")
  ci <- choro_info(idx, stat)
  if (is.null(ci)) return(m)
  lab <- lapply(seq_len(nrow(kec)), function(i)
    HTML(sprintf("<b>Kec. %s</b><br>%s: %s", kec$kec_nama[i], ci$title,
                 ifelse(is.na(ci$v[i]), "-", sprintf("%.3f", ci$v[i])))))
  m %>% addPolygons(data = kec, group = "choro", fillColor = ci$pal(ci$v),
                    fillOpacity = 0.8, color = "#ffffff", weight = 1, label = lab,
                    options = pathOptions(pane = "pane_choro", interactive = FALSE))
}

kec_hover_label <- function() {
  f3 <- function(v) ifelse(is.na(v), "-", sprintf("%.3f", v))
  lapply(seq_len(nrow(kec)), function(i) {
    s <- sprintf("<b>Kecamatan %s</b>", kec$kec_nama[i])
    for (k in IDX_AVAIL) {
      g <- function(st) { cn <- paste0(k, "_", st); if (cn %in% names(kec)) kec[[cn]][i] else NA }
      s <- paste0(s, sprintf("<br>%s: mean %s | median %s | modus %s", k,
                             f3(g("Mean")), f3(g("Median")), f3(g("Modus"))))
    }
    if (!is.null(LC_AREA) && !is.na(LC_AREA$Kelas_Dominan[i])) {
      tot <- LC_AREA$Total_ha[i]
      s <- paste0(s, sprintf("<br>Tutupan dominan: <b>%s</b> | Veg %.1f%% | Bgn %.1f%% | Air %.1f%%",
                             LC_AREA$Kelas_Dominan[i],
                             100 * LC_AREA$Vegetasi[i] / tot, 100 * LC_AREA$Bangunan[i] / tot,
                             100 * LC_AREA$Perairan[i] / tot))
    }
    HTML(s)
  })
}

SCALE_JS <- "
function(el, x) {
  var map = this, box;
  var ctrl = L.control({position: 'bottomleft'});
  ctrl.onAdd = function() {
    box = L.DomUtil.create('div', 'leaflet-control');
    box.style.cssText = 'background:rgba(255,255,255,0.92);padding:2px 8px;border:1px solid #777;border-radius:3px;font:bold 12px Times New Roman,serif;';
    return box;
  };
  ctrl.addTo(map);
  function upd() {
    var c = map.getCenter();
    var mpp = 156543.03392 * Math.cos(c.lat * Math.PI / 180) / Math.pow(2, map.getZoom());
    var den = mpp / 0.000264583;
    var step = den > 50000 ? 1000 : (den > 5000 ? 500 : 100);
    var r = Math.max(step, Math.round(den / step) * step);
    box.innerHTML = 'Skala &asymp; 1 : ' + r.toLocaleString('id-ID');
  }
  map.on('zoomend moveend', upd);
  upd();
  // layer analisis (raster) SELALU di atas basemap, apa pun basemap yang dipilih:
  // gambar raster dipindahkan ke panel khusus 'pane_raster' (z-index 410 > tile 200)
  var rpane = map.getPane('pane_raster');
  function toPane(l) {
    if (!(l instanceof L.ImageOverlay)) return;
    var el = l.getElement ? l.getElement() : l._image;
    if (el && rpane && el.parentNode !== rpane) rpane.appendChild(el);
  }
  function raise() {
    map.eachLayer(function(l) {
      toPane(l);
      if (l instanceof L.ImageOverlay && l.bringToFront) l.bringToFront();
    });
  }
  map.on('layeradd', function(e) { toPane(e.layer); });
  map.on('baselayerchange', function(e) {
    if (e.layer && e.layer.eachLayer) e.layer.eachLayer(function(t) { if (t.bringToBack) t.bringToBack(); });
    raise();
    setTimeout(raise, 250);
  });
  raise();
}"

build_map <- function(opac = 0.85) {
  m <- leaflet(options = leafletOptions(minZoom = 8, maxZoom = 19)) %>%
    addMapPane("pane_raster",   410) %>%
    addMapPane("pane_choro",    420) %>%
    addMapPane("pane_kab_poly", 425) %>%
    addMapPane("pane_kec_poly", 430) %>%
    addMapPane("pane_lines",    440) %>%
    addMapPane("pane_sel",      445) %>%
    addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
             attribution = "&copy; OpenStreetMap contributors", group = BASEMAP_WEB[1],
             options = tileOptions(maxZoom = 19)) %>%
    addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
             attribution = "Tiles &copy; Esri", group = BASEMAP_WEB[2],
             options = tileOptions(maxZoom = 19)) %>%
    addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={z}",
             attribution = "Imagery &copy; Google", group = BASEMAP_WEB[3],
             options = tileOptions(maxZoom = 20))
  
  for (k in names(R_DISP)) m <- add_raster_layer(m, k, opac)
  
  if (!is.null(kab)) {
    m <- m %>%
      addPolygons(data = kab, group = "kab_poly", fillColor = "#FFD54F", fillOpacity = 0.15,
                  color = "#FF6F00", weight = 2, label = "Kabupaten Bantul",
                  options = pathOptions(pane = "pane_kab_poly", interactive = FALSE)) %>%
      addPolylines(data = kab_line, group = "kab_line", color = "#FF1744", weight = 3.5,
                   opacity = 1, options = pathOptions(pane = "pane_lines", interactive = FALSE))
    bb <- sf::st_bbox(kab)
    m <- m %>% fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    bbj <- sprintf("[[%f,%f],[%f,%f]]", bb[["ymin"]], bb[["xmin"]], bb[["ymax"]], bb[["xmax"]])
    m <- m %>% addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Zoom ke Kab. Bantul",
      onClick = JS(sprintf("function(btn, map){ map.fitBounds(%s); }", bbj))))
  } else {
    m <- m %>% setView(110.33, -7.89, 11)
  }
  
  if (!is.null(kec)) {
    m <- draw_choro(m, CHORO_DEFAULT, "Mean")
    # poligon kecamatan: SELALU aktif (hover & klik), berada di atas semua layer lain
    m <- m %>%
      addPolygons(data = kec, group = "kec_poly", layerId = ~kec_nama,
                  fillColor = "#FFFFFF", fillOpacity = 0.02, color = "#FFFFFF", weight = 0.5,
                  label = kec_hover_label(),
                  labelOptions = labelOptions(style = list("font-family" = "Times New Roman, serif",
                                                           "font-size" = "12px")),
                  highlightOptions = highlightOptions(weight = 3, color = "#FFEB3B",
                                                      fillOpacity = 0.25, bringToFront = FALSE),
                  options = pathOptions(pane = "pane_kec_poly")) %>%
      addPolylines(data = kec_line, group = "kec_line", color = "#010101", weight = 1.4,
                   opacity = 0.95, options = pathOptions(pane = "pane_lines", interactive = FALSE))
    ct <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(kec)))
    m <- m %>% addLabelOnlyMarkers(
      lng = ct[, 1], lat = ct[, 2], label = kec$kec_nama, group = "kec_label",
      labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                  style = list("font-weight" = "bold", "color" = "#ffffff",
                                               "font-size" = "11px",
                                               "text-shadow" = "0 0 4px #000, 0 0 4px #000")))
  }
  
  m <- m %>%
    addLayersControl(baseGroups = BASEMAP_WEB, position = "topright",
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
    addMeasure(position = "topleft", primaryLengthUnit = "kilometers",
               primaryAreaUnit = "hectares", activeColor = "#3D535D",
               completedColor = "#7D4479") %>%
    addFullscreenControl(position = "topleft") %>%
    addSearchOSM(options = searchOptions(collapsed = TRUE, zoom = 15,
                                         hideMarkerOnCollapse = TRUE, position = "topleft")) %>%
    addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE,
               minimized = TRUE, position = "bottomright")
  
  if ("lc" %in% LAYER_DEFAULT && !is.null(R_DISP$lc))
    m <- m %>% addLegend(position = "bottomright", colors = LC$warna, labels = LC$kelas,
                         title = "Tutupan Lahan", opacity = 1, layerId = "lg_lc")
  m <- m %>% hideGroup(setdiff(LAYER_ALL, LAYER_DEFAULT))
  htmlwidgets::onRender(m, SCALE_JS)
}

update_legends <- function(proxy, sel, idx, stat) {
  if ("lc" %in% sel && !is.null(R_DISP$lc)) {
    proxy %>% addLegend(position = "bottomright", colors = LC$warna, labels = LC$kelas,
                        title = "Tutupan Lahan", opacity = 1, layerId = "lg_lc")
  } else proxy %>% removeControl("lg_lc")
  
  for (k in names(IDX_RANGE)) {
    id <- paste0("lg_", k)
    if (k %in% sel && k %in% IDX_AVAIL) {
      proxy %>% addLegend(position = "bottomright", pal = PAL_IDX[[k]],
                          values = IDX_RANGE[[k]], title = k, opacity = 0.9, layerId = id)
    } else proxy %>% removeControl(id)
  }
  
  ci <- choro_info(idx, stat)
  if ("choro" %in% sel && !is.null(ci)) {
    proxy %>% addLegend(position = "bottomright", pal = ci$pal, values = ci$rng,
                        title = ci$title, opacity = 0.9, layerId = "lg_choro")
  } else proxy %>% removeControl("lg_choro")
}

# =============================================================================
# 3b. PETA CETAK
# =============================================================================
BASEMAP_ATTR <- c(OpenStreetMap     = "(c) OpenStreetMap contributors",
                  Esri.WorldImagery = "Esri, Maxar, Earthstar Geographics",
                  Google.Satellite  = "Imagery (c) Google",
                  CartoDB.Positron  = "(c) OpenStreetMap contributors, (c) CARTO")

read_logo <- function(f) {
  if (is.null(f) || length(f) == 0 || is.na(f) || !file.exists(f)) return(NULL)
  img <- tryCatch(png::readPNG(f), error = function(e) NULL)
  if (is.null(img)) img <- tryCatch(jpeg::readJPEG(f), error = function(e) NULL)
  img
}

# gambar logo 
logo_w <- function(img, h) if (is.null(img)) 0 else h * dim(img)[2] / dim(img)[1]
draw_logo_at <- function(img, x0, ycen, h) {
  if (is.null(img)) return(invisible(NULL))
  w <- logo_w(img, h)
  rasterImage(img, x0, ycen - h / 2, x0 + w, ycen + h / 2, interpolate = TRUE)
}

get_tile <- function(xl, yl, provider, epsg) {
  tryCatch({
    g <- sf::st_as_sfc(sf::st_bbox(c(xmin = xl[1], ymin = yl[1], xmax = xl[2], ymax = yl[2]),
                                   crs = sf::st_crs(epsg)))
    prov <- if (identical(provider, "Google.Satellite"))
      maptiles::create_provider(name = "Google.Satellite",
                                url = "https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={z}",
                                citation = "Imagery (c) Google")
    else provider
    maptiles::get_tiles(g, provider = prov, crop = TRUE, project = TRUE)
  }, error = function(e) {
    lg("[PERINGATAN] Basemap '", provider, "' gagal dimuat: ", conditionMessage(e))
    NULL
  })
}

get_inset_img <- function(xl, yl, provider, epsg, ncol = 1400) {
  tryCatch({
    g <- sf::st_as_sfc(sf::st_bbox(c(xmin = xl[1], ymin = yl[1], xmax = xl[2], ymax = yl[2]),
                                   crs = sf::st_crs(epsg)))
    g <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(g, max(diff(xl), diff(yl)) * 0.25)))
    prov <- if (identical(provider, "Google.Satellite"))
      maptiles::create_provider(name = "Google.Satellite",
                                url = "https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={z}",
                                citation = "Imagery (c) Google")
    else provider
    tl   <- maptiles::get_tiles(g, provider = prov, crop = FALSE, project = FALSE)
    nr   <- max(2, round(ncol * diff(yl) / diff(xl)))
    tmpl <- terra::rast(xmin = xl[1], xmax = xl[2], ymin = yl[1], ymax = yl[2],
                        ncols = ncol, nrows = nr, crs = paste0("EPSG:", epsg))
    pr   <- terra::project(tl, tmpl, method = "bilinear")
    v    <- terra::values(pr)[, 1:3, drop = FALSE]
    v[is.na(v)] <- 235
    v    <- pmin(pmax(v, 0), 255) / 255
    arr  <- array(0, dim = c(nr, ncol, 3))
    for (i in 1:3) arr[, , i] <- matrix(v[, i], nrow = nr, ncol = ncol, byrow = TRUE)
    as.raster(arr)
  }, error = function(e) {
    lg("[PERINGATAN] Basemap inset '", provider, "' gagal dimuat: ", conditionMessage(e))
    NULL
  })
}

prep_r_cetak <- function(k, epsg) {
  r <- R_FULL[[k]]
  if (is.null(r)) return(NULL)
  if (!isTRUE(tryCatch(terra::same.crs(r, paste0("EPSG:", epsg)), error = function(e) FALSE)))
    r <- terra::project(r, paste0("EPSG:", epsg), method = if (k == "lc") "near" else "bilinear")
  if (k != "lc") r <- terra::clamp(r, IDX_RANGE[[k]][1], IDX_RANGE[[k]][2], values = TRUE)
  r
}

draw_peta <- function(o, quality = "export") {
  W <- 420; H <- 297
  kf <- (grDevices::dev.size("in")[1] * 25.4) / W
  FT <- function(pt) pt / 12 * kf
  LW <- function(x) max(0.2, x * kf)
  fs <- o$paperW / W
  mc <- if (quality == "export") 8e6 else 4e5
  sel <- o$layers
  epsg <- if (!is.null(o$epsg) && !is.na(o$epsg)) as.integer(o$epsg) else UTM_EPSG_USE
  
  if (.Platform$OS.type == "windows")
    try(suppressWarnings(grDevices::windowsFonts(
      `Times New Roman` = grDevices::windowsFont("Times New Roman"))), silent = TRUE)
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", ps = 12,
      xpd = FALSE, family = PETA_FONT)
  
  canvas <- function(first = FALSE) {
    par(fig = c(0, 1, 0, 1), new = !first, mar = c(0, 0, 0, 0))
    plot.new(); plot.window(c(0, W), c(0, H))
  }
  TX <- function(x, y, lab, pt = 7, font = 1, col = "black", adj = c(0, 0.5), ...)
    text(x, y, lab, cex = FT(pt), font = font, col = col, adj = adj, ...)
  CTR <- function(x, y, lab, pt = 7, font = 1, col = "black", ...)
    TX(x, y, lab, pt = pt, font = font, col = col, adj = c(0.5, 0.5), ...)
  wrap_mm <- function(txt, w, pt, font = 1) {
    words <- strsplit(txt, " ", fixed = TRUE)[[1]]
    out <- character(0); cur <- ""
    for (wd in words) {
      t <- if (nzchar(cur)) paste(cur, wd) else wd
      if (!nzchar(cur) || strwidth(t, units = "user", cex = FT(pt), font = font) <= w) cur <- t
      else { out <- c(out, cur); cur <- wd }
    }
    c(out, cur)
  }
  north_arrow <- function(cx_, cy_, h_) {
    w_ <- h_ * 0.26
    polygon(c(cx_, cx_ - w_, cx_), c(cy_ + h_ / 2, cy_ - h_ / 2, cy_ - h_ * 0.22),
            col = "black", border = "black", lwd = LW(0.9))
    polygon(c(cx_, cx_ + w_, cx_), c(cy_ + h_ / 2, cy_ - h_ / 2, cy_ - h_ * 0.22),
            col = "white", border = "black", lwd = LW(0.9))
    CTR(cx_, cy_ + h_ / 2 + 2.6, "U", pt = 12, font = 2)
  }
  
  # ------------------------- geometri & ekstent peta -------------------------
  kab32 <- sf::st_transform(sf::st_union(sf::st_geometry(kab)), epsg)
  kec32 <- if (!is.null(kec)) sf::st_transform(kec, epsg) else NULL
  FR <- c(17, 14, 291, 283)                        
  fw <- FR[3] - FR[1]; fh <- FR[4] - FR[2]
  focus_ok <- !is.null(o$focus) && length(o$focus) == 1 && nzchar(o$focus) &&
    !is.null(kec32) && o$focus %in% kec32$kec_nama
  bb <- if (focus_ok) sf::st_bbox(kec32[kec32$kec_nama == o$focus, ]) else sf::st_bbox(kab32)
  cx <- (bb[["xmin"]] + bb[["xmax"]]) / 2; cy <- (bb[["ymin"]] + bb[["ymax"]]) / 2
  bw <- bb[["xmax"]] - bb[["xmin"]]; bh <- bb[["ymax"]] - bb[["ymin"]]
  bbk <- sf::st_bbox(kab32)
  kbw <- bbk[["xmax"]] - bbk[["xmin"]]; kbh <- bbk[["ymax"]] - bbk[["ymin"]]
  
  # skala 
  den <- NA_real_
  if (!is.null(o$skala) && !identical(o$skala, "auto")) den <- suppressWarnings(as.numeric(o$skala))
  if (!is.finite(den)) {
    ok <- SKALA_BAKU[(fw * fs * SKALA_BAKU / 1000 >= bw * 1.04) &
                       (fh * fs * SKALA_BAKU / 1000 >= bh * 1.04)]
    den <- if (length(ok)) min(ok) else max(SKALA_BAKU)
  }
  ww <- fw * fs * den / 1000; hh <- fh * fs * den / 1000     # lebar/tinggi peta (meter)
  xl <- c(cx - ww / 2, cx + ww / 2); yl <- c(cy - hh / 2, cy + hh / 2)
  mapx <- function(x) FR[1] + (x - xl[1]) / ww * fw
  mapy <- function(y) FR[2] + (y - yl[1]) / hh * fh
  m_per_mm <- ww / fw                                # meter per mm layout
  scale_txt <- paste0("1 : ", format(den, big.mark = ".", decimal.mark = ",", scientific = FALSE))
  
  # grid koordinat (meter)
  nice <- c(250, 500, 1000, 2000, 2500, 5000, 10000, 20000)
  gstep <- if (is.null(o$grid_int) || identical(o$grid_int, "auto")) NA_real_
  else suppressWarnings(as.numeric(o$grid_int))
  if (!is.finite(gstep) || gstep <= 0 || ww / gstep > 60)
    gstep <- nice[which.min(abs(nice - 40 * den / 1000))]
  gseq <- function(lim) {
    v <- seq(ceiling(lim[1] / gstep) * gstep, floor(lim[2] / gstep) * gstep, by = gstep)
    v[v > lim[1] & v < lim[2]]
  }
  xt <- gseq(xl); yt <- gseq(yl)
  
  # ----------------------- kolom informasi kanan ----------------
  CX0 <- 302; CX1 <- 410; CW <- CX1 - CX0
  hts <- c(header = 36, title = 21, scale = 28, info = 44, legend = NA, inset = 52,
           pada = 8, oleh = 16, dosen = 16)
  hts[["legend"]] <- 277 - sum(hts, na.rm = TRUE)
  bx <- list(); ycur <- 287
  for (nm in names(hts)) { bx[[nm]] <- c(ycur - hts[[nm]], ycur); ycur <- ycur - hts[[nm]] }
  
  # =========================== 1. KANVAS & KOLOM ============================
  canvas(TRUE)
  rect(0, 0, W, H, col = "white", border = NA)
  for (nm in names(bx)) rect(CX0, bx[[nm]][1], CX1, bx[[nm]][2], col = "white", border = "black", lwd = LW(1))
  bar <- function(nm, ttl) {
    b <- bx[[nm]]
    rect(CX0, b[2] - 5.5, CX1, b[2], col = "#E6E6E6", border = "black", lwd = LW(1))
    TX(CX0 + 2.5, b[2] - 2.75, ttl, pt = 8.5, font = 2)
  }
  
  # --- header logo ---
  b <- bx$header
  lI <- read_logo(LOGO_ITS); lG <- read_logo(LOGO_GEO)
  lh <- 16; gap <- 6
  wI <- min(logo_w(lI, lh), 62); lhI <- if (wI < logo_w(lI, lh)) lh * wI / logo_w(lI, lh) else lh
  wG <- logo_w(lG, lh)
  x0 <- CX0 + (CW - (wI + (if (wI > 0 && wG > 0) gap else 0) + wG)) / 2
  ylog <- b[2] - 3 - lh / 2
  if (!is.null(lI)) { draw_logo_at(lI, x0, ylog, lhI); x0 <- x0 + wI + gap }
  if (!is.null(lG)) draw_logo_at(lG, x0, ylog, lh)
  ty <- b[2] - 3 - lh - 3.4
  for (ln in PETA_INFO$instansi) {
    pt <- 8
    while (strwidth(ln, units = "user", cex = FT(pt), font = 2) > CW - 5 && pt > 4.5) pt <- pt - 0.2
    CTR(CX0 + CW / 2, ty, ln, pt = pt, font = 2); ty <- ty - 3.6
  }
  
  # --- judul peta ---
  b <- bx$title
  judul <- if (nzchar(o$judul)) o$judul else "PETA TUTUPAN LAHAN"
  tpt <- 12
  repeat {
    tl <- wrap_mm(judul, CW - 8, tpt, 2)
    if (length(tl) <= 2 || tpt <= 7) break
    tpt <- tpt - 0.5
  }
  yy <- b[2] - 4.6
  for (ln in tl) { CTR(CX0 + CW / 2, yy, ln, pt = tpt, font = 2); yy <- yy - tpt * 0.46 }
  if (nzchar(o$sub)) {
    sp <- 8
    while (strwidth(o$sub, units = "user", cex = FT(sp), font = 3) > CW - 8 && sp > 5) sp <- sp - 0.3
    CTR(CX0 + CW / 2, yy - 0.4, o$sub, pt = sp, font = 3); yy <- yy - sp * 0.46 - 0.4
  }
  CTR(CX0 + CW / 2, yy - 0.2, paste("Mata Kuliah", PETA_INFO$matkul), pt = 7)
  
  # --- arah utara + skala ---
  b <- bx$scale
  north_arrow(CX0 + 15, (b[1] + b[2]) / 2 - 1, 13)
  sx0 <- CX0 + 32; sx1 <- CX1 - 4
  CTR((sx0 + sx1) / 2, b[2] - 5, paste("Skala", scale_txt), pt = 10.5, font = 2)
  Lm <- 250
  for (cand in c(250, 500, 1000, 2000, 2500, 5000, 10000, 20000, 25000)) {
    if (cand / m_per_mm <= 50) Lm <- cand
  }
  blen <- Lm / m_per_mm
  bx0 <- (sx0 + sx1) / 2 - blen / 2; by <- b[1] + 10
  for (i in 1:4) rect(bx0 + (i - 1) * blen / 4, by, bx0 + i * blen / 4, by + 2.6,
                      col = if (i %% 2) "black" else "white", border = "black", lwd = LW(0.8))
  lab_len <- function(v) if (Lm >= 1000) format(v / 1000, decimal.mark = ",") else format(v)
  unit <- if (Lm >= 1000) " km" else " m"
  CTR(bx0, by - 2.6, "0", pt = 7); CTR(bx0 + blen / 2, by - 2.6, lab_len(Lm / 2), pt = 7)
  CTR(bx0 + blen, by - 2.6, paste0(lab_len(Lm), unit), pt = 7)
  CTR((sx0 + sx1) / 2, b[1] + 2.2, paste0("Kertas ", o$paper, " | Satuan koordinat: meter"),
      pt = 6, col = "grey25")
  
  # --- informasi peta ---
  bar("info", "INFORMASI PETA")
  b <- bx$info
  tile_attr <- if (!identical(o$basemap, "none") && o$basemap %in% names(BASEMAP_ATTR))
    BASEMAP_ATTR[[o$basemap]] else NULL
  rows <- list(
    list("Sistem Koordinat", sprintf("Universal Transverse Mercator (UTM) Zona %d%s", as.integer(epsg %% 100), if (epsg >= 32700) "S" else "N")),
    list("Proyeksi", sprintf("Transverse Mercator, MT %d BT, FS 0,9996", as.integer(6 * (epsg %% 100) - 183))),
    list("Datum", paste0("WGS 1984 (EPSG:", epsg, ")")),
    list("Satuan", "Meter"),
    list("Grid", if (isTRUE(o$grid)) paste0("Interval ", format(gstep, big.mark = ".", scientific = FALSE), " m")
         else "Tidak ditampilkan"),
    list("Sumber Citra", paste0("Sentinel-2 L2A (Copernicus)",
                                if (!is.na(S2_TGL_TXT)) paste0(", perekaman ", S2_TGL_TXT) else "")),
    list("Batas Admin.", "Ina-Geoportal (Badan Informasi Geospasial)"),
    list("Analisis", sprintf("NDVI > %.2f; NDWI > %.2f; NDBI > %.2f; |NDCI| <= %.2f & B02 > %.2f",
                             THRESH$NDVI_VEGETASI, THRESH$NDWI_PERAIRAN, THRESH$NDBI_BANGUNAN,
                             THRESH$NDCI_AWAN_ABS, THRESH$BLUE_AWAN))
  )
  if (!is.null(tile_attr)) rows[[length(rows) + 1]] <- list("Basemap", tile_attr)
  if (!identical(o$basemap_inset, o$basemap) && o$basemap_inset %in% names(BASEMAP_ATTR))
    rows[[length(rows) + 1]] <- list("Basemap Inset", BASEMAP_ATTR[[o$basemap_inset]])
  avail <- (b[2] - 5.5 - 2) - (b[1] + 1.2)
  ipt <- 7
  repeat {
    lh_ <- ipt * 0.44; lw_ <- 24 * ipt / 7; vw <- CW - 6 - lw_ - 2
    nl <- sum(vapply(rows, function(r) length(wrap_mm(r[[2]], vw, ipt)), numeric(1)))
    if (nl * lh_ + length(rows) * 0.35 <= avail || ipt <= 4.4) break
    ipt <- ipt - 0.2
  }
  iy <- b[2] - 5.5 - 3
  for (r in rows) {
    vl <- wrap_mm(r[[2]], vw, ipt)
    TX(CX0 + 3, iy, r[[1]], pt = ipt, font = 2)
    TX(CX0 + 3 + lw_, iy, ":", pt = ipt)
    for (j in seq_along(vl)) { TX(CX0 + 3 + lw_ + 2, iy, vl[j], pt = ipt); iy <- iy - lh_ }
    iy <- iy - 0.35
  }
  
  # --- legenda otomatis ---
  bar("legend", "LEGENDA")
  b <- bx$legend
  ents <- list()
  add <- function(e) ents[[length(ents) + 1]] <<- e
  if ("lc" %in% sel && !is.null(R_FULL$lc)) {
    add(list(t = "head", txt = "Tutupan Lahan"))
    for (i in seq_len(nrow(LC)))
      add(list(t = "patch", fill = LC$warna[i], border = "grey30", txt = LC$kelas[i]))
  }
  for (k in intersect(IDX_AVAIL, sel)) {
    add(list(t = "head", txt = paste("Indeks", k)))
    add(list(t = "grad", pal = PAL_IDX[[k]], rng = IDX_RANGE[[k]]))
  }
  ci <- if ("choro" %in% sel) choro_info(o$choro_idx, o$choro_stat) else NULL
  if (!is.null(ci)) {
    add(list(t = "head", txt = paste0(ci$title, " / Kecamatan")))
    add(list(t = "grad", pal = ci$pal, rng = ci$rng))
  }
  if (any(c("kab_poly", "kec_line", "kab_line", "kec_label") %in% sel)) {
    add(list(t = "head", txt = "Batas Administrasi"))
    if ("kab_line" %in% sel) add(list(t = "line", col = "#B71C1C", lwd = 2.4, lty = 1, txt = "Batas Kabupaten"))
    if ("kec_line" %in% sel) add(list(t = "line", col = "#212121", lwd = 0.9, lty = 2, txt = "Batas Kecamatan"))
    if ("kab_poly" %in% sel) add(list(t = "patch", fill = adjustcolor("#FFD54F", 0.35), border = "#FF6F00", txt = "Wilayah Kabupaten"))
    if ("kec_label" %in% sel) add(list(t = "text", txt = "Nama Kecamatan"))
  }
  if (length(ents) == 0) add(list(t = "note", txt = "(belum ada layer dipilih)"))
  base_h <- c(head = 5.4, patch = 4.5, line = 4.5, text = 4.5, note = 4.5, grad = 11)
  total <- sum(vapply(ents, function(e) base_h[[e$t]], numeric(1)))
  ytop <- b[2] - 5.5 - 2.5; ybot <- b[1] + 2
  f <- min(1, 2 * (ytop - ybot) / total * 0.97)
  ptL <- max(4.8, 8 * f)
  cwid <- (CW - 6) / 2; colx <- 0; y <- ytop
  fnum <- function(v) formatC(v, format = "f", digits = 2)
  for (e in ents) {
    h <- base_h[[e$t]] * f
    if (y - h < ybot && colx == 0) { colx <- 1; y <- ytop }
    x0 <- CX0 + 3 + colx * cwid
    yc <- y - h / 2
    switch(e$t,
           head  = TX(x0, yc, e$txt, pt = ptL, font = 2),
           note  = TX(x0, yc, e$txt, pt = ptL, font = 3),
           patch = { rect(x0, yc - 1.5 * f, x0 + 8 * f, yc + 1.5 * f, col = e$fill, border = e$border, lwd = LW(0.6))
             TX(x0 + 10 * f, yc, e$txt, pt = ptL) },
           line  = { segments(x0, yc, x0 + 8 * f, yc, col = e$col, lwd = LW(e$lwd), lty = e$lty)
             TX(x0 + 10 * f, yc, e$txt, pt = ptL) },
           text  = { TX(x0 + 0.5, yc, "Abc", pt = ptL, font = 2); TX(x0 + 10 * f, yc, e$txt, pt = ptL) },
           grad  = { n <- 40; bw_ <- 38
           xs <- seq(x0, x0 + bw_, length.out = n + 1)
           vals <- seq(e$rng[1], e$rng[2], length.out = n)
           for (i in seq_len(n)) rect(xs[i], y - 4.3 * f, xs[i + 1], y - 0.8 * f, col = e$pal(vals[i]), border = NA)
           rect(x0, y - 4.3 * f, x0 + bw_, y - 0.8 * f, border = "grey30", lwd = LW(0.6))
           TX(x0, y - 6.6 * f, fnum(e$rng[1]), pt = ptL * 0.85, adj = c(0, 0.5))
           TX(x0 + bw_ / 2, y - 6.6 * f, fnum(mean(e$rng)), pt = ptL * 0.85, adj = c(0.5, 0.5))
           TX(x0 + bw_, y - 6.6 * f, fnum(e$rng[2]), pt = ptL * 0.85, adj = c(1, 0.5)) })
    y <- y - h
  }
  
  # --- inset ---
  bar("inset", "INSET PETA")
  
  # --- Informasi tepi ---
  b <- bx$pada
  CTR(CX0 + CW / 2, mean(b), paste("Dibuat Pada :", o$tgl), pt = 8.5, font = 2)
  b <- bx$oleh
  TX(CX0 + 2.5, b[2] - 3.4, "Dibuat Oleh :", pt = 8.5, font = 2)
  yy <- b[2] - 3.4 - 4.2
  for (i in seq_along(PETA_INFO$anggota)) {
    CTR(CX0 + CW / 2, yy, paste0(i, ". ", PETA_INFO$anggota[i]), pt = 8); yy <- yy - 3.8
  }
  b <- bx$dosen
  TX(CX0 + 2.5, b[2] - 3.4, "Dosen Pengampu :", pt = 8.5, font = 2)
  CTR(CX0 + CW / 2, b[2] - 3.4 - 4.2, PETA_INFO$dosen, pt = 8)
  CTR(CX0 + CW / 2, b[2] - 3.4 - 8, paste0("NIP. ", PETA_INFO$nip), pt = 8)
  
  # ============================ 2. PETA UTAMA ===============================
  par(fig = c(FR[1] / W, FR[3] / W, FR[2] / H, FR[4] / H), new = TRUE, mar = c(0, 0, 0, 0))
  plot.new(); plot.window(xl, yl)
  rect(xl[1], yl[1], xl[2], yl[2], col = "#F4F6F8", border = NA)
  
  if (!identical(o$basemap, "none")) {
    tl_ <- get_tile(xl, yl, o$basemap, epsg)
    if (!is.null(tl_)) terra::plotRGB(tl_, r = 1, g = 2, b = 3, add = TRUE, maxcell = 2e6)
  }
  if ("kab_poly" %in% sel)
    plot(kab32, add = TRUE, col = adjustcolor("#FFD54F", 0.30), border = "#FF6F00", lwd = LW(1.2))
  
  for (k in intersect(IDX_AVAIL, sel)) {
    r <- prep_r_cetak(k, epsg)
    if (is.null(r)) next
    cols <- adjustcolor(PAL_IDX[[k]](seq(IDX_RANGE[[k]][1], IDX_RANGE[[k]][2], length.out = 100)), o$opac)
    terra::plot(r, add = TRUE, col = cols, range = IDX_RANGE[[k]], legend = FALSE, axes = FALSE, maxcell = mc)
  }
  if ("lc" %in% sel && !is.null(R_FULL$lc)) {
    r <- prep_r_cetak("lc", epsg)
    ord <- order(LC$kode)
    terra::plot(r, add = TRUE, col = adjustcolor(LC$warna[ord], o$opac),
                breaks = c(LC$kode[ord][1] - 0.5, LC$kode[ord] + 0.5),
                legend = FALSE, axes = FALSE, maxcell = mc)
  }
  if (!is.null(ci) && !is.null(kec32))
    plot(sf::st_geometry(kec32), add = TRUE, col = adjustcolor(ci$pal(ci$v), o$opac),
         border = "white", lwd = LW(0.6))
  if (isTRUE(o$grid)) {
    gcol <- adjustcolor("#222222", 0.55)
    segments(xt, yl[1], xt, yl[2], col = gcol, lwd = LW(0.45), lty = 3)
    segments(xl[1], yt, xl[2], yt, col = gcol, lwd = LW(0.45), lty = 3)
  }
  if ("kec_line" %in% sel && !is.null(kec32))
    plot(sf::st_geometry(kec32), add = TRUE, col = NA, border = "#212121", lwd = LW(0.9), lty = 2)
  if ("kab_line" %in% sel)
    plot(sf::st_boundary(kab32), add = TRUE, col = "#B71C1C", lwd = LW(2.4))
  if (focus_ok && !is.null(kec32))
    plot(sf::st_geometry(kec32[kec32$kec_nama == o$focus, ]), add = TRUE, col = NA,
         border = "#00B8D4", lwd = LW(2.2))
  if ("kec_label" %in% sel && !is.null(kec32)) {
    ct <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(sf::st_geometry(kec32))))
    off <- m_per_mm * 0.3
    for (dx in c(-off, off)) for (dy in c(-off, off))
      text(ct[, 1] + dx, ct[, 2] + dy, kec32$kec_nama, cex = FT(6.5), font = 2, col = "white")
    text(ct[, 1], ct[, 2], kec32$kec_nama, cex = FT(6.5), font = 2, col = "#111111")
  }
  
  # ============================ 3. INSET PETA ===============================
  bI <- bx$inset
  INS <- c(CX0 + 3, bI[1] + 3, CX1 - 3, bI[2] - 5.5 - 2.5)
  iratio <- (INS[3] - INS[1]) / (INS[4] - INS[2])
  ih <- max(kbw, kbh) * 5; iw2 <- ih * iratio
  icx <- (bbk[["xmin"]] + bbk[["xmax"]]) / 2; icy <- (bbk[["ymin"]] + bbk[["ymax"]]) / 2
  xlI <- c(icx - iw2 / 2, icx + iw2 / 2); ylI <- c(icy - ih / 2, icy + ih / 2)
  mapxI <- function(x) INS[1] + (x - xlI[1]) / (xlI[2] - xlI[1]) * (INS[3] - INS[1])
  mapyI <- function(y) INS[2] + (y - ylI[1]) / (ylI[2] - ylI[1]) * (INS[4] - INS[2])
  par(fig = c(INS[1] / W, INS[3] / W, INS[2] / H, INS[4] / H), new = TRUE, mar = c(0, 0, 0, 0))
  plot.new(); plot.window(xlI, ylI)
  # Basemap inset
  img_in <- if (!identical(o$basemap_inset, "none")) get_inset_img(xlI, ylI, o$basemap_inset, epsg) else NULL
  if (!is.null(img_in)) {
    rasterImage(img_in, xlI[1], ylI[1], xlI[2], ylI[2], interpolate = TRUE)
  } else {
    rect(xlI[1], ylI[1], xlI[2], ylI[2], col = "#BFE3F7", border = NA)        
  }
  ctx32 <- if (!is.null(kab_ctx)) sf::st_transform(kab_ctx, epsg) else NULL
  if (!is.null(ctx32) && nrow(ctx32) > 1) {
    if (!is.null(img_in)) {       # garis batas saja agar basemap tetap terlihat
      plot(sf::st_geometry(ctx32), add = TRUE, col = NA, border = "white", lwd = LW(0.8))
      plot(sf::st_geometry(ctx32), add = TRUE, col = NA, border = "#333333", lwd = LW(0.3))
    } else {
      plot(sf::st_geometry(ctx32), add = TRUE, col = "#3FA145", border = "#2E7D32", lwd = LW(0.25))
    }
  }
  # Grid inset
  lab_top <- NULL; lab_left <- NULL
  gl <- tryCatch({
    crn <- sf::st_as_sfc(sf::st_bbox(c(xmin = xlI[1], ymin = ylI[1], xmax = xlI[2], ymax = ylI[2]),
                                     crs = sf::st_crs(epsg)))
    sf::st_bbox(sf::st_transform(crn, 4326))
  }, error = function(e) NULL)
  if (!is.null(gl)) {
    stp <- 2
    for (s_ in c(0.25, 0.5, 1, 2)) {
      nlon <- floor(gl[["xmax"]] / s_) - ceiling(gl[["xmin"]] / s_) + 1
      if (nlon <= 8) { stp <- s_; break }
    }
    lonv <- seq(ceiling(gl[["xmin"]] / stp) * stp, floor(gl[["xmax"]] / stp) * stp, by = stp)
    latv <- seq(ceiling(gl[["ymin"]] / stp) * stp, floor(gl[["ymax"]] / stp) * stp, by = stp)
    to_utm <- function(lon, lat) sf::st_coordinates(sf::st_transform(
      sf::st_as_sf(data.frame(x = lon, y = lat), coords = c("x", "y"), crs = 4326), epsg))
    gcol <- "#1F3F66"
    for (v in lonv) {
      xy <- to_utm(rep(v, 40), seq(gl[["ymin"]] - 0.3, gl[["ymax"]] + 0.3, length.out = 40))
      lines(xy[, 1], xy[, 2], col = gcol, lwd = LW(0.4))
      xt_ <- to_utm(v, gl[["ymax"]])[1, 1]
      if (xt_ > xlI[1] && xt_ < xlI[2]) lab_top <- rbind(lab_top, data.frame(x = mapxI(xt_), t = v))
    }
    for (v in latv) {
      xy <- to_utm(seq(gl[["xmin"]] - 0.3, gl[["xmax"]] + 0.3, length.out = 40), rep(v, 40))
      lines(xy[, 1], xy[, 2], col = gcol, lwd = LW(0.4))
      yy_ <- to_utm(gl[["xmin"]], v)[1, 2]
      if (yy_ > ylI[1] && yy_ < ylI[2]) lab_left <- rbind(lab_left, data.frame(y = mapyI(yy_), t = v))
    }
  }
  plot(kab32, add = TRUE, col = "#E3151A", border = "#7F0000", lwd = LW(0.5))   
  rect(xl[1], yl[1], xl[2], yl[2], border = "white", lwd = LW(3))               
  rect(xl[1], yl[1], xl[2], yl[2], border = "black", lwd = LW(1.5))
  
  # ============== 4. TEPI PETA ====
  canvas()
  fmtc <- function(v) formatC(v, format = "d", big.mark = "")
  for (v in xt) {
    xx <- mapx(v)
    segments(xx, FR[2], xx, FR[2] - 1.8, lwd = LW(0.8)); segments(xx, FR[4], xx, FR[4] + 1.8, lwd = LW(0.8))
    CTR(xx, FR[2] - 4.4, fmtc(v), pt = 6.2); CTR(xx, FR[4] + 4.4, fmtc(v), pt = 6.2)
  }
  for (v in yt) {
    yy <- mapy(v)
    segments(FR[1], yy, FR[1] - 1.8, yy, lwd = LW(0.8)); segments(FR[3], yy, FR[3] + 1.8, yy, lwd = LW(0.8))
    CTR(FR[1] - 4.2, yy, fmtc(v), pt = 6.2, srt = 90); CTR(FR[3] + 4.2, yy, fmtc(v), pt = 6.2, srt = 90)
  }
  rect(FR[1], FR[2], FR[3], FR[4], border = "black", lwd = LW(1.8))
  rect(INS[1], INS[2], INS[3], INS[4], border = "black", lwd = LW(1))
  dms <- function(v, pos, neg) {
    a <- abs(v); d <- floor(a); m <- round((a - d) * 60)
    if (m == 60) { d <- d + 1; m <- 0 }
    sprintf("%d\u00b0%02d'%s", as.integer(d), as.integer(m), if (v >= 0) pos else neg)
  }
  gtxt <- "#16365C"
  if (!is.null(lab_top)) for (i in seq_len(nrow(lab_top))) {
    lb <- dms(lab_top$t[i], "E", "W")
    CTR(lab_top$x[i], INS[4] - 1.9, lb, pt = 4.8, col = gtxt)
    CTR(lab_top$x[i], INS[2] + 1.9, lb, pt = 4.8, col = gtxt)
  }
  if (!is.null(lab_left)) for (i in seq_len(nrow(lab_left))) {
    lb <- dms(lab_left$t[i], "N", "S")
    CTR(INS[1] + 1.9, lab_left$y[i], lb, pt = 4.8, col = gtxt, srt = 90)
    CTR(INS[3] - 1.9, lab_left$y[i], lb, pt = 4.8, col = gtxt, srt = 90)
  }
  # legenda inset 
  lgx <- INS[1] + 3; lgy <- INS[2] + 4.2
  lg1 <- "KABUPATEN BANTUL"; lg2 <- "LOKASI PETA UTAMA"
  lgw <- max(strwidth(lg1, units = "user", cex = FT(5.6), font = 2),
             strwidth(lg2, units = "user", cex = FT(5.6), font = 2))
  rect(lgx, lgy, lgx + 10 + lgw, lgy + 9.6, col = "white", border = "grey35", lwd = LW(0.5))
  rect(lgx + 1.6, lgy + 5.9, lgx + 6.8, lgy + 8.3, col = "#E3151A", border = "#7F0000", lwd = LW(0.4))
  TX(lgx + 8.3, lgy + 7.1, lg1, pt = 5.6, font = 2)
  rect(lgx + 1.6, lgy + 1.3, lgx + 6.8, lgy + 3.7, col = NA, border = "black", lwd = LW(1.3))
  TX(lgx + 8.3, lgy + 2.5, lg2, pt = 5.6, font = 2)
  rect(5, 5, 415, 292, border = "black", lwd = LW(2.2))
  invisible(TRUE)
}

# =============================================================================
# 4. USER INTERFACE
# =============================================================================
css <- "
.banner{background:linear-gradient(90deg,#1b5e20 0%,#2e7d32 45%,#0277bd 100%);
  color:#fff;padding:14px 22px;border-radius:6px;margin-bottom:14px;
  border-bottom:5px solid #ffd54f;box-shadow:0 2px 6px rgba(0,0,0,.25);}
.banner h1{margin:0;font-size:26px;font-weight:800;letter-spacing:1.2px;}
.banner p{margin:4px 0 0;font-size:13px;opacity:.9;}
.side-panel{padding:0 15px 20px 15px;}
.side-panel h4{color:#ffd54f;font-size:13px;font-weight:700;margin:14px 0 6px;
  text-transform:uppercase;letter-spacing:.6px;border-bottom:1px solid #4b5a63;padding-bottom:3px;}
.side-panel label{color:#e8e8e8;font-weight:500;}
.side-panel .checkbox label{font-size:13px;}
.info-box{min-height:78px;} .info-box-icon{height:78px;line-height:78px;}
.info-box-content{padding-top:2px;padding-bottom:2px;}
#preview img{max-width:100%;height:auto;border:1px solid #bbb;background:#fff;}
.leaflet-container{font-family:'Times New Roman',serif;}
"

# menu cetak
layer_ui <- function(pre, sel_raster, sel_batas, sel_choro = FALSE) {
  id <- function(x) paste0(pre, "_", x)
  tagList(
    tags$h4("Analisis Raster"),
    if (length(CH_RASTER)) checkboxGroupInput(id("raster"), NULL, choices = CH_RASTER, selected = sel_raster)
    else p("Raster belum tersedia", style = "color:#e57373;"),
    tags$h4("Analisis Zonal Kecamatan"),
    checkboxInput(id("choro"), "Choropleth kecamatan", value = sel_choro),
    conditionalPanel(
      sprintf("input.%s", id("choro")),
      selectInput(id("choro_idx"), "Variabel analisis", choices = CHORO_CHOICES),
      conditionalPanel(
        sprintf("input.%1$s && input.%1$s.indexOf('PCT_') !== 0", id("choro_idx")),
        selectInput(id("choro_stat"), "Statistik zonal", choices = CHORO_STATS))),
    tags$h4("Batas Administrasi"),
    checkboxGroupInput(id("batas"), NULL, choices = CH_BATAS, selected = sel_batas)
  )
}

RASTER_FILE_LABEL <- c(lc = "Tutupan_Lahan", NDVI = "NDVI", NDBI = "NDBI", NDWI = "NDWI", NDCI = "NDCI")

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Bantul GIS", titleWidth = 320),
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      id = "tabs",
      menuItem("Peta Interaktif",    tabName = "peta",  icon = icon("map")),
      menuItem("Zonal Statistics",   tabName = "stats", icon = icon("table")),
      menuItem("Threshold & Metode", tabName = "thr",   icon = icon("chart-bar")),
      menuItem("Cetak Peta",         tabName = "cetak", icon = icon("print")),
      menuItem("Unduh Data",         tabName = "unduh", icon = icon("download")),
      menuItem("Tentang",            tabName = "about", icon = icon("info-circle"))
    ),
    conditionalPanel(
      "input.tabs == 'peta'",
      div(class = "side-panel",
          layer_ui("lyr", sel_raster = intersect("lc", unname(CH_RASTER)),
                   sel_batas = c("kab_line", "kec_line")),
          tags$h4("Tampilan"),
          sliderInput("opac", "Transparansi raster", min = 0, max = 1, value = 0.85, step = 0.05),
          materialSwitch("probe", "Info titik", value = TRUE, status = "success"),
          tags$h4("Kecamatan"),
          selectInput("zoom_kec", NULL,
                      choices = c("Tidak memilih kecamatan" = "", if (!is.null(kec)) kec$kec_nama else NULL))
      )
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(css)),
              tags$script(HTML("
$(document).on('shiny:connected', function(){ setDT(); setInterval(setDT, 15000); });
function setDT(){
  var d = new Date(), p = function(n){ return String(n).padStart(2, '0'); };
  Shiny.setInputValue('client_dt', d.getFullYear() + '-' + p(d.getMonth() + 1) + '-' + p(d.getDate()) +
                      ' ' + p(d.getHours()) + ':' + p(d.getMinutes()));
}
$(document).on('mousedown', '#dl_peta, #btn_prev', setDT);
"))),
    div(class = "banner",
        h1("TUTUPAN LAHAN INTERAKTIF KAB. BANTUL"),
        p("Sentinel-2 L2A  |  NDVI  NDBI  NDWI  NDCI  |  Ina-Geoportal")),
    tabItems(
      tabItem("peta",
              uiOutput("warn_raster"),
              uiOutput("boxes"),
              fluidRow(column(12, leafletOutput("map", height = "72vh"))),
              br(),
              fluidRow(
                box(width = 6, status = "success", solidHeader = TRUE,
                    title = "Komposisi Tutupan Lahan Kecamatan Terpilih",
                    h4(textOutput("kec_name")),
                    plotlyOutput("plt_lc", height = "300px")),
                box(width = 6, status = "primary", solidHeader = TRUE,
                    title = "Statistik Indeks Kecamatan Terpilih",
                    DTOutput("tbl_sel"))
              )),
      tabItem("stats",
              box(width = 12, status = "success", solidHeader = TRUE,
                  title = "Zonal Statistics Kecamatan",
                  DTOutput("tbl_zonal")),
              box(width = 12, status = "primary", solidHeader = TRUE,
                  title = "Luas Tutupan Lahan (ha)",
                  DTOutput("tbl_area"))),
      tabItem("thr",
              box(width = 12, status = "warning", solidHeader = TRUE,
                  title = "Aturan Klasifikasi",
                  DTOutput("tbl_rules")),
              box(width = 12, status = "success", solidHeader = TRUE,
                  title = "Distribusi Indeks & Threshold",
                  selectInput("thr_idx", "Indeks", choices = IDX_AVAIL, width = "200px"),
                  plotlyOutput("plt_hist", height = "380px"))),
      tabItem("cetak",
              fluidRow(
                box(width = 4, status = "primary", solidHeader = TRUE,
                    title = "Pengaturan Peta Cetak",
                    textInput("exp_judul", "Judul peta",
                              value = "PETA TUTUPAN LAHAN KABUPATEN BANTUL"),
                    textInput("exp_sub", "Sub judul",
                              value = "Provinsi D.I. Yogyakarta"),
                    layer_ui("exp", sel_raster = intersect("lc", unname(CH_RASTER)),
                             sel_batas = c("kab_line", "kec_line")),
                    tags$h4("Skala, Koordinat & Grid"),
                    selectInput("exp_skala", "Skala",
                                choices = c("Otomatis" = "auto",
                                            "1 : 25.000" = "25000", "1 : 50.000" = "50000",
                                            "1 : 100.000" = "100000", "1 : 150.000" = "150000",
                                            "1 : 200.000" = "200000")),
                    checkboxInput("exp_focus", "Fokus ke kecamatan terpilih", FALSE),
                    selectInput("exp_utm", "Sistem koordinat",
                                choices = c("Otomatis" = "auto",
                                            "UTM 48S" = "32748",
                                            "UTM 49S" = "32749",
                                            "UTM 50S" = "32750")),
                    checkboxInput("exp_grid", "Grid koordinat", value = TRUE),
                    conditionalPanel(
                      "input.exp_grid",
                      selectInput("exp_gridint", "Interval grid",
                                  choices = c("Otomatis" = "auto", "500 m" = "500",
                                              "1.000 m" = "1000", "2.000 m" = "2000", "5.000 m" = "5000",
                                              "10.000 m" = "10000", "20.000 m" = "20000"))),
                    tags$h4("Basemap & Keluaran"),
                    selectInput("exp_basemap", "Basemap utama",
                                choices = c("OpenStreetMap" = "OpenStreetMap",
                                            "Esri World Imagery" = "Esri.WorldImagery",
                                            "Google Satelit" = "Google.Satellite"),
                                selected = "OpenStreetMap"),
                    selectInput("exp_basemap_inset", "Basemap inset",
                                choices = c("OpenStreetMap" = "OpenStreetMap",
                                            "Esri World Imagery" = "Esri.WorldImagery",
                                            "Google Satelit" = "Google.Satellite"),
                                selected = "OpenStreetMap"),
                    sliderInput("exp_opac", "Transparansi raster", 0.2, 1, 0.9, 0.05),
                    selectInput("exp_paper", "Kertas",
                                choices = c("A3" = "A3", "A4" = "A4")),
                    radioButtons("exp_fmt", "Format berkas", c("PNG", "PDF", "JPEG"), inline = TRUE),
                    selectInput("exp_dpi", "Resolusi (DPI)",
                                choices = c("150", "200", "300"), selected = "200"),
                    actionButton("btn_prev", "Perbarui pratinjau", icon = icon("eye")),
                    downloadButton("dl_peta", "Unduh Peta", class = "btn-success"),
                    NULL),
                box(width = 8, status = "success", solidHeader = TRUE, title = "Pratinjau Peta Cetak",
                    plotOutput("preview", width = "100%", height = "auto")))),
      tabItem("unduh",
              fluidRow(
                box(width = 6, status = "primary", solidHeader = TRUE,
                    title = "Raster GeoTIFF",
                    selectInput("dl_crs", "Sistem koordinat",
                                choices = setNames(c("utm", "4326"),
                                                   c(paste("UTM", as.integer(UTM_EPSG_USE %% 100), "meter"), "WGS 84 derajat"))),
                    hr(),
                    if (length(R_FULL)) lapply(names(R_FULL), function(k)
                      div(style = "margin-bottom:8px;",
                          downloadButton(paste0("dl_r_", k), paste("Unduh", RASTER_LABEL[[k]]),
                                         class = "btn-primary")))
                    else p("Raster belum tersedia.", style = "color:#c62828;"),
                    NULL),
                box(width = 6, status = "success", solidHeader = TRUE,
                    title = "Vektor GeoPackage",
                    p("Berisi 4 layer: ", tags$code("batas_kabupaten"), ", ", tags$code("batas_kecamatan"),
                      " (lengkap dengan atribut statistik zonal NDVI/NDBI/NDWI/NDCI dan luas kelas tutupan lahan), ",
                      tags$code("garis_batas_kabupaten"), ", dan ", tags$code("garis_batas_kecamatan"),
                      ". Proyeksi mengikuti pilihan sistem koordinat di sebelah kiri."),
                    downloadButton("dl_gpkg", "Unduh GeoPackage", class = "btn-success")))),
      tabItem("about",
              box(width = 12, title = "Tentang Aplikasi", status = "info", solidHeader = TRUE,
                  p("Aplikasi ini mengolah citra Sentinel-2 Level-2A (format .SAFE) menjadi
                     indeks spektral dan peta tutupan lahan Kabupaten Bantul, D.I. Yogyakarta."),
                  tags$ul(
                    tags$li(strong("NDVI"), " = (B08 - B04) / (B08 + B04) - vegetasi"),
                    tags$li(strong("NDBI"), " = (B11 - B08) / (B11 + B08) - area terbangun"),
                    tags$li(strong("NDWI"), " = (B03 - B08) / (B03 + B08) - perairan"),
                    tags$li(strong("NDCI"), " = (Red - NIR) / (Red + NIR) = (B04 - B08) / (B04 + B08) - awan (nilai mendekati 0, digabung dengan B02 tinggi)")),
                  p("Band 20 m (B11, SCL) di-resample ke 10 m. Nilai DN dikonversi ke
                     reflektansi dengan BOA_ADD_OFFSET dari metadata produk."),
                  p("Sumber data: Copernicus (Sentinel-2 L2A), Ina-Geoportal (batas administrasi).")))
    )
  )
)

# =============================================================================
# 5. SERVER
# =============================================================================
server <- function(input, output, session) {
  
  selected <- reactiveVal(NULL)
  layers_sel <- reactive(c(input$lyr_raster, if (isTRUE(input$lyr_choro)) "choro", input$lyr_batas))
  choro_idx_web  <- reactive(if (is.null(input$lyr_choro_idx)) CHORO_DEFAULT else input$lyr_choro_idx)
  choro_stat_web <- reactive(if (is.null(input$lyr_choro_stat)) "Mean" else input$lyr_choro_stat)
  
  output$map <- renderLeaflet(build_map())
  
  output$warn_raster <- renderUI({
    if (length(R_FULL) > 0) return(NULL)
    div(class = "callout callout-danger",
        h4("Raster belum berhasil dimuat"),
        p("Jalankan 01_ANALISIS_RASTER.R terlebih dahulu, lalu periksa DIR_OUTPUT pada konfigurasi. ",
          "Rincian pesan kesalahan tampil di Console R."))
  })
  
  # --- layer berdasarkan sub-kategori --------------------------------
  observeEvent(layers_sel(), ignoreNULL = FALSE, ignoreInit = TRUE, {
    sel <- layers_sel()
    px  <- leafletProxy("map")
    for (g in LAYER_ALL) if (g %in% sel) showGroup(px, g) else hideGroup(px, g)
    update_legends(px, sel, choro_idx_web(), choro_stat_web())
  })
  
  # Raster analisis diatas basemap
  last_base <- reactiveVal(NULL)
  observeEvent(input$map_groups, {
    g <- intersect(input$map_groups, BASEMAP_WEB)
    g1 <- if (length(g)) g[1] else NULL
    if (identical(g1, last_base())) return()
    last_base(g1)
    px <- leafletProxy("map"); sel <- layers_sel()
    for (gg in LAYER_ALL) if (gg %in% sel) showGroup(px, gg) else hideGroup(px, gg)
  })
  
  observeEvent(input$opac, ignoreInit = TRUE, {
    px <- leafletProxy("map")
    for (k in names(R_DISP)) {
      add_raster_layer(px, k, input$opac)
      if (!k %in% layers_sel()) hideGroup(px, k)
    }
  })
  
  observeEvent(list(choro_idx_web(), choro_stat_web()), ignoreInit = TRUE, {
    px <- leafletProxy("map")
    draw_choro(px, choro_idx_web(), choro_stat_web())
    if (!"choro" %in% layers_sel()) hideGroup(px, "choro")
    update_legends(px, layers_sel(), choro_idx_web(), choro_stat_web())
  })
  
  # --- pilihan kecamatan ---------
  neutral <- function() {
    selected(NULL)
    updateSelectInput(session, "zoom_kec", selected = "")
    px <- leafletProxy("map") %>% clearGroup("sel") %>% clearPopups()
    if (!is.null(kab)) {
      bb <- sf::st_bbox(kab)
      px %>% fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    }
  }
  
  observeEvent(input$map_shape_click, {
    cl <- input$map_shape_click
    if (!identical(cl$group, "kec_poly")) return()
    if (identical(cl$id, selected())) { mark_click(cl$lat, cl$lng); neutral() }
    else { selected(cl$id); show_info(cl$lat, cl$lng) }
  })
  
  observeEvent(input$zoom_kec, ignoreInit = TRUE, {
    req(kec)
    id <- input$zoom_kec
    if (!nzchar(id)) { neutral(); return() }
    selected(id)
    bb <- sf::st_bbox(kec[kec$kec_nama == id, ])
    leafletProxy("map") %>% fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })
  
  observeEvent(selected(), ignoreNULL = FALSE, ignoreInit = TRUE, {
    px <- leafletProxy("map") %>% clearGroup("sel")
    id <- selected()
    if (!is.null(id) && !is.null(kec)) {
      px %>% addPolygons(data = kec[kec$kec_nama == id, ], group = "sel", fill = FALSE,
                         color = "#00E5FF", weight = 4, opacity = 1,
                         options = pathOptions(pane = "pane_sel", interactive = FALSE))
    }
  })
  
  # --- informasi titik ----
  last_click <- new.env(); last_click$key <- ""; last_click$t <- 0
  click_key <- function(lat, lng) sprintf("%.6f,%.6f", lat, lng)
  mark_click <- function(lat, lng) { last_click$key <- click_key(lat, lng); last_click$t <- as.numeric(Sys.time()) }
  show_info <- function(lat, lng) {
    now <- as.numeric(Sys.time())
    if (identical(click_key(lat, lng), last_click$key) && now - last_click$t < 1) return(invisible(NULL))
    mark_click(lat, lng)
    if (!isTRUE(input$probe) || length(R_FULL) == 0) return(invisible(NULL))
    sel <- layers_sel()
    ks <- intersect(names(R_FULL), sel)                  # raster yang sedang tampil
    if (length(ks) == 0) ks <- names(R_FULL)
    pt  <- terra::vect(cbind(lng, lat), crs = "EPSG:4326")
    pts <- sf::st_sfc(sf::st_point(c(lng, lat)), crs = 4326)
    ik <- NA_integer_
    if (!is.null(kec)) {
      hit <- which(lengths(suppressMessages(sf::st_intersects(sf::st_geometry(kec), pts))) > 0)
      if (length(hit)) ik <- hit[1]
    }
    nm_kec <- if (!is.na(ik)) kec$kec_nama[ik] else NA_character_
    xy <- tryCatch(sf::st_coordinates(sf::st_transform(pts, UTM_EPSG_USE)), error = function(e) NULL)
    fv <- function(v, d = 3) ifelse(is.na(v), "-", formatC(v, format = "f", digits = d))
    getv <- function(k) tryCatch(terra::extract(R_FULL[[k]], terra::project(pt, terra::crs(R_FULL[[k]])))[1, 2],
                                 error = function(e) NA_real_)
    
    rows <- character(0)
    for (k in intersect(ks, IDX_AVAIL)) {
      v  <- getv(k)
      sk <- if (!is.na(ik) && !is.null(STATS_TABLE)) STATS_TABLE[ik, ] else NULL
      g  <- function(st) if (is.null(sk)) NA_real_ else sk[[paste0(k, "_", st)]]
      rows <- c(rows, sprintf(
        "<tr><td><b>%s</b></td><td style='background:#fff3cd'><b>%s</b></td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
        k, fv(v), fv(g("Mean")), fv(g("Median")), fv(g("Modus")), fv(g("Min")), fv(g("Max")), fv(g("SD"))))
      if (!is.null(STATS_KAB)) {
        rk <- STATS_KAB[STATS_KAB$Indeks == k, ]
        if (nrow(rk) == 1) rows <- c(rows, sprintf(
          "<tr style='color:#555'><td>&nbsp;&nbsp;Kab. Bantul</td><td>-</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
          fv(rk$Mean), fv(rk$Median), fv(rk$Modus), fv(rk$Min), fv(rk$Max), fv(rk$SD)))
      }
    }
    tbl <- if (length(rows)) paste0(
      "<div style='margin:4px 0 2px'><b>Nilai indeks di titik &amp; statistik zonal</b>",
      if (!is.na(nm_kec)) paste0(" (Kecamatan ", nm_kec, ")") else "", "</div>",
      "<table border='1' cellpadding='3' style='border-collapse:collapse;font-size:11px;text-align:center'>",
      "<tr style='background:#e9ecef'><th>Layer</th><th>Titik</th><th>Mean</th><th>Median</th><th>Modus</th><th>Min</th><th>Max</th><th>SD</th></tr>",
      paste(rows, collapse = ""), "</table>") else ""
    
    lc_html <- ""
    if ("lc" %in% ks) {
      v <- getv("lc")
      lc_html <- if (is.na(v)) "<div style='margin-top:4px'>Tutupan lahan: di luar cakupan raster</div>" else
        sprintf("<div style='margin-top:5px'><b>Tutupan lahan di titik:</b> <span style='color:%s;font-size:15px'>&#9632;</span> <b>%s</b></div>",
                LC$warna[match(round(v), LC$kode)], LC$kelas[match(round(v), LC$kode)])
      if (!is.na(ik) && !is.null(LC_AREA) && !is.na(LC_AREA$Total_ha[ik]) && LC_AREA$Total_ha[ik] > 0) {
        ha <- unlist(LC_AREA[ik, LC$kelas])
        lc_html <- paste0(lc_html, sprintf(
          "<div>Komposisi Kec. %s: %s<br>Dominan: <b>%s</b> | Luas total %s ha</div>", nm_kec,
          paste(sprintf("%s %.1f%%", LC$kelas, 100 * ha / sum(ha)), collapse = " | "),
          LC_AREA$Kelas_Dominan[ik], fmt_ha(LC_AREA$Total_ha[ik])))
      }
    }
    ch_html <- ""
    if ("choro" %in% sel && !is.na(ik)) {
      ci <- choro_info(choro_idx_web(), choro_stat_web())
      if (!is.null(ci)) ch_html <- sprintf("<div style='margin-top:4px'>Choropleth <b>%s</b> (Kec. %s): <b>%s</b></div>",
                                           ci$title, nm_kec, fv(ci$v[ik]))
    }
    all_na <- length(rows) == 0 && !nzchar(lc_html)
    zona <- sprintf("UTM %d%s", as.integer(UTM_EPSG_USE %% 100), if (UTM_EPSG_USE >= 32700) "S" else "N")
    html <- paste0(
      "<div style='font-family:Times New Roman,serif;font-size:12px'>",
      "<b style='font-size:13px'>Informasi Titik</b><br>",
      sprintf("Lat/Lon: %.5f, %.5f", lat, lng),
      if (!is.null(xy)) sprintf("<br>%s: E %s | N %s", zona,
                                format(round(xy[1, 1]), big.mark = "."), format(round(xy[1, 2]), big.mark = ".")) else "",
      sprintf("<br>Kecamatan: <b>%s</b>", if (is.na(nm_kec)) "(di luar batas kecamatan)" else nm_kec),
      tbl, lc_html, ch_html,
      if (all_na) "<br><i>Titik berada di luar cakupan raster.</i>" else "", "</div>")
    leafletProxy("map") %>% clearPopups() %>%
      addPopups(lng, lat, html, options = popupOptions(maxWidth = 560, minWidth = 320))
    invisible(NULL)
  }
  observeEvent(input$map_click, {
    cl <- input$map_click
    show_info(cl$lat, cl$lng)
  })
  
  # --- ringkasan luas kelas -------------------------------------------
  output$boxes <- renderUI({
    tot <- sum(LC_TOTAL, na.rm = TRUE)
    ic  <- c("leaf", "building", "tint", "cloud")
    co  <- c("green", "red", "blue", "black")
    fluidRow(lapply(seq_len(nrow(LC)), function(i) {
      ha <- LC_TOTAL[[i]]
      infoBox(title = LC$kelas[i],
              value = if (is.na(ha)) "-" else paste(fmt_ha(ha), "ha"),
              subtitle = if (is.na(ha) || tot == 0) "raster belum dimuat"
              else sprintf("%.1f%% dari area studi", 100 * ha / tot),
              icon = icon(ic[i]), color = co[i], fill = TRUE, width = 3)
    }))
  })
  
  # --- detail kecamatan -----------------------------------------------------
  output$kec_name <- renderText({
    if (is.null(selected())) "Kondisi netral - klik sebuah kecamatan pada peta"
    else paste("Kecamatan", selected())
  })
  
  output$plt_lc <- renderPlotly({
    validate(need(!is.null(LC_AREA), "Raster klasifikasi belum dimuat."),
             need(!is.null(selected()), "Pilih kecamatan terlebih dahulu."))
    row <- LC_AREA[match(selected(), LC_AREA$Kecamatan), ]
    plot_ly(labels = LC$kelas, values = as.numeric(row[, LC$kelas]), type = "pie",
            hole = 0.5, sort = FALSE, marker = list(colors = LC$warna),
            textinfo = "label+percent", hoverinfo = "label+value",
            hovertemplate = "%{label}: %{value:,.1f} ha<extra></extra>") %>%
      layout(showlegend = FALSE, margin = list(t = 10, b = 10, l = 10, r = 10),
             paper_bgcolor = "rgba(0,0,0,0)")
  })
  
  output$tbl_sel <- renderDT({
    validate(need(!is.null(STATS_TABLE), "Raster indeks belum dimuat."),
             need(!is.null(selected()), "Pilih kecamatan terlebih dahulu."))
    row <- STATS_TABLE[match(selected(), STATS_TABLE$Kecamatan), ]
    df <- do.call(rbind, lapply(IDX_AVAIL, function(k)
      data.frame(Indeks = k, Mean = row[[paste0(k, "_Mean")]], Min = row[[paste0(k, "_Min")]],
                 Max = row[[paste0(k, "_Max")]], Median = row[[paste0(k, "_Median")]],
                 SD = row[[paste0(k, "_SD")]], Modus = row[[paste0(k, "_Modus")]])))
    datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = 10)) %>%
      formatRound(2:7, 4)
  })
  
  # --- tabel zonal statistics -----------------------------------------------
  output$tbl_zonal <- renderDT({
    validate(need(!is.null(STATS_TABLE),
                  "Hasil analisis belum tersedia. Jalankan 01_ANALISIS_RASTER.R terlebih dahulu."))
    datatable(STATS_TABLE, rownames = FALSE, extensions = "Buttons",
              options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"),
                             scrollX = TRUE, pageLength = 20)) %>%
      formatRound(2:ncol(STATS_TABLE), 4)
  })
  
  output$tbl_area <- renderDT({
    validate(need(!is.null(LC_AREA), "Raster klasifikasi belum tersedia."))
    datatable(LC_AREA, rownames = FALSE, extensions = "Buttons",
              options = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"),
                             scrollX = TRUE, pageLength = 20)) %>%
      formatRound(2:6, 2)
  })
  
  # --- tab threshold --------------------------------------------------------
  output$tbl_rules <- renderDT({
    datatable(RULES, rownames = FALSE, escape = FALSE,
              options = list(dom = "t", ordering = FALSE))
  })
  
  output$plt_hist <- renderPlotly({
    validate(need(length(IDX_AVAIL) > 0, "Raster indeks belum tersedia."))
    k <- input$thr_idx
    req(k %in% names(SAMPLES), length(SAMPLES[[k]]) > 0)
    tv <- switch(k,
                 NDVI = THRESH$NDVI_VEGETASI, NDWI = THRESH$NDWI_PERAIRAN,
                 NDBI = THRESH$NDBI_BANGUNAN,
                 NDCI = c(-THRESH$NDCI_AWAN_ABS, THRESH$NDCI_AWAN_ABS))
    shp <- lapply(tv, function(t)
      list(type = "line", x0 = t, x1 = t, y0 = 0, y1 = 1, yref = "paper",
           line = list(color = "#D50000", dash = "dash", width = 2)))
    plot_ly(x = SAMPLES[[k]], type = "histogram", nbinsx = 100,
            marker = list(color = "#2E7D32")) %>%
      layout(shapes = shp,
             xaxis = list(title = k, range = IDX_RANGE[[k]]),
             yaxis = list(title = "Jumlah piksel (sampel)"),
             bargap = 0.02)
  })
  
  # ======================= EKSPOR PETA CETAK ===============================
  observeEvent(list(input$lyr_raster, input$lyr_choro, input$lyr_batas,
                    input$lyr_choro_idx, input$lyr_choro_stat),
               ignoreNULL = FALSE, {
                 updateCheckboxGroupInput(session, "exp_raster", selected = input$lyr_raster)
                 updateCheckboxGroupInput(session, "exp_batas", selected = input$lyr_batas)
                 updateCheckboxInput(session, "exp_choro", value = isTRUE(input$lyr_choro))
                 if (!is.null(input$lyr_choro_idx)) updateSelectInput(session, "exp_choro_idx", selected = input$lyr_choro_idx)
                 if (!is.null(input$lyr_choro_stat)) updateSelectInput(session, "exp_choro_stat", selected = input$lyr_choro_stat)
               })
  
  observeEvent(input$map_groups, {
    g <- intersect(input$map_groups, names(BASEMAP_CETAK))
    if (length(g) >= 1) updateSelectInput(session, "exp_basemap", selected = BASEMAP_CETAK[[g[1]]])
  })
  
  tgl_now <- function() {
    x <- input$client_dt
    d <- if (!is.null(x) && nzchar(x)) as.Date(substr(x, 1, 10))
    else as.Date(format(Sys.time(), tz = "Asia/Jakarta"))
    tgl_indo(d)
  }
  exp_opts <- function() {
    list(judul = trimws(input$exp_judul), sub = trimws(input$exp_sub),
         layers = c(input$exp_raster, if (isTRUE(input$exp_choro)) "choro", input$exp_batas),
         basemap = if (is.null(input$exp_basemap)) "OpenStreetMap" else input$exp_basemap,
         basemap_inset = if (is.null(input$exp_basemap_inset)) "OpenStreetMap" else input$exp_basemap_inset,
         paper = input$exp_paper,
         paperW = if (identical(input$exp_paper, "A4")) 297 else 420,
         choro_idx = if (is.null(input$exp_choro_idx)) CHORO_DEFAULT else input$exp_choro_idx,
         choro_stat = if (is.null(input$exp_choro_stat)) "Mean" else input$exp_choro_stat,
         opac = input$exp_opac, tgl = tgl_now(),
         epsg = if (is.null(input$exp_utm) || identical(input$exp_utm, "auto")) UTM_EPSG_USE
         else as.integer(input$exp_utm),
         grid = isTRUE(input$exp_grid),
         grid_int = if (is.null(input$exp_gridint)) "auto" else input$exp_gridint,
         skala = if (is.null(input$exp_skala)) "auto" else input$exp_skala,
         focus = if (isTRUE(input$exp_focus)) isolate(selected()) else NULL)
  }
  
  output$preview <- bindEvent(
    renderPlot({
      validate(need(!is.null(kab), "Batas kabupaten belum dimuat (jalankan 01_ANALISIS_RASTER.R terlebih dahulu)."))
      draw_peta(exp_opts(), "preview")
    }, width = 1000, height = 707, res = 96),
    input$btn_prev, ignoreInit = TRUE)
  
  output$dl_peta <- downloadHandler(
    filename = function() sprintf("Peta_Tutupan_Lahan_Bantul_%s.%s",
                                  format(Sys.time(), "%Y%m%d_%H%M"),
                                  switch(input$exp_fmt, PDF = "pdf", PNG = "png", JPEG = "jpg")),
    content = function(file) {
      if (is.null(kab)) {
        showNotification("Batas kabupaten belum dimuat.", type = "error")
        stop("Batas kabupaten belum dimuat")
      }
      o <- exp_opts()
      Wm <- o$paperW; Hm <- Wm * 297 / 420
      dpi <- as.integer(input$exp_dpi)
      pw <- round(Wm / 25.4 * dpi); ph <- round(Hm / 25.4 * dpi)
      withProgress(message = "Membuat peta cetak...", value = 0.4, {
        if (identical(input$exp_fmt, "PDF")) {
          if (isTRUE(capabilities("cairo")))
            grDevices::cairo_pdf(file, width = Wm / 25.4, height = Hm / 25.4, pointsize = 12)
          else grDevices::pdf(file, width = Wm / 25.4, height = Hm / 25.4, pointsize = 12)
        } else {
          args <- list(file, width = pw, height = ph, units = "px", res = dpi, pointsize = 12)
          if (isTRUE(capabilities("cairo"))) args$type <- "cairo"
          if (identical(input$exp_fmt, "JPEG")) { args$quality <- 95; do.call(grDevices::jpeg, args) }
          else do.call(grDevices::png, args)
        }
        tryCatch(draw_peta(o, "export"), error = function(e) {
          showNotification(paste("Gagal membuat peta:", conditionMessage(e)), type = "error", duration = 20)
          lg("[GAGAL] Ekspor peta: ", conditionMessage(e))
        })
        grDevices::dev.off()
      })
    })
  
  # ======================= UNDUH DATA RASTER & GEOPACKAGES ========
  dl_epsg <- function() if (identical(input$dl_crs, "4326")) 4326L else as.integer(UTM_EPSG_USE)
  
  for (k in names(R_FULL)) local({
    kk <- k
    output[[paste0("dl_r_", kk)]] <- downloadHandler(
      filename = function() sprintf("%s_Bantul_EPSG%d.tif", RASTER_FILE_LABEL[[kk]], dl_epsg()),
      content = function(file) {
        r <- R_FULL[[kk]]
        if (is.null(r)) stop("Raster belum tersedia")
        ep <- dl_epsg()
        withProgress(message = "Menyiapkan GeoTIFF...", value = 0.5, {
          if (!isTRUE(tryCatch(terra::same.crs(r, paste0("EPSG:", ep)), error = function(e) FALSE)))
            r <- terra::project(r, paste0("EPSG:", ep), method = if (kk == "lc") "near" else "bilinear")
          if (kk == "lc") {
            names(r) <- "tutupan_lahan"
            tryCatch(terra::coltab(r) <- data.frame(value = LC$kode, col = LC$warna),
                     error = function(e) NULL)
            terra::writeRaster(r, file, overwrite = TRUE, datatype = "INT1U", NAflag = 0,
                               gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
          } else {
            names(r) <- kk
            terra::writeRaster(r, file, overwrite = TRUE, datatype = "FLT4S",
                               gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
          }
        })
      })
  })
  
  output$dl_gpkg <- downloadHandler(
    filename = function() sprintf("Vektor_Bantul_EPSG%d.gpkg", dl_epsg()),
    content = function(file) {
      if (is.null(kab) || is.null(kec)) stop("Batas administrasi belum dimuat")
      ep <- dl_epsg()
      withProgress(message = "Menyiapkan GeoPackage...", value = 0.5, {
        kab_o <- sf::st_sf(nama = "Kabupaten Bantul", geometry = sf::st_union(sf::st_geometry(kab)))
        kec_o <- sf::st_sf(kecamatan = kec$kec_nama, geometry = sf::st_geometry(kec))
        if (!is.null(STATS_TABLE))
          for (cn in setdiff(names(STATS_TABLE), "Kecamatan")) kec_o[[cn]] <- STATS_TABLE[[cn]]
        if (!is.null(LC_AREA)) {
          for (kl in LC$kelas) {
            kec_o[[paste0(kl, "_ha")]] <- LC_AREA[[kl]]
            kec_o[[paste0("PCT_", kl)]] <- kec[[paste0("PCT_", kl)]]
          }
          kec_o$Total_ha <- LC_AREA$Total_ha
          kec_o$Kelas_Dominan <- LC_AREA$Kelas_Dominan
        }
        kab_o <- sf::st_transform(kab_o, ep); kec_o <- sf::st_transform(kec_o, ep)
        unlink(file)
        sf::st_write(kab_o, file, layer = "batas_kabupaten", quiet = TRUE)
        sf::st_write(kec_o, file, layer = "batas_kecamatan", quiet = TRUE)
        sf::st_write(sf::st_sf(nama = kab_o$nama, geometry = sf::st_boundary(sf::st_geometry(kab_o))),
                     file, layer = "garis_batas_kabupaten", quiet = TRUE)
        sf::st_write(sf::st_sf(kecamatan = kec_o$kecamatan,
                               geometry = sf::st_boundary(sf::st_geometry(kec_o))),
                     file, layer = "garis_batas_kecamatan", quiet = TRUE)
      })
    })
}

shinyApp(ui, server)
