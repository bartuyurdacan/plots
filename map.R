# ============================================================
# 0) VERİYİ YÜKLE
# ============================================================
library(readxl)
data <- read_excel("C:/Users/bartu/Desktop/data.xlsx")

# Hızlı bakış
print(summary(data))
stopifnot(all(c("Province","District","Average SES score") %in% names(data)))

# ============================================================
# 1) PAKETLER
# ============================================================
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(stringi)
  library(ggplot2)
  library(viridis)
  library(tibble)
})

# ============================================================
# 2) TÜRKÇE NORMALİZASYON
# ============================================================
normalize_tr <- function(x) {
  x %>%
    str_replace_all("İ", "I") %>%
    str_replace_all("İ", "I") %>%
    str_replace_all("ı", "i") %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_upper(locale = "tr") %>%
    str_squish()
}

# ============================================================
# 3) VERİYİ HAZIRLA
#    (District boş/NA olanlar il ortalaması satırlarıdır)
# ============================================================
df_ilce <- data %>%
  rename(
    il   = Province,
    ilce = District,
    ses  = `Average SES score`
  ) %>%
  mutate(
    il   = str_squish(as.character(il)),
    ilce = str_squish(as.character(ilce)),
    ses  = suppressWarnings(as.numeric(ses))
  ) %>%
  filter(!is.na(ilce) & ilce != "") %>%   # sadece ilçe düzeyi
  mutate(
    il_key   = normalize_tr(il),
    ilce_key = normalize_tr(ilce)
  )

df_il <- data %>%
  rename(
    il   = Province,
    ilce = District,
    ses  = `Average SES score`
  ) %>%
  filter(is.na(ilce) | ilce == "") %>%
  transmute(il = str_squish(as.character(il)), ses_il = suppressWarnings(as.numeric(ses)))

# ============================================================
# 4) COĞRAFYA: İLÇE SINIRLARINI OKU (Yerel GeoJSON/Shape)
# ============================================================
path_geojson <- "tr_ilce.geojson"   # <- kendi yolunu ver
ilce_sf <- sf::st_read(path_geojson, quiet = TRUE)

# il / ilçe kolonu adlarını otomatik bul
name_candidates <- names(ilce_sf)
col_il   <- name_candidates[str_detect(name_candidates, regex("\\b(il|province|NAME_1)\\b", ignore_case = TRUE))][1]
col_ilce <- name_candidates[str_detect(name_candidates, regex("\\b(ilce|district|NAME_2)\\b",    ignore_case = TRUE))][1]
if (is.na(col_il) || is.na(col_ilce)) {
  stop("GeoJSON içindeki il/ilçe sütunlarını tespit edemedim. Lütfen 'col_il' ve 'col_ilce' değişkenlerini elle ayarla.")
}

ilce_sf <- ilce_sf %>%
  rename(il_geo = !!sym(col_il),
         ilce_geo = !!sym(col_ilce)) %>%
  mutate(
    il_key_geo   = normalize_tr(as.character(il_geo)),
    ilce_key_geo = normalize_tr(as.character(ilce_geo))
  )

# ============================================================
# 5) EŞLEŞTİRME
# ============================================================
map_df <- ilce_sf %>%
  left_join(
    df_ilce %>% select(il_key, ilce_key, ses),
    by = c("il_key_geo" = "il_key", "ilce_key_geo" = "ilce_key")
  )

# ============================================================
# 6) NA KONTROLÜ — EKRANA YAZDIR
# ============================================================
unmatched <- map_df %>%
  filter(is.na(ses)) %>%
  st_drop_geometry() %>%
  distinct(il_geo, ilce_geo) %>%
  arrange(il_geo, ilce_geo) %>%
  as_tibble()

cat("\n[NA LİSTESİ — Haritada var, veride yok] Toplam:", nrow(unmatched), "\n\n")
if (nrow(unmatched) > 0) {
  print(unmatched, n = nrow(unmatched))
} else {
  cat("NA yok. Tüm ilçeler eşleşti.\n")
}

cat("\n[İL BAZINDA ÖZET]\n\n")
na_by_prov <- unmatched %>% count(il_geo, sort = TRUE)
if (nrow(na_by_prov) > 0) print(na_by_prov, n = nrow(na_by_prov)) else cat("—\n")

cat("\n[MAPPING ŞABLONU — ilce_fix'i elle doldurabilirsiniz]\n\n")
mapping_skeleton <- unmatched %>%
  transmute(il_geo, ilce_geo, ilce_fix = "")
if (nrow(mapping_skeleton) > 0) print(mapping_skeleton, n = nrow(mapping_skeleton)) else cat("—\n")

# ============================================================
# 7) (OPSİYONEL) HIZLI MAPPING UYGULA VE TEKRAR KONTROL ET
#    APPLY_FIX <- TRUE yaparsanız yaygın farkları düzeltir.
# ============================================================
APPLY_FIX <- FALSE   # <- isterseniz TRUE yapın

if (APPLY_FIX) {
  mapping_ilce_add <- unmatched %>%
    mutate(
      ilce_fix = dplyr::case_when(
        # "Merkez" -> yeni merkez ilçeler (örnekler)
        il_geo == "Aydın"         & ilce_geo == "Merkez"        ~ "Efeler",
        il_geo == "Balıkesir"     & ilce_geo == "Merkez"        ~ "Altıeylül",
        il_geo == "Denizli"       & ilce_geo == "Merkez"        ~ "Merkezefendi",
        il_geo == "Diyarbakır"    & ilce_geo == "Merkez"        ~ "Kayapınar",
        il_geo == "Eskişehir"     & ilce_geo == "Merkez"        ~ "Odunpazarı",
        il_geo == "Kahramanmaraş" & ilce_geo == "Merkez"        ~ "Onikişubat",
        il_geo == "Manisa"        & ilce_geo == "Merkez"        ~ "Şehzadeler",
        il_geo == "Muğla"         & ilce_geo == "Merkez"        ~ "Menteşe",
        il_geo == "Tekirdağ"      & ilce_geo == "Merkez"        ~ "Süleymanpaşa",
        il_geo == "Şanlıurfa"     & ilce_geo == "Merkez"        ~ "Haliliye",
        il_geo == "Antalya"       & ilce_geo == "Merkez"        ~ "Muratpaşa",
        # Yazım/isim değişiklikleri
        il_geo == "Afyonkarahisar" & ilce_geo == "Sincanlı"     ~ "Sinanpaşa",
        il_geo == "Ağrı"           & grepl("^Doğubey", ilce_geo, ignore.case = TRUE) ~ "Doğubayazıt",
        il_geo == "Erzurum"        & grepl("^Ilıca$|^İlıca$", ilce_geo) ~ "Aziziye",
        il_geo == "İstanbul"       & (ilce_geo %in% c("Eyüp","EYUP","Eyup")) ~ "Eyüpsultan",
        il_geo == "Samsun"         & grepl("ONDOKUZ|Ondokuz", normalize_tr(ilce_geo)) ~ "Ondokuzmayıs",
        il_geo == "Siirt"          & ilce_geo == "Aydınlar"     ~ "Tillo",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(ilce_fix)) %>%
    select(il_geo, ilce_geo, ilce_fix)
  
  cat("\n[UYGULANACAK MAPPING]\n\n")
  if (nrow(mapping_ilce_add) > 0) print(mapping_ilce_add, n = nrow(mapping_ilce_add)) else cat("Mapping listesi boş.\n")
  
  # Mapping'i uygula
  ilce_sf <- ilce_sf %>%
    left_join(mapping_ilce_add, by = c("il_geo","ilce_geo")) %>%
    mutate(ilce_geo = dplyr::coalesce(ilce_fix, ilce_geo)) %>%
    select(-ilce_fix) %>%
    mutate(
      il_key_geo   = normalize_tr(as.character(il_geo)),
      ilce_key_geo = normalize_tr(as.character(ilce_geo))
    )
  
  # Yeniden eşleştir
  map_df <- ilce_sf %>%
    left_join(df_ilce %>% select(il_key, ilce_key, ses),
              by = c("il_key_geo" = "il_key", "ilce_key_geo" = "ilce_key"))
  
  unmatched_after <- map_df %>%
    filter(is.na(ses)) %>%
    st_drop_geometry() %>%
    distinct(il_geo, ilce_geo) %>%
    arrange(il_geo, ilce_geo) %>%
    as_tibble()
  
  cat("\n[KONTROL] Mapping sonrası NA kalan:", nrow(unmatched_after), "\n\n")
  if (nrow(unmatched_after) > 0) print(unmatched_after, n = nrow(unmatched_after)) else cat("Tebrikler! NA = 0.\n")
}

# ============================================================
# 8) ESTETİK: İL SINIRI KONTURU
# ============================================================
il_borders <- ilce_sf %>%
  st_drop_geometry() %>%
  select(il_geo) %>%
  distinct() %>%
  left_join(
    ilce_sf %>% group_by(il_geo) %>% summarise(geometry = st_union(geometry), .groups = "drop"),
    by = "il_geo"
  ) %>% st_as_sf()

# ============================================================
# 9) HARİTA (İlçe bazlı)
# ============================================================
p_ilce <- ggplot() +
  geom_sf(data = map_df, aes(fill = ses), color = "white", size = 0.08) +
  geom_sf(data = il_borders, fill = NA, color = "grey25", size = 0.25) +
  scale_fill_viridis(name = "Ortalama SES", option = "magma", direction = -1, na.value = "grey90") +
  coord_sf(crs = 4326) +
  labs(
    title = "Türkiye İlçe Bazlı Ortalama SES Skoru",
    subtitle = "Kaynak: data (Province/District/Average SES score)",
    caption  = "R: sf · dplyr · ggplot2 · viridis"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

print(p_ilce)
ggsave("turkiye_ilce_ses.png", p_ilce, width = 12, height = 9, dpi = 300)

# ============================================================
# 10) (İSTEĞE BAĞLI) İL ORT. HARİTASI
# ============================================================
il_union <- ilce_sf %>%
  group_by(il_geo) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()

il_means <- map_df %>%
  st_drop_geometry() %>%
  group_by(il_geo) %>%
  summarise(ses_il_calc = mean(ses, na.rm = TRUE), .groups = "drop")

il_map <- il_union %>% left_join(il_means, by = "il_geo")

p_il <- ggplot(il_map) +
  geom_sf(aes(fill = ses_il_calc), color = "white", size = 0.2) +
  scale_fill_viridis(name = "İl Ort. SES", option = "plasma", direction = -1, na.value = "grey90") +
  labs(
    title = "Türkiye İl Bazlı Ortalama SES Skoru",
    subtitle = "İlçe SES değerlerinin il düzeyinde (ağırlıksız) ortalaması",
    caption = "R: sf · dplyr · ggplot2 · viridis"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )

print(p_il)
ggsave("turkiye_il_ses.png", p_il, width = 10, height = 7.5, dpi = 300)

suppressPackageStartupMessages({
  library(leaflet)
  library(htmlwidgets)
})

# Renk skalası (NA'lar için açık gri)
pal <- colorNumeric(palette = "magma",
                    domain = map_df$ses,
                    reverse = TRUE,
                    na.color = "#D9D9D9")

leaf <- leaflet(map_df, options = leafletOptions(minZoom = 5)) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(ses),
    weight = 0.3,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(weight = 1.5, color = "black", bringToFront = TRUE),
    label = ~paste0(ilce_geo, " (", il_geo, "): ",
                    ifelse(is.na(ses), "—", sprintf("%.1f", ses))),
    popup = ~paste0("<b>", ilce_geo, "</b> (", il_geo, ")",
                    "<br/>SES: ", ifelse(is.na(ses), "—", sprintf("%.2f", ses)))
  ) %>%
  addLegend("bottomright", pal = pal, values = ~ses,
            title = "Ortalama SES", opacity = 1)

# Ekranda göster
leaf

# HTML olarak kaydet (çift tıkla açabilirsin)
saveWidget(leaf, file = "turkiye_ilce_ses_interaktif.html", selfcontained = TRUE)
cat('\nEtkileşimli harita kaydedildi: "turkiye_ilce_ses_interaktif.html"\n')







# ============================================================
# 11) ETKİLEŞİMLİ HARİTA (Leaflet)
# ============================================================
suppressPackageStartupMessages({
  library(leaflet)
  library(htmlwidgets)
})

# Renk skalası (NA'lar için açık gri)
pal <- colorNumeric(palette = "magma",
                    domain = map_df$ses,
                    reverse = TRUE,
                    na.color = "#D9D9D9")

leaf <- leaflet(map_df, options = leafletOptions(minZoom = 5)) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(ses),
    weight = 0.3,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(weight = 1.5, color = "black", bringToFront = TRUE),
    label = ~paste0(ilce_geo, " (", il_geo, "): ",
                    ifelse(is.na(ses), "—", sprintf("%.1f", ses))),
    popup = ~paste0("<b>", ilce_geo, "</b> (", il_geo, ")",
                    "<br/>SES: ", ifelse(is.na(ses), "—", sprintf("%.2f", ses)))
  ) %>%
  addLegend("bottomright", pal = pal, values = ~ses,
            title = "Ortalama SES", opacity = 1)

# Ekranda göster
leaf

# HTML olarak kaydet (çift tıkla açabilirsin)
saveWidget(leaf, file = "turkiye_ilce_ses_interaktif.html", selfcontained = TRUE)
cat('\nEtkileşimli harita kaydedildi: "turkiye_ilce_ses_interaktif.html"\n')

