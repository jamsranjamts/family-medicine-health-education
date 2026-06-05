# ============================================================
# Өрхийн эмнэлгийн эрүүл мэндийн боловсролын Quarto site
# One-click render + QR generator
# ============================================================

# 1) Энэ файлыг site folder дотор ажиллуулна.
# 2) Quarto суусан байх шаардлагатай: https://quarto.org/docs/get-started/
# 3) Сайтаа GitHub Pages/Netlify дээр байршуулсны дараа site_url-г өөрийн URL болгоно.

site_url <- "https://YOUR-GITHUB-USERNAME.github.io/YOUR-REPOSITORY-NAME/"

required_packages <- c("quarto", "readr", "dplyr")
for (p in required_packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

# QR код үүсгэх package. Алдаа гарвал install хийнэ.
if (!requireNamespace("qrencoder", quietly = TRUE)) {
  install.packages("qrencoder")
}

project_dir <- getwd()
message("Project folder: ", project_dir)

# Quarto CLI байгаа эсэхийг шалгах
if (!quarto::quarto_available()) {
  stop("Quarto CLI олдсонгүй. Quarto суулгаад RStudio-г restart хийнэ үү: https://quarto.org/docs/get-started/")
}

# Site render хийх
message("Rendering Quarto website...")
quarto::quarto_render(input = project_dir)
message("Render completed. Output folder: docs/")

# QR холбоосын жагсаалт
links <- data.frame(
  page = c(
    "Нүүр хуудас",
    "Тархины цус харвалт",
    "Зүрхний шигдээс",
    "Эпилепси",
    "Бөөрний дутагдал",
    "Аутизм",
    "Харвалтын дараах дасгал",
    "Холголт цооролт"
  ),
  file = c(
    "index.html",
    "stroke.html",
    "heart-attack.html",
    "epilepsy.html",
    "kidney-failure.html",
    "autism.html",
    "post-stroke-exercises.html",
    "pressure-injury.html"
  ),
  qr_name = c(
    "qr_index.png",
    "qr_stroke.png",
    "qr_heart_attack.png",
    "qr_epilepsy.png",
    "qr_kidney.png",
    "qr_autism.png",
    "qr_post_stroke_exercises.png",
    "qr_pressure_injury.png"
  ),
  stringsAsFactors = FALSE
)

links$url <- paste0(gsub("/$", "", site_url), "/", links$file)

dir.create("qr", showWarnings = FALSE)
readr::write_csv(links, "qr/qr_links.csv")

# URL солигдоогүй бол QR үүсгэхгүй, зөвхөн template гаргана.
if (grepl("YOUR-GITHUB-USERNAME|YOUR-REPOSITORY-NAME", site_url)) {
  message("site_url тохируулаагүй байна. Сайтаа online байршуулсны дараа site_url-г өөрийн URL болгож дахин ажиллуулна уу.")
  message("QR холбоосын template: qr/qr_links.csv")
} else {
  message("Generating QR codes...")
  for (i in seq_len(nrow(links))) {
    out_file <- file.path("qr", links$qr_name[i])
    tryCatch({
      qrencoder::qrencode_png(links$url[i], out_file)
      message("Saved: ", out_file)
    }, error = function(e) {
      message("QR үүсгэхэд алдаа гарлаа: ", links$page[i], " - ", e$message)
    })
  }
  message("QR codes saved in qr/ folder.")
}

message("Done.")
