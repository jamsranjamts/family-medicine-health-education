# ============================================================
# Нинжин манал ӨЭМТ — PURPOSEFUL FULL REBUILD SCRIPT
# Quarto website + meaningful vector diagrams + logo QR + A4 print pages
# Author credit: © АШУҮИС, оюутан Ш.Жамсранжамц
# ============================================================

# ---------- 0) BASIC SETTINGS ----------
project_dir <- "C:/quarto_sites/family_medicine_health_site"
if (!dir.exists(project_dir)) {
  message("C:/quarto_sites/family_medicine_health_site олдсонгүй. Одоогийн folder дээр ажиллуулна.")
  project_dir <- getwd()
}
setwd(project_dir)

clinic_name <- "Нинжин манал ӨЭМТ"
site_title  <- "Нинжин манал ӨЭМТ | Эрүүл мэндийн зөвлөмж"
site_url    <- "https://jamsranjamts.github.io/family-medicine-health-education/"
copyright_text <- "© АШУҮИС, оюутан Ш.Жамсранжамц"
disclaimer <- "Энэхүү мэдээлэл нь олон нийтийн эрүүл мэндийн боловсролд зориулагдсан бөгөөд эмчийн үзлэг, оношилгоо, эмчилгээг орлохгүй. Яаралтай шинж илэрвэл 103 болон ойролцоох эрүүл мэндийн байгууллагад хандана уу."

message("Project folder: ", normalizePath(project_dir, winslash = "/", mustWork = FALSE))

# ---------- 1) PACKAGES ----------
need <- c("qrencoder", "png", "jpeg", "base64enc")
for (p in need) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cran.rstudio.com")
  }
}

# ---------- 2) DIRECTORIES ----------
dirs <- c(
  "assets", "assets/logo", "assets/illustrations", "qr", "print", "docs"
)
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---------- 3) LOGO HANDLING ----------
# Script нь хамгийн түрүүнд assets/logo эсвэл assets/images доторх logo файлыг хайна.
# Хэрэв байхгүй бол түр placeholder logo үүсгэнэ.
find_logo <- function() {
  candidates <- c(
    "assets/logo/logo-original.png",
    "assets/logo/logo-original.jpg",
    "assets/logo/logo-original.jpeg",
    "assets/logo/logo.png",
    "assets/logo/logo.jpg",
    "assets/logo/logo.jpeg",
    "assets/logo/ninjin_manal_logo.png",
    "assets/logo/ninjin_manal_logo.jpg",
    "assets/images/logo.png",
    "assets/images/logo.jpg",
    "347020167_966058017909615_6932910234920813803_n.jpg"
  )
  candidates[file.exists(candidates)][1]
}

read_image_rgba <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("png")) {
    img <- png::readPNG(path)
  } else if (ext %in% c("jpg", "jpeg")) {
    img <- jpeg::readJPEG(path)
  } else {
    stop("Unsupported logo type: ", path)
  }
  if (length(dim(img)) == 2) {
    img <- array(rep(img, 4), dim = c(dim(img), 4))
    img[, , 4] <- 1
  }
  if (dim(img)[3] == 3) {
    alpha <- matrix(1, nrow = dim(img)[1], ncol = dim(img)[2])
    img <- array(c(img, alpha), dim = c(dim(img)[1], dim(img)[2], 4))
  }
  img
}

make_placeholder_logo <- function(out = "assets/logo/logo-transparent.png") {
  png::writePNG(array(1, dim = c(10, 10, 4)), out)
  png(filename = out, width = 900, height = 900, bg = "transparent", res = 150)
  par(mar = c(0, 0, 0, 0))
  plot.new()
  symbols(0.5, 0.5, circles = 0.46, inches = FALSE, bg = "#0f766e", fg = "#0f766e", add = TRUE)
  symbols(0.5, 0.5, circles = 0.39, inches = FALSE, bg = "#ffffff", fg = "#ffffff", add = TRUE)
  text(0.5, 0.57, "Нинжин", cex = 2.4, font = 2, col = "#0f766e")
  text(0.5, 0.47, "манал", cex = 2.4, font = 2, col = "#0f766e")
  text(0.5, 0.34, "ӨЭМТ", cex = 1.9, font = 2, col = "#1e3a8a")
  dev.off()
  out
}

make_transparent_logo <- function(input_path, out = "assets/logo/logo-transparent.png") {
  if (is.na(input_path) || !file.exists(input_path)) return(make_placeholder_logo(out))
  img <- read_image_rgba(input_path)
  # white / near-white background арилгах энгийн арга
  rgb_mean <- (img[, , 1] + img[, , 2] + img[, , 3]) / 3
  whiteish <- img[, , 1] > 0.88 & img[, , 2] > 0.88 & img[, , 3] > 0.88 & rgb_mean > 0.9
  img[, , 4][whiteish] <- 0
  png::writePNG(img, out)
  out
}

logo_input <- find_logo()
logo_png <- make_transparent_logo(logo_input, "assets/logo/logo-transparent.png")
file.copy(logo_png, "assets/logo/logo-small.png", overwrite = TRUE)

logo_data_uri <- function(path = logo_png) {
  if (!file.exists(path)) return("")
  enc <- base64enc::base64encode(path)
  paste0("data:image/png;base64,", enc)
}
logo_uri <- logo_data_uri(logo_png)

# ---------- 4) MEANINGFUL VECTOR ILLUSTRATIONS ----------
esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

wrap_svg_text <- function(text, x, y, max_chars = 32, size = 26, color = "#334155", weight = 400) {
  words <- unlist(strsplit(text, " "))
  lines <- character(0)
  cur <- ""
  for (w in words) {
    test <- if (cur == "") w else paste(cur, w)
    if (nchar(test) > max_chars) {
      lines <- c(lines, cur)
      cur <- w
    } else cur <- test
  }
  if (cur != "") lines <- c(lines, cur)
  paste0(
    sprintf('<text x="%s" y="%s" font-family="Arial, sans-serif" font-size="%s" fill="%s" font-weight="%s">', x, y, size, color, weight),
    paste0(sprintf('<tspan x="%s" dy="%s">%s</tspan>', x, c(0, rep(size * 1.25, max(0, length(lines)-1))), esc(lines)), collapse = ""),
    '</text>'
  )
}

make_diagram_svg <- function(file, title, subtitle, panels, accent = "#0f766e") {
  w <- 1800; h <- 1050
  n <- length(panels)
  # 2 columns layout
  col_w <- 800; row_h <- 300
  xs <- c(120, 930, 120, 930, 120, 930)
  ys <- c(275, 275, 595, 595, 0, 0)
  svg_parts <- c(
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%s" height="%s" viewBox="0 0 %s %s">', w, h, w, h),
    '<defs><filter id="shadow" x="-20%" y="-20%" width="140%" height="140%"><feDropShadow dx="0" dy="10" stdDeviation="10" flood-color="#0f172a" flood-opacity="0.13"/></filter></defs>',
    '<rect width="1800" height="1050" rx="42" fill="#f8fafc"/>',
    sprintf('<rect x="0" y="0" width="1800" height="205" fill="%s"/>', accent),
    '<circle cx="1620" cy="75" r="160" fill="#ffffff" opacity="0.12"/>',
    '<circle cx="1450" cy="35" r="90" fill="#ffffff" opacity="0.10"/>',
    sprintf('<text x="105" y="92" font-family="Arial, sans-serif" font-size="52" font-weight="800" fill="#ffffff">%s</text>', esc(title)),
    sprintf('<text x="108" y="148" font-family="Arial, sans-serif" font-size="30" fill="#e0f2fe">%s</text>', esc(subtitle))
  )
  for (i in seq_len(n)) {
    x <- xs[i]; y <- ys[i]
    p <- panels[[i]]
    svg_parts <- c(svg_parts,
      sprintf('<rect x="%s" y="%s" width="%s" height="%s" rx="34" fill="#ffffff" filter="url(#shadow)"/>', x, y, col_w, row_h),
      sprintf('<circle cx="%s" cy="%s" r="54" fill="%s" opacity="0.16"/>', x+80, y+82, accent),
      sprintf('<circle cx="%s" cy="%s" r="37" fill="%s"/>', x+80, y+82, accent),
      sprintf('<text x="%s" y="%s" text-anchor="middle" dominant-baseline="middle" font-family="Arial" font-size="30" font-weight="800" fill="#ffffff">%s</text>', x+80, y+82, i),
      sprintf('<text x="%s" y="%s" font-family="Arial, sans-serif" font-size="34" font-weight="800" fill="#0f172a">%s</text>', x+150, y+70, esc(p$title)),
      wrap_svg_text(p$text, x+150, y+120, max_chars = 38, size = 26, color = "#334155"),
      sprintf('<path d="M %s %s L %s %s" stroke="%s" stroke-width="8" stroke-linecap="round" opacity="0.35"/>', x+150, y+245, x+700, y+245, accent)
    )
  }
  # footer logo/brand strip
  svg_parts <- c(svg_parts,
    '<rect x="0" y="950" width="1800" height="100" fill="#ffffff"/>',
    '<line x1="90" y1="950" x2="1710" y2="950" stroke="#e2e8f0" stroke-width="3"/>'
  )
  if (nzchar(logo_uri)) {
    svg_parts <- c(svg_parts, sprintf('<image href="%s" x="104" y="967" width="62" height="62" preserveAspectRatio="xMidYMid meet"/>', logo_uri))
  }
  svg_parts <- c(svg_parts,
    sprintf('<text x="182" y="1008" font-family="Arial, sans-serif" font-size="30" font-weight="800" fill="#0f766e">%s</text>', esc(clinic_name)),
    sprintf('<text x="1710" y="1008" text-anchor="end" font-family="Arial, sans-serif" font-size="24" fill="#64748b">%s</text>', esc(copyright_text)),
    '</svg>'
  )
  writeLines(svg_parts, file, useBytes = TRUE)
}

illustrations <- list(
  stroke = list(
    file = "assets/illustrations/stroke-fast.svg", accent = "#dc2626",
    title = "Тархины цус харвалт: FAST арга",
    subtitle = "Нүүр, гар, хэл яриа, цаг — 103 дуудна",
    panels = list(
      list(title="Нүүр", text="Нүүрний нэг тал унжих, инээхэд тэгш бус болох."),
      list(title="Гар", text="Нэг гар сулрах, өргөж чадахгүй болох."),
      list(title="Хэл яриа", text="Үг ээрэх, ойлгомжгүй ярих, ойлгохгүй болох."),
      list(title="Цаг", text="Шинж илэрсэн цагийг тэмдэглээд 103 дуудна.")
    )
  ),
  heart = list(
    file = "assets/illustrations/heart-attack-warning.svg", accent = "#e11d48",
    title = "Зүрхний шигдээс: аюултай шинж",
    subtitle = "Цээж өвдөх, амьсгаадах, хүйтэн хөлс — яаралтай тусламж",
    panels = list(
      list(title="Цээжний өвдөлт", text="Дарах, базлах мэт өвдөлт 5 минутаас дээш үргэлжлэх."),
      list(title="Дамжих өвдөлт", text="Зүүн гар, мөр, эрүү, нуруу руу дамжиж болно."),
      list(title="Дагалдах шинж", text="Амьсгаадах, хүйтэн хөлс гарах, дотор муухайрах."),
      list(title="103 дууд", text="Өөрөө машин барихгүй, түргэн тусламж дуудна.")
    )
  ),
  epilepsy = list(
    file = "assets/illustrations/epilepsy-first-aid.svg", accent = "#7c3aed",
    title = "Эпилепси: таталтын үеийн тусламж",
    subtitle = "Хажуу тийш харуулж, хамгаалж, цаг харна",
    panels = list(
      list(title="Хамгаална", text="Толгойг зөөлөн зүйлээр хамгаалж, ойр орчны хатуу зүйлсийг холдуулна."),
      list(title="Хажуу байрлал", text="Амьсгалын замыг чөлөөтэй байлгахын тулд хажуу тийш харуулна."),
      list(title="Аманд юм хийхгүй", text="Халбага, ус, эм зэргийг аманд хийхгүй."),
      list(title="5 минут", text="Таталт 5 минутаас дээш бол 103 дуудна.")
    )
  ),
  kidney = list(
    file = "assets/illustrations/kidney-monitoring.svg", accent = "#0891b2",
    title = "Бөөрний дутагдал: эрсдэлийг хянах",
    subtitle = "Даралт, сахар, шээс, креатинин — тогтмол хяналт",
    panels = list(
      list(title="Даралт", text="Даралт ихсэлт бөөр гэмтээдэг тул тогтмол хэмжинэ."),
      list(title="Сахар", text="Чихрийн шижинтэй бол сахарын хяналт сайн байх хэрэгтэй."),
      list(title="Шинжилгээ", text="Шээсний уураг, креатинин, eGFR зэрэг үзүүлэлтийг хянана."),
      list(title="Эмчийн хяналт", text="Хаван, шээс багасах, ядрах шинж илэрвэл эмчид үзүүлнэ.")
    )
  ),
  autism = list(
    file = "assets/illustrations/autism-therapy.svg", accent = "#2563eb",
    title = "Аутизм: эрт дэмжлэг ба засал",
    subtitle = "Хэл засал, хөдөлгөөн засал, хөдөлмөр засал — хамтын дэмжлэг",
    panels = list(
      list(title="Хэл засал", text="Харилцаа, ойлголт, үг хэллэгийг өдөр тутам дэмжинэ."),
      list(title="Хөдөлгөөн засал", text="Тэнцвэр, хөдөлгөөний зохицуулалт, булчингийн чадварыг хөгжүүлнэ."),
      list(title="Хөдөлмөр засал", text="Өөртөө үйлчлэх, тоглох, суралцах дадлыг сайжруулна."),
      list(title="Гэр бүл", text="Эцэг эх, багш, эмч мэргэжилтэн хамтран ажиллана.")
    )
  ),
  exercise = list(
    file = "assets/illustrations/post-stroke-exercises.svg", accent = "#16a34a",
    title = "Харвалтын дараах саажилт: дасгал",
    subtitle = "Аюулгүй, тогтмол, бага багаар ахиулна",
    panels = list(
      list(title="Гар", text="Хуруу тэнийлгэх, атгах, бугуй хөдөлгөх дасгалыг зөөлөн хийнэ."),
      list(title="Мөр", text="Мөрний үеийг өвдөлтгүй хүрээнд хөдөлгөнө."),
      list(title="Хөл", text="Шагай дээш доош хөдөлгөх, өвдөг нугалах дасгал хийнэ."),
      list(title="Суух тэнцвэр", text="Орноос босох, суух, тэнцвэр барихыг хүнтэй хамт давтана.")
    )
  ),
  pressure = list(
    file = "assets/illustrations/pressure-injury-care.svg", accent = "#ea580c",
    title = "Холголт, цооролт: урьдчилан сэргийлэлт",
    subtitle = "Байрлал солих, арьс шалгах, хуурай цэвэр байлгах",
    panels = list(
      list(title="2 цаг тутам", text="Хэвтрийн хүний байрлалыг тогтмол сольж даралтыг багасгана."),
      list(title="Арьс шалгах", text="Улайх, халуу оргих, өвдөх, цэврүүтэхийг өдөр бүр шалгана."),
      list(title="Дэр, зөөлөвч", text="Өсгий, ууц, тохой зэрэг дарагдах хэсгийг зөөлөвчилнө."),
      list(title="Хуурай байлгах", text="Чийг, хөлс, шээс, бохирдлыг даруй цэвэрлэнэ.")
    )
  )
)

for (nm in names(illustrations)) {
  x <- illustrations[[nm]]
  make_diagram_svg(x$file, x$title, x$subtitle, x$panels, x$accent)
}

# ---------- 5) TOPIC DATA ----------
topics <- list(
  list(
    id="stroke", file="stroke.qmd", html="stroke.html", title="Тархины цус харвалт", short="FAST аргаар шинжийг таньж, цаг алдалгүй 103 дуудна.", image="assets/illustrations/stroke-fast.svg", color="#dc2626",
    sections=list(
      list(id="oilgolt", title="Товч ойлголт", body="Тархины цус харвалт нь тархины судас бөглөрөх эсвэл хагарах үед үүсдэг яаралтай тусламж шаардсан эмгэг юм. Эрт таньж, түргэн тусламж авах нь амь нас болон саажилтын эрсдэлийг бууруулна."),
      list(id="fast", title="FAST арга", body="Нүүрний нэг тал унжих, нэг гар сулрах, хэл яриа ээрэх эсвэл ойлгомжгүй болох шинж илэрвэл цаг алдалгүй 103 дуудна. Шинж эхэлсэн цагийг тэмдэглэх нь эмчилгээний шийдвэрт чухал."),
      list(id="yaraltai", title="Яаралтай 103 дуудах шинж", body="Гэнэт гар хөл сулрах, нүүр мурийх, хэл яриа өөрчлөгдөх, хараа бүдгэрэх, толгой хүчтэй өвдөх, тэнцвэр алдагдах шинжүүдийн аль нэг илэрвэл яаралтай тусламж дуудна."),
      list(id="sergiilelt", title="Урьдчилан сэргийлэлт", body="Даралт, сахар, холестеринээ хянах, тамхинаас татгалзах, архи хэтрүүлэхгүй байх, жингээ зохистой барих, тогтмол хөдөлгөөн хийх нь харвалтын эрсдэлийг бууруулна.")
    )
  ),
  list(
    id="heart-attack", file="heart-attack.qmd", html="heart-attack.html", title="Зүрхний шигдээс", short="Цээж базлах, амьсгаадах, хүйтэн хөлс гарах үед яаралтай тусламж дуудна.", image="assets/illustrations/heart-attack-warning.svg", color="#e11d48",
    sections=list(
      list(id="oilgolt", title="Товч ойлголт", body="Зүрхний шигдээс нь зүрхний булчинд очих цусны урсгал багассанаас үүсдэг яаралтай эмгэг юм. Шинжийг үл тоомсорлох нь амь насанд эрсдэлтэй."),
      list(id="shinj", title="Аюултай шинж", body="Цээжээр дарах, базлах, шатах мэт өвдөх, зүүн гар, эрүү, нуруу руу дамжих, амьсгаадах, хүйтэн хөлс гарах, дотор муухайрах шинж илэрч болно."),
      list(id="yu-hiih", title="Гэртээ юу хийх вэ?", body="Хөдөлгөөнөө зогсоож сууна, тусламж дуудна, ганцаараа үлдэхгүй. Өөрөө машин барьж эмнэлэг явахгүй. Эмчийн өмнө заасан эм байгаа бол зааврын дагуу хэрэглэнэ."),
      list(id="sergiilelt", title="Урьдчилан сэргийлэлт", body="Даралт, сахар, холестеринээ хянах, тамхи татахгүй байх, өөх тос ихтэй хоол багасгах, тогтмол алхах, эмчийн бичсэн эмийг таслахгүй хэрэглэх нь чухал.")
    )
  ),
  list(
    id="epilepsy", file="epilepsy.qmd", html="epilepsy.html", title="Эпилепси", short="Таталтын үед хүнийг хамгаалж, хажуу тийш харуулж, аманд зүйл хийхгүй.", image="assets/illustrations/epilepsy-first-aid.svg", color="#7c3aed",
    sections=list(
      list(id="oilgolt", title="Товч ойлголт", body="Эпилепси нь тархины цахилгаан идэвхийн өөрчлөлтөөс давтамжтай таталт үүсэх эмгэг юм. Таталтын үед зөв тусламж үзүүлэх нь гэмтлээс сэргийлнэ."),
      list(id="tuslamj", title="Таталтын үед авах арга хэмжээ", body="Хүнийг аюулгүй газар хэвтүүлж, толгойг хамгаална. Боломжтой бол хажуу тийш харуулна. Таталт эхэлсэн цагийг харна."),
      list(id="bolohgui", title="Хийж болохгүй зүйл", body="Аманд нь халбага, ус, эм хийхгүй. Хүчээр барьж дарахгүй. Ухаан ороогүй үед идэж уух зүйл өгөхгүй."),
      list(id="yaraltai", title="Хэзээ 103 дуудах вэ?", body="Таталт 5 минутаас дээш үргэлжлэх, давтан татах, амьсгал хэвийн болохгүй байх, жирэмсэн, усанд байсан, гэмтсэн тохиолдолд 103 дуудна.")
    )
  ),
  list(
    id="kidney-failure", file="kidney-failure.qmd", html="kidney-failure.html", title="Бөөрний дутагдал", short="Даралт, сахар, шээс, креатининыг тогтмол хянах нь бөөр хамгаална.", image="assets/illustrations/kidney-monitoring.svg", color="#0891b2",
    sections=list(
      list(id="oilgolt", title="Товч ойлголт", body="Бөөрний дутагдал нь бөөр биеийн илүүдэл шингэн, хорт бодисыг шүүх чадвараа алдах үед үүсдэг. Эрт үед шинж бага байж болох тул хяналтын шинжилгээ чухал."),
      list(id="ersdel", title="Эрсдэлт хүчин зүйл", body="Даралт ихсэлт, чихрийн шижин, бөөрний үрэвсэл, өвчин намдаах эмийг удаан хэрэглэх, удамшил, насжилт зэрэг нь эрсдэл нэмэгдүүлнэ."),
      list(id="shinjilgee", title="Хяналтын шинжилгээ", body="Шээсний уураг, креатинин, eGFR, цусны даралт, сахарын хяналт, хаван байгаа эсэхийг эмчийн зөвлөгөөгөөр шалгана."),
      list(id="sergiilelt", title="Урьдчилан сэргийлэлт", body="Давс багасгах, даралт сахараа барих, ус шингэнээ зохистой хэрэглэх, эмийг дур мэдэн хэрэглэхгүй байх, эмчийн хяналтад тогтмол үзүүлэх нь чухал.")
    )
  ),
  list(
    id="autism", file="autism.qmd", html="autism.html", title="Аутизм ба засал", short="Хүүхдийн харилцаа, хэл яриа, хөдөлгөөн, өдөр тутмын чадварыг эрт дэмжинэ.", image="assets/illustrations/autism-therapy.svg", color="#2563eb",
    sections=list(
      list(id="oilgolt", title="Товч ойлголт", body="Аутизмын хүрээний эмгэг нь харилцаа, нийгмийн харилцан үйлдэл, давтагдмал зан үйлээр илэрч болох хөгжлийн онцлог юм. Эрт илрүүлэлт, тогтмол дэмжлэг чухал."),
      list(id="hel-zasal", title="Хэл засал", body="Хэл засал нь хүүхдийн ойлгох, илэрхийлэх, ээлжлэн харилцах, дохио зангаа, үг хэллэгийн чадварыг өдөр тутмын орчинд хөгжүүлэхэд чиглэнэ."),
      list(id="hudulguun", title="Хөдөлгөөн засал", body="Хөдөлгөөн засал нь тэнцвэр, биеийн байрлал, хөдөлгөөний зохицуулалт, булчингийн хүч, орчныг мэдрэх чадварыг дэмжинэ."),
      list(id="hudulmur", title="Хөдөлмөр засал", body="Хөдөлмөр засал нь хувцаслах, хооллох, бичих, тоглох, анхаарал төвлөрүүлэх, мэдрэхүйн зохицуулалт зэрэг өдөр тутмын чадварыг хөгжүүлнэ.")
    )
  ),
  list(
    id="post-stroke-exercises", file="post-stroke-exercises.qmd", html="post-stroke-exercises.html", title="Саажилтын үеийн дасгал", short="Харвалтын дараах дасгалыг аюулгүй, бага багаар, тогтмол хийдэг.", image="assets/illustrations/post-stroke-exercises.svg", color="#16a34a",
    sections=list(
      list(id="ankhaaral", title="Эхлэхийн өмнө", body="Дасгалыг өвдөлтгүй хүрээнд, унах эрсдэлгүй орчинд, боломжтой бол асран хамгаалагчтай хамт хийнэ. Толгой эргэх, амьсгаадах, өвдөх үед зогсооно."),
      list(id="gar", title="Гарын дасгал", body="Хуруу тэнийлгэх, атгах, бугуй дээш доош хөдөлгөх, тохой нугалж тэнийлгэх дасгалыг удаан, зөөлөн давтана."),
      list(id="hul", title="Хөлийн дасгал", body="Шагай дээш доош хөдөлгөх, өвдөг нугалах, өсгий гулсуулах, суугаа байрлалаас өвдөг тэнийлгэх дасгалыг бага давтамжаар эхэлнэ."),
      list(id="tentsver", title="Тэнцвэр ба шилжилт", body="Орноос суух, сандал дээр зөв суух, хоёр тал руу жин шилжүүлэх, босож суух дасгалыг хамгаалалттайгаар хийнэ.")
    )
  ),
  list(
    id="pressure-injury", file="pressure-injury.qmd", html="pressure-injury.html", title="Холголт, цооролт", short="Байрлал солих, арьсаа шалгах, хуурай цэвэр байлгах нь хамгийн чухал.", image="assets/illustrations/pressure-injury-care.svg", color="#ea580c",
    sections=list(
      list(id="oilgolt", title="Товч ойлголт", body="Холголт, цооролт нь нэг хэсэг газар удаан дарагдсанаас арьс болон доорх эд гэмтэхийг хэлнэ. Хэвтрийн, хөдөлгөөн хязгаарлагдсан хүмүүст элбэг."),
      list(id="ersdel", title="Эрсдэлтэй хэсэг", body="Ууц, өсгий, тохой, дал, ташаа, чихний ар зэрэг яс товойсон хэсэгт даралт их үүсдэг тул өдөр бүр шалгана."),
      list(id="sergiilelt", title="Урьдчилан сэргийлэх", body="2 цаг тутам байрлал солих, арьсыг хуурай цэвэр байлгах, зөөлөвч хэрэглэх, үрэлт чирэгдлийг багасгах, хоол унд уургийг анхаарах хэрэгтэй."),
      list(id="emchid", title="Хэзээ эмчид хандах вэ?", body="Арьс улайгаад арилахгүй байх, цэврүүтэх, шархлах, шүүс гарах, эвгүй үнэр гарах, халуурах шинж илэрвэл эмчид үзүүлнэ.")
    )
  )
)

# ---------- 6) CSS ----------
styles <- '
:root{
  --teal:#0f766e; --teal2:#14b8a6; --blue:#1d4ed8; --sky:#e0f2fe;
  --red:#dc2626; --rose:#e11d48; --purple:#7c3aed; --green:#16a34a; --orange:#ea580c;
  --ink:#0f172a; --muted:#64748b; --line:#e2e8f0; --soft:#f8fafc; --card:#ffffff;
}
body{background:linear-gradient(180deg,#f8fafc 0%,#eefdfb 100%); color:var(--ink); font-family:Arial, sans-serif;}
.navbar{box-shadow:0 8px 28px rgba(15,23,42,.10); border-bottom:1px solid rgba(15,118,110,.16)}
.navbar-title{font-weight:900; letter-spacing:.2px}
.hero{position:relative; overflow:hidden; border-radius:34px; padding:46px; margin:20px 0 30px; color:white; background:radial-gradient(circle at 85% 10%,rgba(255,255,255,.22),transparent 30%),linear-gradient(135deg,#0f766e 0%,#2563eb 100%); box-shadow:0 18px 50px rgba(15,118,110,.25)}
.hero:after{content:""; position:absolute; right:-80px; bottom:-100px; width:340px; height:340px; border-radius:50%; background:rgba(255,255,255,.13)}
.hero h1{font-size:44px; line-height:1.05; margin:0 0 14px; font-weight:950}.hero p{font-size:20px; max-width:900px; opacity:.96}.hero-badges{display:flex; flex-wrap:wrap; gap:12px; margin-top:20px}.badge-soft{background:rgba(255,255,255,.18); border:1px solid rgba(255,255,255,.34); color:#fff; padding:10px 14px; border-radius:999px; font-weight:800}
.brand-logo{width:74px; height:74px; object-fit:contain; background:white; padding:8px; border-radius:22px; box-shadow:0 10px 22px rgba(0,0,0,.14)}
.hero-top{display:flex; align-items:center; gap:18px; margin-bottom:20px}
.topic-grid{display:grid; grid-template-columns:repeat(auto-fit,minmax(260px,1fr)); gap:22px; margin:28px 0}.topic-card{background:white; border:1px solid var(--line); border-radius:28px; padding:22px; box-shadow:0 12px 32px rgba(15,23,42,.08); transition:.18s transform,.18s box-shadow; min-height:260px}.topic-card:hover{transform:translateY(-4px); box-shadow:0 18px 42px rgba(15,23,42,.13)}.topic-card img{width:100%; height:150px; object-fit:cover; border-radius:20px; background:#f1f5f9; border:1px solid #e2e8f0}.topic-card h3{font-size:23px; margin:16px 0 8px; font-weight:950}.topic-card p{color:var(--muted); min-height:54px}.btn-main{display:inline-block; padding:11px 16px; border-radius:999px; color:white!important; background:linear-gradient(135deg,#0f766e,#2563eb); text-decoration:none; font-weight:900; margin-top:10px}.btn-secondary{display:inline-block; padding:9px 13px; border-radius:999px; color:#0f766e!important; background:#ecfeff; text-decoration:none; font-weight:850; margin:5px 6px 5px 0; border:1px solid #99f6e4}
.page-head{background:white; border:1px solid var(--line); border-radius:34px; padding:28px; margin:22px 0; box-shadow:0 14px 36px rgba(15,23,42,.08)}.page-head-inner{display:grid; grid-template-columns:1.2fr .8fr; gap:28px; align-items:center}.page-head h1{font-size:40px; margin:0 0 12px; font-weight:950}.page-head p{font-size:19px; color:var(--muted)}.page-head img{width:100%; border-radius:26px; box-shadow:0 12px 28px rgba(15,23,42,.12); border:1px solid var(--line)}
.subnav{background:#ffffffcc; backdrop-filter:blur(10px); border:1px solid #ccfbf1; border-radius:24px; padding:14px; margin:18px 0 28px; box-shadow:0 10px 26px rgba(15,118,110,.08)}.subnav-title{font-weight:950; color:#0f766e; margin:0 0 8px}.section-card{background:white; border:1px solid var(--line); border-left:10px solid var(--teal); border-radius:28px; padding:26px 28px; margin:22px 0; box-shadow:0 12px 30px rgba(15,23,42,.075)}.section-card h2{margin:0 0 12px; font-size:28px; font-weight:950}.section-card p{font-size:18px; line-height:1.75; color:#334155}.alert-box{border-radius:24px; padding:20px; margin:18px 0; background:#fff7ed; border:1px solid #fed7aa; color:#7c2d12}.danger-box{border-radius:24px; padding:20px; margin:18px 0; background:#fef2f2; border:1px solid #fecaca; color:#7f1d1d}.info-box{border-radius:24px; padding:20px; margin:18px 0; background:#eff6ff; border:1px solid #bfdbfe; color:#1e3a8a}
.qr-grid{display:grid; grid-template-columns:repeat(auto-fit,minmax(220px,1fr)); gap:20px}.qr-card{background:white; border:1px solid var(--line); border-radius:26px; padding:18px; text-align:center; box-shadow:0 10px 28px rgba(15,23,42,.08)}.qr-card img{width:180px; height:180px; object-fit:contain}.qr-card h3{font-size:18px; margin:10px 0 4px; font-weight:950}.qr-card a{font-size:14px; color:#0f766e; font-weight:800}.print-link{display:inline-block; margin-top:8px; padding:8px 12px; border-radius:999px; background:#f0fdfa; color:#0f766e!important; border:1px solid #99f6e4; text-decoration:none; font-weight:850}
.footer-note{background:#0f172a; color:white; border-radius:28px; padding:24px; margin:36px 0 18px}.footer-note strong{color:#99f6e4}.disclaimer{font-size:14px; color:#dbeafe}.print-only{display:none}
@media(max-width:780px){.hero{padding:28px}.hero h1{font-size:32px}.page-head-inner{grid-template-columns:1fr}.page-head h1{font-size:32px}}
@media print{@page{size:A4;margin:12mm}body{background:white}.navbar,.sidebar,.subnav,.btn-main,.btn-secondary,.print-link{display:none!important}.page-head,.section-card,.qr-card{box-shadow:none;break-inside:avoid}.print-only{display:block}.hero{box-shadow:none;color:#000;background:white;border:2px solid #0f766e}.hero .badge-soft{color:#0f766e;border-color:#0f766e}.topic-card{break-inside:avoid}}
'
writeLines(styles, "styles.css", useBytes = TRUE)

# ---------- 7) QUARTO CONFIG ----------
q_yml <- paste0(
'project:\n',
'  type: website\n',
'  output-dir: docs\n\n',
'website:\n',
'  title: "', clinic_name, '"\n',
'  navbar:\n',
'    logo: assets/logo/logo-transparent.png\n',
'    left:\n',
'      - href: index.qmd\n        text: Нүүр\n',
'      - href: stroke.qmd\n        text: Тархины харвалт\n',
'      - href: heart-attack.qmd\n        text: Зүрхний шигдээс\n',
'      - href: epilepsy.qmd\n        text: Эпилепси\n',
'      - href: kidney-failure.qmd\n        text: Бөөр\n',
'      - href: autism.qmd\n        text: Аутизм\n',
'      - href: post-stroke-exercises.qmd\n        text: Дасгал\n',
'      - href: pressure-injury.qmd\n        text: Холголт\n',
'      - href: downloads.qmd\n        text: QR татах\n',
'      - href: references.qmd\n        text: Эх сурвалж\n',
'  page-footer:\n',
'    left: "', copyright_text, '"\n',
'    right: "', clinic_name, '"\n\n',
'format:\n',
'  html:\n',
'    theme: cosmo\n',
'    css: styles.css\n',
'    toc: true\n',
'    toc-location: right\n',
'    page-layout: full\n',
'    smooth-scroll: true\n'
)
writeLines(q_yml, "_quarto.yml", useBytes = TRUE)

# ---------- 8) QR LINK TABLE ----------
normalize_url <- function(base, path) paste0(sub("/+$", "", base), "/", path)
qr_items <- data.frame(
  id = "home", title = "Нүүр хуудас", path = "index.html", type = "Үндсэн", stringsAsFactors = FALSE
)
for (tp in topics) {
  qr_items <- rbind(qr_items, data.frame(id = tp$id, title = tp$title, path = tp$html, type = "Үндсэн сэдэв", stringsAsFactors = FALSE))
  for (sec in tp$sections) {
    qr_items <- rbind(qr_items, data.frame(
      id = paste(tp$id, sec$id, sep = "-"),
      title = paste(tp$title, "—", sec$title),
      path = paste0(tp$html, "#", sec$id),
      type = "Дэд хэсэг",
      stringsAsFactors = FALSE
    ))
  }
}

# ---------- 9) QR GENERATION WITH CENTER LOGO ----------
resize_nearest <- function(mat, scale) mat[rep(seq_len(nrow(mat)), each = scale), rep(seq_len(ncol(mat)), each = scale), , drop = FALSE]

safe_read_logo <- function(path) {
  if (!file.exists(path)) return(NULL)
  img <- tryCatch(png::readPNG(path), error = function(e) NULL)
  if (is.null(img)) return(NULL)
  if (length(dim(img)) == 2) {
    img <- array(rep(img, 4), dim = c(dim(img), 4)); img[, , 4] <- 1
  }
  if (dim(img)[3] == 3) {
    a <- matrix(1, dim(img)[1], dim(img)[2])
    img <- array(c(img, a), dim = c(dim(img)[1], dim(img)[2], 4))
  }
  img
}

make_qr_png <- function(url, out_file, title = "", logo_file = logo_png, scale = 16, logo_frac = 0.12) {
  mat <- qrencoder::qrencode(url)  # compatible with older qrencoder versions
  mat <- as.matrix(mat)
  mat <- mat != 0
  border <- 4
  m <- matrix(FALSE, nrow = nrow(mat) + border*2, ncol = ncol(mat) + border*2)
  m[(border+1):(border+nrow(mat)), (border+1):(border+ncol(mat))] <- mat
  pix <- m[rep(seq_len(nrow(m)), each=scale), rep(seq_len(ncol(m)), each=scale)]
  h <- nrow(pix); w <- ncol(pix)
  img <- array(1, dim = c(h, w, 4))
  # dark teal modules
  img[, , 1][pix] <- 0.02
  img[, , 2][pix] <- 0.17
  img[, , 3][pix] <- 0.16
  img[, , 4] <- 1
  # white rounded-ish square behind logo and small logo overlay
  logo <- safe_read_logo(logo_file)
  if (!is.null(logo)) {
    L <- round(min(h, w) * logo_frac)
    # keep logo aspect ratio
    lh0 <- dim(logo)[1]; lw0 <- dim(logo)[2]
    if (lw0 >= lh0) { lw <- L; lh <- max(1, round(L * lh0 / lw0)) } else { lh <- L; lw <- max(1, round(L * lw0 / lh0)) }
    # resize logo using nearest neighbor
    rr <- round(seq(1, lh0, length.out = lh))
    cc <- round(seq(1, lw0, length.out = lw))
    lg <- logo[rr, cc, , drop=FALSE]
    cx <- floor(w/2); cy <- floor(h/2)
    pad <- round(L * 0.35)
    x0 <- max(1, cx - floor(lw/2)); x1 <- min(w, x0 + lw - 1)
    y0 <- max(1, cy - floor(lh/2)); y1 <- min(h, y0 + lh - 1)
    bx0 <- max(1, x0 - pad); bx1 <- min(w, x1 + pad)
    by0 <- max(1, y0 - pad); by1 <- min(h, y1 + pad)
    img[by0:by1, bx0:bx1, 1:3] <- 1
    img[by0:by1, bx0:bx1, 4] <- 1
    lg <- lg[1:(y1-y0+1), 1:(x1-x0+1), , drop=FALSE]
    a <- lg[, , 4]
    for (ch in 1:3) img[y0:y1, x0:x1, ch] <- lg[, , ch] * a + img[y0:y1, x0:x1, ch] * (1-a)
    img[y0:y1, x0:x1, 4] <- pmax(img[y0:y1, x0:x1, 4], a)
  }
  png::writePNG(img, out_file)
  invisible(out_file)
}

for (i in seq_len(nrow(qr_items))) {
  qr_items$url[i] <- normalize_url(site_url, qr_items$path[i])
  qr_items$file[i] <- paste0("qr/", qr_items$id[i], ".png")
  make_qr_png(qr_items$url[i], qr_items$file[i], qr_items$title[i])
}
write.csv(qr_items, "qr/qr_links.csv", row.names = FALSE, fileEncoding = "UTF-8")

# ---------- 10) PAGE WRITERS ----------
footer_html <- paste0(
'<div class="footer-note">\n',
'<p><strong>', clinic_name, '</strong></p>\n',
'<p>', copyright_text, '</p>\n',
'<p class="disclaimer">', disclaimer, '</p>\n',
'</div>\n'
)

card_html <- function(tp) {
  paste0(
'<div class="topic-card">\n',
'<img src="', tp$image, '" alt="', tp$title, '">\n',
'<h3>', tp$title, '</h3>\n',
'<p>', tp$short, '</p>\n',
'<a class="btn-main" href="', tp$html, '">Дэлгэрэнгүй</a>\n',
'<a class="btn-secondary" href="print/', tp$id, '-print.html">A4 хэвлэх</a>\n',
'</div>\n'
  )
}

index_lines <- c(
'---',
'format: html',
'---',
'',
paste0('<div class="hero"><div class="hero-top"><img class="brand-logo" src="assets/logo/logo-transparent.png"><div><h1>', clinic_name, '</h1><p>Өрхийн эмнэлгээс иргэдэд зориулсан эрүүл мэндийн боловсролын цахим зөвлөмж. QR код уншуулаад хаанаас ч үзэх боломжтой.</p></div></div><div class="hero-badges"><span class="badge-soft">QR-аар уншина</span><span class="badge-soft">A4 хэвлэх боломжтой</span><span class="badge-soft">Иргэдэд ойлгомжтой</span></div></div>'),
'',
'<div class="info-box"><strong>Анхааруулга:</strong> Яаралтай шинж илэрвэл 103 болон ойролцоох эрүүл мэндийн байгууллагад хандана уу.</div>',
'',
'## Сэдвүүд',
'',
'<div class="topic-grid">',
unlist(lapply(topics, card_html)),
'</div>',
footer_html
)
writeLines(index_lines, "index.qmd", useBytes = TRUE)

make_topic_qmd <- function(tp) {
  section_links <- paste0('<a class="btn-secondary" href="#', sapply(tp$sections, `[[`, "id"), '">', sapply(tp$sections, `[[`, "title"), '</a>', collapse = "\n")
  sec_cards <- c()
  for (sec in tp$sections) {
    # highlight emergency-like sections
    box <- if (grepl("Яаралтай|103|Аюултай", sec$title)) "danger-box" else if (grepl("Урьдчилан|Хийж болохгүй", sec$title)) "alert-box" else "info-box"
    sec_cards <- c(sec_cards,
      paste0('<section id="', sec$id, '" class="section-card" style="border-left-color:', tp$color, '">'),
      paste0('<h2>', sec$title, '</h2>'),
      paste0('<p>', sec$body, '</p>'),
      paste0('<div class="', box, '"><strong>Санамж:</strong> Энэ хэсгийн QR кодыг QR татах хуудсаас авч хэвлэх боломжтой.</div>'),
      '</section>'
    )
  }
  lines <- c(
    '---', 'format: html', '---', '',
    paste0('<div class="page-head"><div class="page-head-inner"><div><h1>', tp$title, '</h1><p>', tp$short, '</p><a class="btn-main" href="qr/', tp$id, '.png">Энэ сэдвийн QR татах</a> <a class="btn-secondary" href="print/', tp$id, '-print.html">A4 хэвлэх</a></div><div><img src="', tp$image, '" alt="', tp$title, '"></div></div></div>'),
    '<div class="subnav"><div class="subnav-title">Энэ хуудсанд</div>', section_links, '</div>',
    sec_cards,
    footer_html
  )
  writeLines(lines, tp$file, useBytes = TRUE)
}
for (tp in topics) make_topic_qmd(tp)

# ---------- 11) QR DOWNLOAD PAGE ----------
qr_card <- function(id, title, path, type) {
  paste0(
'<div class="qr-card">\n',
'<img src="qr/', id, '.png" alt="', title, '">\n',
'<h3>', title, '</h3>\n',
'<p style="color:#64748b;font-size:13px;">', type, '</p>\n',
'<a href="qr/', id, '.png" download>QR татах</a>\n',
'</div>\n'
  )
}
qr_main <- qr_items[qr_items$type != "Дэд хэсэг",]
qr_sub <- qr_items[qr_items$type == "Дэд хэсэг",]

download_lines <- c(
'---', 'format: html', '---', '',
'<div class="hero"><div class="hero-top"><img class="brand-logo" src="assets/logo/logo-transparent.png"><div><h1>QR кодууд</h1><p>Үндсэн сэдэв болон дэд хэсэг бүрийн QR код. Хэвлэхэд тохиромжтой PNG байдлаар татаж авна.</p></div></div></div>',
'## Үндсэн QR кодууд',
'<div class="qr-grid">',
apply(qr_main, 1, function(r) qr_card(r[["id"]], r[["title"]], r[["path"]], r[["type"]])),
'</div>',
'## Дэд хэсгийн QR кодууд',
'<div class="qr-grid">',
apply(qr_sub, 1, function(r) qr_card(r[["id"]], r[["title"]], r[["path"]], r[["type"]])),
'</div>',
'## A4 хэвлэх хувилбарууд',
'<div class="topic-grid">',
paste0('<div class="topic-card"><h3>', sapply(topics, `[[`, "title"), '</h3><p>A4 цаасан дээр хэвлэхэд тохиромжтой товч зөвлөмж.</p><a class="btn-main" href="print/', sapply(topics, `[[`, "id"), '-print.html">Хэвлэх хувилбар</a></div>'),
'</div>',
footer_html
)
writeLines(download_lines, "downloads.qmd", useBytes = TRUE)

# ---------- 12) REFERENCES PAGE ----------
refs <- c(
'---', 'format: html', '---', '',
'# Эх сурвалж ба санамж', '',
'<div class="section-card">',
'<h2>Ашигласан чиглэл</h2>',
'<p>Энэхүү цахим зөвлөмж нь өрхийн анагаах ухааны түвшинд иргэдэд ойлгомжтой байдлаар боловсруулсан олон нийтийн эрүүл мэндийн боловсролын материал юм. Яаралтай шинж, анхны тусламж, урьдчилан сэргийлэлтийн хэсгүүдийг олон улсад нийтлэг хэрэглэгддэг эрүүл мэндийн боловсролын зарчимд нийцүүлэв.</p>',
'</div>',
'<div class="section-card">',
'<h2>Санамж</h2>',
paste0('<p>', disclaimer, '</p>'),
'</div>',
footer_html
)
writeLines(refs, "references.qmd", useBytes = TRUE)

# ---------- 13) PRINT PAGES ----------
print_css <- paste0('
<!doctype html><html lang="mn"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>{TITLE}</title>
<style>
@page{size:A4;margin:12mm}body{font-family:Arial,sans-serif;color:#0f172a;margin:0;background:#fff}.sheet{width:100%;box-sizing:border-box}.head{display:flex;align-items:center;justify-content:space-between;border-bottom:4px solid #0f766e;padding-bottom:14px;margin-bottom:16px}.brand{display:flex;gap:12px;align-items:center}.brand img{width:62px;height:62px;object-fit:contain}.brand h1{font-size:25px;margin:0;color:#0f766e}.qr{width:105px;height:105px}.title{background:linear-gradient(135deg,#0f766e,#2563eb);color:#fff;border-radius:18px;padding:18px;margin:14px 0}.title h2{font-size:30px;margin:0}.title p{font-size:16px;margin:8px 0 0}.ill{width:100%;max-height:250px;object-fit:cover;border-radius:16px;border:1px solid #e2e8f0;margin:10px 0 14px}.grid{display:grid;grid-template-columns:1fr 1fr;gap:12px}.box{border:1px solid #e2e8f0;border-left:7px solid #0f766e;border-radius:16px;padding:13px;break-inside:avoid}.box h3{margin:0 0 7px;font-size:18px}.box p{font-size:14px;line-height:1.45;margin:0;color:#334155}.foot{border-top:2px solid #e2e8f0;margin-top:14px;padding-top:10px;font-size:12px;color:#64748b;display:flex;justify-content:space-between;gap:20px}.warn{background:#fff7ed;border:1px solid #fed7aa;border-radius:14px;padding:10px;margin-top:12px;font-size:13px;color:#7c2d12}@media print{.sheet{page-break-after:always}.no-print{display:none}}
</style></head><body><div class="sheet">')

make_print_page <- function(tp) {
  out <- paste0("print/", tp$id, "-print.html")
  qr_file <- paste0("../qr/", tp$id, ".png")
  html <- gsub("{TITLE}", tp$title, print_css, fixed = TRUE)
  html <- paste0(html,
    '<div class="head"><div class="brand"><img src="../assets/logo/logo-transparent.png"><div><h1>', clinic_name, '</h1><div>', copyright_text, '</div></div></div><img class="qr" src="', qr_file, '"></div>',
    '<div class="title"><h2>', tp$title, '</h2><p>', tp$short, '</p></div>',
    '<img class="ill" src="../', tp$image, '">',
    '<div class="grid">'
  )
  for (sec in tp$sections) {
    html <- paste0(html, '<div class="box"><h3>', sec$title, '</h3><p>', sec$body, '</p></div>')
  }
  html <- paste0(html, '</div><div class="warn"><strong>Санамж:</strong> ', disclaimer, '</div>',
                 '<div class="foot"><span>', clinic_name, '</span><span>', copyright_text, '</span></div>',
                 '</div></body></html>')
  writeLines(html, out, useBytes = TRUE)
}
for (tp in topics) make_print_page(tp)

# all QR print sheet
qr_print <- '<!doctype html><html lang="mn"><head><meta charset="utf-8"><title>QR кодууд</title><style>@page{size:A4;margin:10mm}body{font-family:Arial,sans-serif}.grid{display:grid;grid-template-columns:repeat(3,1fr);gap:10px}.card{border:1px solid #ddd;border-radius:12px;padding:8px;text-align:center;break-inside:avoid}.card img{width:120px;height:120px}.card h3{font-size:12px;margin:4px 0}.head{display:flex;align-items:center;gap:12px;border-bottom:3px solid #0f766e;margin-bottom:12px;padding-bottom:8px}.head img{width:55px}</style></head><body><div class="head"><img src="../assets/logo/logo-transparent.png"><div><h1>Нинжин манал ӨЭМТ — QR кодууд</h1><p>© АШУҮИС, оюутан Ш.Жамсранжамц</p></div></div><div class="grid">'
for (i in seq_len(nrow(qr_items))) qr_print <- paste0(qr_print, '<div class="card"><img src="../', qr_items$file[i], '"><h3>', qr_items$title[i], '</h3></div>')
qr_print <- paste0(qr_print, '</div></body></html>')
writeLines(qr_print, "print/all-qr-print.html", useBytes = TRUE)

# ---------- 14) OPTIONAL: CLEAN OLD LOW-QUALITY AI IMAGES ----------
# Хуучин утгагүй AI зураг байвал хэрэглэгдэхгүй. Шинэ сайтын бүх зураг assets/illustrations/*.svg-ээс уншина.

# ---------- 15) RENDER QUARTO ----------
# docs-г цэвэрлээд render хийх боловч qr/ ба print/ source folder-уудыг хадгална.
unlink("docs", recursive = TRUE, force = TRUE)
dir.create("docs", showWarnings = FALSE)

message("Rendering Quarto website...")
render_ok <- FALSE
try({
  quarto::quarto_render(input = ".", as_job = FALSE)
  render_ok <- TRUE
}, silent = TRUE)
if (!render_ok) {
  system2("quarto", args = c("render"), stdout = TRUE, stderr = TRUE)
}

# Static folders-ийг docs руу баталгаатай хуулна.
if (dir.exists("qr")) {
  dir.create("docs/qr", recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files("qr", full.names = TRUE), "docs/qr", overwrite = TRUE, recursive = TRUE)
}
if (dir.exists("print")) {
  dir.create("docs/print", recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files("print", full.names = TRUE), "docs/print", overwrite = TRUE, recursive = TRUE)
}
if (!file.exists("docs/.nojekyll")) file.create("docs/.nojekyll")

message("DONE ✅")
message("Check local file: ", normalizePath("docs/index.html", winslash = "/", mustWork = FALSE))
message("Then push:")
message("git add -A")
message("git commit -m \"Full purposeful rebuild: QR, visuals, print pages\"")
message("git push")
