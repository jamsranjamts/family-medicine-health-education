# ============================================================
# Нинжин манал ӨЭМТ — Text-only Quarto website rebuild
# Зураггүй хувилбар: зөвхөн эмнэлгийн лого + текст + QR + A4 print pages
# Ажиллуулах газар: C:/quarto_sites/family_medicine_health_site
# ============================================================

# ---------------------------
# 0. Project settings
# ---------------------------
project_dir <- "C:/quarto_sites/family_medicine_health_site"
site_url <- "https://jamsranjamts.github.io/family-medicine-health-education/"
clinic_name <- "Нинжин манал ӨЭМТ"
owner_text <- "© АШУҮИС, оюутан Ш.Жамсранжамц"

if (!dir.exists(project_dir)) {
  dir.create(project_dir, recursive = TRUE)
}
setwd(project_dir)
cat("Project folder:", normalizePath(getwd(), winslash = "/"), "\n")

# ---------------------------
# 1. Packages
# ---------------------------
need <- c("qrencoder", "png", "jpeg")
for (p in need) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cran.rstudio.com")
  }
}

# ---------------------------
# 2. Folders
# ---------------------------
dirs <- c("assets", "assets/logo", "qr", "print", "docs")
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 3. Logo
# ---------------------------
# Энэ script-тэй ижил folder дотор ninjin_manal_logo_transparent.png байвал түүнийг ашиглана.
# Үгүй бол assets/logo дотор байгаа өмнөх logo-г ашиглана.
script_candidates <- c(
  file.path(getwd(), "ninjin_manal_logo_transparent.png"),
  file.path(getwd(), "assets/logo/ninjin_manal_logo.png"),
  file.path(getwd(), "assets/logo/logo.png")
)
logo_src <- script_candidates[file.exists(script_candidates)][1]

if (is.na(logo_src)) {
  # fallback simple SVG logo if real logo is not copied yet
  fallback_svg <- '<svg xmlns="http://www.w3.org/2000/svg" width="900" height="260" viewBox="0 0 900 260"><rect width="900" height="260" fill="white"/><circle cx="120" cy="110" r="55" fill="#0b7fc3"/><path d="M70 190 C110 120,150 120,190 190" fill="none" stroke="#f7a51d" stroke-width="28" stroke-linecap="round"/><text x="250" y="115" font-family="Arial, sans-serif" font-size="72" font-weight="800" fill="#0072b2">НИНЖИН</text><text x="250" y="200" font-family="Arial, sans-serif" font-size="72" font-weight="800" fill="#0072b2">МАНАЛ</text></svg>'
  writeLines(fallback_svg, "assets/logo/ninjin_manal_logo.svg", useBytes = TRUE)
  logo_file <- "assets/logo/ninjin_manal_logo.svg"
} else {
  file.copy(logo_src, "assets/logo/ninjin_manal_logo.png", overwrite = TRUE)
  logo_file <- "assets/logo/ninjin_manal_logo.png"
}

# ---------------------------
# Helper functions
# ---------------------------
write_utf8 <- function(path, text) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(text), con = con, useBytes = TRUE)
}

slugify <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "-", x)
  x <- gsub("^-|-$", "", x)
  tolower(x)
}

# ---------------------------
# 4. Data for topics
# ---------------------------
topics <- list(
  list(
    id="stroke", file="stroke.qmd", print="print/stroke-print.qmd", title="Тархины цус харвалт", icon="🧠",
    short="Нүүр мурийх, гар сулрах, хэл яриа өөрчлөгдөх шинжийг таньж, цаг алдалгүй тусламж дуудах.",
    sections=list(
      c("Товч ойлголт", "Тархины цус харвалт нь тархины судас бөглөрөх эсвэл хагарах үед үүсдэг яаралтай тусламж шаарддаг өвчин юм. Эмчилгээ эрт эхлэх тусам саажилт, хэл ярианы алдагдал, амь насанд аюултай хүндрэлээс сэргийлэх боломж нэмэгдэнэ."),
      c("FAST аргаар шинж таних", "Face: нүүрний нэг тал унжих. Arm: нэг гар сулрах эсвэл өргөж чадахгүй болох. Speech: хэл яриа ээдрэх, ойлгомжгүй болох. Time: эдгээр шинж илэрвэл цаг алдалгүй 103 дуудна."),
      c("Яаралтай тусламж дуудах шинж", "Гэнэт гар, хөл сулрах, хэл яриа өөрчлөгдөх, ам муруйх, хараа гэнэт муудах, хүчтэй толгой өвдөх, ухаан балартах шинж илэрвэл гэртээ ажиглаж хүлээхгүй."),
      c("Урьдчилан сэргийлэлт", "Даралт, цусан дахь сахар, холестеринээ хянах, тамхи татахгүй байх, архины хэрэглээг багасгах, тогтмол хөдөлгөөн хийх, эмчийн бичсэн эмийг дур мэдэн зогсоохгүй байх."),
      c("Иргэдэд өгөх санамж", "Харвалтын үед өвчтөнд хоол, ус, эм дур мэдэн өгөхгүй. Ухаангүй бол хажуу тийш харуулж, амьсгалын замыг чөлөөтэй байлгана. Яаралтай тусламж иртэл тайван байлгаарай.")
    )
  ),
  list(
    id="heart-attack", file="heart-attack.qmd", print="print/heart-attack-print.qmd", title="Зүрхний шигдээс", icon="❤️",
    short="Цээжээр дарах, базлах, амьсгаадах, зүүн гар, эрүү, нуруу руу өвдөлт дамжих үед яаралтай хандана.",
    sections=list(
      c("Товч ойлголт", "Зүрхний шигдээс нь зүрхний булчинг цусаар хангах судас бөглөрөх үед үүсдэг яаралтай өвчин юм. Эмчилгээ оройтох тусам зүрхний булчин гэмтэх эрсдэл нэмэгдэнэ."),
      c("Анхаарах шинж", "Цээжний төв хэсгээр дарах, базлах, шатах мэдрэмж төрөх, амьсгаадах, хүйтэн хөлс гарах, дотор муухайрах, өвдөлт зүүн гар, мөр, эрүү, нуруу руу дамжих."),
      c("Хэзээ 103 дуудах вэ?", "Цээжний өвдөлт 10 минутаас дээш үргэлжлэх, амьсгал давчдах, ухаан балартах, хүйтэн хөлс гарах, даралт огцом унах зэрэг шинж илэрвэл шууд 103 дуудна."),
      c("Гэрийн нөхцөлд хийх зүйл", "Өвчтөнийг суулган амрааж, бариу хувцсыг суллана. Эмчийн зааваргүйгээр олон эм давхарлан өгөхгүй. Өмнө нь эмч нитроглицерин зөвлөсөн бол зөв хэрэглэнэ."),
      c("Урьдчилан сэргийлэлт", "Даралт, сахар, холестеринээ хянах, тамхинаас гарах, жингээ зохистой барих, давс, өөх тосны хэрэглээг багасгах, тогтмол алхах нь эрсдэлийг бууруулна.")
    )
  ),
  list(
    id="epilepsy", file="epilepsy.qmd", print="print/epilepsy-print.qmd", title="Эпилепси", icon="🟣",
    short="Таталтын үед хүнийг аюулгүй байрлуулж, ам руу нь юм хийхгүй, 5 минутаас дээш үргэлжилбэл 103 дуудна.",
    sections=list(
      c("Товч ойлголт", "Эпилепси нь тархины цахилгаан идэвхжил түр зуур алдагдсанаас давтамжтай таталт илрэх эмгэг юм. Ихэнх хүн эмчийн хяналт, тогтмол эмчилгээгээр хэвийн амьдрах боломжтой."),
      c("Таталтын үед авах арга хэмжээ", "Орчны аюултай зүйлсийг холдуулж, толгой дор зөөлөн зүйл тавина. Хувцсыг суллаж, таталт дууссаны дараа хажуу тийш харуулна. Таталтын хугацааг тэмдэглэнэ."),
      c("Хийж болохгүй зүйл", "Ам руу халбага, хуруу, ус, эм хийхгүй. Хүчээр барьж дарахгүй. Таталт үргэлжилж байх үед ус уулгахгүй, хоол өгөхгүй."),
      c("Хэзээ 103 дуудах вэ?", "Таталт 5 минутаас дээш үргэлжлэх, нэг таталт дуусаад дахин давтагдах, гэмтэл авах, жирэмсэн хүн татах, усанд татах, анх удаа таталт өгөх үед яаралтай тусламж дуудна."),
      c("Өдөр тутмын зөвлөмж", "Эмийг тогтмол уух, нойр дутуу явахгүй байх, архи хэтрүүлэхгүй байх, гэр бүл болон ойр дотныхондоо таталтын үед авах арга хэмжээг зааж өгөх нь чухал.")
    )
  ),
  list(
    id="kidney-failure", file="kidney-failure.qmd", print="print/kidney-failure-print.qmd", title="Бөөрний дутагдал", icon="🩺",
    short="Даралт, сахар, шээсний шинжилгээ, бөөрний үйл ажиллагааг тогтмол хянах нь бөөр хамгаална.",
    sections=list(
      c("Товч ойлголт", "Бөөр нь шингэн, давс, бодисын солилцооны хаягдлыг шүүх үүрэгтэй. Бөөрний үйл ажиллагаа удаан хугацаанд буурах үед биеийн хорт бодис гадагшлах нь мууддаг."),
      c("Эрсдэлт хүчин зүйл", "Цусны даралт ихсэх, чихрийн шижин, бөөрний үрэвсэл, өвчин намдаах эмийг дур мэдэн олон хэрэглэх, удамшил, давс ихтэй хооллолт нь эрсдэлийг нэмэгдүүлнэ."),
      c("Анхаарах шинж", "Хавагнах, шээсний хэмжээ өөрчлөгдөх, шээс хөөсрөх, ядрах, даралт ихсэх, хоолонд дургүй болох шинж илэрч болно. Зарим үед эхний шатанд шинж багатай байдаг."),
      c("Хяналтын шинжилгээ", "Цусны креатинин, eGFR, шээсний ерөнхий шинжилгээ, альбумин/креатинины харьцаа, даралт, цусан дахь сахарыг эмчийн зөвлөсөн давтамжаар шалгана."),
      c("Урьдчилан сэргийлэлт", "Давс, өөх тосны хэрэглээг багасгах, даралт ба сахараа тогтмол хянах, хангалттай шингэн уух, эмийг дур мэдэн хэрэглэхгүй байх, эмчид тогтмол үзүүлэх.")
    )
  ),
  list(
    id="autism", file="autism.qmd", print="print/autism-print.qmd", title="Аутизм ба дэмжих засал", icon="🧩",
    short="Хэл яриа, харилцаа, хөдөлгөөн, өдөр тутмын чадварыг эрт үеэс дэмжих нь хүүхдийн хөгжилд чухал.",
    sections=list(
      c("Товч ойлголт", "Аутизмын хүрээний эмгэг нь хүүхдийн харилцаа, нийгмийн харилцан үйлчлэл, зан үйл, мэдрэхүйн онцлогтой холбоотой хөгжлийн ялгаатай байдал юм."),
      c("Эрт анзаарах шинж", "Нэрээр дуудахад хариулахгүй байх, нүдээр харилцах нь бага байх, хэл яриа хоцрох, тоглоомыг давтамжтай нэг хэвээр ашиглах, өөрчлөлтөд хүчтэй хариу үзүүлэх зэрэг шинж ажиглагдаж болно."),
      c("Хэл засал", "Хэл яриа, ойлголт, заах, хүсэлт илэрхийлэх, ээлжлэн харилцах чадварыг хөгжүүлэхэд чиглэнэ. Эцэг эх гэртээ өдөр бүр богино, тогтмол дасгал хэлбэрээр дэмжих нь үр дүнтэй."),
      c("Хөдөлгөөн ба хөдөлмөр засал", "Том болон жижиг хөдөлгөөн, гарын эв дүй, хувцаслах, хооллох, тоглох, өдөр тутмын үйл ажиллагаанд оролцох чадварыг хөгжүүлнэ."),
      c("Гэр бүлийн дэмжлэг", "Тогтмол өдөр тутмын хуваарь, тайван орчин, эерэг урамшуулал, хүүхдийн сонирхолд тулгуурласан харилцаа, олон мэргэжлийн багийн хамтын ажиллагаа чухал.")
    )
  ),
  list(
    id="post-stroke-exercises", file="post-stroke-exercises.qmd", print="print/post-stroke-exercises-print.qmd", title="Харвалтын дараах дасгал", icon="🚶",
    short="Саажилтын үед дасгалыг аюулгүй, бага багаар, тогтмол хийж, уналтаас сэргийлнэ.",
    sections=list(
      c("Товч ойлголт", "Тархины харвалтын дараах саажилтын үед хөдөлгөөн сэргээх дасгал нь үе мөч хөшингө болох, булчин сулрах, уналт, хэвтрийн хүндрэлээс сэргийлэхэд тусална."),
      c("Аюулгүй байдлын санамж", "Дасгалыг өвдөлт хүчтэй нэмэгдэхээр хийхгүй. Толгой эргэх, амьсгаадах, цээж өвдөх, даралт огцом ихсэх үед зогсоож эмчид хандана."),
      c("Гарын дасгал", "Мөр, тохой, бугуй, хурууг аажмаар нугалах, тэнийлгэх, дээш өргөх хөдөлгөөнийг өдөр бүр бага давтамжтай хийнэ. Саажсан гарыг татаж чангаахгүй."),
      c("Хөлийн дасгал", "Шагай хөдөлгөх, өвдөг нугалах, тэнийлгэх, суусан байрлалаас хөлөө урагш сунгах зэрэг дасгалыг аюулгүй орчинд хийнэ."),
      c("Тэнцвэр ба алхалт", "Суух, босох, зогсох, богино зайд алхах дасгалыг уналтаас хамгаалж, шаардлагатай үед асран хамгаалагчийн тусламжтай хийж гүйцэтгэнэ.")
    )
  ),
  list(
    id="pressure-injury", file="pressure-injury.qmd", print="print/pressure-injury-print.qmd", title="Холголт, цооролт", icon="🛏️",
    short="Удаан хэвтрийн үед арьс дарагдахаас сэргийлж байрлал солих, арьсыг шалгах, хуурай цэвэр байлгах.",
    sections=list(
      c("Товч ойлголт", "Холголт, цооролт нь удаан хугацаанд нэг байрлалд хэвтэх, суух үед арьс болон доорх эд дарагдаж цусан хангамж муудсанаас үүсдэг арьсны гэмтэл юм."),
      c("Их дарагддаг хэсэг", "Дагз, чихний ар, дал, тохой, ууц, өгзөг, ташаа, өвдөг, өсгий зэрэг яс товойсон хэсгүүд илүү эрсдэлтэй."),
      c("Урьдчилан сэргийлэх", "Байрлалыг 2 цаг тутам өөрчлөх, арьсыг өдөр бүр шалгах, арьсыг цэвэр хуурай байлгах, зөөлөн дэр, зориулалтын гудас ашиглах, үрэлт чирэлтээс хамгаалах."),
      c("Анхаарах шинж", "Арьс улайх, халуу оргих, өвдөх, цэврүү үүсэх, арьс шалбарах, харлах, эвгүй үнэртэй шингэн гарах үед эмчид үзүүлнэ."),
      c("Асаргааны зөвлөмж", "Зөв хооллолт, уураг, шингэн, арьсны чийгшил, ариун цэвэр, хөдөлгөөнийг дэмжих нь эдгэрэлт болон урьдчилан сэргийлэлтэд чухал.")
    )
  )
)

# ---------------------------
# 5. CSS — text-only clean design, spacing, font, print
# ---------------------------
css <- r"CSS(
:root{
  --nm-blue:#006DAE;
  --nm-blue-dark:#004C86;
  --nm-green:#198754;
  --nm-gold:#F2A51A;
  --nm-red:#D93025;
  --nm-ink:#132238;
  --nm-muted:#617287;
  --nm-bg:#F5F9FC;
  --nm-card:#FFFFFF;
  --nm-border:#DDE8F2;
  --shadow:0 14px 40px rgba(0,62,107,.10);
}
*{box-sizing:border-box;}
html{scroll-behavior:smooth;}
body{
  font-family:"Segoe UI", Arial, "Noto Sans", "Noto Sans Mongolian", sans-serif;
  color:var(--nm-ink);
  background:linear-gradient(180deg,#ffffff 0%,#f5f9fc 35%,#ffffff 100%);
  line-height:1.72;
  font-size:17px;
}
.navbar{
  background:#fff!important;
  border-bottom:1px solid var(--nm-border);
  box-shadow:0 8px 24px rgba(0,76,134,.08);
  padding:.55rem 0;
}
.navbar-brand img, .navbar-logo{
  max-height:54px!important;
  width:auto!important;
}
.navbar-title{
  font-weight:900;
  color:var(--nm-blue-dark)!important;
  letter-spacing:.2px;
  white-space:normal!important;
  line-height:1.15;
  font-size:1.28rem;
}
.nav-link{font-weight:700;color:#1d3858!important;}
.nav-link:hover{color:var(--nm-blue)!important;}
.quarto-title-block .title{
  font-size:2.45rem;
  font-weight:900;
  color:var(--nm-blue-dark);
  letter-spacing:-.5px;
  line-height:1.15;
  margin-top:.35rem;
}
.quarto-title-block .subtitle{color:var(--nm-muted);font-weight:650;}
h1,h2,h3,h4{letter-spacing:-.2px;line-height:1.22;}
h1{font-weight:900;color:var(--nm-blue-dark);}
h2{
  font-size:1.62rem;
  margin-top:2.35rem;
  margin-bottom:1rem;
  padding-top:.65rem;
  color:var(--nm-blue-dark);
  border-top:1px solid var(--nm-border);
}
h3{
  font-size:1.26rem;
  margin-top:1.75rem;
  margin-bottom:.7rem;
  color:#0D5E98;
}
p{margin-bottom:1rem;}
ul,ol{padding-left:1.35rem;}
li{margin:.38rem 0;}
.nm-hero{
  display:grid;
  grid-template-columns:minmax(0,1.1fr) minmax(260px,.9fr);
  gap:2rem;
  align-items:center;
  padding:2.4rem;
  border-radius:28px;
  background:radial-gradient(circle at top left, #E6F4FF, transparent 36%),linear-gradient(135deg,#fff,#EEF8FF 58%,#FFF7E5);
  border:1px solid var(--nm-border);
  box-shadow:var(--shadow);
  margin:1.1rem 0 2rem;
}
.nm-logo-panel{
  background:#fff;
  border:1px solid var(--nm-border);
  border-radius:24px;
  padding:1.1rem;
  text-align:center;
}
.nm-logo-panel img{max-width:100%;height:auto;max-height:170px;object-fit:contain;}
.nm-eyebrow{
  color:var(--nm-green);
  font-weight:900;
  text-transform:uppercase;
  font-size:.92rem;
  letter-spacing:.06em;
}
.nm-title-large{
  font-size:2.7rem;
  line-height:1.08;
  margin:.55rem 0 .8rem;
  color:var(--nm-blue-dark);
  font-weight:950;
}
.nm-lead{font-size:1.12rem;color:#2f455e;max-width:720px;}
.nm-badges{display:flex;flex-wrap:wrap;gap:.6rem;margin-top:1.15rem;}
.nm-badge{display:inline-flex;align-items:center;border:1px solid #cbe5f6;background:#fff;padding:.45rem .75rem;border-radius:999px;font-weight:800;color:#0d5e98;font-size:.92rem;}
.topic-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(245px,1fr));gap:1rem;margin:1.5rem 0 2rem;}
.topic-card{
  background:#fff;
  border:1px solid var(--nm-border);
  border-radius:22px;
  padding:1.15rem;
  box-shadow:0 10px 30px rgba(0,76,134,.07);
  min-height:210px;
  display:flex;
  flex-direction:column;
  gap:.65rem;
}
.topic-card .icon{font-size:2.15rem;line-height:1;}
.topic-card h3{margin:.1rem 0 .2rem;font-size:1.2rem;border:0;padding:0;color:var(--nm-blue-dark);}
.topic-card p{font-size:.98rem;color:#44566d;margin:0;}
.topic-card a{margin-top:auto;display:inline-block;text-decoration:none;font-weight:850;color:#fff;background:var(--nm-blue);padding:.55rem .8rem;border-radius:12px;text-align:center;}
.topic-card a:hover{background:var(--nm-blue-dark);}
.section-box{
  background:#fff;
  border:1px solid var(--nm-border);
  border-left:6px solid var(--nm-blue);
  border-radius:18px;
  padding:1.15rem 1.25rem;
  margin:1rem 0 1.25rem;
  box-shadow:0 10px 28px rgba(0,76,134,.06);
}
.section-box h2,.section-box h3{border:0;padding:0;margin-top:0;}
.warn-box{border-left-color:var(--nm-red);background:#fffafa;}
.good-box{border-left-color:var(--nm-green);background:#fbfffd;}
.qr-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:1rem;margin:1.4rem 0;}
.qr-card{background:#fff;border:1px solid var(--nm-border);border-radius:20px;padding:1rem;text-align:center;box-shadow:0 10px 28px rgba(0,76,134,.06);}
.qr-card img{max-width:150px;width:100%;height:auto;margin:.35rem auto .7rem;display:block;}
.qr-card h3{font-size:1rem;margin:.25rem 0 .3rem;border:0;padding:0;}
.qr-card small{color:var(--nm-muted);word-break:break-word;}
.print-link{display:inline-block;background:var(--nm-green);color:#fff!important;text-decoration:none;font-weight:850;border-radius:12px;padding:.5rem .75rem;margin-top:.4rem;}
.site-footer{
  margin-top:3rem;
  padding:1.6rem;
  background:linear-gradient(90deg,var(--nm-blue-dark),var(--nm-blue));
  color:#fff;
  border-radius:24px 24px 0 0;
  display:grid;
  grid-template-columns:170px 1fr;
  gap:1rem;
  align-items:center;
}
.site-footer img{max-width:150px;max-height:80px;object-fit:contain;background:#fff;border-radius:14px;padding:.35rem;}
.site-footer p{margin:.25rem 0;color:#fff;}
.disclaimer{font-size:.92rem;opacity:.95;}
.a4-page{
  max-width:850px;
  margin:0 auto;
  background:#fff;
  border:1px solid var(--nm-border);
  border-radius:22px;
  padding:1.3rem;
  box-shadow:var(--shadow);
}
.a4-header{display:grid;grid-template-columns:160px 1fr 135px;gap:1rem;align-items:center;border-bottom:2px solid var(--nm-blue);padding-bottom:.9rem;margin-bottom:1rem;}
.a4-header img.logo{max-width:155px;max-height:72px;object-fit:contain;}
.a4-header img.qr{max-width:118px;justify-self:end;}
.a4-title{font-size:1.75rem;font-weight:950;color:var(--nm-blue-dark);line-height:1.1;}
.a4-section{border-left:5px solid var(--nm-blue);padding:.65rem .85rem;margin:.7rem 0;background:#fbfdff;border-radius:12px;}
.a4-section h3{margin:0 0 .35rem;font-size:1.08rem;color:var(--nm-blue-dark);}
.a4-section p{font-size:.96rem;margin:0;line-height:1.55;}
.a4-footer{border-top:1px solid var(--nm-border);margin-top:1rem;padding-top:.7rem;font-size:.82rem;color:#3e5268;display:flex;justify-content:space-between;gap:1rem;}
@media(max-width:800px){.nm-hero{grid-template-columns:1fr;padding:1.4rem}.nm-title-large{font-size:2rem}.site-footer{grid-template-columns:1fr}.a4-header{grid-template-columns:1fr;text-align:center}.a4-header img.qr{justify-self:center}.navbar-title{font-size:1.05rem}}
@media print{
  @page{size:A4;margin:12mm;}
  body{background:white!important;font-size:12pt;}
  .navbar,.sidebar,.toc-actions,.page-columns .column-margin,.quarto-title-block,.no-print{display:none!important;}
  .a4-page{box-shadow:none;border:0;border-radius:0;padding:0;max-width:100%;}
  .a4-title{font-size:20pt;}
  .a4-section{break-inside:avoid;}
  a{text-decoration:none;color:inherit;}
}
)CSS"
write_utf8("styles.css", css)

# ---------------------------
# 6. Quarto config
# ---------------------------
q_yml <- r"YAML(
project:
  type: website
  output-dir: docs
  render:
    - "*.qmd"
    - "print/*.qmd"

website:
  title: "Нинжин манал ӨЭМТ"
  navbar:
    logo: "assets/logo/ninjin_manal_logo.png"
    title: "Нинжин манал ӨЭМТ"
    background: light
    left:
      - text: "Нүүр"
        href: index.qmd
      - text: "Тархины харвалт"
        href: stroke.qmd
      - text: "Зүрхний шигдээс"
        href: heart-attack.qmd
      - text: "Эпилепси"
        href: epilepsy.qmd
      - text: "Бөөр"
        href: kidney-failure.qmd
      - text: "Аутизм"
        href: autism.qmd
      - text: "Дасгал"
        href: post-stroke-exercises.qmd
      - text: "Холголт"
        href: pressure-injury.qmd
      - text: "QR татах"
        href: qr-codes.qmd
      - text: "Эх сурвалж"
        href: references.qmd

format:
  html:
    theme: cosmo
    css: styles.css
    toc: true
    toc-location: right
    page-layout: full
    lang: mn
    smooth-scroll: true
)YAML"
write_utf8("_quarto.yml", q_yml)

# ---------------------------
# 7. QR generation
# ---------------------------
make_qr <- function(url, out_file, label = NULL) {
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  qr <- qrencoder::qrencode(url)
  qr <- as.matrix(qr)
  qr <- ifelse(qr, 1, 0)
  n <- nrow(qr)
  # add quiet zone
  border <- 4
  padded <- matrix(0, n + 2*border, n + 2*border)
  padded[(border+1):(border+n), (border+1):(border+n)] <- qr
  # image expects y-axis reversed
  png::writePNG(array(1, dim = c(1,1,4)), tempfile(fileext = ".png"))
  png(out_file, width = 1000, height = 1000, res = 150, bg = "white")
  par(mar = c(0,0,0,0), xaxs = "i", yaxs = "i")
  image(t(padded[nrow(padded):1, ]), col = c("white", "black"), axes = FALSE, useRaster = TRUE)
  # put small real logo in middle if png logo exists
  if (file.exists("assets/logo/ninjin_manal_logo.png")) {
    lg <- tryCatch(png::readPNG("assets/logo/ninjin_manal_logo.png"), error = function(e) NULL)
    if (!is.null(lg)) {
      # white rounded-ish square under logo
      rect(.405, .405, .595, .595, col = "white", border = NA)
      rasterImage(lg, .425, .445, .575, .555, interpolate = TRUE)
    }
  }
  dev.off()
}

page_links <- list(list(id="home", title="Нүүр хуудас", href="index.html"))
for (tp in topics) {
  page_links[[length(page_links)+1]] <- list(id=tp$id, title=tp$title, href=sub("\\.qmd$", ".html", tp$file))
}
print_links <- list()
for (tp in topics) {
  print_links[[length(print_links)+1]] <- list(id=paste0(tp$id,"-print"), title=paste0(tp$title, " — A4 хэвлэх"), href=sub("\\.qmd$", ".html", tp$print))
}

all_qr <- c(page_links, print_links)
for (x in all_qr) {
  make_qr(paste0(site_url, x$href), file.path("qr", paste0(x$id, ".png")), x$title)
}

# ---------------------------
# 8. Page builders
# ---------------------------
footer_md <- paste0('\n\n<div class="site-footer">\n<img src="', logo_file, '" alt="Нинжин манал ӨЭМТ logo">\n<div>\n<p><strong>', clinic_name, '</strong></p>\n<p>', owner_text, '</p>\n<p class="disclaimer">Энэхүү мэдээлэл нь олон нийтийн эрүүл мэндийн боловсролд зориулагдсан бөгөөд эмчийн үзлэг, оношилгоо, эмчилгээг орлохгүй. Яаралтай шинж илэрвэл 103 болон ойролцоох эрүүл мэндийн байгууллагад хандана уу.</p>\n</div>\n</div>\n')

make_topic_card <- function(tp) {
  paste0('<div class="topic-card">\n<div class="icon">', tp$icon, '</div>\n<h3>', tp$title, '</h3>\n<p>', tp$short, '</p>\n<a href="', sub("\\.qmd$", ".html", tp$file), '">Дэлгэрэнгүй</a>\n</div>')
}

make_topic_page <- function(tp) {
  sections <- ""
  for (i in seq_along(tp$sections)) {
    sec <- tp$sections[[i]]
    cls <- if (grepl("Яаралтай|Анхаарах|Хэзээ", sec[1])) "section-box warn-box" else if (grepl("Урьдчилан|зөвлөмж|сэргийл", sec[1], ignore.case=TRUE)) "section-box good-box" else "section-box"
    sections <- paste0(sections, '\n\n<div class="', cls, '" id="', slugify(sec[1]), '">\n\n## ', sec[1], '\n\n', sec[2], '\n\n</div>\n')
  }
  print_href <- sub("\\.qmd$", ".html", tp$print)
  qr_id <- tp$id
  paste0('---\ntitle: "', tp$title, '"\nsubtitle: "', clinic_name, ' — иргэдэд зориулсан товч зөвлөмж"\n---\n\n',
         '<div class="section-box">\n\n<strong>Товч санаа:</strong> ', tp$short, '\n\n</div>\n\n',
         '<div class="qr-card no-print" style="max-width:230px;float:right;margin:0 0 1rem 1rem;">\n<img src="qr/', qr_id, '.png" alt="QR">\n<small>Энэ хуудсыг QR-аар унших</small>\n</div>\n\n',
         sections,
         '\n\n<div class="section-box no-print">\n\n## Хэвлэх хувилбар\n\n<a class="print-link" href="', print_href, '">A4 хэвлэх хувилбар нээх</a>\n\n</div>\n',
         footer_md)
}

# ---------------------------
# 9. Index page
# ---------------------------
cards <- paste(vapply(topics, make_topic_card, character(1)), collapse="\n")
index_page <- paste0('---\ntitle: "Нинжин манал ӨЭМТ"\nsubtitle: "Өрхийн эрүүл мэндийн боловсролын цахим зөвлөмж"\n---\n\n',
'<div class="nm-hero">\n<div>\n<div class="nm-eyebrow">Олон нийтэд зориулсан эрүүл мэндийн мэдээлэл</div>\n<div class="nm-title-large">Эрүүл мэндийн зөвлөмжийг QR-аар уншаарай</div>\n<p class="nm-lead">Тархины харвалт, зүрхний шигдээс, эпилепси, бөөрний эрүүл мэнд, аутизмын дэмжлэг, харвалтын дараах дасгал, холголт цооролтоос сэргийлэх зөвлөмжийг нэг дороос унших боломжтой.</p>\n<div class="nm-badges"><span class="nm-badge">103 — яаралтай тусламж</span><span class="nm-badge">A4 хэвлэх хувилбар</span><span class="nm-badge">QR кодтой</span></div>\n</div>\n<div class="nm-logo-panel"><img src="', logo_file, '" alt="Нинжин манал ӨЭМТ logo"></div>\n</div>\n\n',
'## Сэдвүүд\n\n<div class="topic-grid">\n', cards, '\n</div>\n\n',
'## Хэвлэх болон QR ашиглах\n\n<div class="section-box">Сэдэв бүрийн үндсэн хуудас болон A4 хэвлэх хувилбар тус бүрт QR код үүсгэсэн. QR татах хэсгээс зураг хэлбэрээр авч хэвлэх боломжтой.</div>\n\n',
footer_md)
write_utf8("index.qmd", index_page)

# ---------------------------
# 10. Topic pages
# ---------------------------
for (tp in topics) {
  write_utf8(tp$file, make_topic_page(tp))
}

# ---------------------------
# 11. Print pages
# ---------------------------
make_print_page <- function(tp) {
  sections <- ""
  for (sec in tp$sections) {
    sections <- paste0(sections, '<div class="a4-section"><h3>', sec[1], '</h3><p>', sec[2], '</p></div>\n')
  }
  qrid <- paste0(tp$id, "-print")
  paste0('---\ntitle: "', tp$title, ' — A4 хэвлэх"\ntoc: false\n---\n\n',
'<div class="a4-page">\n',
'<div class="a4-header">\n<img class="logo" src="../', logo_file, '" alt="logo">\n<div><div class="a4-title">', tp$title, '</div><div>', clinic_name, ' — олон нийтэд зориулсан зөвлөмж</div></div>\n<img class="qr" src="../qr/', qrid, '.png" alt="QR">\n</div>\n',
sections,
'<div class="a4-footer"><span>', owner_text, '</span><span>Эмчийн үзлэг, оношилгоо, эмчилгээг орлохгүй.</span></div>\n',
'</div>\n')
}
for (tp in topics) {
  write_utf8(tp$print, make_print_page(tp))
}

# ---------------------------
# 12. QR download page
# ---------------------------
make_qr_card <- function(x) {
  paste0('<div class="qr-card">\n<img src="qr/', x$id, '.png" alt="QR">\n<h3>', x$title, '</h3>\n<small>', site_url, x$href, '</small>\n</div>')
}
qr_page_cards <- paste(vapply(page_links, make_qr_card, character(1)), collapse="\n")
qr_print_cards <- paste(vapply(print_links, make_qr_card, character(1)), collapse="\n")
qr_page <- paste0('---\ntitle: "QR татах"\nsubtitle: "Сайтын хуудас болон A4 хэвлэх хувилбарын QR кодууд"\n---\n\n',
'## Сайтын нүүр болон сэдэв тус бүрийн QR\n\n<div class="qr-grid">\n', qr_page_cards, '\n</div>\n\n',
'## A4 хэвлэх хувилбаруудын QR\n\n<div class="qr-grid">\n', qr_print_cards, '\n</div>\n',
footer_md)
write_utf8("qr-codes.qmd", qr_page)

# ---------------------------
# 13. References page
# ---------------------------
ref_page <- paste0('---\ntitle: "Эх сурвалж ба санамж"\n---\n\n',
'## Ашигласан мэдээллийн чиглэл\n\n',
'<div class="section-box">Энэхүү сайт нь өрхийн анагаах ухааны түвшинд иргэдэд ойлгомжтой байдлаар эрүүл мэндийн боловсрол олгох зорилготой. Агуулга нь олон улсын нийтлэг зөвлөмж, яаралтай шинж тэмдгийг таних, гэрийн нөхцөлд авах аюулгүй арга хэмжээ, урьдчилан сэргийлэлтийн зарчимд тулгуурласан.</div>\n\n',
'## Санамж\n\n',
'<div class="section-box warn-box">Энэ мэдээлэл нь эмчийн үзлэг, оношилгоо, эмчилгээг орлохгүй. Амь насанд аюултай шинж илэрвэл цаг алдалгүй 103 дуудна уу.</div>\n\n',
'## Оюуны өмч\n\n',
'<div class="section-box good-box">', owner_text, '</div>\n',
footer_md)
write_utf8("references.qmd", ref_page)

# ---------------------------
# 14. Optional simple downloads placeholder removed: create redirect-like downloads page
# ---------------------------
write_utf8("downloads.qmd", '---\ntitle: "QR татах"\n---\n\nQR кодуудыг харах бол [QR татах](qr-codes.html) хэсэг рүү орно уу.\n')

# ---------------------------
# 15. Render Quarto
# ---------------------------
if (!nzchar(Sys.which("quarto"))) {
  stop("Quarto CLI олдсонгүй. Quarto суусан эсэхийг шалгана уу.")
}
cat("Rendering Quarto site...\n")
res <- system2("quarto", args = c("render"), stdout = TRUE, stderr = TRUE)
cat(paste(res, collapse = "\n"), "\n")

# Ensure resources exist in docs
file.copy("assets/logo/ninjin_manal_logo.png", "docs/assets/logo/ninjin_manal_logo.png", overwrite = TRUE)
if (dir.exists("qr")) {
  dir.create("docs/qr", recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files("qr", full.names = TRUE), "docs/qr", overwrite = TRUE)
}
file.create("docs/.nojekyll")

cat("DONE ✅ Text-only site rebuilt.\n")
cat("Next commands:\n")
cat("git add -A\n")
cat("git commit -m \"Rebuild text-only Ninjin Manal site with QR and A4 print pages\"\n")
cat("git push\n")
