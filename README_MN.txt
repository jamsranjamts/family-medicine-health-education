Нинжин манал ӨЭМТ — Text-only Quarto site update package

Зорилго:
- Ямар нэгэн сэдвийн зураггүй, зөвхөн текстэн мэдээлэлтэй сайт болгох
- Зөвхөн эмнэлгийн logo ашиглах
- Сайтын үндсэн хуудсууд болон A4 хэвлэх poster хуудсууд руу тус бүр QR үүсгэх
- Фонт, дэд гарчиг, зай, эмнэлгийн нэр бүтэн харагдах байдлыг сайжруулах

Ажиллуулах:
1) Энэ ZIP-ийг задлаад доторх файлуудыг C:/quarto_sites/family_medicine_health_site дотор хуулна.
2) RStudio Console дээр:

setwd("C:/quarto_sites/family_medicine_health_site")
source("ninjin_manal_TEXT_ONLY_REBUILD.R")

3) Амжилттай болсны дараа PowerShell дээр:
cd C:\quarto_sitesamily_medicine_health_site
git add -A
git commit -m "Rebuild text-only Ninjin Manal site with QR and A4 print pages"
git push

Анхаарах:
- Кирилл үсэгтэй folder path дээр ажиллуулахгүй.
- Энэ script одоогийн site-ийн qmd/css файлуудыг шинэ text-only хувилбараар overwrite хийнэ.


Зассан хувилбар: raw string literal алдаа зассан. RStudio Console дээр:
setwd("C:/quarto_sites/family_medicine_health_site")
source("ninjin_manal_TEXT_ONLY_REBUILD_FIXED.R")
