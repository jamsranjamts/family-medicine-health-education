# Нинжин манал ӨЭМТ — final package apply script
# 1) Энэ файлыг ZIP задласан package root folder-оос ажиллуулна.
# 2) site/ folder-ийн бүх файлыг C:/quarto_sites/family_medicine_health_site руу хуулна.
# 3) Quarto render хийнэ.

project_dir <- "C:/quarto_sites/family_medicine_health_site"
source_dir <- file.path(getwd(), "site")

if (!dir.exists(source_dir)) {
  stop("site/ folder олдсонгүй. RStudio setwd() нь ZIP задласан package root folder байх ёстой.")
}

dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)

old_files <- c("shilen-dans.qmd", "print-shilen-dans.qmd")
for (f in old_files) {
  p <- file.path(project_dir, f)
  if (file.exists(p)) unlink(p)
}

copy_dir <- function(from, to) {
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  items <- list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  for (item in items) {
    dest <- file.path(to, basename(item))
    if (dir.exists(item)) {
      if (dir.exists(dest)) unlink(dest, recursive = TRUE, force = TRUE)
      copy_dir(item, dest)
    } else {
      file.copy(item, dest, overwrite = TRUE)
    }
  }
}

copy_dir(source_dir, project_dir)
setwd(project_dir)
message("Copied package files to: ", project_dir)

if (nzchar(Sys.which("quarto"))) {
  system("quarto render")
  file.create(file.path("docs", ".nojekyll"))
  message("Quarto render done. Next: git add -A; git commit -m 'Final update'; git push")
} else {
  message("Quarto command олдсонгүй. RStudio/Quarto суусан эсэхийг шалгаад, project folder дээр quarto render ажиллуулна.")
}
