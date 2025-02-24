# System call to render Quarto file into both formats
# system("quarto render analysis_scripts/docs/user_manual.qmd --to html")
# system("quarto render analysis_scripts/docs/user_manual.qmd --to pdf")
# 
# 


# Automate Publishing User Manual to GitHub
# This script ensures the HTML manual is uploaded to GitHub Pages and links correctly to the PDF.

# Load necessary libraries
library(rmarkdown)
library(quarto)

# Define paths
manual_file <- "docs/user_manual.qmd"
html_output <- "docs/user_manual.html"
pdf_output <- "docs/user_manual.pdf"

# Ensure Quarto is installed
if (Sys.which("quarto") == "") {
  stop("❌ Quarto is not installed. Install from https://quarto.org")
}

# Check if the file exists
if (!file.exists(manual_file)) {
  stop(paste("❌ File not found:", manual_file, "Check the path and try again."))
}

# Render HTML and PDF user manuals
# system2("quarto", args = c("render", manual_file, "--to", "html"))
# system2("quarto", args = c("render", manual_file, "--to", "pdf"))

system("quarto render docs/user_manual.qmd --to html")
system("quarto render docs/user_manual.qmd --to pdf")

# Modify HTML to include PDF download link
html_content <- readLines(html_output)
pdf_link <- "<p><a href=\"https://urban-malaria.github.io/MRMT/docs/user_manual.pdf\" target=\"_blank\">📄 Download the User Manual (PDF)</a></p>"


# Insert PDF link before closing body tag
html_content <- gsub("</body>", paste0(pdf_link, "</body>"), html_content)
writeLines(html_content, html_output)

# Push changes to GitHub
system("git add /docs/user_manual.html docs/user_manual.pdf -f")
system("git commit -m 'Updated user manual' ")
system("git push origin main")

# Open HTML in browser
browseURL(html_output)


