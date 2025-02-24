# System call to render Quarto file into both formats
system("quarto render docs/user_manual.qmd --to html")
system("quarto render docs/user_manual.qmd --to pdf")
