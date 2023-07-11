#
xtable_md = function(table){
  pr = print.xtable(xtable(table),comment =F)
  pr = gsub(pattern = "table", replacement = "aligned", x = pr)
  pr = gsub(pattern = "tabular", replacement = "array", x = pr)
  pr = gsub(pattern = "\\\\centering", replacement = "", x = pr)
  cat("$$",sub("\n", "", pr),"$$")
}
#
