setwd("~/Desktop/receipt-id-633221-part-001 modernism/ngram1")
FILES <- list.files( pattern = ".txt")
for (i in 1:length(FILES)) {
  table=read.table(file=FILES[i],header=F)
  colnames(table)<-c("WORDCOUNTS", "WEIGHT")
  txt=paste(rep(table$WORDCOUNTS, table$WEIGHT), collapse = " ")
  writeLines(txt,paste0("~/Desktop/modernismtexts/",FILES[i]))
}