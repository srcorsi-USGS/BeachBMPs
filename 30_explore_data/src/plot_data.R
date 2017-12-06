#Plot time series of response and IVs, one variable3 per page

filenm <- "varPlots63rd.pdf"
pdf(filenm)
plotVarsTS(df63,variables,"pdate",beach=beach)
dev.off()
shell.exec(filenm)

