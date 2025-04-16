devtools::load_all()
library(r4ss)

test_example_path <- system.file("extdata", "simple_small", package = "r4ss")
ss3rep <- SS_output(test_example_path)
SSplotRunstest(ss3rep, add = TRUE, subplots = "len")
cols.vir <- viridis(5, option = "H")
SSplotRetro(retrosum.simple, subplots = "SSB", lwd = 8, uncertainty = T, pt.cex = 3)
SSplotHCxval(retrosum.simple, subplots = "cpue")

mvln <- SSdeltaMVLN(ss3rep, run = "Simple")
SSplotEnsemble(mvln[["kb"]], ylabs = mvln[["labels"]], add = T, verbose = F)
