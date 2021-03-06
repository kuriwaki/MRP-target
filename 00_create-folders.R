library(fs)
library(glue)


dir_create("data/input")
dir_create("data/output")


dir_create("data/output/cces/sample-TX")
dir_create("data/output/CDs")
dir_create("data/output/reg")


for (output in c("CDs", "reg")) {
  for (model in c("stan_glmer", "brms")) {
    for (sd in c("01", "02", "05", "10", "default", "hanretty")) {
      for (outcome in c("turn", "turn-vshare", "ahca", "sanc"))
        dir_create(path("data", "output",  output, model, str_c("sd-", sd), outcome))
    }
  }
}

dir_create(path("data", "input", "CCES"))
