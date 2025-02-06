library(simChef)

options(simChef.plot_theme = "vthemes")

render_docs(
  save_dir = here::here("results"),
  show_eval = FALSE,
  viz_cache = ".png",
  write_rmd = TRUE
)
