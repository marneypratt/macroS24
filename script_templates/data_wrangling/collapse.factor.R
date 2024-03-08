#collapse a factor into fewer categories by using forecats::fct_collapse 
# within dplyr::mutate

# here is an example
macros.new <- macros  |>  
  mutate(microhab.light = fct_collapse
         (microhabitat,
           high = c("DM", "DSP", "UM", "USU"),
           low = c("DSR", "DFR", "DSH", "USR", "UFR", "USH")))
  