
# Dans ce programme on sélectionne les régresseurs à partir de WS créé avec RJDemetra (sans jamais ouvrir la GUI) + cruncher

# Import data -------------------------------------------------------------

raw_series_ipi <- read.csv("./data/IPI_nace4.csv", 
                           sep = ";", dec = ".")

series_ipi_ts <- raw_series_ipi |> 
    ts(start = 1990L, frequency = 12L)

regs_cjo_ts <- read.csv("./data/regs_cjo.csv", sep = ";", dec = ".") |> 
    ts(start = 1990L, frequency = 12L)


# Create the specs --------------------------------------------------------


