getwd()

# Import des données brutes -----------------------------------------------
############ VERIf
## OBJ ?
# donnees ancienene base jusqu'en dec 2023
ipi_2023 <- read.csv("Data/Ipi_rjd3/IPI_nace4_dec2023_old_base.csv", sep = ";")

#donnees nouvelle base juqu'en oct 2023 pour es
ipi_2024 <- read.csv("Data/Ipi_rjd3/IPI_nace4.csv", sep = ";")


# Comparaison de la structure des données brutes --------------------------

sum(!colnames(ipi_2024) %in% colnames(ipi_2023))
# Aucune colonne de 2024 n'était pas présente en 2023 = 0 nouvelles colonnes

sum(!colnames(ipi_2023) %in% colnames(ipi_2024))
# Aucune colonne de 2023 n'est pas présente en 2024 = 0 nouvelles colonnes

all(colnames(ipi_2023) == colnames(ipi_2023))
# Le fichier a une structure inchangée.

## Indicateurs à creer ?


# Creation et export des graphiques ---------------------------------------

# Open a pdf file QT ve ecraser precedent
pdf("Data/Ipi_rjd3/output/raw_data.pdf")

# Create a plot
### plot (ts) : best optiion ? lines = ajout autre serie ?
for (k in colnames(ipi_2023)) {
    if (k != "date") {
        y_range <- range(c(ipi_2023[, k], ipi_2024[, k])) # QT ?
        y_range <- (6 / 5) * y_range - sum(y_range) / 10
        plot(ts(ipi_2023[, k], start = 1990, frequency = 12),
            col = "black",
            ylim = y_range, ylab = "IPI", main = paste("Série IPI", k)
        )
        lines(ts(ipi_2024[, k], start = 1990, frequency = 12), col = "red")
        legend("topright", legend = c("Série 2023", "Série 2024"), pch = 16, col = c("black", "red"))
    }
}

# Close the pdf file
dev.off()
