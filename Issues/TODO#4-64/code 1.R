export_xlsx.mQR_matrix <- function(x,
                                   export_dir,
                                   layout_file = c("ByComponent", "ByQRMatrix", "AllTogether"),
                                   auto_format = TRUE,
                                   overwrite = TRUE) {
    # by component = 1file / component (different QR in same file) = 2 files
    # by QRMatrix = 1file / QR (different component in same file)
    # All together = All Qr and components in same file

    layout_file <- match.arg(layout_file)
    export_dir <- normalizePath(export_dir)

    if (layout_file == "ByQRMatrix") {
        for (id_qr in seq_along(x)) {
            qr <- x[[is_qr]]
            name <- ifelse(
                test = is.null(names(x)) || nchar(names(x)[id_qr]) == 0,
                yes = paste0("QR_", id_qr),
                no = names(x)[id_qr]
            )
            file <- file.path(export_dir, paste0(name, ".xlsx"))
            export_xlsx(x = qr, file = file, auto_format = auto_format, overwrite = overwrite)
        }
    } else if (layout_file == "ByComponent") {
        wb_modalities <- openxlsx::createWorkbook(title = "Modalities of the QR", subject = "Seasonal Adjustment")
        wb_values <- openxlsx::createWorkbook(title = "Values of the QR", subject = "Seasonal Adjustment")

        for (id_qr in seq_along(x)) {
            qr <- x[[is_qr]]
            name <- ifelse(
                test = is.null(names(x)) || nchar(names(x)[id_qr]) == 0,
                yes = paste0("QR_", id_qr),
                no = names(x)[id_qr]
            )

            openxlsx::addWorksheet(wb = wb_modalities, sheetName = name)
            openxlsx::addWorksheet(wb = wb_values, sheetName = name)

            openxlsx::writeData(
                wb = wb_modalities,
                sheet = name,
                x = qr$modalities,
                headerStyle = ifelse(test = auto_format, yes = header_style, no = NULL)
            )
            openxlsx::writeData(
                wb = wb_values,
                sheet = name,
                x = qr$values,
                headerStyle = ifelse(test = auto_format, yes = header_style, no = NULL)
            )
            if (auto_format) {
                wb_modalities <- apply_BQ_style(
                    wb = wb_modalities, x = qr,
                    modalities_sheet = name
                )
                wb_values <- apply_BQ_style(
                    wb = wb_values, x = qr,
                    values_sheet = name
                )
            }
        }

        file_modalities <- file.path(export_dir, "modalities.xlsx")
        file_values <- file.path(export_dir, "values.xlsx")

        openxlsx::saveWorkbook(wb = wb_modalities, file = file_modalities, overwrite = overwrite)
        openxlsx::saveWorkbook(wb = wb_values, file = file_values, overwrite = overwrite)
    } else if (layout_file == "AllTogether") {
        wb_mqr <- openxlsx::createWorkbook(title = "Multiple QR", subject = "Seasonal Adjustment")

        for (id_qr in seq_along(x)) {
            qr <- x[[is_qr]]
            name <- ifelse(
                test = is.null(names(x)) || nchar(names(x)[id_qr]) == 0 || sum(names(x) == names(x)[id_qr]) > 1,
                yes = paste0("QR_", id_qr),
                no = names(x)[id_qr]
            )

            openxlsx::addWorksheet(wb = wb_mqr, sheetName = paste0(name, "_modalities"))
            openxlsx::addWorksheet(wb = wb_mqr, sheetName = paste0(name, "_values"))

            openxlsx::writeData(
                wb = wb_mqr,
                sheet = paste0(name, "_modalities"),
                x = qr$modalities,
                headerStyle = ifelse(test = auto_format, yes = header_style, no = NULL)
            )
            openxlsx::writeData(
                wb = wb_mqr,
                sheet = paste0(name, "_values"),
                x = qr$values,
                headerStyle = ifelse(test = auto_format, yes = header_style, no = NULL)
            )
            if (auto_format) {
                wb_mqr <- apply_BQ_style(
                    wb = wb_mqr, x = qr,
                    modalities_sheet = paste0(name, "_modalities"),
                    values_sheet = paste0(name, "_values")
                )
            }
        }

        file <- file.path(export_dir, "mQR.xlsx")
        openxlsx::saveWorkbook(wb = wb_mqr, file = file, overwrite = overwrite)
    }

    return(invisible(x))
}
