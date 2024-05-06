library("rjd3revisions")

long <- rjd3revisions:::simulate_long(
    n_period = 4,
    n_revision = 6,
    start_period = as.Date("2023-01-01"),
    periodicity = 4L
)

my_vintages <- create_vintages(x = long, type = "long", periodicity = 4)


my_rslt <- revision_analysis(
    vintages = my_vintages,
    gap = 1,
    view = "diagonal",
    n.releases = 3
)

render_report(
    rslt = my_rslt,
    output_path = "my_report",
    output_format = "pdf_document",
    open_report = TRUE
)

