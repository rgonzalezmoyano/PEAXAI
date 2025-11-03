#' Spanish Food Industry Firms Dataset
#'
#' Dataset containing information on food industry companies located in Spain,
#' used to illustrate efficiency analysis within the \pkg{PEAXAI} package.
#' The dataset reflects the institutional and market heterogeneity that shapes firm-level efficiency
#' across Spain’s 17 autonomous communities.
#'
#' @details
#' The dataset includes 917 food industry firms with more than 50 employees, collected from the
#' \emph{SABI} database for the year 2023. Each observation corresponds to a single company.
#' Variables reflect both operational and financial dimensions relevant for productivity and efficiency assessment.
#'
#' The output variable is:
#' \itemize{
#'   \item \code{operating_income} — Operating income (in millions of euros), measuring revenues generated
#'   from core business activities.
#' }
#'
#' The input variables are:
#' \itemize{
#'   \item \code{total_assets} — Total assets (millions of euros), representing resources employed.
#'   \item \code{employees} — Number of employees, indicating workforce size (only firms with more than 50 workers are included).
#'   \item \code{fixed_assets} — Tangible fixed assets (millions of euros), such as buildings and machinery.
#'   \item \code{personnel_expenses} — Personnel expenses (millions of euros), including salaries, benefits, and training.
#' }
#'
#' The variable \code{autonomous_community} identifies the territorial location of each firm within Spain.
#'
#' The sample displays substantial dispersion across variables, encompassing both small and large firms.
#' This heterogeneity affects measures of central tendency—mean and median values differ considerably—thus providing a realistic challenge for efficiency and explainability analyses.
#'
#' @format
#' A \code{data.frame} with 917 rows and 6 columns:
#' \describe{
#'   \item{total_assets}{Total assets (millions of euros).}
#'   \item{employees}{Number of employees.}
#'   \item{fixed_assets}{Tangible fixed assets (millions of euros).}
#'   \item{personnel_expenses}{Personnel expenses (millions of euros).}
#'   \item{operating_income}{Operating income (millions of euros).}
#'   \item{autonomous_community}{Autonomous community where the firm operates.}
#' }
#'
#' @source
#' SABI (Sistema de Análisis de Balances Ibéricos) database, 2023.
#' Firms with more than 50 employees in the Spanish food industry.
#'
#' @examples
#' data(firms)
#' str(firms)
#' summary(firms)
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(firms, ggplot2::aes(x = employees, y = operating_income)) +
#'     ggplot2::geom_point(alpha = 0.6) +
#'     ggplot2::labs(
#'       x = "Number of employees",
#'       y = "Operating income (millions of euros)",
#'       title = "Spanish Food Industry Firms (2023)"
#'     ) +
#'     ggplot2::theme_minimal() +
#'     ggplot2::theme(
#'       plot.title = ggplot2::element_text(face = "bold"),
#'       axis.line = ggplot2::element_line(color = "black"),
#'       axis.ticks = ggplot2::element_line(color = "black"),
#'       panel.grid.minor = ggplot2::element_blank()
#'     )
#' }
#'
#' @docType data
#' @name firms
#' @usage data(firms)
#' @keywords datasets
"firms"
