#' @title Australian Harmonized Export Commodity Classification (AHECC)
#' @description  A data set containing the Australian Harmonized Export Commodity Classification (AHECC).
#' @format Data sets \code{ahecc_2017}, \code{ahecc_2012} and \code{ahecc_2007} contain the respective AHECCs.
#'   The datasets contain the following variables:
#' \describe{
#'   \item{ahecc_code}{12-digit AHECC commodity code}
#'   \item{chapter_code}{Two-digit AHECC chapter code}
#'   \item{chapter_label}{AHECC chapter label}
#'   \item{heading_code}{Four-digit AHECC heading code}
#'   \item{heading_label}{Five-digit AHECC heading label}
#'   \item{hs5_code}{Five-digit AHECC heading code}
#'   \item{hs5_label}{Five-digit AHECC heading label}
#'   \item{hs_code}{Six-digit AHECC heading code}
#'   \item{hs_label}{Six-digit AHECC heading label}
#'   \item{hs7_code}{Seven-digit AHECC heading code}
#'   \item{hs7_label}{Seven-digit AHECC heading label}
#'   \item{export_item}{Eight-digit AHECC export item code}
#'   \item{quantity_unit}{Unit of quantity}
#'   \item{description}{AHECC export item description}
#'   \item{free_descriptor}{Free-standing description.}
#' }
#' @details The Australian Harmonized Export Commodity Classification (AHECC) is
#'   designed for use by exporters, customs brokers and freight forwarders in the
#'   classification of goods when providing export declarations to the Department
#'   of Immigration and Border Protection, and to assist users interpret export
#'   statistics published by the Australian Bureau of Statistics (ABS).
#'
#'   The classification is based on the six digit international Harmonized Commodity
#'   Description and Coding System (HS) developed by the World Customs Organization
#'   (WCO) for describing internationally traded goods. The ABS extends the six digit
#'   international HS by two digits to provide a finer level of detail to meet
#'   Australian statistical requirements (Statistical codes).
#' 
#'   For further details, see:
#'   \href{http://www.abs.gov.au/ausstats/abs@.nsf/mf/1233.0}{Australian Harmonized Export Commodity Classification (AHECC)}.
#' @source The AHECC is produced and maintained by the Australian Bureau of Statistics. 
#' @references Australian Bureau of Statistics 2016, \emph{Australian Harmonized Export Commodity Classification (AHECC), 2017}, Catalogue no. 1233.0, ABS, Canberra. URL: \url{http://www.abs.gov.au/ausstats/abs@.nsf/mf/1233.0}.
"ahecc_2017"
"ahecc_2012"
"ahecc_2007"


#' @title AHECC Classification Codes and Abbreviations
#' @description  Data set containing the Australian Harmonized Export Commodity
#'   Classification (AHECC) supporting codes and abbreviations
#' @format Data sets \code{ahecc_uq}, \code{ahecc_abb}, \code{ahecc_currencies}
#'   and \code{ahecc_countries} contain supporting .
#'   The datasets contain the following variables:
#' \describe{
#'   \item{abbreviation}{unit of quantity abbreviation or AHECC abbreviation codes}
#'   \item{description}{unit of quantity description or AHECC abbreviation description}
#'   \item{country}{Country name}
#'   \item{currency_code}{Three-character country currency code}
#'   \item{currency}{Currency description}
#'   \item{code}{Two-character country code}
#'   \item{notes}{AHECC country notes.}
#' }
#' @details The Australian Harmonized Export Commodity Classification (AHECC) is
#'   designed for use by exporters, customs brokers and freight forwarders in the
#'   classification of goods when providing export declarations to the Department
#'   of Immigration and Border Protection, and to assist users interpret export
#'   statistics published by the Australian Bureau of Statistics (ABS).
#'
#'   These data sets provide supporting AHECC Classification Codes and Abbreviations:
#'   \itemize{
#'     \item{Unit of Quantity (\code{ahecc_uq})}
#'     \item{Abbreviations (\code{ahecc_abb})}
#'     \item{FOB Currency Codes (\code{ahecc_currencies})}
#'     \item{Countries (\code{ahecc_countries})}
#'   }
#'   For further details, see:
#'   \href{http://www.abs.gov.au/ausstats/abs@.nsf/mf/1233.0}{Australian Harmonized Export Commodity Classification (AHECC)}.
#' @source The AHECC is produced and maintained by the Australian Bureau of Statistics. 
#' @references Australian Bureau of Statistics 2016, \emph{Australian Harmonized Export Commodity Classification (AHECC), 2017}, Catalogue no. 1233.0, ABS, Canberra. URL: \url{http://www.abs.gov.au/ausstats/abs@.nsf/mf/1233.0}.
"ahecc_uq"
"ahecc_abb"
"ahecc_currencies"
"ahecc_countries"
