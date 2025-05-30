% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{db_ops}
\alias{db_ops}
\title{Database Operations Class}
\description{
R6 Class for managing database operations related to survey data storage
and retrieval using PostgreSQL. Includes automatic tracking of creation date,
update date, session ID, and IP address.
}
\details{
This class handles all database interactions for survey data, including:
\itemize{
\item Table creation and modification with tracking columns
\item Data insertion with automatic timestamp management
\item Session and IP tracking
\item Transaction management
\item Error handling and logging
}

Tracking columns automatically added to each table:
\itemize{
\item date_created: Timestamp when record was created
\item date_updated: Timestamp when record was last updated
\item session_id: Shiny session identifier
\item ip_address: Client IP address
}
}
\examples{

## ------------------------------------------------
## Method `db_ops$get_client_ip`
## ------------------------------------------------

\dontrun{
# Inside a Shiny server function
server <- function(input, output, session) {
  db_ops <- db_ops$new(pool, session$token, logger)
  client_ip <- db_ops$get_client_ip()
}
}

}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{session_id}}{Unique identifier for the current session}

\item{\code{pool}}{Database connection pool}

\item{\code{logger}}{Logger instance for tracking operations}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Database Operations-new}{\code{db_ops$new()}}
\item \href{#method-Database Operations-operate}{\code{db_ops$operate()}}
\item \href{#method-Database Operations-ensure_tracking_columns}{\code{db_ops$ensure_tracking_columns()}}
\item \href{#method-Database Operations-create_survey_table}{\code{db_ops$create_survey_table()}}
\item \href{#method-Database Operations-update_survey_table}{\code{db_ops$update_survey_table()}}
\item \href{#method-Database Operations-read_table}{\code{db_ops$read_table()}}
\item \href{#method-Database Operations-update_by_id}{\code{db_ops$update_by_id()}}
\item \href{#method-Database Operations-get_client_ip}{\code{db_ops$get_client_ip()}}
\item \href{#method-Database Operations-clone}{\code{db_ops$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database Operations-new"></a>}}
\if{latex}{\out{\hypertarget{method-Database Operations-new}{}}}
\subsection{Method \code{new()}}{
Create a new Database Operations instance
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{db_ops$new(pool, session_id, logger)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pool}}{Pool object. Database connection pool}

\item{\code{session_id}}{Character. Unique identifier for the current session}

\item{\code{logger}}{survey_logger object. Logger instance for tracking operations}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database Operations-operate"></a>}}
\if{latex}{\out{\hypertarget{method-Database Operations-operate}{}}}
\subsection{Method \code{operate()}}{
Execute a database operation with transaction handling
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{db_ops$operate(operation, error_message)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{operation}}{Function. The database operation to execute}

\item{\code{error_message}}{Character. Message to display if operation fails}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Result of the operation or error message if failed
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database Operations-ensure_tracking_columns"></a>}}
\if{latex}{\out{\hypertarget{method-Database Operations-ensure_tracking_columns}{}}}
\subsection{Method \code{ensure_tracking_columns()}}{
Ensure tracking columns exist in a table
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{db_ops$ensure_tracking_columns(table_name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table_name}}{Character. Name of the table to check/modify}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Invisible NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database Operations-create_survey_table"></a>}}
\if{latex}{\out{\hypertarget{method-Database Operations-create_survey_table}{}}}
\subsection{Method \code{create_survey_table()}}{
Create new survey data table with tracking columns
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{db_ops$create_survey_table(write_table, data, survey_obj = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{write_table}}{Character. Name of the table to create}

\item{\code{data}}{data.frame. Data frame containing the schema for the new table}

\item{\code{survey_obj}}{Survey.JS definition object that contains the complete survey structure.
A nested list containing:
\describe{
\item{pages}{List of survey pages}
\item{elements}{List of survey questions/elements, where each element contains:}
\item{name}{Question identifier}
\item{type}{Question type (e.g., "checkbox", "text", "radio")}
\item{showOtherItem}{Logical. Whether the question has an "other" option}
}
Used to determine column types and handle special question features like "other" options.
Default: NULL}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Character. The sanitized table name
Update existing survey table with new data
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database Operations-update_survey_table"></a>}}
\if{latex}{\out{\hypertarget{method-Database Operations-update_survey_table}{}}}
\subsection{Method \code{update_survey_table()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{db_ops$update_survey_table(write_table, data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{write_table}}{Character. Name of the table to update}

\item{\code{data}}{data.frame. Data frame containing the new data}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Character. The sanitized table name
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database Operations-read_table"></a>}}
\if{latex}{\out{\hypertarget{method-Database Operations-read_table}{}}}
\subsection{Method \code{read_table()}}{
Read data from a survey table with optional filtering
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{db_ops$read_table(
  table_name,
  columns = NULL,
  filters = NULL,
  order_by = NULL,
  desc = FALSE,
  limit = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table_name}}{Character. Name of the table to read from}

\item{\code{columns}}{Character vector. Specific columns to read (NULL for all columns)}

\item{\code{filters}}{List. Named list of filter conditions (e.g., list(status = "active"))}

\item{\code{order_by}}{Character vector. Columns to order by}

\item{\code{desc}}{Logical. If TRUE, sort in descending order}

\item{\code{limit}}{Numeric. Maximum number of rows to return (NULL for all rows)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
data.frame. The requested data
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database Operations-update_by_id"></a>}}
\if{latex}{\out{\hypertarget{method-Database Operations-update_by_id}{}}}
\subsection{Method \code{update_by_id()}}{
Update specific columns in a table for a given row ID
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{db_ops$update_by_id(table_name, id, values)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{table_name}}{Character. Name of the table to update}

\item{\code{id}}{Numeric. Row ID to update}

\item{\code{values}}{List. Named list of column-value pairs to update}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Invisible(NULL)
Get Client IP Address
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database Operations-get_client_ip"></a>}}
\if{latex}{\out{\hypertarget{method-Database Operations-get_client_ip}{}}}
\subsection{Method \code{get_client_ip()}}{
Retrieves the client IP address from HTTP request headers in order of preference.
This method checks multiple headers to handle scenarios involving proxies and load balancers.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{db_ops$get_client_ip()}\if{html}{\out{</div>}}
}

\subsection{Details}{
The method checks the following headers in order:
\enumerate{
\item X-Real-IP
\item X-Forwarded-For (takes first IP if multiple are present)
\item REMOTE_ADDR
}

If no IP address is found in any header, returns "0.0.0.0" as a fallback.
}

\subsection{Returns}{
Character string containing the client IP address. Returns "0.0.0.0" if no IP address can be determined.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
# Inside a Shiny server function
server <- function(input, output, session) {
  db_ops <- db_ops$new(pool, session$token, logger)
  client_ip <- db_ops$get_client_ip()
}
}

}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Database Operations-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Database Operations-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{db_ops$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
