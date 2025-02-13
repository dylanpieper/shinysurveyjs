% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{db_pool_close}
\alias{db_pool_close}
\title{Close Database Pool}
\usage{
db_pool_close(session)
}
\arguments{
\item{session}{A Shiny session object that represents the current user session.
This is used to register the cleanup operation.}
}
\description{
Closes the database connection pool and performs cleanup operations
when the application is shutting down. This function ensures proper
resource management by closing all open connections and removing the
pool object from the global environment.
}
\details{
This function performs the following operations:
\enumerate{
\item Registers a cleanup handler using shiny::onStop
\item Checks for the existence of 'app_pool' in the global environment
\item Calls cleanup_pool() on the existing pool if found
\item Removes the pool object from the global environment
}
}
\note{
The function assumes the existence of a cleanup_pool() function and
that the database pool is stored in the global environment as 'app_pool'.
}
\examples{
\dontrun{
# In your Shiny server function
function(input, output, session) {
  db_pool_close(session)
}
}

}
\seealso{
\code{\link[shiny]{onStop}} for more information about Shiny's cleanup handlers
}
\keyword{internal}
