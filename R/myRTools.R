#' myRTools Fast CSV writer
#'
#' myRTools.fwrite is an extension of \code{\link[data.table]{fwrite}} for
#' convenient R programming. In the first line, it stores classes of \code{x}
#' followed by \code{x} itselfs.
#' @param x Any list of same length vectors; e.g. data.frame and data.table.
#' @param file Output file name. "" indicates output to the console.
#' @param append If TRUE, the file is opened in append mode and column names (header row) are not written.
#' @param ... XXX
#' @author Christian Frei
#' @details myRTools.fwrite is an extension of \code{\link[data.table]{fwrite}}
#' for convenient R programming. In the first line, it stores classes of
#' \code{x} followed by \code{x} itselfs.
#' @seealso \code{\link[myRTools]{myRTools.fread}} \code{\link[data.table]{fwrite}}
#' @examples
#' dateToday1 <- today()
#' timeNow1 <- now()
#' dt1 <- data.table(int = 1:5,
#'                  num = seq(0.2, 1, by = 0.2),
#'                  char = c("a", "b", "c", "d", "f"),
#'                  date = dateToday1 + days(0:4),
#'                  time = timeNow1 + days(0:4))
#' myRTools.fwrite(dt1, file = "test.csv")
#'
#' dateToday2 <- today() - years(1)
#' timeNow2 <- now() - years(1)
#' dt2 <- data.table(int = 6:10,
#'                   num = seq(1.2, 2, by = 0.2),
#'                   char = c("v", "w", "x", "y", "z"),
#'                   date = dateToday2 + days(0:4),
#'                   time = timeNow2 + days(0:4))
#' myRTools.fwrite(dt2, file = "test.csv", append = TRUE)
#'
#' @export
#' @import data.table
myRTools.fwrite <- function (x, file, append = FALSE, ...) {
   vecClass <- sapply(x, FUN = function(x) {class(x)[1]})

   if (append == FALSE) {
      dtClass <- data.table(1)
      for(i in vecClass) {
         dtClass <- cbind(dtClass, i[1])
      }
      data.table::fwrite(dtClass[, -1], file, col.names = FALSE, ...)

      fwrite(x, file, append = TRUE, col.names = TRUE, ...)
   } else {
      if(file.exists(file)) {
         vecClassOfFile <- as.character(fread(file = file, nrows = 1, header = FALSE))
         if (length(vecClass) == length(vecClassOfFile) & all(vecClass == vecClassOfFile)){
            data.table::fwrite(x, file, append = TRUE, ...)
         } else {
            stop("myRTools.fwrite (append = TURE, ...): Number of columns or types of columns are not equal.")
         }
      }
   }
}

#' myRTools Fast and friendly file finagler
#'
#' myRTools.fread is an extension of \code{\link[data.table]{fread}} for
#' convenient R programming. In the first line, it stores classes of \code{x}
#' followed by \code{x} itselfs.
#' @param ... XXX
#' @return A data.table by default. A data.frame when argument data.table=FALSE;
#' e.g. options(datatable.fread.datatable=FALSE).
#' @author Christian Frei
#' @details myRTools.fread is an extension of \code{\link[data.table]{fread}} for
#' convenient R programming. In the first line, it stores classes of \code{x}
#' followed by \code{x} itselfs.
#' \code{\link[myRTools]{myRTools.fwrite}}. Afterwards it load the rest of the
#' file and converts all columns according to the class definition.
#' @seealso \code{\link[myRTools]{myRTools.fwrite}}, \code{\link[data.table]{fread}}, \code{\link[fasttime]{fastPOSIXct}}, \code{\link[lubridate]{as_date}}
#' @examples
#' dateToday1 <- today()
#' timeNow1 <- now()
#' dt1 <- data.table(int = 1:5,
#'                  num = seq(0.2, 1, by = 0.2),
#'                  char = c("a", "b", "c", "d", "f"),
#'                  date = dateToday1 + days(0:4),
#'                  time = timeNow1 + days(0:4))
#' myRTools.fwrite(dt1, file = "test.csv")
#'
#' dateToday2 <- today() - years(1)
#' timeNow2 <- now() - years(1)
#' dt2 <- data.table(int = 6:10,
#'                   num = seq(1.2, 2, by = 0.2),
#'                   char = c("v", "w", "x", "y", "z"),
#'                   date = dateToday2 + days(0:4),
#'                   time = timeNow2 + days(0:4))
#' myRTools.fwrite(dt2, file = "test.csv", append = TRUE)
#'
#' dt3 <- myRTools.fread(file="test.csv")
#' print(dt3)
#' @export
#' @import data.table
#' @importFrom fasttime fastPOSIXct
#' @importFrom lubridate as_date
myRTools.fread <- function (...) {
   vecClass <- as.character(data.table::fread(nrows = 1, header = FALSE, ...))

   dt <- data.table::fread(skip = 1, colClasses = vecClass, ...)

   convert <- which(!(vecClass %in% c("integer",
                                      "numeric",
                                      "character")))
   if(length(convert) > 0) {
      for(col in convert){
         class <- vecClass[col]

         if(isS4(class)){
            dt[,col := new(class, col)]
         } else {
            if(class == "POSIXct"){
               dt[, (col) := lapply(.SD, fasttime::fastPOSIXct), .SDcols=col]
            } else if (class == "Date") {
               dt[, (col) := lapply(.SD, lubridate::as_date), .SDcols=col]
            }
         }
      }
   }
   dt
}

