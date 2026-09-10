#' Widen Data Frame by Pivoting Columns
#'
#' This function takes a data frame and performs a pivot operation to widen it by 
#' creating new columns based on the values in specified columns. It is useful 
#' for reshaping data frames in preparation for analysis or visualization.
#'
#' @param data A data frame that you want to pivot.
#' @param name A character vector or column name specifying the column(s) whose 
#'             unique values will become the new column names.
#' @param value A character vector or column name specifying the column(s) whose 
#'              values will populate the cells of the newly created columns.
#'
#' @return A widened data frame with columns created based on unique values from 
#'         the 'name' column(s), and their corresponding values in the 'value' 
#'         column(s).
#'
#' @examples
#' # Example usage:
#' # Create a data frame
#' df <- data.frame(ID = c(1, 2, 3, 1, 2),
#'                  Category = c("A", "B", "A", "B", "C"),
#'                  Value = c(10, 20, 15, 25, 30))
#'
#' # Pivot the data frame to widen it by 'Category'
#' result <- elements_wider(df, "Category", "Value")
#' result
#'
#' # Output:
#' #   ID  A  B  C
#' # 1  1 10 25 NA
#' # 2  2 NA 20 30
#' # 3  3 15 NA NA
elements_wider <- function(data, name, value) {
  tidyr::pivot_wider(data, names_from = tidyselect::all_of(name), 
                     values_from = tidyselect::all_of(value))
}
