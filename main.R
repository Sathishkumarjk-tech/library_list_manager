# Library List Manager
# Run in an interactive R/RStudio console: source("main.R")

library_books <- list(
  list(title = "Wings of Fire", author = "A. P. J. Abdul Kalam", year = 1999),
  list(title = "The Alchemist", author = "Paulo Coelho", year = 1988),
  list(title = "Ponniyin Selvan", author = "Kalki Krishnamurthy", year = 1950)
)

show_books <- function(books) {
  if (length(books) == 0) {
    cat("No books in the library.\n")
    return(invisible(NULL))
  }
  for (i in seq_along(books)) {
    cat(i, ". ", books[[i]]$title, " | ", books[[i]]$author,
        " | ", books[[i]]$year, "\n", sep = "")
  }
}

repeat {
  cat("\n===== LIBRARY LIST MANAGER =====\n")
  cat("1. Show all books\n2. Add a book\n3. Search by title\n4. Remove a book\n0. Exit\n")
  choice <- trimws(readline("Choose an option: "))

  if (choice == "0") {
    cat("Library manager closed.\n")
    break
  } else if (choice == "1") {
    show_books(library_books)
  } else if (choice == "2") {
    title <- trimws(readline("Book title: "))
    author <- trimws(readline("Author: "))
    year <- suppressWarnings(as.integer(readline("Publication year: ")))
    if (title == "" || author == "" || is.na(year)) {
      cat("Invalid details. Book was not added.\n")
    } else {
      library_books[[length(library_books) + 1]] <- list(
        title = title, author = author, year = year
      )
      cat("Book added successfully.\n")
    }
  } else if (choice == "3") {
    query <- tolower(trimws(readline("Title to search: ")))
    matches <- which(vapply(library_books, function(book) {
      grepl(query, tolower(book$title), fixed = TRUE)
    }, logical(1)))
    if (length(matches) == 0) cat("No matching book found.\n")
    else show_books(library_books[matches])
  } else if (choice == "4") {
    show_books(library_books)
    number <- suppressWarnings(as.integer(readline("Book number to remove: ")))
    if (is.na(number) || number < 1 || number > length(library_books)) {
      cat("Invalid book number.\n")
    } else {
      removed_title <- library_books[[number]]$title
      library_books <- library_books[-number]
      cat("Removed:", removed_title, "\n")
    }
  } else {
    cat("Invalid choice.\n")
  }
}
