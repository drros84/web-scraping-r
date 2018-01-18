
library(crandb)
library(dplyr)

## Get all package names, version, and title using the "crandb" package
all_packages <- list_packages(limit = 20000, archived = FALSE) %>%
  unlist()
# When downloading from crandb, I get a list. Below, unpack it by taking elements
# at specific intervals
all_packages <- data.frame(package_name = names(all_packages)[seq(1, length(all_packages), 2)],
                            package_version = all_packages[seq(1, length(all_packages), 2)],
                            package_title = all_packages[seq(2, length(all_packages), 2)])

# Convert to character
all_packages$package_name <- as.character(all_packages$package_name)
all_packages$package_version <- as.character(all_packages$package_version)
all_packages$package_title <- as.character(all_packages$package_title)

# Only keep the actual name of the package
all_packages$package_name <- substr(all_packages$package_name, 1,
                                     nchar(all_packages$package_name) - 8)

# Re-index the dataframe
row.names(all_packages) <- c(1:nrow(all_packages))

# Save the dataframe as csv
# write.csv(all_packages3, "/Users/davidrosenfeld/Documents/web_scraping_r/all_packages.csv")
# write.csv(merged_tables, "/Users/davidrosenfeld/Documents/web_scraping_r/merged_tables.csv")
# write.csv(author_clean, "/Users/davidrosenfeld/Documents/web_scraping_r/author_clean.csv")
# write.csv(depend_matrix, "/Users/davidrosenfeld/Documents/web_scraping_r/depend_matrix.csv")



# # Read the dataframe
# all_packages <- read.csv("/Users/davidrosenfeld/Documents/web_scraping_r/all_packages.csv")

# Load packages for web-scraping and data analysis
library(httr)
library(rvest)
library(XML)
library(reshape2)
library(purrr)
library(dplyr)

# Function for extracting data for each package name as input
get_package_data <- function(name_of_package) {
  
  # Scrape CRAN web page for a given R package 
  stored_html <- read_html(paste0("https://cran.r-project.org/web/packages/", 
                                  name_of_package,
                                  "/index.html"))
  
  get_table <- tryCatch(html_table(stored_html)[[1]] %>% data.frame(), 
           error = function(e) data.frame(X1 = c("Version", "Imports", "Suggests",
                                                 "Published", "Author"),
                                          X2 = NA))
  
  if(sum(!is.na(get_table[,2])) > 0) {
    
    # Remove colons from the first column of the table
    get_table$X1 <- substr(get_table$X1, 1, nchar(get_table$X1) - 1)
    
    # Create a vector including string "name" and the name of the package
    name_vector <- c("name", html_text(html_nodes(stored_html, css = "title")))
    # Clean up vector name
    name_vector[2] <- substr(name_vector[2], 16, nchar(name_vector[2]))
    
    # Create a vector with string "long_name" and the title of the package
    long_name_vector <- c("long_name", html_text(html_nodes(stored_html, css = "h2")))
    
    # Create a vector with string "package_description" and a description of what
    # the package does
    package_description_vec <- c("package_description",
                                 html_text(html_nodes(stored_html, css = "p"))[[1]])
    
    # Bind the table with the vectors to create a unique dataframe
    get_table <- rbind(get_table, name_vector, long_name_vector, package_description_vec)
    
    # Name the second column after the package name
    colnames(get_table)[2] <- name_vector[2]
    
  }
  
  # # Extract the first table from the stored web page, which includes useful info
  # get_table <- html_table(stored_html)[[1]] %>%
  #   data.frame()
  # 
  # # Remove colons from the first column of the table
  # get_table$X1 <- substr(get_table$X1, 1, nchar(get_table$X1) - 1)
  # 
  # # Create a vector including string "name" and the name of the package
  # name_vector <- c("name", html_text(html_nodes(stored_html, css = "title")))
  # # Clean up vector name
  # name_vector[2] <- substr(name_vector[2], 16, nchar(name_vector[2]))
  # 
  # # Create a vector with string "long_name" and the title of the package
  # long_name_vector <- c("long_name", html_text(html_nodes(stored_html, css = "h2")))
  # 
  # # Create a vector with string "package_description" and a description of what
  # # the package does
  # package_description_vec <- c("package_description",
  #                              html_text(html_nodes(stored_html, css = "p"))[[1]])
  # 
  # # Bind the table with the vectors to create a unique dataframe
  # get_table <- rbind(get_table, name_vector, long_name_vector, package_description_vec)
  # 
  # # Name the second column after the package name
  # colnames(get_table)[2] <- name_vector[2]
  
  return(get_table)
}

stored_html <- read_html(paste0("https://cran.r-project.org/web/packages/", 
                                "dataRetrieval",
                                "/index.html"))

get_table <- html_table(stored_html)[[1]] %>%
  data.frame()

tryCatch(html_table(stored_html)[[1]], 
         error = function(e) data.frame(X1 = c("Version", "Imports", "Suggests",
                                               "Published", "Author"),
                                        name_of_package = NA))

for(i in seq_along(A)){
  tryCatch(print(A[[i]][[2]]),
           error = function(e) print(NA))
}




first_thousand_packages <- create_packages_dataframe(all_packages$package_name[1:1000], 5)

write.csv(first_thousand_packages, "/Users/davidrosenfeld/Documents/web_scraping_r/first_thousand_packages.csv")

first_thousands <- read.csv("/Users/davidrosenfeld/Documents/web_scraping_r/first_thousands.csv")



new_package_list <- all_packages$package_name[9682:12010]
# Create an empty list to fill with package data
combined_tables <- list()
# Loop over package names. For each, create dataframe using the get_package_data
# function above, and add it to the combined_tables list.
# Pause for 5 seconds between every request
for(i in 1:length(new_package_list)) {
  
  combined_tables[[i]] <- get_package_data(new_package_list[i])
  Sys.sleep(5)
  
}

closeAllConnections()

merged_tables <- reduce(combined_tables, full_join, by = "X1")

up_to_now <- full_join(up_to_now, merged_tables, by = "X1")

write.csv(up_to_now, "/Users/davidrosenfeld/Documents/web_scraping_r/up_to_now.csv",
          row.names = FALSE)


# Full join all the tables in the list, but maintaining all the rows (which is why 
# I use full_join rather that left_join). Not all packages include the same info,
# it is important to keep all rows. Join by the "X1" column, which is common to
# all dataframes in the list
merged_tables <- reduce(combined_tables, full_join, by = "X1")





# Extract data from the "Depends" column and the package name. This will give us 
# the dependencies for each package
dependencies <- merged_tables[merged_tables$X1 %in% c("Depends", "name"), ] %>%
  t() 
colnames(dependencies) <- dependencies[1, ]
dependencies <- dependencies[-1, ]
dependencies <- data.frame(dependencies)

# Import R packages that facilitate text mining
library(stringr)
library(stringi)
library(rebus)

# Split the string of dependencies by using commas as separators. Use the 
# "simplify = TRUE" argument to store these as a matrix
depend_matrix <- str_split(dependencies$Depends, pattern = fixed(","), simplify = TRUE)

# Clean package names by getting rid of spaces at the start or end of each package name
test <- str_replace_all(depend_matrix, pattern = or(START %R% " ", " " %R% END),
                        replacement = "") %>%
  as.matrix(nrow = nrow(depend_matrix), ncol = ncol(depend_matrix), byrow = FALSE)





# Extract author names for each package
author_matrix <- up_to_now[up_to_now$X1 %in% c("Author", "name"), ] %>%
  t() 
colnames(author_matrix) <- author_matrix[1, ]
author_matrix <- author_matrix[-1, ]
author_matrix <- data.frame(author_matrix)

# # Split the string by using the square brackets (and their content) as a separator. The 
# # square brackets includes strings like "cre, aut". Use the "simplify = TRUE" argument
# # to store results as a matrix
# test <- str_split(author_matrix$Author, pattern = " [" %R% one_or_more(ANY_CHAR) %R%
#                                                        "], ", 
#                   simplify = TRUE)
# 
# # Extract the contents of the square brackets
# test <- str_extract_all(author_matrix$Author, pattern = " [" %R% one_or_more(ANY_CHAR) %R%
#                           "], ")

# str_view_all(author_matrix$Author, pattern = OPEN_BRACKET %R% WRD %R% zero_or_more(", ") 
#              %R% zero_or_more(WRD) %R% CLOSE_BRACKET)
# 
# str_view_all(author_matrix$Author, pattern = OPEN_BRACKET %R% WRD %R% "[:punct:]" %R% 
#                "[:space:]" %R%
#              WRD %R% CLOSE_BRACKET)
# 
# str_view_all(author_matrix$Author, pattern = OPEN_BRACKET %R% one_or_more(WRD) %R% 
#                zero_or_more(fixed(", ")) %R% zero_or_more(WRD) %R% CLOSE_BRACKET)


# Extract the content of the square brackets
author_types <- str_extract_all(author_matrix$Author, pattern = OPEN_BRACKET %R% one_or_more(WRD) %R% 
               zero_or_more(fixed(", ")) %R% zero_or_more(WRD) %R% CLOSE_BRACKET,
               simplify = TRUE)

# Remove the square brackets and their content from the author data
# author_clean <- str_replace_all(author_matrix$Author, pattern = SPACE %R% 
#                                   OPEN_BRACKET %R% one_or_more(WRD) %R% 
#                   zero_or_more(fixed(", ")) %R% zero_or_more(WRD) %R% 
#                     zero_or_more(fixed(", ")) %R% zero_or_more(WRD) %R%CLOSE_BRACKET,
#                 replacement = "")

# Remove the square brackets and their content from the author data
author_clean <- str_replace_all(author_matrix$Author, 
                                pattern = "\\[.*\\]",
                                replacement = "")


author_clean <- gsub( " *\\(.*?\\) *", "", author_clean)

# Remove the "\n" that seem to have appeared after the previous operation
author_clean <- str_replace_all(author_clean, pattern = or(fixed("\n "),
                                                           fixed("and"),
                                                           "Inc" %R% zero_or_more(".")
                                                           ),
                                replacement = "")

author_clean <- str_replace_all(author_clean, pattern = "with" %R% one_or_more(SPACE) %R%
                                  "contributions" %R% one_or_more(SPACE) %R% "by", 
                                replacement = ",")

# split the author data based on commas 
author_clean <- str_split(author_clean, pattern = or(zero_or_more(SPACE) %R%
                                                       or(",", ";") %R% zero_or_more(SPACE),
                                                     # fixed(" , "),
                                                     # fixed(", "), 
                                                     # fixed(","),
                                                     # " with contributions " %R%
                                                     #   or("by", "from"),
                                                     # fixed(" with contributions by "),
                                                     zero_or_more(",") %R%
                                                       zero_or_more(SPACE) %R%
                                                       "and"
                                                     ), 
                          simplify = TRUE)



# Add package names as row.names
row.names(author_clean) <- all_packages$package_name

# Transpose matrix (so columns are now packages, rows are authors),
# then melt to create author-package pairs,
# remove the first column,
# remove empty cells
author_clean <- author_clean %>%
  t() %>%
  melt() %>%
  select(Var2, value) %>%
  filter(value != "")

colnames(author_clean) <- c("package_name", "author")

author_clean$author <- str_replace_all(author_clean$author, pattern = or(START %R% SPACE,
                                                           SPACE %R% END),
                                replacement = "")


author_clean$author <- str_replace_all(author_clean$author, pattern = SPACE %R% SPACE,
                                       replacement = " ")

# ranked_packages <- cran_logs %>% 
#   colSums() %>%
#   sort(decreasing = TRUE)
# 
# ranked_packages <- data.frame(package_name = names(ranked_packages),
#                               n_downloads = ranked_packages)

author_downloads <- author_clean %>%
  left_join(ranked_packages, by = "package_name") %>%
  group_by(author) %>%
  summarise(n_downloads = sum(n_downloads), n_packages = n()) %>%
  arrange(desc(n_downloads))

author_downloads <- author_downloads %>%
  mutate(rank = row.names(author_downloads))

str_view_all(author_downloads$author, pattern = "Hadley Wickham", match = TRUE)


#########
# Get number of downloads for each package
library(cranlogs)

test <- cran_downloads(packages = all_packages$package_name[1:3], 
                       from = "1999-01-01", to = "2017-12-31")

library(reshape2)
test3 <- dcast(test, date ~ package, value.var = "count")
test2 <- cran_downloads(packages = "ggplot2", from = "1999-01-01", to = "2017-12-31")

test$count %>%
  sum()
test %>%
ggplot(aes(x = date, y = count, col = package)) +
  geom_line()

c(1:100) + (100 * 119)

cran_logs <- data.frame(matrix(NA, nrow = 6940, ncol = 12010))
row.names(cran_logs) <- test3$date
colnames(cran_logs) <- all_packages$package_name

for(i in 0:119) {
  
  selector <- c(1:100) + (100 * i)
  
  latest_logs <- cran_downloads(packages = all_packages$package_name[selector], 
                              from = "1999-01-01", to = "2017-12-31")
  
  latest_logs <- dcast(latest_logs, date ~ package, value.var = "count")
  
  cran_logs[, selector] <- latest_logs[2:101]
  
  Sys.sleep(5)
  
}

write.csv(cran_logs, "/Users/davidrosenfeld/Documents/web_scraping_r/cran_logs.csv")




cran_logs <- cran_downloads(packages = all_packages$package_name, 
                       from = "1999-01-01", to = "2017-12-31")


ranked_packages <- cran_logs %>% 
  colSums() %>%
  sort(decreasing = TRUE)

  






