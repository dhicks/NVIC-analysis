## This script parses the pages downloaded by wget

library(tidyverse)
library(stringr)
library(xml2)

parse_file = function (path_to_file) {
	print(path_to_file)
	## Detect PDF files
	top_line = readLines(path_to_file, n = 1)
	# print(top_line)
	if (str_detect(top_line, 'GIF|PNG')) {
		## Skip image files entirely
		parsed_file = tibble(path = path_to_file, 
							 title = NA, 
							 text = NA, 
							 note = 'image')
		return(parsed_file)
	} else if (str_detect(top_line, 'PDF')) {
		pdf_version = path_to_file
		text_version = str_replace(pdf_version, '.aspx', '.txt')
		
		if (!file.exists(text_version)) {
			clean_pathstring = function (badstring) {
				str_replace_all(badstring, '([\\(\\)])', '\\\\\\1')
			}
			
			print('PDF w/o converted text found; calling pdftotext')
			## pdftotext: <http://www.foolabs.com/xpdf/download.html>
			system2('pdftotext', paste(clean_pathstring(pdf_version), 
									   clean_pathstring(text_version)))
		}
		## Read and parse text version
		text = read_lines(text_version)
		title = text[1]
		text = paste(text, collapse = '\n')
		parsed_file = tibble(
			path = path_to_file, 
			title = title, 
			text = text, 
			note = 'pdf'
		)
		# unlink(text_version)
		return(parsed_file)
	}
	
	## Nice idea — but many pages don't have their content in <p> tags
	# read_file = read_html(path_to_file)
	# parsed_title = xml_find_all(read_file, '//title') %>% xml_text(trim = TRUE)
	# # print(parsed_title)
	# parsed_text = xml_find_all(read_file, '//p') %>% 
	# 	xml_text(trim = TRUE) %>%
	# 	paste(collapse = '\n')
	read_file = read_file(path_to_file)
	## Extract title
	parsed_title = stringi::stri_match_first_regex(read_file, '<title>(.*)</title>', dotall = TRUE)[1,2]
	## Remove multiline scripts
	read_file = stringi::stri_replace_all_regex(read_file, '<script[^<]*</script>', '', dotall = TRUE)
	## Remove other HTML tags
	read_file = str_replace_all(read_file, '<[^>]*>', '')
	## Remove HTML escapes
	read_file = str_replace_all(read_file, '&[a-z]*;', '')
	
	parsed_file = tibble(path = path_to_file,
						 title = parsed_title, 
						 text = read_file)
	return(parsed_file)
}

# Having trouble cleaning out the javascript
# see https://github.com/tidyverse/stringr/issues/145
# 
# this_file = 'www.nvic.org/NVIC-Vaccine-News/April-2014.aspx'
# read_file = read_file(this_file)
# results = stringi::stri_replace_all_regex(read_file, '<script[^<]*</script>', '', dotall = TRUE)
# 
# parse_file(this_file)
# 
# text <- "<em>works</em><div\nid=\"thing\">doesn't work</div>"
# str_match_all(text, "<.+?>")
# text <- "<em>matched</em><em>not \n matched</em>"
# str_match_all(text, "<em>.*</em>")
# text <- "<em>matched</em><em>not \n matched</em>"
# str_match_all(text, regex("<em>.*</em>", multiline = TRUE))


parse_folder = function(this_folder) {
	files_here = list.files(this_folder)
	files_here = files_here[str_detect(files_here, '.aspx$')]
	
	if (length(files_here) > 0) {
		files_here = str_c(this_folder, '/', files_here)
		parsed = lapply(files_here, parse_file) %>% bind_rows()
	} else {
		parsed = tibble()
	}
	
	dirs_here = list.dirs(this_folder, recursive = FALSE)
	if (length(dirs_here) > 0) {
		parsed_subfolders = lapply(dirs_here, parse_folder) %>% bind_rows()
		parsed = bind_rows(parsed, parsed_subfolders)
	}

	return(parsed)
}

## this_folder should point to the results of a wget call
## wget: <https://www.gnu.org/software/wget/>
## For Macs, it's on Homebrew and MacPorts: <https://www.macports.org/ports.php?by=library&substr=wget>
## example call: wget -r --no-parent www.nvic.org

this_folder = 'www.nvic.org'
parsed_site = parse_folder(this_folder)
# save(parsed_site, file = str_c(this_folder, '.Rdata'))

