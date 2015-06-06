Initial dissertation data was supplied as a .xlsx (Microsoft Excel) file from ProQuest, and included fields for Publication Number, School, Author, Advisor, Title, Subjects (closed vocabulary), Degree, Year, Page Count, Abstract, and Keywords (open vocabulary).

For initial preparation and overview of the data, I read and analyzed the abstracts in MS Excel, adding from 1-7 Methods tags to each abstract in an evolving schema; dissertations tagged early in this process were revisited and methods re-coded once the schema was finalized.

The JSON files in this directory were generated in Google Refine (now Open Refine).

One set of files contains the commands used initially to clean the data for coding:

A.  clean_titles_and_abstracts.json
B.  remove_allcaps_keywords.json
C.  split_advisor_from_advisortype.json
D.  simple_keyword_merging.json

The commands in file B above were used to remove keywords written in all capital letters, which seemed machine-generated rather than author-supplied. Before this process, there were 20,824 keywords; afterwards, there were 7,974. The commands in file D above were used to locate and merge obvious synonyms among the author-supplied keywords, such as "Kenneth Burke" with "Burke, Kenneth" or "Rhetoric" with "rhetoric." This process further reduced the number of unique keywords to 7,572.

Another set of files can be used to expand and contract records between a single row (for analysis in R) and multiple rows (for ease of reading in Excel), through Refine's "Undo/Redo > Apply" feature.

A typical order for preparation with an Excel file loaded in Google Refine was as follows:

1.	compress_and_clean_multirows.json
2.  expand_method_tags.json
3.  count_methods.json
4.  method_words_to_bits.json or method_words_to_bits2.json
5.	compress_and_clean_multirows.json
6.	exclude_bits.json
7.  (export as .csv for use in R)

Note that steps 3, 4, and 6 above are now accomplished by 'method tag array.R'.

