Initial dissertation data was supplied as a .xlsx (Microsoft Excel) file from ProQuest,
and included fields for Publication Number, School, Author, Advisor, Title, Subjects
(closed vocabulary), Degree, Year, Page Count, Abstract, and Keywords (open vocabulary).

For initial preparation and overview of the data, I read and analyzed the abstracts in 
MS Excel, adding from 1-7 Methods tags to each abstract in an evolving schema;
dissertations tagged early in this process were revisited and methods re-coded once the
schema was finalized.

The JSON files in this directory were generated in Google Refine (now Open Refine), and
can be used to expand and contract records between a single row (for analysis in R) and
multiple rows (for ease of reading in Excel).

A typical order for preparation was:
0.
1.	compress_and_clean_multirows.json 
2.	expand_method_tags.json
3.	count_methods.json
4.	method_words_to_bits.json or method_words_to_bits2.json
5.	compress_and_clean_multirows.json
6.	exclude_bits.json

Note that steps 3, 4, and 6 above are now accomplished by 'method tag array.R'.
