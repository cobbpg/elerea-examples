.SUFFIXES: .lhs .mkd .html .tex .pdf

PANDOC := pandoc --no-wrap -s -S
HSCOLOUR := HsColour -lit
UNIOPS := sed "s/>&lt;-</>\&larr;</g"

.lhs.html:
	cat $< | $(HSCOLOUR) -css | $(PANDOC) -t html -c hscolour.css | $(UNIOPS) > $@
