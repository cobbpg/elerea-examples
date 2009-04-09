.SUFFIXES: .lhs .html

PANDOC := pandoc --no-wrap -s -S
HSCOLOUR := HsColour -lit
UNIOPS := sed "s/>&lt;-</>\&larr;</g"

all: ../src/Breakout.html ../src/Chase.html

.lhs.html:
	cat $< | $(HSCOLOUR) -css | $(PANDOC) -t html -c hscolour.css | $(UNIOPS) > $(notdir $@)
