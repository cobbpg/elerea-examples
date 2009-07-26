.SUFFIXES: .lhs .html

PANDOC := pandoc --no-wrap -s -S
HSCOLOUR := HsColour -lit
UNIOPS := sed "s/>&lt;-</>\&larr;</g" | sed "s/>-&gt;</>\&rarr;</g" | sed "s/varid'>mdo/keyword'>mdo/g"

all: ../src/Breakout.html ../src/Chase.html

.lhs.html:
	cat $< $(dir $<)Common/*lhs | $(HSCOLOUR) -css | $(PANDOC) -t html -c hscolour.css | $(UNIOPS) > $(notdir $@)
