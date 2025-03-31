STYLESHEETS := $(wildcard *.css)
STYLESHEETS := $(filter-out uikit%.css, $(STYLESHEETS))

all:
	stylelint $(STYLESHEETS)
