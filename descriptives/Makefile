#
# Authors:     OE
# Maintainers: OE
# Copyright:   2020, Data Cívica
# ===============================================
# blog-exceso-muertes-jul20/descriptives/Makefile

HERE := $(shell git rev-parse --show-toplevel)

.PHONY: all clean

all: output/tend-actas.png output/tend-actas.svg \
	 output/diff-actas.png output/diff-actas.svg \
	 output/diff-covid.png output/diff-covid.svg \
	 output/actas-avg.png output/actas-avg.svg

clean:
	rm output/*

outputs := output/tend-actas.png output/tend-actas.svg \
		output/diff-actas.png output/diff-actas.svg \
		output/diff-covid.png output/diff-covid.svg \
		output/actas-avg.png output/actas-avg.svg

inputs := src/descriptives.R \
		$(HERE)/clean-data/output/exceso-mortalidad.rds

$(outputs): $(inputs)
	Rscript --vanilla $<

# done.
