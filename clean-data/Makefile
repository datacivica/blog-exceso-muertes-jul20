#
# Authors:     OE
# Maintainers: OE
# Copyright:   2020, Data Cívica
# ===============================================
# blog-exceso-muertes-jul20/clean-data/Makefile

HERE := $(shell git rev-parse --show-toplevel)

.PHONY: all clean

all: output/exceso-mortalidad.rds

clean:
	rm output/*

inpfiles := $(HERE)/import/output/covid-ssa.rds \
		$(HERE)/import/output/solicitudes.rds \
		$(HERE)/import/output/sinais/defun14.rds \
		$(HERE)/import/output/sinais/defun15.rds \
		$(HERE)/import/output/sinais/defun16.rds \
		$(HERE)/import/output/sinais/defun17.rds \
		$(HERE)/import/output/sinais/defun18.rds

output/exceso-mortalidad.rds: \
		src/clean.R \
		$(inpfiles)
	Rscript --vanilla $<

# done.
