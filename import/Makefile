#
# Authors:     OE
# Maintainers: OE
# Copyright:   2020, Data Cívica, GPL v2 or later
# ===============================================
# blog-exceso-muertes-jul20/import/Makefile

.PHONY: all clean

all: output/solicitudes.rds \
	 output/covid-ssa.rds \
	 output/sinais/defun14.rds \
	 output/sinais/defun15.rds \
	 output/sinais/defun16.rds \
	 output/sinais/defun17.rds \
	 output/sinais/defun18.rds


clean:
	rm output/*

output := output/solicitudes.rds \
	 	output/covid-ssa.rds \
		output/sinais/defun14.rds \
		output/sinais/defun15.rds \
		output/sinais/defun16.rds \
		output/sinais/defun17.rds \
		output/sinais/defun18.rds

input := src/import.R \
		input/solicitudes.csv \
		input/200725COVID19MEXICO.csv \
		input/sinais/DEFUN14.dbf \
		input/sinais/DEFUN15.dbf \
		input/sinais/DEFUN16.dbf \
		input/sinais/DEFUN17.dbf \
		input/sinais/DEFUN18.dbf

$(output): $(input)
	Rscript --vanilla $<

# done.
