
# Point this at the name of the main paper file:
# Typically use something descriptive for this name.
PAPER=main_locationcalc

# Token files that are used to control build mode:
EDMARK=activateeditingmarks
# ^ Usually I leave this checked into the repo at the beginning of
# paper writing, and delete it later.
GREYMARK=activategreybg

# Sometimes it helps to point latexmk at a subdir, like ./build:
OUTDIR= ./build
MKOPTS= -outdir=${OUTDIR} -pdf -file-line-error -interaction=nonstopmode
LATEXMK=latexmk ${MKOPTS}
ROOT_LATEX_AUX = ${PAPER}.aux ${PAPER}.bbl ${PAPER}.blg ${PAPER}.fdb_latexmk ${PAPER}.fls ${PAPER}.bcf ${PAPER}.run.xml

GIBBON_SOA_DIR ?= ../../gibbon/gibbon-compiler/examples/soa_examples
GIBBON_TABLE_SRC ?= ${GIBBON_SOA_DIR}/figures/performance_table.tex
PER_PROGRAM_TABLE_DIR = tables/per_program
PER_PROGRAM_RUNTIME_TABLE_DIR = tables/per_program_runtime
PER_PROGRAM_PAPI_TABLE_DIR = tables/per_program_papi
PER_PROGRAM_PAPI_MAIN_TABLE_DIR = tables/per_program_papi_main
END_TO_END_TABLE = tables/end_to_end_summary.tex
PROGRAM_TABLE_NAMES = Compiler DBQuery DecisionTree DomTree KDTree LinearListReduction reduceNestedList List MonoTree ObjectGraph OctTree PiecewiseFunctions TernaryTree Trie
LOCAL_FULL_TABLES = $(addsuffix .tex,$(addprefix $(PER_PROGRAM_TABLE_DIR)/,$(PROGRAM_TABLE_NAMES)))
LOCAL_RUNTIME_TABLES = $(addsuffix .tex,$(addprefix $(PER_PROGRAM_RUNTIME_TABLE_DIR)/,$(PROGRAM_TABLE_NAMES)))
LOCAL_PAPI_TABLES = $(addsuffix .tex,$(addprefix $(PER_PROGRAM_PAPI_TABLE_DIR)/,$(PROGRAM_TABLE_NAMES)))
LOCAL_MAIN_PAPI_TABLES = $(PER_PROGRAM_PAPI_MAIN_TABLE_DIR)/Compiler.tex
LOCAL_TABLES = ${LOCAL_FULL_TABLES} ${LOCAL_RUNTIME_TABLES} ${LOCAL_PAPI_TABLES} ${LOCAL_MAIN_PAPI_TABLES} ${END_TO_END_TABLE}
EVAL_TABLES_STAMP = ${OUTDIR}/eval_tables.stamp

paper: eval_tables ${OUTDIR}
	rm -f ${ROOT_LATEX_AUX}
	${LATEXMK} ${PAPER}.tex

# Continuous building
cont: continuous
continuous: eval_tables ${OUTDIR}
	rm -f ${ROOT_LATEX_AUX}
	${LATEXMK} -pvc ${PAPER}.tex

${OUTDIR}:
	mkdir -p ${OUTDIR}

nix: clean
	nix-shell --pure --command "make paper"

nix_cont: clean
	nix-shell --pure --command "make cont"

# Build with editorial markup rather than the final pdf:
# ============================================================
ed: editingmarks
editingmarks: eval_tables
	touch $(EDMARK)
	rm -f ${ROOT_LATEX_AUX}
	${LATEXMK} ${PAPER}.tex

plain: eval_tables
	rm -f $(EDMARK)
	rm -f ${ROOT_LATEX_AUX}
	${LATEXMK} -g ${PAPER}.tex

both: eval_tables
	touch $(EDMARK)
	rm -f ${ROOT_LATEX_AUX}
	${LATEXMK} -g ${PAPER}.tex
	cp ${OUTDIR}/${PAPER}.pdf ./wmarks.pdf

	rm -f $(EDMARK)
	rm -f ${ROOT_LATEX_AUX}
	${LATEXMK} -g ${PAPER}.tex
	cp ${OUTDIR}/${PAPER}.pdf ./plain.pdf

# A little shorthand for Ryan.  Easier on the eyes:
grey:
	touch $(GREYMARK)
	${MAKE} paper
white:
	rm -f $(GREYMARK)
	${MAKE} paper

# ============================================================

eval_tables: ${EVAL_TABLES_STAMP}

${EVAL_TABLES_STAMP}: ${LOCAL_TABLES} | ${OUTDIR}
	@echo "Using local per-program tables in ${PER_PROGRAM_TABLE_DIR}"
	touch ${EVAL_TABLES_STAMP}

pull_latest_eval_data: ${PER_PROGRAM_TABLE_DIR} ${PER_PROGRAM_RUNTIME_TABLE_DIR} ${PER_PROGRAM_PAPI_TABLE_DIR} ${PER_PROGRAM_PAPI_MAIN_TABLE_DIR}
	./scripts/sync_eval_tables.sh "${GIBBON_TABLE_SRC}" "${PER_PROGRAM_TABLE_DIR}" "${END_TO_END_TABLE}"
	@echo "Updated per-program tables and end-to-end summary from ${GIBBON_TABLE_SRC}"

${PER_PROGRAM_TABLE_DIR}:
	mkdir -p ${PER_PROGRAM_TABLE_DIR}

${PER_PROGRAM_RUNTIME_TABLE_DIR}:
	mkdir -p ${PER_PROGRAM_RUNTIME_TABLE_DIR}

${PER_PROGRAM_PAPI_TABLE_DIR}:
	mkdir -p ${PER_PROGRAM_PAPI_TABLE_DIR}

${PER_PROGRAM_PAPI_MAIN_TABLE_DIR}:
	mkdir -p ${PER_PROGRAM_PAPI_MAIN_TABLE_DIR}

UNAME := $(shell uname)

# Open the PDF in a viewer after building:
open: ${OUTDIR}/${PAPER}.pdf
ifeq (${UNAME}, Darwin)
	open ${OUTDIR}/${PAPER}.pdf
endif
ifeq (${UNAME}, Linux)
	evince ${OUTDIR}/${PAPER}.pdf &
endif

# Skim is great for auto-reloading papers on Mac OS, even if it isn't
# set up as the default:
skim:
	open -a skim ${OUTDIR}/${PAPER}.pdf

clean: 
	rm -f ${ROOT_LATEX_AUX}
	cd ${OUTDIR}; rm -f *.out *.aux *.bbl *.log *.blg *.fls *.fdb_latexmk *.bcf *.run.xml ${PAPER}.pdf

.PHONY: editingmarks ed paper plain both grey white continuous cont skim clean eval_tables pull_latest_eval_data


# Plots
# ================================================================================

# Build plots here, Example:
plots:
#	cd data; \
	for variant in A B C; do \
		./fetch_data.sh $$variant; \
		./do_plots.sh $$variant; \
	done
