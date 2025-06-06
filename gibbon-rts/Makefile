# ======================================================================
# Arguments:
# ~~~~~~~~~~~~
#
# MODE      = release | debug
# VERBOSITY = 1 | 2 | 3
# GC        = gen | nongen
#
# Toggles:
# ~~~~~~~~~~~~~
#
# GCSTATS
# POINTER
# PARALLEL
# BUMPALLOC
#
#
# GC toggles:
# ~~~~~~~~~~~~~
#
# BURN                      = 0 | 1
# COMPACT                   = 0 | 1
# EAGER_PROMOTION           = 0 | 1
# SIMPLE_WRITE_BARRIER      = 0 | 1
#
# ======================================================================


CC        := gcc
AR        := gcc-ar
CFLAGS    := -Wall -Wextra -Wpedantic -Wshadow -Werror -std=gnu11 -flto
RSC       := cargo
RSFLAGS   := -v
VERBOSITY := 1
MODE      := release

EAGER_PROMOTION      ?= 1
BURN                 ?= 1
COMPACT              ?= 1
SIMPLE_WRITE_BARRIER ?= 0

CFLAGS += -D_GIBBON_VERBOSITY=$(VERBOSITY)

ifeq ($(MODE), debug)
	CFLAGS += -O0 -g -D_GIBBON_DEBUG
else
	CFLAGS += -O3
	RSFLAGS += --release
endif

ifeq ($(shell test $(VERBOSITY) -gt 2; echo $$?),0)
	RSFLAGS += --features=verbose_evac
endif

ifeq ($(GC), nongen)
	CFLAGS += -D_GIBBON_GENGC=0
endif

ifeq ($(GCSTATS), 1)
	CFLAGS += -D_GIBBON_GCSTATS
	RSFLAGS += --features=gcstats
endif

ifeq ($(POINTER), 1)
	CFLAGS += -D_GIBBON_POINTER
endif

ifeq ($(PARALLEL), 1)
	CFLAGS += -fcilkplus -D_GIBBON_PARALLEL
endif

ifeq ($(BUMPALLOC), 1)
	CFLAGS += -D_GIBBON_BUMPALLOC_LISTS -D_GIBBON_BUMPALLOC_HEAP
endif

ifeq ($(EAGER_PROMOTION), 1)
	CFLAGS += -D_GIBBON_EAGER_PROMOTION=1
else
	CFLAGS += -D_GIBBON_EAGER_PROMOTION=0
	RSFLAGS += --features=disable_eager_promotion
endif

ifeq ($(SIMPLE_WRITE_BARRIER), 1)
	CFLAGS += -D_GIBBON_SIMPLE_WRITE_BARRIER=1
else
	CFLAGS += -D_GIBBON_SIMPLE_WRITE_BARRIER=0
endif

ifeq ($(BURN), 0)
	RSFLAGS += --features=noburn
endif

ifeq ($(COMPACT), 0)
	RSFLAGS += --features=nocompact
endif

# Add user passed flags at the end so that they take precedence.
CFLAGS += $(USER_CFLAGS)

# Assume current directory if not set.
GIBBONDIR         ?= ./
RUST_RTS_DIR      := $(GIBBONDIR)/gibbon-rts
C_RTS_DIR         := $(RUST_RTS_DIR)/rts-c
BUILD_DIR         := $(RUST_RTS_DIR)/build
NAME              := gibbon_rts
RUST_RTS_SO       := libgibbon_rts_ng.so
RUST_RTS_PATH     := $(RUST_RTS_DIR)/target/$(MODE)/$(RUST_RTS_SO)
RUST_SOURCES      := $(shell find $(RUST_RTS_DIR) -type f -name *.rs)


all: rts

rts: c_rts rs_rts

c_rts: $(BUILD_DIR)/lib$(NAME).a $(BUILD_DIR)/$(NAME).o $(BUILD_DIR)/$(NAME).h

rs_rts: $(BUILD_DIR)/$(RUST_RTS_SO)

test: t_rs_rts

t_rs_rts: $(RUST_SOURCES)
	cd  $(RUST_RTS_DIR) && \
	$(RSC) test -- --nocapture

$(BUILD_DIR)/$(RUST_RTS_SO): $(RUST_RTS_PATH)
	mkdir -p $(BUILD_DIR) && \
	mv $^ $@

# The Rust sources are not specified as prerequisites and a FORCE
# prerequisite is used to force unconditional recompilation.
# The reason is that otherwise we're missing recompilations
# due to changes in the Make variables.
# See https://github.com/iu-parfunc/gibbon/issues/244.
$(RUST_RTS_PATH): FORCE
	cd  $(RUST_RTS_DIR) && \
	$(RSC) build -p gibbon-rts-ng $(RSFLAGS)

$(BUILD_DIR)/lib%.a: $(BUILD_DIR)/%.o
	$(AR) crs $@ $^

$(BUILD_DIR)/%.o: $(C_RTS_DIR)/%.o
	mkdir -p $(BUILD_DIR) && \
	mv $^ $@

# Same as with the Rust part (see above),
# the C sources are not specified as prerequisites and a FORCE
# prerequisite is used to force unconditional recompilation.
$(C_RTS_DIR)/$(NAME).o: FORCE
	$(CC) $(CFLAGS) -c -o $@ $(C_RTS_DIR)/$(NAME).c

$(BUILD_DIR)/%.h: $(C_RTS_DIR)/%.h
	mkdir -p $(BUILD_DIR) && \
	ln -s $^ $@

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

clean:
	rm -rf $(BUILD_DIR) $(RUST_RTS_DIR)/target

# See https://www.gnu.org/software/make/manual/make.html#Force-Targets
FORCE:

.PHONY: clean
