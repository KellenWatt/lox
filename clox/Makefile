CC := gcc
BUILD_DIR := build
SOURCE_DIR := src

NAME := clox

CFLAGS := -std=c99
CFLAGS += -Wall -Wextra -Werror -Wno-unused-parameter

ifeq ($(MODE), release)
	CFLAGS += -O3
	OUTPUT_DIR := $(BUILD_DIR)/release
else
	CFLAGS += -O0 -DDEBUG -g
	OUTPUT_DIR := $(BUILD_DIR)/debug
endif

OBJ_DIR := $(OUTPUT_DIR)/obj
BIN := $(OUTPUT_DIR)/$(NAME)

# Generates the collections of source/output files
HEADERS := $(wildcard $(SOURCE_DIR)/*.h)
SOURCES := $(wildcard $(SOURCE_DIR)/*.c)
OBJECTS := $(addprefix $(OBJ_DIR)/, $(notdir $(SOURCES:.c=.o)))

.PHONY: all clean run

all: $(OBJ_DIR) $(BIN)

run: all
	@ $(BIN)

clean:
	@rm -r $(BUILD_DIR)

$(OBJ_DIR):
	@mkdir -p $(OBJ_DIR)

# execute gcc on all of the generated .o files ($^) and compile into binary ($@)
$(BIN): $(OBJECTS)
	$(CC) $(CFLAGS) $^ -o $@

# gcc on all source files ($<) and compile into .o $($@)
$(OBJ_DIR)/%.o: $(SOURCE_DIR)/%.c $(HEADERS)
	$(CC) -c $(CFLAGS) -o $@ $<
	
