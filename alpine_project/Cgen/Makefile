CC = gcc
CFLAGS = -std=c11

all: output

output: output.c builtin.h
	@$(CC) $(CFLAGS) $< -o $@
	@echo
	@./output
	@echo
	@rm -rf output

