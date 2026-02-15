CC ?= cc
CFLAGS ?= -Wall -Wextra -Werror -pedantic -std=c99 -O2

.PHONY: all clean

all: supervisor-runas supervisor-logd

supervisor-runas: supervisor-runas.c
	$(CC) $(CFLAGS) -o $@ $<

supervisor-logd: supervisor-logd.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f supervisor-runas supervisor-logd
