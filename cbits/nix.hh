#include <stdint.h>
#include <stddef.h>

struct string {
  char const * data;

  size_t size;
};

struct strings {
  struct string * data;

  size_t size;
};

struct PathInfo {
    struct string deriver;
    struct string narHash;
    uint64_t narSize;
    struct strings references;
    struct strings sigs;
};
