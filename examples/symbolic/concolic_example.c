
uint64_t main() {
  uint64_t a;
  uint64_t* x1;
  uint64_t* x2;
  uint64_t* x3;
  uint64_t* x4;
  uint64_t* x5;

  x1 = malloc(8);
  x2 = malloc(8);
  x3 = malloc(8);
  x4 = malloc(8);
  x5 = malloc(8);

  read(0, x1, 8);
  read(0, x2, 8);
  read(0, x3, 8);
  read(0, x4, 8);
  read(0, x5, 8);

  if (*x1 == 1) {
    exit(1);
  }

  if (*x2 > 12) {
    exit(1);
  }

  if (*x3 < 3) {
    exit(1);
  }

  if (*x4 >= 14) {
    exit(1);
  }

  if (*x5 <= 5) {
    exit(1);
  }
}
