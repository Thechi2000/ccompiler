int test() {
  int a = 3;
  int b = 3;
  return a + b;
}

int main() {
  int a = 5;
  int b = 2;
  int c = 0;

  while (c < a) {
    c += b;
  }

  do {
    c += b;
  } while (c < a);

  if (a > c) {
    a = b;
  } else {
    a = c;
  }

  return c;
}