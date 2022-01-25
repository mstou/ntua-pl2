#define MAXN 1000000
#define MAXV 2000000

int samenum(int N, int *x) {
  int p[MAXV+1];
  for (int i = 0; i <= MAXV; ++i) p[i] = -1;

  int best = 0;

  for (int i = 0; i < N; ++i)
    if (p[x[i]] == -1) p[x[i]] = i;
    else if (i-p[x[i]] > best) best = i-p[x[i]];

  return best;
}
