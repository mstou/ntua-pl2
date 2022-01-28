 // Miltiadis Stouras 03116022
 //
 // command: frama-c -wp -wp-prover alt-ergo -wp-rte -wp-timeout 60 -wp-verbose 0 samenum_s.c -then -report
 // Frama-C version: 24.0 (Chromium)
 // alt-ergo version: 2.4.1

#define MAXN 1000000
#define MAXV 2000000

/*@
  @ predicate exists_pair_at_distance_d(integer d, integer n, int* x) =
  @  \exists integer i,j;
  @        0 <= i < j <= n && x[i] == x[j] ==> d == j-i;
  @*/

/*@
  @ predicate max_distance(integer distance, integer n, int *x) =
  @  exists_pair_at_distance_d(distance, n, x) &&
  @  \forall integer i,j;
  @     0 <= i < j <= n && x[i] == x[j] ==> distance >= j-i;
  @*/

/*@ requires 1 <= N <= MAXN;
  @ requires \valid(x + (0 .. N-1));
  @ requires \forall integer i; 0 <= i < N ==> 1 <= x[i] <= MAXV;
  @
  @ assigns \nothing;
  @
  @ ensures max_distance(\result, N-1, x);
  @*/
int samenum(int N, int *x) {
  int p[MAXV+1];

  /*@ loop invariant 0 <= i <= MAXV+1;
    @ loop invariant \forall integer j; 0 <= j < i ==> p[j] == -1;
    @
    @ loop assigns i, p[0 .. MAXV];
    @
    @ loop variant MAXV-i+1;
    @*/
  for (int i = 0; i <= MAXV; ++i) p[i] = -1;

  int best = 0;

  /*
    1) Invariant for the index i

    2) In every iteration, we have either entered the if-clause
    (ie p[x[j]] will become j) or it held that 'best >= (j-p[j])'
    because if it was the case that 'best < j-p[x[j]]' then we
    entered the else-if-clause

    3) It will always hold that p[x[j]] >= -1
    This trivially holds but Frama-C wants it declared
    to be able to prove the absence of singed_overflow errors

    4) If for some value l in [0...MAXV], p[l] is different than -1,
    then we have found the element in the array

    5) p[l] denotes the earliest occurance of the value l.
    If two indices j1 < j2 have the same value, then p[x[j2]] <= j1

    6) The best answer so far is at least as much as the distance
    between j and the first occurance of x[j]
  */

  /*@ loop invariant 0 <= i <= N;
    @ loop invariant \forall integer j; 0 <= j < i ==> (p[x[j]]==j || best >= j-p[x[j]]);
    @ loop invariant \forall integer j; 0 <= j < N ==> p[x[j]] >= -1;
    @ loop invariant \forall integer l; 0 <= l <= MAXV && p[l] >= 0 ==> \exists integer i; 0 <= i < N && x[i] == l;
    @ loop invariant \forall integer j1,j2; 0 <= j1 <= j2 < i && x[j1] == x[j2] ==> p[x[j2]] <= j1;
    @ loop invariant \forall integer j; 0 <= j < i ==> best >= j - p[x[j]];
		@
    @ loop assigns i, best, p[0 .. MAXV];
    @
    @ loop variant N-i+1;
    @*/
  for (int i = 0; i < N; ++i)
    if (p[x[i]] == -1) p[x[i]] = i;
    else if (i-p[x[i]] > best) best = i-p[x[i]];

  return best;
}
