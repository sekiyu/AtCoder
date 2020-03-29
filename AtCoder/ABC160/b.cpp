#include <cmath>
#include <cstdio>
#include <vector>
#include <map>
#include <queue>
#include <list>
#include <iostream>
#include <algorithm>
using namespace std;
#define rep(i, n) for (int i = 0; i < (n); ++i)
using ll = long long;
using P = pair<int, int>;
const int INF = 10000000;

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);

  int x;
  cin >> x;
  int fh = x / 500;
  int f = (x % 500) / 5;
  cout << fh * 1000 + f * 5 << endl;
}