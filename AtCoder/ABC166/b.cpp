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

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);

  int n,k;
  cin >> n >> k;
  vector< vector<int> > as(k);
  for (auto & v : as) {
    int d;
    cin >> d;
    v = vector<int>(d);
    for (auto & e : v) cin >> e;
  }

  vector<int> snk(n, 0);
  for (const auto v : as) for (const auto x : v) ++snk[x - 1];
  int count = 0;
  for (const auto s : snk) count += s == 0 ? 1 : 0;
  cout << count << endl;

}