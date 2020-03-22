#include <iostream>
#include <vector>
#include <cmath>
#include <utility>
#include <queue>
#include <map>
#include <list>
#include <set>
#define rep(i, n) for (int i = 0; i < (n); ++i)
using namespace std;
using ll = long long;

const static int MAX_N = 200000;
vector<ll> c(MAX_N, 0);

ll count(ll x) {
  return x * (x - 1) / 2;
}

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);
  int n;
  cin >> n;
  vector<int> as(n);
  ll total = 0;
  rep(i, n) {
    ll a;
    cin >> a;
    as[i] = a;
    ++c[a];
  }
  for (const auto x : c) {
    total += count(x);
  }

  for (const auto a : as) {
    cout << total - count(c[a]) + count(c[a] - 1) << endl;
  }

  return 0;
}


