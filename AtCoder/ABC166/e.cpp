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

  int n;
  cin >> n;
  map<ll, ll> dp;
  ll count = 0;
  rep(i, n) {
    int a;
    cin >> a;
    int k = i + 1 - a;
    if (dp.count(k) > 0) count += dp[k];
    int m = i + 1 + a;
    if (dp.count(m) > 0) dp[m] += 1;
    else dp[i + 1 + a] = 1;
  }
  cout << count << endl;
}