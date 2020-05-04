#include <cmath>
#include <cstdio>
#include <vector>
#include <map>
#include <queue>
#include <list>
#include <iostream>
#include <algorithm>
using namespace std;
using ll = long long;
#define rep(i, n) for (ll i = 0; i < (n); ++i)
using P = pair<int, int>;

vector< int64_t > divisor(int64_t n) {
  vector< int64_t > ret;
  for(int64_t i = 1; i * i <= n; i++) {
    if(n % i == 0) {
      ret.push_back(i);
      if(i * i != n) ret.push_back(n / i);
    }
  }
  sort(begin(ret), end(ret));
  return (ret);
}

ll func(ll a, ll b) {
  return pow(a, 4) + pow(a, 3) * b + pow(a, 2) * pow(b, 2) + a * pow(b, 3) + pow(b, 4);
}

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);

  ll x;
  cin >> x;

  for (const auto d : divisor(x)) {
    ll start = - d / 2;
    const ll upper = 200;
    const ll lower = -upper;

    ll left = start;
    ll right = upper;

    while(right - left > 1) {
      // cout << d << ", " << left << ", " << right << endl;
      ll m = (left + right) / 2;
      if (func(m + d, m) * d > x) left = m;
      else right = m;
    }
    if (func(left + d, left) * d == x) {
        cout << left + d << " " << left << endl;
        return 0;
    }

    left = lower;
    right = start;

    while(right - left > 1) {
      // cout << d << ", " << left << ", " << right << endl;
      ll m = (left + right) / 2;
      if (func(m + d, m) * d < x) right = m;
      else left = m;
    }
    if (func(left + d, left) * d == x) {
        cout << left + d << " " << left << endl;
        return 0;
    }
  }


}