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

ll func(ll a, ll b, ll x) {
  return floor(a * x / b) - a * floor(x / b);
} 

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);

  ll a, b, n;
  cin >> a >> b >> n;
  if (b - 1 < n) {
    cout << func(a, b, b -1) << endl;
  } else {
    cout << func(a, b, n) << endl;
  }

  // rep(i, b + 1) cout << func(a, b, i) << " ";

}