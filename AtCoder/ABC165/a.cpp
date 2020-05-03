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

  int k,a,b;
  cin >> k >> a >> b;
  if (a / k < b / k || a % k == 0 || b % k == 0) cout << "OK" << endl;
  else cout << "NG" << endl;
} 