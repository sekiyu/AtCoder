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

  int n,x,y;
  cin >> n >> x >> y;
  vector< vector<int> > d(n, vector<int>(n + 2, INF));
  rep(i, n-1) for (int j = i + 1; j < n; ++j) {
    d[i + 1][j + 1] = j - i;
  }
  // --x;
  // --y;
  d[x][y] = 1;
  for(int i = x; i > 0; --i) for(int j = y; j > 0; --j) {
    if (i >= j) continue;
    if (i == x && j == y) continue;
    int mn = min(d[i+1][j], d[i][j+1]) + 1;
    d[i][j] = min(mn, d[i][j]);
  }
  for(int i = x; i <= n; ++i) for(int j = y; j > 0; --j) {
    if (i >= j) continue;
    if (i == x && j == y) continue;
    int mn = min(d[i-1][j], d[i][j+1]) + 1;
    d[i][j] = min(mn, d[i][j]);
  }
  for(int i = x; i > 0; --i) for(int j = y; j <= n; ++j) {
    if (i >= j) continue;
    if (i == x && j == y) continue;
    int mn = min(d[i+1][j], d[i][j-1]) + 1;
    d[i][j] = min(mn, d[i][j]);
  }
  for(int i = x; i <= n; ++i) for(int j = y; j <= n; ++j) {
    if (i >= j) continue;
    if (i == x && j == y) continue;
    int mn = min(d[i-1][j], d[i][j-1]) + 1;
    d[i][j] = min(mn, d[i][j]);
  }


  vector<int> ret(n-1, 0);
  rep(i, n-1) for (int j = i + 1; j < n; ++j) {
    // cout << i << "," << j << "," << d[i][j] << endl;
    ret[d[i + 1][j + 1] - 1] += 1;
    // if (d[i + 1][j + 1] > 5) cout << i << "," << j << "," << d[i][j] << endl;
  }
  for (auto v : ret) cout << v << endl;

}