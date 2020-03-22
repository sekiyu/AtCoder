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

int h, w;
int dp[100][100];

int minPath(int i, int j, const vector< vector<bool> >& ss) {
  if (dp[i][j] >= 0) return dp[i][j];
  if (i == h - 1 && j == w - 1 ) {
    return dp[i][j] = 0;
  }
  if (i == h - 1) {
    return dp[i][j] = ((ss[i][j] && !ss[i][j + 1]) ? 1 : 0) + minPath(i, j + 1, ss);
  }
  if (j == w - 1) {
    return dp[i][j] = ((ss[i][j] && !ss[i + 1][j]) ? 1 : 0) + minPath(i + 1, j, ss);
  }
  return dp[i][j] = min(
    ((ss[i][j] && !ss[i + 1][j]) ? 1 : 0) + minPath(i + 1, j, ss),
    ((ss[i][j] && !ss[i][j + 1]) ? 1 : 0) + minPath(i, j + 1, ss)
  );
}

int solve(const vector< vector<bool> >& ss) {
  return minPath(0, 0, ss) + (ss[0][0] ? 0 : 1);
}

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);
  cin >> h >> w;
  vector< vector<bool> > ss(h, vector<bool>(w));
  rep(i, h) rep(j, w) {
    char s;
    cin >> s;
    ss[i][j] = s == '.';
  }

  for(auto& v : dp) for(auto& x : v) x = -1;  
  cout << solve(ss) << endl;
  return 0;
}


