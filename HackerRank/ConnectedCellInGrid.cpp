// #include <bits/stdc++.h>
#include <iostream>
#include <vector>
#include <cmath>
#include <utility>
#include <queue>
#include <stack>
#include <map>
#include <list>
#include <set>
#define rep(i, n) for (int i = 0; i < (n); ++i)
using namespace std;
using ll = long long;
using P = pair<int, int>;

list<P> edges(int i, int j, int n, int m) {
  list<P> ret;
  ret.push_back(P(i + 1, j + 1));
  ret.push_back(P(i + 1, j));
  ret.push_back(P(i + 1, j - 1));
  ret.push_back(P(i, j + 1));
  ret.push_back(P(i, j - 1));
  ret.push_back(P(i - 1, j + 1));
  ret.push_back(P(i - 1, j));
  ret.push_back(P(i - 1, j - 1));
  auto pred = [n, m] (P p) { return p.first < 0 || p.second < 0 || n <= p.first || m <= p.second; };
  ret.remove_if(pred);
  return ret;
}

// Complete the maxRegion function below.
int maxRegion(const vector<vector<int>>& grid) {
  int n = grid.size();
  int m = grid[0].size();
  vector< vector<bool> > visited(n);
  for (auto &v : visited) {
    vector<bool> x(m, false);
    v = x;
  }

  int mx = 0;
  rep(i, n) rep(j, m) {
    if (visited[i][j] || grid[i][j] == 0) continue;
    // cout << i << ", " << j << endl;
    visited[i][j] = true;
    stack<P> s;
    s.push(P(i, j));
    int region = 0;
    while(!s.empty()) {
      P v = s.top();
      s.pop();
      ++region;
      for (const auto e : edges(v.first, v.second, n, m)) {
        if (visited[e.first][e.second] || grid[e.first][e.second] == 0) continue;
        // cout << "visit " << e.first << ", " << e.second << endl;
        visited[e.first][e.second] = true;
        s.push(e);
      }
    }
    mx = max(mx, region);
  }
  return mx;

}

int main()
{
    // ofstream fout(getenv("OUTPUT_PATH"));

    int n;
    cin >> n;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    int m;
    cin >> m;
    cin.ignore(numeric_limits<streamsize>::max(), '\n');

    vector<vector<int>> grid(n);
    for (int i = 0; i < n; i++) {
        grid[i].resize(m);

        for (int j = 0; j < m; j++) {
            cin >> grid[i][j];
        }

        cin.ignore(numeric_limits<streamsize>::max(), '\n');
    }

    int res = maxRegion(grid);

    cout << res << endl;
    // fout << res << "\n";

    // fout.close();

    return 0;
}
