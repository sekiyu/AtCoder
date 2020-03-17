// HackerRank Find the nearest clone
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

const static int MAX_V = 10e6;
int color[MAX_V];
map< int, list<int> > edges;
// vector< vector<int> > edges;

int bfs(int t) {
  queue<pair<int, int>> q;
  q.push(pair<int, int>(t, 0));
  set<int> visited;
  while (!q.empty()) {
    const auto p = q.front();
    q.pop();
    visited.insert(p.first);
    for (int e : edges[p.first]) {
      if (visited.find(e) != visited.end()) continue;
      if (color[t - 1] == color[e - 1]) {
        return p.second + 1;
      }
      q.push(pair<int, int>(e, p.second + 1));
    }
  }
  return MAX_V;
}

int main() {
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);
  int n, m;
  cin >> n >> m;
  rep(i, m) {
    int x, y;
    cin >> x >> y;
    edges[x].push_back(y);
    edges[y].push_back(x);
  }
  rep(i, n) {
    int c;
    cin >> c;
    color[i] = c;
  }
  int target;
  cin >> target;
  vector<int> targets;
  rep(i, n) {
    if (color[i] == target) targets.push_back(i + 1);
  }

  int dist = MAX_V;
  for (const auto t : targets) {
    dist = min(dist, bfs(t));
  }
  if (dist == MAX_V) dist = -1;
  cout << dist << endl;
  return 0;
}


