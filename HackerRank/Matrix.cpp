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

struct UnionFind {
  vector<int> data;
  UnionFind(int size) : data(size, -1) { }
  bool unite(int x, int y) {
    x = root(x); y = root(y);
    if (x != y) {
      if (data[y] < data[x]) swap(x, y);
      data[x] += data[y]; data[y] = x;
    }
    return x != y;
  }
  bool find(int x, int y) {
    return root(x) == root(y);
  }
  int root(int x) {
    return data[x] < 0 ? x : data[x] = root(data[x]);
  }
  int size(int x) {
    return -data[root(x)];
  }
};

int solve(list<int> machines, list< vector<int> > edges) {
  int n = edges.size() + 1;
  edges.sort([](const auto& x, const auto& y) {return x[2] > y[2]; } );
  UnionFind uf(n);
  vector<bool> isMachine(n, false);
  for (int m : machines) isMachine[m] = true;

  int ret = 0;
  for (const auto& e : edges) {
    int x = e[0];
    int y = e[1];
    int w = e[2];
    // if (uf.find(x, y)) continue;
    if (isMachine[uf.root(x)] && isMachine[uf.root(y)]) {
      ret += w;
      continue;
    }
    if (isMachine[uf.root(x)] || isMachine[uf.root(y)]) {
      isMachine[uf.root(x)] = true;
      isMachine[uf.root(y)] = true;
    }
    uf.unite(x, y);
  }
  return ret;
}

// int bfs(int i, vector<bool> isMachine, map<int, list<P> > edges) {
//   int ret = 0;
//   queue<P> q;
//   q.push(P(i, 0));
//   vector<bool> visited(edges.size(), false);
//   visited[i] = true;
//   while(!q.empty()) {
//     const auto v = q.front();
//     q.pop();
//     visited[v.first] = true;
//     if (isMachine[v.first]) {
//       ret += v.second;
//     }
//     // cout << "visiting " << v.first << endl;
//     for (const auto e : edges[v.first]) {
//       int node = e.first;
//       if (visited[node]) continue;
//       int w = e.second;
//       if (isMachine[v.first]) {
//         q.push(P(node, w));
//       } else {
//         q.push(P(node, min(v.second, w)));
//       }
//     }
//   }
//   return ret;
// }

int main()
{
  std::cin.tie(0);
  std::ios::sync_with_stdio(false);
  int n, k;
  cin >> n >> k;
  // map<int, list<P> > edges;
  list< vector<int> > edges(n - 1, vector<int>(3));
  rep(i, n - 1) {
    int x, y, w;
    cin >> x >> y >> w;
    edges.push_back({x, y, w});
    // edges[x].push_back(P(y, w));
    // edges[y].push_back(P(x, w));
  }
  list<int> machines;
  rep(i, k) {
    int j;
    cin >> j;
    machines.push_back(j);
  }

  cout << solve(machines, edges) << endl;

  // rep(i, n) {
  //   if (!isMachine[i]) continue;
  //   cout << bfs(i, isMachine, edges) << endl;
  //   return 0;
  // }

  return 0;
}
