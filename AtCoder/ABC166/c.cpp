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

  int n,m;
  cin >> n >> m;
  vector<int> hs(n);
  for (auto & x : hs ) cin >> x;

  vector< priority_queue<int> > adj(n);
  rep(i, m) {
    int a, b;
    cin >> a >> b;
    adj[a-1].push(hs[b-1]);
    adj[b-1].push(hs[a-1]);
  }
  int count = 0;
  rep(i, n) {
    if (adj[i].empty()) {
      ++count;
      continue;
    }
    count += adj[i].top() < hs[i] ? 1 : 0;
  }
  cout << count << endl;
}